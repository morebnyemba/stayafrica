from rest_framework import viewsets, status, serializers as drf_serializers
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from django.core.exceptions import ValidationError
from django.db import transaction
from apps.bookings.models import Booking
from apps.bookings.serializers import BookingSerializer
from services.payment_gateway_enhanced import PaymentGatewayService
from utils.validators import validate_booking_dates
from utils.helpers import is_booking_date_available, calculate_nights, calculate_booking_total
from utils.decorators import api_ratelimit, log_action
from services.audit_logger import AuditLoggerService
import logging

logger = logging.getLogger(__name__)

# Try to import Celery tasks, but gracefully handle if Celery isn't available
try:
    from tasks.email_tasks import send_booking_confirmation_email
    CELERY_AVAILABLE = True
except (ImportError, ModuleNotFoundError) as e:
    logger.warning(f"Celery tasks not available: {e}")
    CELERY_AVAILABLE = False
    send_booking_confirmation_email = None

class BookingViewSet(viewsets.ModelViewSet):
    serializer_class = BookingSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        """Return bookings for current user based on their active_profile, or all for admin"""
        user = self.request.user
        if user.is_staff:
            qs = Booking.objects.all().select_related('guest', 'rental_property__host')
            search = self.request.query_params.get('search')
            if search:
                from django.db.models import Q
                qs = qs.filter(Q(booking_ref__icontains=search) | Q(guest__email__icontains=search))
        else:
            active_profile = getattr(user, 'active_profile', user.role)
            if active_profile == 'host':
                qs = Booking.objects.filter(rental_property__host=user).select_related('guest', 'rental_property')
            else:
                qs = Booking.objects.filter(guest=user).select_related('rental_property__host')

        # Status filter applies to all roles
        status_filter = self.request.query_params.get('status')
        if status_filter:
            qs = qs.filter(status=status_filter)
        return qs
    
    @transaction.atomic
    @log_action('create_booking')
    def perform_create(self, serializer):
        """Create booking with comprehensive validation and fee calculation"""
        property_obj = serializer.validated_data['rental_property']
        check_in = serializer.validated_data['check_in']
        check_out = serializer.validated_data['check_out']
        active_profile = getattr(self.request.user, 'active_profile', getattr(self.request.user, 'role', 'guest'))

        if not self.request.user.is_staff and property_obj.host_id == self.request.user.id:
            raise drf_serializers.ValidationError({'detail': 'You cannot book your own property'})

        if not self.request.user.is_staff and active_profile == 'host':
            raise drf_serializers.ValidationError(
                {'detail': 'Switch to travel mode before booking another host\'s property'}
            )
        
        # Validate booking dates
        try:
            validate_booking_dates(check_in, check_out)
        except ValidationError as e:
            logger.error(f"Date validation failed: {str(e)}")
            raise drf_serializers.ValidationError({'detail': str(e)})
        
        # Check availability
        if not is_booking_date_available(property_obj, check_in, check_out):
            logger.warning(f"Property {property_obj.id} not available for {check_in} to {check_out}")
            raise drf_serializers.ValidationError({'detail': 'Property is not available for the selected dates'})
        
        # Check if property is active
        if property_obj.status != 'active':
            raise drf_serializers.ValidationError({'detail': 'Property is not available for booking'})
        
        # Calculate totals with dynamic pricing
        nights = calculate_nights(check_in, check_out)
        totals = calculate_booking_total(
            property_obj.price_per_night,
            nights,
            cleaning_fee=serializer.validated_data.get('cleaning_fee', property_obj.cleaning_fee if hasattr(property_obj, 'cleaning_fee') else 0),
            property_obj=property_obj,
            check_in=check_in,
            check_out=check_out
        )
        
        # Create booking with calculated values
        booking = serializer.save(
            guest=self.request.user,
            number_of_guests=serializer.validated_data.get('number_of_guests', 1),
            nightly_total=totals['nightly_total'],
            service_fee=totals['service_fee'],
            commission_fee=totals['commission_fee'],
            cleaning_fee=totals['cleaning_fee'],
            taxes=totals.get('taxes', 0),
            grand_total=totals['grand_total'],
            currency=property_obj.currency
        )
        
        # Check for instant booking
        instant_confirmed = False
        try:
            from services.instant_booking_service import InstantBookingService
            confirmed, reason = InstantBookingService.auto_confirm_booking(booking)
            instant_confirmed = confirmed
            if confirmed:
                logger.info(f"Booking {booking.booking_ref} instantly confirmed: {reason}")
        except Exception as e:
            logger.warning(f"Instant booking check failed: {e}")
        
        # Log the action
        from django.contrib.contenttypes.models import ContentType
        content_type = ContentType.objects.get_for_model(Booking)
        AuditLoggerService.log_action(
            user=self.request.user,
            action='create',
            content_type=content_type,
            object_id=booking.id,
            changes={
                'booking_ref': booking.booking_ref,
                'property': property_obj.title,
                'instant_confirmed': instant_confirmed
            }
        )
        
        # Send confirmation email if Celery is available
        if CELERY_AVAILABLE and send_booking_confirmation_email:
            try:
                send_booking_confirmation_email.delay(str(booking.id))
            except Exception as e:
                logger.warning(f"Could not queue booking confirmation email: {e}")
        else:
            logger.info("Celery not available, skipping confirmation email")
        
        # Send push notification (only if not instant-confirmed, as auto_confirm_booking already sends it)
        if not instant_confirmed:
            try:
                from services.notification_service import NotificationService
                NotificationService.send_booking_confirmation(booking)
            except Exception as e:
                logger.warning(f"Could not send push notification: {e}")
        
        logger.info(f"Booking created: {booking.booking_ref} (instant_confirmed={instant_confirmed})")
    
    @action(detail=False, methods=['post'], permission_classes=[IsAuthenticated])
    def calculate_total(self, request):
        """Preview booking price with dynamic pricing rules applied.
        POST /api/v1/bookings/calculate_total/
        Body: { rental_property, check_in, check_out, number_of_guests }
        """
        from apps.properties.models import Property
        from services.pricing_service import PricingService

        property_id = request.data.get('rental_property')
        check_in = request.data.get('check_in')
        check_out = request.data.get('check_out')
        guests = request.data.get('number_of_guests', 1)

        if not property_id or not check_in or not check_out:
            return Response(
                {'error': 'rental_property, check_in, and check_out are required'},
                status=status.HTTP_400_BAD_REQUEST
            )

        try:
            property_obj = Property.objects.get(id=property_id)
        except Property.DoesNotExist:
            return Response({'error': 'Property not found'}, status=status.HTTP_404_NOT_FOUND)

        try:
            nights = calculate_nights(check_in, check_out)
            if nights <= 0:
                return Response({'error': 'Check-out must be after check-in'}, status=status.HTTP_400_BAD_REQUEST)

            # Get dynamic pricing breakdown
            dynamic = PricingService.calculate_price_for_booking(property_obj, check_in, check_out)

            # Also get the full booking total (with service fee, commission)
            totals = calculate_booking_total(
                property_obj.price_per_night, nights,
                cleaning_fee=getattr(property_obj, 'cleaning_fee', 0) or 0,
                property_obj=property_obj,
                check_in=check_in, check_out=check_out
            )

            return Response({
                'base_price_per_night': dynamic['base_price_per_night'],
                'adjusted_price_per_night': dynamic['adjusted_price_per_night'],
                'nights': nights,
                'nightly_total': float(totals['nightly_total']),
                'service_fee': float(totals['service_fee']),
                'cleaning_fee': float(totals['cleaning_fee']),
                'taxes': float(totals['taxes']),
                'grand_total': float(totals['grand_total']),
                'applied_rules': dynamic['applied_rules'],
                'fee_breakdown': dynamic.get('fee_breakdown', []),
                'tax_breakdown': dynamic.get('tax_breakdown', []),
                'currency': property_obj.currency,
            })
        except Exception as e:
            logger.error(f"Error calculating booking total: {e}")
            return Response({'error': str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)

    @action(detail=True, methods=['post'])
    @log_action('approve_booking')
    def approve(self, request, pk=None):
        """Approve a booking request (host only)"""
        booking = self.get_object()
        
        # Only host can approve
        if request.user != booking.rental_property.host and not request.user.is_admin_user:
            return Response(
                {'error': 'Only the host can approve bookings'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        if booking.status != 'requested':
            return Response(
                {'error': 'Can only approve requested bookings'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Check if still available (no conflicts, excluding current booking)
        if not is_booking_date_available(booking.rental_property, booking.check_in, booking.check_out, exclude_booking_id=booking.id):
            return Response(
                {'error': 'Booking dates are no longer available'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        with transaction.atomic():
            booking.status = 'pending'
            booking.save()
            
            # Log the action
            from django.contrib.contenttypes.models import ContentType
            content_type = ContentType.objects.get_for_model(Booking)
            AuditLoggerService.log_action(
                user=request.user,
                action='approve',
                content_type=content_type,
                object_id=booking.id,
                changes={'status': 'pending'}
            )
            
            # Send push notification to guest
            try:
                from services.notification_service import NotificationService
                NotificationService.send_booking_confirmation(booking)
            except Exception as e:
                logger.warning(f"Could not send push notification: {e}")
        
        # Send confirmation email
        if CELERY_AVAILABLE and send_booking_confirmation_email:
            try:
                send_booking_confirmation_email.delay(str(booking.id))
            except Exception as e:
                logger.warning(f"Could not queue confirmation email: {e}")
        
        # Trigger automated welcome message from host
        try:
            from services.automated_messaging_service import AutomatedMessagingService
            AutomatedMessagingService.trigger_automated_message('booking_confirmed', booking=booking)
        except Exception as e:
            logger.warning(f"Could not trigger automated message: {e}")
        
        logger.info(f"Booking approved: {booking.booking_ref}")
        return Response({'status': 'pending', 'booking_ref': booking.booking_ref})
    
    @action(detail=True, methods=['post'])
    @log_action('cancel_booking')
    def cancel(self, request, pk=None):
        """Cancel a booking"""
        booking = self.get_object()
        
        # Check permissions
        if request.user != booking.guest and request.user != booking.rental_property.host and not request.user.is_admin_user:
            return Response(
                {'error': 'You do not have permission to cancel this booking'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        if booking.status not in ['requested', 'pending', 'confirmed']:
            return Response(
                {'error': 'Cannot cancel completed or cancelled bookings'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        with transaction.atomic():
            old_status = booking.status
            booking.status = 'cancelled'
            booking.save()
            
            # Log the action
            from django.contrib.contenttypes.models import ContentType
            content_type = ContentType.objects.get_for_model(Booking)
            AuditLoggerService.log_action(
                user=request.user,
                action='cancel',
                content_type=content_type,
                object_id=booking.id,
                changes={'status': old_status + ' -> cancelled'}
            )
        
        # Auto-refund if payment was successful
        refund_info = None
        if hasattr(booking, 'payment') and booking.payment.status == 'success':
            payment = booking.payment
            cancelled_by = 'host' if request.user == booking.rental_property.host else 'guest'
            reason = f'Booking cancelled by {cancelled_by}'
            
            from services.payment_gateway_enhanced import PaymentGatewayService
            payment_service = PaymentGatewayService()

            # Try gateway refund first for supported providers
            if payment.provider in PaymentGatewayService.GATEWAY_REFUND_SUPPORTED:
                result = payment_service.process_refund(
                    provider=payment.provider,
                    gateway_ref=payment.gateway_ref,
                    currency=payment.currency,
                    reason=reason,
                )
                if result.get('success'):
                    payment.status = 'refunded'
                    payment.save()
                    refund_info = {'method': 'gateway', 'provider': payment.provider}
                else:
                    # Gateway refund failed — fall back to wallet
                    from apps.payments.views import PaymentViewSet
                    if PaymentViewSet._credit_wallet(payment, payment.amount, reason):
                        payment.status = 'refunded'
                        payment.save()
                        refund_info = {'method': 'wallet'}
            else:
                # Provider doesn't support refunds — credit wallet
                from apps.payments.views import PaymentViewSet
                if PaymentViewSet._credit_wallet(payment, payment.amount, reason):
                    payment.status = 'refunded'
                    payment.save()
                    refund_info = {'method': 'wallet'}

            if refund_info:
                logger.info(f"Auto-refund processed for {booking.booking_ref}: {refund_info}")
            else:
                logger.error(f"Auto-refund FAILED for {booking.booking_ref}")
        
        logger.info(f"Booking cancelled: {booking.booking_ref}")
        
        # Send cancellation notifications
        try:
            from tasks.notification_tasks import send_push_notification
            cancelled_by = 'host' if request.user == booking.rental_property.host else 'guest'
            # Notify guest
            send_push_notification.delay(
                booking.guest.id,
                'Booking Cancelled',
                f'Your booking {booking.booking_ref} at {booking.rental_property.title} has been cancelled.',
                {'booking_id': str(booking.id), 'type': 'booking_cancelled'}
            )
            # Notify host
            send_push_notification.delay(
                booking.rental_property.host.id,
                'Booking Cancelled',
                f'Booking {booking.booking_ref} has been cancelled by {cancelled_by}.',
                {'booking_id': str(booking.id), 'type': 'booking_cancelled'}
            )
        except Exception as e:
            logger.warning(f"Could not send cancellation notifications: {e}")
        
        response_data = {'status': 'cancelled', 'booking_ref': booking.booking_ref}
        if refund_info:
            response_data['refund'] = refund_info
        return Response(response_data)
    
    @action(detail=True, methods=['post'])
    @log_action('complete_booking')
    def complete(self, request, pk=None):
        """Mark booking as completed (after checkout)"""
        booking = self.get_object()
        
        # Only host or admin can mark as complete
        if request.user != booking.rental_property.host and not request.user.is_admin_user:
            return Response(
                {'error': 'Only the host can complete bookings'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        if booking.status not in ('confirmed', 'checked_out'):
            return Response(
                {'error': 'Can only complete confirmed or checked-out bookings'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Check if past checkout date
        from datetime import date
        if booking.check_out > date.today():
            return Response(
                {'error': 'Cannot complete booking before checkout date'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        with transaction.atomic():
            booking.status = 'completed'
            if not booking.checked_out_at:
                from django.utils.timezone import now as tz_now
                booking.checked_out_at = tz_now()
            booking.save()
            
            # Log the action
            from django.contrib.contenttypes.models import ContentType
            content_type = ContentType.objects.get_for_model(Booking)
            AuditLoggerService.log_action(
                user=request.user,
                action='complete',
                content_type=content_type,
                object_id=booking.id,
                changes={'status': 'completed'}
            )
        
        # Send completion notifications
        try:
            from tasks.notification_tasks import send_push_notification
            # Notify guest
            send_push_notification.delay(
                booking.guest.id,
                'Stay Completed',
                f'Your stay at {booking.rental_property.title} is complete. We hope you enjoyed it!',
                {'booking_id': str(booking.id), 'type': 'booking_completed'}
            )
            # Notify host
            send_push_notification.delay(
                booking.rental_property.host.id,
                'Booking Completed',
                f'Booking {booking.booking_ref} has been marked as completed.',
                {'booking_id': str(booking.id), 'type': 'booking_completed'}
            )
        except Exception as e:
            logger.warning(f"Could not send completion notifications: {e}")
        
        # Trigger automated checkout / review-request message
        try:
            from services.automated_messaging_service import AutomatedMessagingService
            AutomatedMessagingService.trigger_automated_message('booking_completed', booking=booking)
        except Exception as e:
            logger.warning(f"Could not trigger automated message: {e}")
        
        logger.info(f"Booking completed: {booking.booking_ref}")
        return Response({'status': 'completed', 'booking_ref': booking.booking_ref})

    @action(detail=True, methods=['post'])
    @log_action('checkin_booking')
    def checkin(self, request, pk=None):
        """
        Mark guest as checked in.
        Host can check in a guest, or guest can self-check-in on/after the check-in date.
        Optionally accepts check_in_instructions and access_code from host.
        """
        booking = self.get_object()
        is_guest = request.user == booking.guest
        is_host = request.user == booking.rental_property.host
        is_admin = request.user.is_admin_user

        if not (is_guest or is_host or is_admin):
            return Response(
                {'error': 'Only the guest, host, or admin can check in'},
                status=status.HTTP_403_FORBIDDEN
            )

        if booking.status != 'confirmed':
            return Response(
                {'error': 'Can only check in confirmed bookings'},
                status=status.HTTP_400_BAD_REQUEST
            )

        from datetime import date
        if booking.check_in > date.today():
            return Response(
                {'error': 'Cannot check in before the check-in date'},
                status=status.HTTP_400_BAD_REQUEST
            )

        with transaction.atomic():
            from django.utils.timezone import now as tz_now
            booking.status = 'checked_in'
            booking.checked_in_at = tz_now()

            # Host can attach instructions / access code
            if is_host or is_admin:
                if 'check_in_instructions' in request.data:
                    booking.check_in_instructions = request.data['check_in_instructions']
                if 'access_code' in request.data:
                    booking.access_code = request.data['access_code']

            booking.save()

            from django.contrib.contenttypes.models import ContentType
            content_type = ContentType.objects.get_for_model(Booking)
            AuditLoggerService.log_action(
                user=request.user,
                action='checkin',
                content_type=content_type,
                object_id=booking.id,
                changes={'status': 'checked_in', 'by': 'guest' if is_guest else 'host'}
            )

        # Notifications
        try:
            from tasks.notification_tasks import send_push_notification
            if is_guest:
                send_push_notification.delay(
                    booking.rental_property.host.id,
                    'Guest Checked In',
                    f'{booking.guest.first_name or booking.guest.email} has checked in at {booking.rental_property.title}.',
                    {'booking_id': str(booking.id), 'type': 'guest_checked_in'}
                )
            else:
                send_push_notification.delay(
                    booking.guest.id,
                    'You\'re Checked In!',
                    f'Welcome to {booking.rental_property.title}! Enjoy your stay.',
                    {'booking_id': str(booking.id), 'type': 'guest_checked_in'}
                )
        except Exception as e:
            logger.warning(f"Could not send check-in notification: {e}")

        try:
            from services.automated_messaging_service import AutomatedMessagingService
            AutomatedMessagingService.trigger_automated_message('guest_checked_in', booking=booking)
        except Exception as e:
            logger.warning(f"Could not trigger check-in message: {e}")

        logger.info(f"Booking checked in: {booking.booking_ref}")
        return Response({
            'status': 'checked_in',
            'booking_ref': booking.booking_ref,
            'checked_in_at': booking.checked_in_at.isoformat(),
        })

    @action(detail=True, methods=['post'])
    @log_action('checkout_booking')
    def checkout(self, request, pk=None):
        """
        Mark guest as checked out.
        Host or admin marks checkout. Guests cannot self-checkout.
        """
        booking = self.get_object()
        is_host = request.user == booking.rental_property.host
        is_admin = request.user.is_admin_user
        is_guest = request.user == booking.guest

        if not (is_host or is_admin or is_guest):
            return Response(
                {'error': 'Only the host or admin can check out a guest'},
                status=status.HTTP_403_FORBIDDEN
            )

        if booking.status != 'checked_in':
            return Response(
                {'error': 'Can only check out a checked-in booking'},
                status=status.HTTP_400_BAD_REQUEST
            )

        with transaction.atomic():
            from django.utils.timezone import now as tz_now
            booking.status = 'checked_out'
            booking.checked_out_at = tz_now()
            booking.save()

            from django.contrib.contenttypes.models import ContentType
            content_type = ContentType.objects.get_for_model(Booking)
            AuditLoggerService.log_action(
                user=request.user,
                action='checkout',
                content_type=content_type,
                object_id=booking.id,
                changes={'status': 'checked_out'}
            )

        # Notifications
        try:
            from tasks.notification_tasks import send_push_notification
            send_push_notification.delay(
                booking.guest.id,
                'Checked Out',
                f'Thank you for staying at {booking.rental_property.title}! We hope you had a great time.',
                {'booking_id': str(booking.id), 'type': 'guest_checked_out'}
            )
            send_push_notification.delay(
                booking.rental_property.host.id,
                'Guest Checked Out',
                f'Booking {booking.booking_ref} has been checked out.',
                {'booking_id': str(booking.id), 'type': 'guest_checked_out'}
            )
        except Exception as e:
            logger.warning(f"Could not send checkout notification: {e}")

        try:
            from services.automated_messaging_service import AutomatedMessagingService
            AutomatedMessagingService.trigger_automated_message('booking_completed', booking=booking)
        except Exception as e:
            logger.warning(f"Could not trigger checkout message: {e}")

        logger.info(f"Booking checked out: {booking.booking_ref}")
        return Response({
            'status': 'checked_out',
            'booking_ref': booking.booking_ref,
            'checked_out_at': booking.checked_out_at.isoformat(),
        })

    @action(detail=True, methods=['patch'], url_path='check-in-info')
    def check_in_info(self, request, pk=None):
        """Host updates check-in instructions / access code without changing status."""
        booking = self.get_object()
        is_host = request.user == booking.rental_property.host
        is_admin = request.user.is_admin_user

        if not (is_host or is_admin):
            return Response(
                {'error': 'Only the host can update check-in info'},
                status=status.HTTP_403_FORBIDDEN
            )

        if booking.status in ('cancelled', 'completed'):
            return Response(
                {'error': 'Cannot update check-in info for this booking'},
                status=status.HTTP_400_BAD_REQUEST
            )

        if 'check_in_instructions' in request.data:
            booking.check_in_instructions = request.data['check_in_instructions']
        if 'access_code' in request.data:
            booking.access_code = request.data['access_code']
        booking.save(update_fields=['check_in_instructions', 'access_code', 'updated_at'])

        return Response({
            'check_in_instructions': booking.check_in_instructions,
            'access_code': booking.access_code,
        })
