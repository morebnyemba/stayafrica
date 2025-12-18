from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from django.core.exceptions import ValidationError
from django.db import transaction
from apps.bookings.models import Booking
from apps.bookings.serializers import BookingSerializer
from services.payment_gateway import PaymentGatewayService
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
except Exception as e:
    logger.warning(f"Celery tasks not available: {e}")
    CELERY_AVAILABLE = False
    send_booking_confirmation_email = None

class BookingViewSet(viewsets.ModelViewSet):
    serializer_class = BookingSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        """Return bookings for current user"""
        user = self.request.user
        if user.is_host:
            return Booking.objects.filter(rental_property__host=user).select_related('guest', 'rental_property')
        return Booking.objects.filter(guest=user).select_related('rental_property__host')
    
    @transaction.atomic
    @log_action('create_booking')
    def perform_create(self, serializer):
        """Create booking with comprehensive validation and fee calculation"""
        property_obj = serializer.validated_data['rental_property']
        check_in = serializer.validated_data['check_in']
        check_out = serializer.validated_data['check_out']
        
        # Validate booking dates
        try:
            validate_booking_dates(check_in, check_out)
        except ValidationError as e:
            logger.error(f"Date validation failed: {str(e)}")
            raise ValidationError(str(e))
        
        # Check availability
        if not is_booking_date_available(property_obj, check_in, check_out):
            logger.warning(f"Property {property_obj.id} not available for {check_in} to {check_out}")
            raise ValidationError("Property is not available for the selected dates")
        
        # Check if property is active
        if property_obj.status != 'active':
            raise ValidationError("Property is not available for booking")
        
        # Calculate totals
        nights = calculate_nights(check_in, check_out)
        totals = calculate_booking_total(
            property_obj.price_per_night,
            nights,
            cleaning_fee=serializer.validated_data.get('cleaning_fee', 0)
        )
        
        # Create booking with calculated values
        booking = serializer.save(
            guest=self.request.user,
            nightly_total=totals['nightly_total'],
            service_fee=totals['service_fee'],
            commission_fee=totals['commission_fee'],
            grand_total=totals['grand_total'],
            currency=property_obj.currency
        )
        
        # Log the action
        from django.contrib.contenttypes.models import ContentType
        content_type = ContentType.objects.get_for_model(Booking)
        AuditLoggerService.log_action(
            user=self.request.user,
            action='create',
            content_type=content_type,
            object_id=booking.id,
            changes={'booking_ref': booking.booking_ref, 'property': property_obj.title}
        )
        
        # Send confirmation email if Celery is available
        if CELERY_AVAILABLE and send_booking_confirmation_email:
            try:
                send_booking_confirmation_email.delay(booking.id)
            except Exception as e:
                logger.warning(f"Could not queue booking confirmation email: {e}")
        else:
            logger.info("Celery not available, skipping confirmation email")
        
        logger.info(f"Booking created: {booking.booking_ref}")
    
    @action(detail=True, methods=['post'])
    @api_ratelimit(rate='20/h')
    @log_action('confirm_booking')
    def confirm(self, request, pk=None):
        """Confirm a pending booking (host only)"""
        booking = self.get_object()
        
        # Only host can confirm
        if request.user != booking.rental_property.host and not request.user.is_admin_user:
            return Response(
                {'error': 'Only the host can confirm bookings'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        if booking.status != 'pending':
            return Response(
                {'error': 'Can only confirm pending bookings'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Check if still available (no conflicts, excluding current booking)
        if not is_booking_date_available(booking.rental_property, booking.check_in, booking.check_out, exclude_booking_id=booking.id):
            return Response(
                {'error': 'Booking dates are no longer available'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        with transaction.atomic():
            booking.status = 'confirmed'
            booking.save()
            
            # Log the action
            AuditLoggerService.log_action(
                user=request.user,
                action='confirm',
                model=Booking,
                object_id=booking.id,
                changes={'status': 'confirmed'}
            )
        
        logger.info(f"Booking confirmed: {booking.booking_ref}")
        return Response({'status': 'confirmed', 'booking_ref': booking.booking_ref})
    
    @action(detail=True, methods=['post'])
    @api_ratelimit(rate='20/h')
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
        
        if booking.status not in ['pending', 'confirmed']:
            return Response(
                {'error': 'Cannot cancel completed or cancelled bookings'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        with transaction.atomic():
            old_status = booking.status
            booking.status = 'cancelled'
            booking.save()
            
            # Log the action
            AuditLoggerService.log_action(
                user=request.user,
                action='cancel',
                model=Booking,
                object_id=booking.id,
                changes={'status': old_status + ' -> cancelled'}
            )
        
        logger.info(f"Booking cancelled: {booking.booking_ref}")
        return Response({'status': 'cancelled', 'booking_ref': booking.booking_ref})
    
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
        
        if booking.status != 'confirmed':
            return Response(
                {'error': 'Can only complete confirmed bookings'},
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
            booking.save()
            
            # Log the action
            AuditLoggerService.log_action(
                user=request.user,
                action='complete',
                model=Booking,
                object_id=booking.id,
                changes={'status': 'completed'}
            )
        
        logger.info(f"Booking completed: {booking.booking_ref}")
        return Response({'status': 'completed', 'booking_ref': booking.booking_ref})
