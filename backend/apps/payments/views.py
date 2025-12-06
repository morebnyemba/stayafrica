from rest_framework import viewsets, status
from rest_framework.decorators import action, permission_classes as drf_permission_classes
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated, AllowAny
from django.db import transaction
from django.conf import settings
from apps.payments.models import Payment
from apps.payments.serializers import PaymentSerializer
from services.payment_gateway import PaymentGatewayService
from utils.decorators import api_ratelimit, log_action
from utils.helpers import verify_webhook_signature
from tasks.email_tasks import send_payment_receipt_email
from services.audit_logger import AuditLoggerService
import logging
import json

logger = logging.getLogger(__name__)

class PaymentViewSet(viewsets.ModelViewSet):
    serializer_class = PaymentSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        """Return payments for current user"""
        user = self.request.user
        if user.is_admin_user:
            return Payment.objects.all().select_related('booking__guest', 'booking__property')
        return Payment.objects.filter(booking__guest=user).select_related('booking__property')
    
    @action(detail=False, methods=['post'])
    @api_ratelimit(rate='5/m')
    @log_action('initiate_payment')
    def initiate(self, request):
        """Initiate a payment with comprehensive validation"""
        booking_id = request.data.get('booking_id')
        provider = request.data.get('provider')
        
        if not booking_id or not provider:
            return Response(
                {'error': 'booking_id and provider are required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        try:
            from apps.bookings.models import Booking
            booking = Booking.objects.select_related('property').get(
                id=booking_id,
                guest=request.user
            )
        except Booking.DoesNotExist:
            logger.warning(f"Booking {booking_id} not found for user {request.user.id}")
            return Response(
                {'error': 'Booking not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        # Check if booking is in valid state
        if booking.status not in ['pending', 'confirmed']:
            return Response(
                {'error': 'Booking is not in a valid state for payment'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Check if payment already exists
        if hasattr(booking, 'payment'):
            existing_payment = booking.payment
            if existing_payment.status == 'success':
                return Response(
                    {'error': 'Payment already completed for this booking'},
                    status=status.HTTP_400_BAD_REQUEST
                )
            elif existing_payment.status == 'initiated':
                # Return existing payment
                serializer = PaymentSerializer(existing_payment)
                return Response(serializer.data)
        
        # Check if payment provider is available for user's country
        payment_service = PaymentGatewayService()
        available_providers = payment_service.get_available_providers(
            request.user.country_of_residence or 'International'
        )
        
        if provider not in available_providers:
            return Response(
                {
                    'error': f'Payment provider not available for your country',
                    'available_providers': available_providers
                },
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Create payment record
        with transaction.atomic():
            import uuid
            payment = Payment.objects.create(
                booking=booking,
                provider=provider,
                gateway_ref=f'{booking.booking_ref}-{provider}-{uuid.uuid4().hex[:8]}',
                amount=booking.grand_total,
                currency=booking.currency,
                status='initiated'
            )
            
            # Log the action
            AuditLoggerService.log_action(
                user=request.user,
                action='initiate',
                model=Payment,
                object_id=payment.id,
                changes={
                    'gateway_ref': payment.gateway_ref,
                    'provider': provider,
                    'amount': str(booking.grand_total)
                }
            )
        
        logger.info(f"Payment initiated: {payment.gateway_ref}")
        serializer = PaymentSerializer(payment)
        return Response(serializer.data, status=status.HTTP_201_CREATED)
    
    @action(detail=False, methods=['post'], permission_classes=[AllowAny])
    @api_ratelimit(rate='100/h')
    def webhook(self, request):
        """Handle payment provider webhooks with signature verification"""
        provider = request.data.get('provider') or request.GET.get('provider')
        
        if not provider:
            logger.error("Webhook received without provider")
            return Response(
                {'error': 'Provider not specified'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Get signature from headers (different providers use different header names)
        signature = (
            request.headers.get('X-Webhook-Signature') or
            request.headers.get('X-Paynow-Signature') or
            request.headers.get('X-PayFast-Signature') or
            request.headers.get('Stripe-Signature')
        )
        
        # Verify webhook signature if available (from system config)
        from apps.admin_dashboard.models import SystemConfiguration
        config = SystemConfiguration.get_config()
        
        webhook_secrets = {
            'paynow': config.paynow_webhook_secret,
            'payfast': config.payfast_webhook_secret,
            'stripe': config.stripe_webhook_secret,
            'ozow': '',  # Not configured yet
        }
        webhook_secret = webhook_secrets.get(provider.lower())
        if webhook_secret and signature:
            payload = json.dumps(request.data)
            if not verify_webhook_signature(payload, signature, webhook_secret):
                logger.error(f"Invalid webhook signature from {provider}")
                return Response(
                    {'error': 'Invalid signature'},
                    status=status.HTTP_403_FORBIDDEN
                )
        
        # Extract payment details
        gateway_ref = request.data.get('gateway_ref') or request.data.get('reference')
        payment_status = request.data.get('status') or request.data.get('payment_status')
        
        if not gateway_ref:
            logger.error("Webhook received without gateway_ref")
            return Response(
                {'error': 'Gateway reference not provided'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        try:
            payment = Payment.objects.select_related('booking__guest').get(
                gateway_ref=gateway_ref,
                provider=provider
            )
        except Payment.DoesNotExist:
            logger.error(f"Payment not found for gateway_ref: {gateway_ref}")
            return Response(
                {'error': 'Payment not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        # Update payment status
        with transaction.atomic():
            old_status = payment.status
            
            # Map provider status to our status
            if payment_status in ['success', 'completed', 'paid', 'succeeded']:
                payment.status = 'success'
                # Update booking status
                payment.booking.status = 'confirmed'
                payment.booking.save()
                
                # Send receipt email
                send_payment_receipt_email.delay(payment.id)
                
            elif payment_status in ['failed', 'cancelled', 'declined']:
                payment.status = 'failed'
            else:
                payment.status = 'pending'
            
            payment.save()
            
            # Log the action
            AuditLoggerService.log_action(
                user=payment.booking.guest,
                action='webhook_update',
                model=Payment,
                object_id=payment.id,
                changes={
                    'status': f'{old_status} -> {payment.status}',
                    'provider': provider
                }
            )
        
        logger.info(f"Webhook processed for payment: {gateway_ref}, status: {payment.status}")
        return Response({'status': 'received', 'payment_status': payment.status})
    
    @action(detail=True, methods=['get'])
    def status(self, request, pk=None):
        """Get payment status"""
        payment = self.get_object()
        return Response({
            'gateway_ref': payment.gateway_ref,
            'status': payment.status,
            'provider': payment.provider,
            'amount': payment.amount,
            'currency': payment.currency,
            'created_at': payment.created_at,
        })
