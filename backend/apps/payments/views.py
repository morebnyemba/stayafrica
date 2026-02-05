from rest_framework import viewsets, status
from rest_framework.decorators import action, permission_classes as drf_permission_classes
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated, AllowAny
from django.db import transaction
from django.conf import settings
from apps.payments.models import Payment
from apps.payments.serializers import PaymentSerializer
from services.payment_gateway_enhanced import PaymentGatewayService
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
            return Payment.objects.all().select_related('booking__guest', 'booking__rental_property')
        return Payment.objects.filter(booking__guest=user).select_related('booking__rental_property')
    
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
            booking = Booking.objects.select_related('rental_property').get(
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
        
        # Initiate payment with provider SDK
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
            
            # Call provider SDK to initiate payment
            result = payment_service.initiate_payment(
                payment,
                booking,
                provider,
                request.user.email,
                request.user.get_full_name()
            )
            
            if not result['success']:
                return Response(
                    {'error': result.get('error', 'Payment initialization failed')},
                    status=status.HTTP_500_INTERNAL_SERVER_ERROR
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
            
            # Add SDK response data
            response_data = PaymentSerializer(payment).data
            if 'checkout_url' in result:
                response_data['checkout_url'] = result['checkout_url']
            if 'redirect_url' in result:
                response_data['redirect_url'] = result['redirect_url']
            if 'payment_link' in result:
                response_data['payment_link'] = result['payment_link']
        
        logger.info(f"Payment initiated: {payment.gateway_ref}")
        return Response(response_data, status=status.HTTP_201_CREATED)
    
    @action(detail=False, methods=['post'], permission_classes=[AllowAny])
    @api_ratelimit(rate='100/h')
    def webhook(self, request):
        """Handle payment provider webhooks with SDK signature verification"""
        provider = request.data.get('provider') or request.GET.get('provider')
        
        if not provider:
            logger.error("Webhook received without provider")
            return Response(
                {'error': 'Provider not specified'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        payment_service = PaymentGatewayService()
        
        # Verify webhook using SDK methods
        if provider == 'stripe':
            signature = request.headers.get('Stripe-Signature')
            if not signature:
                return Response({'error': 'Missing signature'}, status=status.HTTP_403_FORBIDDEN)
            
            event = payment_service.verify_stripe_webhook(request.body, signature)
            if not event:
                return Response({'error': 'Invalid signature'}, status=status.HTTP_403_FORBIDDEN)
            
            # Extract payment info from Stripe event
            if event['type'] == 'checkout.session.completed':
                session = event['data']['object']
                gateway_ref = session['id']
                payment_status = 'succeeded'
            else:
                return Response({'status': 'ignored'})
        
        elif provider == 'paypal':
            # Verify PayPal webhook
            if not payment_service.verify_paypal_webhook(dict(request.headers), request.body.decode('utf-8')):
                return Response({'error': 'Invalid signature'}, status=status.HTTP_403_FORBIDDEN)
            
            # Extract payment info from PayPal webhook
            event_type = request.data.get('event_type')
            resource = request.data.get('resource', {})
            gateway_ref = resource.get('id')
            
            if event_type == 'PAYMENT.SALE.COMPLETED':
                payment_status = 'completed'
            elif event_type in ['PAYMENT.SALE.DENIED', 'PAYMENT.SALE.REFUNDED']:
                payment_status = 'failed'
            else:
                return Response({'status': 'ignored'})
        
        else:
            # For other providers, use existing signature verification
            signature = (
                request.headers.get('X-Webhook-Signature') or
                request.headers.get('X-Paynow-Signature') or
                request.headers.get('X-PayFast-Signature') or
                request.headers.get('X-Flutterwave-Signature')
            )
            
            from apps.admin_dashboard.models import SystemConfiguration
            config = SystemConfiguration.get_config()
            
            webhook_secrets = {
                'paynow': config.paynow_webhook_secret,
                'payfast': config.payfast_webhook_secret,
                'flutterwave': getattr(config, 'flutterwave_webhook_secret', ''),
                'ozow': '',
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
            
            gateway_ref = request.data.get('gateway_ref') or request.data.get('reference') or request.data.get('tx_ref')
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
            if payment_status in ['success', 'completed', 'paid', 'succeeded', 'successful']:
                payment.status = 'success'
                # Update booking status
                payment.booking.status = 'confirmed'
                payment.booking.save()
                
                # Send receipt email
                send_payment_receipt_email.delay(payment.id)
                
            elif payment_status in ['failed', 'cancelled', 'declined', 'canceled']:
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
    
    @action(detail=True, methods=['post'], permission_classes=[IsAuthenticated])
    def refund(self, request, pk=None):
        """Admin action to refund a payment"""
        if not request.user.is_staff:
            return Response(
                {'error': 'Only admin users can process refunds'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        payment = self.get_object()
        amount = request.data.get('amount')
        
        if payment.status != 'completed':
            return Response(
                {'error': 'Only completed payments can be refunded'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # TODO: Integrate with actual payment gateway to process refund
        # For now, just update status
        payment.status = 'refunded'
        payment.save()
        
        # Log the action
        from django.contrib.contenttypes.models import ContentType
        from services.audit_logger import AuditLoggerService
        content_type = ContentType.objects.get_for_model(Payment)
        AuditLoggerService.log_action(
            user=request.user,
            action='refund',
            content_type=content_type,
            object_id=payment.id,
            changes={
                'status': 'refunded',
                'refund_amount': amount or payment.amount,
                'refunded_by': request.user.id
            }
        )
        
        serializer = self.get_serializer(payment)
        return Response(serializer.data)


class WalletViewSet(viewsets.ModelViewSet):
    """ViewSet for managing user wallets"""
    permission_classes = [IsAuthenticated]
    
    def get_serializer_class(self):
        from apps.payments.serializers import WalletSerializer
        return WalletSerializer
    
    def get_queryset(self):
        """Return wallet for current user or all if admin"""
        from apps.payments.models import Wallet
        user = self.request.user
        if user.is_admin_user:
            return Wallet.objects.all().select_related('user')
        return Wallet.objects.filter(user=user)
    
    @action(detail=False, methods=['get'])
    def my_wallet(self, request):
        """Get current user's wallet"""
        from services.transaction_service import TransactionService
        wallet = TransactionService.create_wallet(request.user)
        serializer = self.get_serializer(wallet)
        return Response(serializer.data)
    
    @action(detail=True, methods=['get'])
    def balance(self, request, pk=None):
        """Get wallet balance"""
        wallet = self.get_object()
        return Response({
            'balance': wallet.balance,
            'currency': wallet.currency,
            'status': wallet.status
        })
    
    @action(detail=True, methods=['get'])
    @api_ratelimit(rate='20/m')
    def transactions(self, request, pk=None):
        """Get wallet transactions"""
        from apps.payments.models import WalletTransaction
        from apps.payments.serializers import WalletTransactionSerializer
        
        wallet = self.get_object()
        transactions = WalletTransaction.objects.filter(wallet=wallet).order_by('-created_at')
        
        # Apply filters
        txn_type = request.query_params.get('type')
        if txn_type:
            transactions = transactions.filter(txn_type=txn_type)
        
        txn_status = request.query_params.get('status')
        if txn_status:
            transactions = transactions.filter(status=txn_status)
        
        # Paginate
        from rest_framework.pagination import PageNumberPagination
        paginator = PageNumberPagination()
        paginator.page_size = 20
        page = paginator.paginate_queryset(transactions, request)
        
        serializer = WalletTransactionSerializer(page, many=True)
        return paginator.get_paginated_response(serializer.data)


class WalletTransactionViewSet(viewsets.ReadOnlyModelViewSet):
    """ViewSet for viewing wallet transactions"""
    permission_classes = [IsAuthenticated]
    
    def get_serializer_class(self):
        from apps.payments.serializers import WalletTransactionSerializer
        return WalletTransactionSerializer
    
    def get_queryset(self):
        """Return transactions for current user's wallet"""
        from apps.payments.models import WalletTransaction
        user = self.request.user
        if user.is_admin_user:
            return WalletTransaction.objects.all().select_related('wallet__user', 'booking')
        return WalletTransaction.objects.filter(wallet__user=user).select_related('wallet', 'booking')


class BankAccountViewSet(viewsets.ModelViewSet):
    """ViewSet for managing bank accounts"""
    permission_classes = [IsAuthenticated]
    
    def get_serializer_class(self):
        from apps.payments.serializers import BankAccountSerializer
        return BankAccountSerializer
    
    def get_queryset(self):
        """Return bank accounts for current user"""
        from apps.payments.models import BankAccount
        return BankAccount.objects.filter(user=self.request.user)
    
    def perform_create(self, serializer):
        """Set user to current user"""
        serializer.save(user=self.request.user)
    
    @action(detail=True, methods=['post'])
    def set_primary(self, request, pk=None):
        """Set bank account as primary"""
        from apps.payments.models import BankAccount
        account = self.get_object()
        
        # Unset other primary accounts
        with transaction.atomic():
            BankAccount.objects.filter(user=request.user, is_primary=True).update(is_primary=False)
            account.is_primary = True
            account.save(update_fields=['is_primary', 'updated_at'])
        
        logger.info(f"Set bank account {account.id} as primary for user {request.user.id}")
        serializer = self.get_serializer(account)
        return Response(serializer.data)


class WithdrawalViewSet(viewsets.ModelViewSet):
    """ViewSet for managing withdrawals"""
    permission_classes = [IsAuthenticated]
    http_method_names = ['get', 'post', 'head', 'options']  # No update/delete
    
    def get_serializer_class(self):
        from apps.payments.serializers import WithdrawalSerializer, WithdrawalCreateSerializer
        if self.action == 'create':
            return WithdrawalCreateSerializer
        return WithdrawalSerializer
    
    def get_queryset(self):
        """Return withdrawals for current user or all if admin"""
        from apps.payments.models import Withdrawal
        user = self.request.user
        if user.is_admin_user:
            return Withdrawal.objects.all().select_related('wallet__user', 'bank_account')
        return Withdrawal.objects.filter(wallet__user=user).select_related('wallet', 'bank_account')
    
    @transaction.atomic
    @api_ratelimit(rate='5/m')
    @log_action('initiate_withdrawal')
    def create(self, request, *args, **kwargs):
        """Initiate a withdrawal"""
        from services.transaction_service import TransactionService, TransactionError, InsufficientBalanceError
        
        serializer = self.get_serializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        
        wallet = serializer.validated_data['wallet']
        bank_account = serializer.validated_data['bank_account']
        amount = serializer.validated_data['amount']
        
        # Verify wallet belongs to user
        if wallet.user != request.user:
            return Response(
                {'error': 'You can only withdraw from your own wallet'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        try:
            withdrawal = TransactionService.initiate_withdrawal(wallet, bank_account, amount)
            
            # Log the action
            from django.contrib.contenttypes.models import ContentType
            from apps.payments.models import Withdrawal
            content_type = ContentType.objects.get_for_model(Withdrawal)
            AuditLoggerService.log_action(
                user=request.user,
                action='initiate_withdrawal',
                content_type=content_type,
                object_id=withdrawal.id,
                changes={
                    'amount': str(amount),
                    'reference': withdrawal.reference
                }
            )
            
            from apps.payments.serializers import WithdrawalSerializer
            result_serializer = WithdrawalSerializer(withdrawal)
            return Response(result_serializer.data, status=status.HTTP_201_CREATED)
            
        except InsufficientBalanceError as e:
            logger.warning(f"Insufficient balance for withdrawal: {str(e)}")
            return Response(
                {'error': str(e)},
                status=status.HTTP_400_BAD_REQUEST
            )
        except TransactionError as e:
            logger.error(f"Withdrawal error: {str(e)}")
            return Response(
                {'error': str(e)},
                status=status.HTTP_400_BAD_REQUEST
            )
    
    @action(detail=True, methods=['post'], permission_classes=[IsAuthenticated])
    @log_action('complete_withdrawal')
    def complete(self, request, pk=None):
        """Complete a withdrawal (admin only)"""
        if not request.user.is_admin_user:
            return Response(
                {'error': 'Only admins can complete withdrawals'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        from services.transaction_service import TransactionService, TransactionError
        
        notes = request.data.get('notes', '')
        
        try:
            withdrawal = TransactionService.complete_withdrawal(pk, notes)
            serializer = self.get_serializer(withdrawal)
            return Response(serializer.data)
        except TransactionError as e:
            return Response(
                {'error': str(e)},
                status=status.HTTP_400_BAD_REQUEST
            )
    
    @action(detail=True, methods=['post'], permission_classes=[IsAuthenticated])
    @log_action('fail_withdrawal')
    def fail(self, request, pk=None):
        """Fail a withdrawal and reverse transaction (admin only)"""
        if not request.user.is_admin_user:
            return Response(
                {'error': 'Only admins can fail withdrawals'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        from services.transaction_service import TransactionService, TransactionError
        
        reason = request.data.get('reason', '')
        
        try:
            withdrawal = TransactionService.fail_withdrawal(pk, reason)
            serializer = self.get_serializer(withdrawal)
            return Response(serializer.data)
        except TransactionError as e:
            return Response(
                {'error': str(e)},
                status=status.HTTP_400_BAD_REQUEST
            )
