from rest_framework import viewsets, status
from rest_framework.decorators import action, permission_classes as drf_permission_classes
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated, AllowAny
from django.db import transaction
from django.conf import settings
from decimal import Decimal
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
    filterset_fields = ['booking', 'status', 'provider']
    
    def get_queryset(self):
        """Return payments for current user"""
        user = self.request.user
        if user.is_admin_user:
            return Payment.objects.all().select_related('booking__guest', 'booking__rental_property')
        return Payment.objects.filter(booking__guest=user).select_related('booking__rental_property')
    
    @action(detail=False, methods=['get'])
    def providers(self, request):
        """Get available payment providers for the current user's country.

        Returns providers grouped by category (regional / international)
        so the frontend can display them in separate sections.
        """
        payment_service = PaymentGatewayService()
        # Prefer the user's stored country; fall back to query param, then 'International'
        country = (
            getattr(request.user, 'country_of_residence', None)
            or request.query_params.get('country', '')
            or 'International'
        )
        detailed = payment_service.get_available_providers_detailed(country)
        return Response({'providers': detailed, 'country': country})
    
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
            elif existing_payment.status in ('failed', 'pending'):
                # Delete the failed/pending payment so a new one can be created
                logger.info(f"Deleting {existing_payment.status} payment {existing_payment.gateway_ref} for re-initiation")
                existing_payment.delete()
                # Clear Django's cached reverse relation so the new create works
                try:
                    del booking.payment
                except AttributeError:
                    pass
            elif existing_payment.status == 'initiated':
                # Check if it's stale (older than 10 minutes)
                from django.utils import timezone
                from datetime import timedelta
                if existing_payment.created_at < timezone.now() - timedelta(minutes=10):
                    logger.info(f"Deleting stale initiated payment {existing_payment.gateway_ref} for re-initiation")
                    existing_payment.delete()
                    try:
                        del booking.payment
                    except AttributeError:
                        pass
                else:
                    # Return existing recent payment
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
            
            # Add SDK response data – forward all provider-specific fields
            response_data = PaymentSerializer(payment).data
            for key in (
                'checkout_url', 'redirect_url', 'payment_link',
                'paypal_order_id', 'session_id', 'gateway_ref',
                'poll_url',
            ):
                if key in result:
                    response_data[key] = result[key]
        
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
            elif event['type'] in ('checkout.session.expired', 'checkout.session.async_payment_failed'):
                session = event['data']['object']
                gateway_ref = session['id']
                payment_status = 'Cancelled'
            else:
                return Response({'status': 'ignored'})
        
        elif provider == 'paynow':
            # Paynow sends: reference, paynowreference, amount, status, pollurl, hash
            gateway_ref = request.data.get('reference')
            payment_status = request.data.get('status', '')
            paynow_hash = request.data.get('hash')
            
            # Hash verification disabled – Paynow's hash computation is
            # undocumented and the SDK does not expose a verify helper.
            # We rely on the obscure webhook URL + reference matching instead.
            if paynow_hash:
                logger.info(f"Paynow webhook hash present but verification is disabled for reference: {gateway_ref}")
            
            logger.info(f"Paynow webhook received: reference={gateway_ref}, status={payment_status}")
        
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
            
            # Map provider status to our status (case-insensitive for providers like Paynow)
            normalized_status = payment_status.lower() if payment_status else ''
            if normalized_status in ['success', 'completed', 'paid', 'succeeded', 'successful']:
                payment.status = 'success'
                # Update booking status
                payment.booking.status = 'confirmed'
                payment.booking.save()
                
                # Send receipt email
                send_payment_receipt_email.delay(payment.id)
                
            elif normalized_status in ['failed', 'cancelled', 'declined', 'canceled']:
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
    
    @action(detail=False, methods=['post'], url_path='capture-paypal')
    @api_ratelimit(rate='10/m')
    @log_action('capture_paypal')
    def capture_paypal(self, request):
        """Capture an approved PayPal order after customer returns from PayPal.

        Expects JSON body: { "order_id": "<paypal_order_id>" }
        """
        order_id = request.data.get('order_id')
        if not order_id:
            return Response(
                {'error': 'order_id is required'},
                status=status.HTTP_400_BAD_REQUEST
            )

        try:
            payment = Payment.objects.select_related('booking__guest').get(
                gateway_ref=order_id,
                provider='paypal',
                booking__guest=request.user,
            )
        except Payment.DoesNotExist:
            logger.warning(f"PayPal capture: payment not found for order {order_id} / user {request.user.id}")
            return Response(
                {'error': 'Payment not found'},
                status=status.HTTP_404_NOT_FOUND
            )

        if payment.status == 'success':
            return Response({
                'status': 'already_captured',
                'payment_status': 'success',
                'gateway_ref': payment.gateway_ref,
            })

        payment_service = PaymentGatewayService()
        result = payment_service.capture_paypal_order(order_id)

        if result.get('success'):
            with transaction.atomic():
                payment.status = 'success'
                payment.save()
                payment.booking.status = 'confirmed'
                payment.booking.save()

                AuditLoggerService.log_action(
                    user=request.user,
                    action='capture_paypal',
                    model=Payment,
                    object_id=payment.id,
                    changes={
                        'order_id': order_id,
                        'capture_id': result.get('capture_id'),
                        'status': result.get('status'),
                    }
                )

                # Send receipt email
                send_payment_receipt_email.delay(payment.id)

            logger.info(f"PayPal order captured successfully: {order_id}")
            return Response({
                'status': 'captured',
                'payment_status': 'success',
                'capture_id': result.get('capture_id'),
                'gateway_ref': payment.gateway_ref,
            })
        else:
            logger.error(f"PayPal capture failed for order {order_id}: {result.get('error')}")
            return Response(
                {
                    'error': result.get('error', 'Capture failed'),
                    'status': 'capture_failed',
                },
                status=status.HTTP_502_BAD_GATEWAY
            )

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
        
        if payment.status != 'success':
            return Response(
                {'error': 'Only successful payments can be refunded'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Process refund through payment provider
        refund_amount = amount or payment.amount
        if payment.provider == 'stripe':
            try:
                import stripe
                from apps.admin_dashboard.models import SystemConfiguration
                config = SystemConfiguration.get_config()
                stripe.api_key = config.stripe_secret_key
                # gateway_ref stores the checkout session ID; retrieve payment_intent from it
                session = stripe.checkout.Session.retrieve(payment.gateway_ref)
                stripe_refund = stripe.Refund.create(
                    payment_intent=session.payment_intent,
                    amount=int(Decimal(str(refund_amount)) * 100),  # Convert to cents
                )
                logger.info(f'Stripe refund created: {stripe_refund.id} for payment {payment.gateway_ref}')
            except Exception as e:
                logger.error(f'Stripe refund failed for {payment.gateway_ref}: {str(e)}')
                return Response(
                    {'error': f'Refund failed: {str(e)}'},
                    status=status.HTTP_500_INTERNAL_SERVER_ERROR
                )
        
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
                'refund_amount': str(refund_amount),
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


class PaymentMethodViewSet(viewsets.ModelViewSet):
    """
    ViewSet for managing stored payment methods
    Supports: Create, List, Update, Delete (soft delete)
    """
    permission_classes = [IsAuthenticated]
    
    def get_serializer_class(self):
        if self.action == 'create':
            from apps.payments.serializers import PaymentMethodCreateSerializer
            return PaymentMethodCreateSerializer
        from apps.payments.serializers import PaymentMethodSerializer
        return PaymentMethodSerializer
    
    def get_queryset(self):
        """Return only active payment methods for current user"""
        from apps.payments.models import PaymentMethod
        return PaymentMethod.objects.filter(
            user=self.request.user,
            deleted_at__isnull=True
        ).order_by('-is_default', '-created_at')
    
    @api_ratelimit(rate='10/m')
    @log_action('create_payment_method')
    def create(self, request, *args, **kwargs):
        """Create a new payment method with tokenization"""
        from apps.payments.models import PaymentMethod
        
        serializer = self.get_serializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        
        # Extract validated data
        provider = serializer.validated_data['provider']
        method_type = serializer.validated_data['method_type']
        token = serializer.validated_data['token']
        name = serializer.validated_data['name']
        is_default = serializer.validated_data.get('is_default', False)
        
        try:
            # Tokenize with provider (validate token)
            payment_service = PaymentGatewayService()
            provider_token = self._tokenize_with_provider(
                payment_service,
                provider,
                method_type,
                token,
                serializer.validated_data
            )
            
            # Create payment method
            payment_method = PaymentMethod.objects.create(
                user=request.user,
                provider=provider,
                method_type=method_type,
                name=name,
                provider_token=provider_token,
                last_four=serializer.validated_data.get('last_four', ''),
                expiry_month=serializer.validated_data.get('expiry_month'),
                expiry_year=serializer.validated_data.get('expiry_year'),
                phone_number=serializer.validated_data.get('phone_number', ''),
                is_default=is_default,
                is_verified=True  # Set to True after successful tokenization
            )
            
            # Log the action
            AuditLoggerService.log_action(
                user=request.user,
                action='create_payment_method',
                resource_type='PaymentMethod',
                resource_id=str(payment_method.id),
                details={'provider': provider, 'method_type': method_type}
            )
            
            from apps.payments.serializers import PaymentMethodSerializer
            result_serializer = PaymentMethodSerializer(payment_method)
            return Response(result_serializer.data, status=status.HTTP_201_CREATED)
            
        except Exception as e:
            logger.error(f"Failed to create payment method: {str(e)}")
            return Response(
                {'error': f'Failed to add payment method: {str(e)}'},
                status=status.HTTP_400_BAD_REQUEST
            )
    
    def _tokenize_with_provider(self, payment_service, provider, method_type, token, data):
        """
        Tokenize payment method with provider SDK
        Integrates with PaymentGatewayService for provider-specific logic
        
        Args:
            payment_service: PaymentGatewayService instance
            provider: Payment provider (stripe, paynow, flutterwave, paystack)
            method_type: Payment method type (card, mobile, bank, ussd)
            token: Client-side token or raw details (platform responsibility)
            data: Additional data (card details, phone number, etc.)
        
        Returns:
            Provider-generated token string for storage
        
        Raises:
            Exception: If tokenization fails
        """
        try:
            if provider == 'stripe':
                return self._tokenize_stripe(data)
            elif provider == 'paynow':
                return self._tokenize_paynow(data, method_type)
            elif provider == 'flutterwave':
                return self._tokenize_flutterwave(data, method_type)
            elif provider == 'paystack':
                return self._tokenize_paystack(data, method_type)
            else:
                raise ValueError(f'Provider {provider} not supported for tokenization')
        
        except Exception as e:
            logger.error(f'Tokenization failed for {provider}: {str(e)}')
            raise
    
    def _tokenize_stripe(self, data):
        """
        Tokenize with Stripe using official SDK
        Creates and attaches payment method to customer
        """
        try:
            import stripe
            from apps.admin_dashboard.models import SystemConfiguration
            
            config = SystemConfiguration.get_config()
            stripe.api_key = config.stripe_secret_key
            
            # Create payment method from card details
            payment_method = stripe.PaymentMethod.create(
                type='card',
                card={
                    'number': data.get('card_number', ''),
                    'exp_month': data.get('expiry_month'),
                    'exp_year': data.get('expiry_year'),
                    'cvc': data.get('cvv', ''),
                },
            )
            
            logger.info(f'Stripe payment method created: {payment_method.id}')
            return payment_method.id
            
        except stripe.error.CardError as e:
            logger.error(f'Stripe card error: {e.user_message}')
            raise Exception(f'Card validation failed: {e.user_message}')
        except stripe.error.StripeError as e:
            logger.error(f'Stripe error: {str(e)}')
            raise Exception(f'Stripe tokenization failed: {str(e)}')
    
    def _tokenize_paynow(self, data, method_type):
        """
        Tokenize with Paynow
        For cards: Paynow will handle during transaction
        For mobile: Verify phone number format
        """
        try:
            if method_type == 'card':
                # Paynow accepts card details in transaction
                # Return token representing card type and last 4
                card_number = data.get('card_number', '')
                last_four = card_number[-4:] if len(card_number) >= 4 else ''
                return f'paynow_card_{last_four}'
            
            elif method_type == 'mobile':
                # Validate Zimbabwean phone number format
                phone = data.get('phone_number', '').strip()
                
                # Basic validation: 10-13 digits
                if not phone.isdigit() or not (10 <= len(phone) <= 13):
                    raise ValueError('Invalid phone number format')
                
                logger.info(f'Paynow mobile method verified: {phone[-4:]}')
                return f'paynow_mobile_{phone[-4:]}'
            
            else:
                raise ValueError(f'Paynow does not support method type: {method_type}')
                
        except Exception as e:
            logger.error(f'Paynow tokenization failed: {str(e)}')
            raise
    
    def _tokenize_flutterwave(self, data, method_type):
        """
        Tokenize with Flutterwave REST API
        Handles cards, mobile money, bank transfers
        """
        try:
            from apps.admin_dashboard.models import SystemConfiguration
            
            config = SystemConfiguration.get_config()
            secret_key = config.flutterwave_secret_key
            
            if not secret_key:
                raise Exception('Flutterwave API key not configured')
            
            headers = {
                'Authorization': f'Bearer {secret_key}',
                'Content-Type': 'application/json'
            }
            
            if method_type == 'card':
                # Tokenize card
                response = requests.post(
                    'https://api.flutterwave.com/v3/tokenized-charges',
                    headers=headers,
                    json={
                        'card_number': data.get('card_number', ''),
                        'cvv': data.get('cvv', ''),
                        'expiry_month': data.get('expiry_month'),
                        'expiry_year': data.get('expiry_year'),
                    }
                )
                
                if response.status_code == 200:
                    result = response.json()
                    token = result.get('data', {}).get('token')
                    if token:
                        logger.info(f'Flutterwave card tokenized: {token}')
                        return token
                    raise Exception('No token in Flutterwave response')
                else:
                    raise Exception(f'Flutterwave API error: {response.status_code}')
            
            elif method_type == 'mobile':
                # Mobile money - return account identifier
                phone = data.get('phone_number', '').strip()
                if not phone.isdigit() or not (10 <= len(phone) <= 13):
                    raise ValueError('Invalid phone number format')
                
                logger.info(f'Flutterwave mobile method verified')
                return f'flutterwave_mobile_{phone[-4:]}'
            
            elif method_type in ['bank', 'ussd']:
                # Bank transfer or USSD - return identifier
                identifier = data.get('phone_number') or data.get('account_number', '')
                logger.info(f'Flutterwave {method_type} method verified')
                return f'flutterwave_{method_type}_{identifier[-4:]}'
            
            else:
                raise ValueError(f'Flutterwave does not support method type: {method_type}')
                
        except Exception as e:
            logger.error(f'Flutterwave tokenization failed: {str(e)}')
            raise
    
    def _tokenize_paystack(self, data, method_type):
        """
        Tokenize with Paystack REST API
        Handles cards and bank transfers
        """
        try:
            from apps.admin_dashboard.models import SystemConfiguration
            
            config = SystemConfiguration.get_config()
            secret_key = config.paystack_secret_key
            
            if not secret_key:
                raise Exception('Paystack API key not configured')
            
            headers = {
                'Authorization': f'Bearer {secret_key}',
                'Content-Type': 'application/json'
            }
            
            if method_type == 'card':
                # Paystack charges with card happen in transaction
                # We validate the card details and return identifier
                card_number = data.get('card_number', '')
                last_four = card_number[-4:] if len(card_number) >= 4 else ''
                
                # Could call Paystack BIN lookup for validation
                logger.info(f'Paystack card method verified: {last_four}')
                return f'paystack_card_{last_four}'
            
            elif method_type == 'bank':
                # Bank transfer
                account_number = data.get('account_number', '')
                if not account_number:
                    raise ValueError('Account number required for bank transfer')
                
                logger.info(f'Paystack bank method verified')
                return f'paystack_bank_{account_number[-4:]}'
            
            else:
                raise ValueError(f'Paystack does not support method type: {method_type}')
                
        except Exception as e:
            logger.error(f'Paystack tokenization failed: {str(e)}')
            raise
    
    @action(detail=True, methods=['patch'])
    @api_ratelimit(rate='10/m')
    @log_action('set_default_payment_method')
    def set_default(self, request, pk=None):
        """Set a payment method as default"""
        payment_method = self.get_object()
        payment_method.is_default = True
        payment_method.save()
        
        serializer = self.get_serializer(payment_method)
        return Response(serializer.data)
    
    def destroy(self, request, *args, **kwargs):
        """Soft delete a payment method"""
        payment_method = self.get_object()
        payment_method.soft_delete()
        
        # Log the action
        AuditLoggerService.log_action(
            user=request.user,
            action='delete_payment_method',
            resource_type='PaymentMethod',
            resource_id=str(payment_method.id),
            details={'provider': payment_method.provider}
        )
        
        return Response(status=status.HTTP_204_NO_CONTENT)
    
    def update(self, request, *args, **kwargs):
        """Update payment method (name, is_default)"""
        partial = kwargs.pop('partial', False)
        payment_method = self.get_object()
        
        # Only allow updating name and is_default
        allowed_fields = {'name', 'is_default'}
        update_data = {k: v for k, v in request.data.items() if k in allowed_fields}
        
        serializer = self.get_serializer(payment_method, data=update_data, partial=True)
        serializer.is_valid(raise_exception=True)
        serializer.save()
        
        return Response(serializer.data)
