# Payment Processing Integration Guide

## Overview

This guide outlines the complete integration of payment processing with bookings in the StayAfrica platform. The payment system supports multiple payment providers (Stripe, Paynow, Flutterwave, Paystack) with a two-phase workflow:

1. **Phase 1 (Complete)**: Mobile UI for adding and managing payment methods ✅
2. **Phase 2 (In Progress)**: Backend payment processing integration
3. **Phase 3 (Pending)**: Payment provider webhook handling

---

## Architecture Overview

```
Mobile App Payment Flow:
┌─────────────────────────────────────────────────────────────────┐
│                    USER BOOKING FLOW                             │
├─────────────────────────────────────────────────────────────────┤
│ 1. Browse Property → 2. Select Dates/Guests → 3. Confirm → Pay │
│                                                      ↓            │
│                                              /booking/payment    │
│                              Fetch Payment Methods (GET)         │
│                              Select Method                        │
│                              POST to /bookings/{id}/pay/         │
└─────────────────────────────────────────────────────────────────┘
                                  ↓
┌─────────────────────────────────────────────────────────────────┐
│                    BACKEND PAYMENT PROCESSING                   │
├─────────────────────────────────────────────────────────────────┤
│ 1. Validate Booking & Method                                     │
│ 2. Retrieve Tokenized Method from Database                       │
│ 3. Initialize Payment with Provider API                          │
│ 4. Return Status/Checkout URL to Client                          │
│ 5. Listen for Webhook Confirmation                               │
│ 6. Update Booking & Transaction Status                           │
└─────────────────────────────────────────────────────────────────┘
                                  ↓
┌─────────────────────────────────────────────────────────────────┐
│                    PAYMENT PROVIDER                             │
│          (Stripe|Paynow|Flutterwave|Paystack)                   │
├─────────────────────────────────────────────────────────────────┤
│ Process Payment → Webhook → Success/Failure Callback             │
└─────────────────────────────────────────────────────────────────┘
```

---

## Database Schema

### Payment Method Storage

```python
# models/payment.py

class PaymentMethod(models.Model):
    PROVIDER_CHOICES = [
        ('stripe', 'Stripe'),
        ('paynow', 'Paynow'),
        ('flutterwave', 'Flutterwave'),
        ('paystack', 'Paystack'),
    ]
    
    METHOD_CHOICES = [
        ('card', 'Credit/Debit Card'),
        ('mobile', 'Mobile Money'),
        ('bank', 'Bank Transfer'),
        ('ussd', 'USSD'),
    ]
    
    id = models.UUIDField(primary_key=True, default=uuid.uuid4)
    user = models.ForeignKey(User, on_delete=models.CASCADE, related_name='payment_methods')
    provider = models.CharField(max_length=20, choices=PROVIDER_CHOICES)
    method_type = models.CharField(max_length=20, choices=METHOD_CHOICES)
    
    # Tokenized data (never store raw card data)
    provider_token = models.CharField(max_length=500)  # Stripe token, Paynow ID, etc.
    last_four = models.CharField(max_length=4, blank=True)
    expiry_month = models.IntegerField(blank=True, null=True)
    expiry_year = models.IntegerField(blank=True, null=True)
    
    # Metadata
    is_default = models.BooleanField(default=False)
    name = models.CharField(max_length=100)  # User-friendly name (e.g., "My Visa")
    
    # Timestamps
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    deleted_at = models.DateTimeField(null=True, blank=True)  # Soft delete
    
    class Meta:
        ordering = ['-is_default', '-created_at']
        indexes = [
            models.Index(fields=['user', 'deleted_at']),
        ]


class PaymentTransaction(models.Model):
    STATUS_CHOICES = [
        ('pending', 'Pending'),
        ('processing', 'Processing'),
        ('completed', 'Completed'),
        ('failed', 'Failed'),
        ('refunded', 'Refunded'),
    ]
    
    id = models.UUIDField(primary_key=True, default=uuid.uuid4)
    booking = models.OneToOneField(Booking, on_delete=models.CASCADE, related_name='payment')
    payment_method = models.ForeignKey(PaymentMethod, on_delete=models.PROTECT)
    
    # Payment details
    amount = models.DecimalField(max_digits=10, decimal_places=2)
    currency = models.CharField(max_length=3, default='USD')
    
    # Provider transaction ID
    provider_transaction_id = models.CharField(max_length=200, unique=True)
    
    # Status tracking
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='pending')
    error_message = models.TextField(blank=True)
    
    # Timestamps
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    completed_at = models.DateTimeField(null=True, blank=True)
    
    class Meta:
        ordering = ['-created_at']
        indexes = [
            models.Index(fields=['booking', 'status']),
            models.Index(fields=['provider_transaction_id']),
        ]
```

---

## Backend API Endpoints

### 1. Wallet - Payment Methods Management

**Endpoint**: `GET /api/wallet/payment-methods/`

**Purpose**: List all saved payment methods for the authenticated user

**Request**:
```bash
GET /api/wallet/payment-methods/
Authorization: Bearer {token}
```

**Response**:
```json
{
  "count": 3,
  "next": null,
  "previous": null,
  "results": [
    {
      "id": "mpm-123-abc",
      "provider": "stripe",
      "method_type": "card",
      "name": "My Visa Card",
      "last_four": "4242",
      "expiry_month": 12,
      "expiry_year": 2026,
      "is_default": true,
      "created_at": "2025-12-01T10:30:00Z"
    },
    {
      "id": "mpm-456-def",
      "provider": "paynow",
      "method_type": "mobile",
      "name": "Ecocash Account",
      "last_four": "0719",
      "is_default": false,
      "created_at": "2025-12-02T15:45:00Z"
    }
  ]
}
```

**Implementation**:
```python
# views/wallet.py

from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from .models import PaymentMethod
from .serializers import PaymentMethodSerializer

class PaymentMethodViewSet(viewsets.ModelViewSet):
    permission_classes = [IsAuthenticated]
    serializer_class = PaymentMethodSerializer
    
    def get_queryset(self):
        return PaymentMethod.objects.filter(
            user=self.request.user,
            deleted_at__isnull=True
        )
    
    def list(self, request, *args, **kwargs):
        """List all payment methods for authenticated user"""
        queryset = self.get_queryset()
        serializer = self.get_serializer(queryset, many=True)
        return Response({
            'count': queryset.count(),
            'results': serializer.data
        })
```

---

### 2. Add Payment Method

**Endpoint**: `POST /api/wallet/payment-methods/`

**Purpose**: Add a new saved payment method

**Request**:
```json
{
  "provider": "stripe",
  "method_type": "card",
  "name": "My Visa Card",
  "token": "tok_visa",  // From Stripe, provided by client
  "last_four": "4242",
  "expiry_month": 12,
  "expiry_year": 2026,
  "is_default": true
}
```

**Response** (201 Created):
```json
{
  "id": "mpm-789-ghi",
  "provider": "stripe",
  "method_type": "card",
  "name": "My Visa Card",
  "last_four": "4242",
  "expiry_month": 12,
  "expiry_year": 2026,
  "is_default": true,
  "created_at": "2025-12-03T10:00:00Z"
}
```

**Implementation**:
```python
@action(detail=False, methods=['post'])
def create(self, request, *args, **kwargs):
    """Create a new payment method"""
    serializer = self.get_serializer(data=request.data)
    serializer.is_valid(raise_exception=True)
    
    # Tokenize with provider
    provider = serializer.validated_data.get('provider')
    method_data = self._tokenize_payment_method(
        provider,
        request.data.get('token'),
        request.data.get('method_type')
    )
    
    # Save payment method
    instance = serializer.save(
        user=request.user,
        provider_token=method_data['provider_token']
    )
    
    return Response(
        PaymentMethodSerializer(instance).data,
        status=status.HTTP_201_CREATED
    )

def _tokenize_payment_method(self, provider, token, method_type):
    """Tokenize payment method with provider"""
    if provider == 'stripe':
        return self._tokenize_stripe(token)
    elif provider == 'paynow':
        return self._tokenize_paynow(token, method_type)
    elif provider == 'flutterwave':
        return self._tokenize_flutterwave(token)
    elif provider == 'paystack':
        return self._tokenize_paystack(token)
    else:
        raise ValueError(f"Unsupported provider: {provider}")
```

---

### 3. Update Payment Method

**Endpoint**: `PATCH /api/wallet/payment-methods/{id}/`

**Purpose**: Update payment method (set as default, change name)

**Request**:
```json
{
  "is_default": true,
  "name": "Primary Card"
}
```

**Response**:
```json
{
  "id": "mpm-789-ghi",
  "provider": "stripe",
  "method_type": "card",
  "name": "Primary Card",
  "last_four": "4242",
  "is_default": true,
  "updated_at": "2025-12-03T11:00:00Z"
}
```

---

### 4. Delete Payment Method

**Endpoint**: `DELETE /api/wallet/payment-methods/{id}/`

**Purpose**: Remove a payment method

**Response**: 204 No Content

**Implementation**:
```python
def destroy(self, request, *args, **kwargs):
    """Soft delete payment method"""
    instance = self.get_object()
    instance.deleted_at = timezone.now()
    instance.save()
    return Response(status=status.HTTP_204_NO_CONTENT)
```

---

## Booking Payment Processing

### 1. Initiate Payment

**Endpoint**: `POST /api/bookings/{booking_id}/pay/`

**Purpose**: Initiate payment for a booking using a saved payment method

**Request**:
```json
{
  "payment_method_id": "mpm-789-ghi"
}
```

**Response**:
```json
{
  "status": "processing",
  "transaction_id": "txn-123-abc",
  "booking_id": "BOOK-001",
  "amount": "250.00",
  "currency": "USD"
}
```

OR (for external payment):
```json
{
  "status": "pending",
  "transaction_id": "txn-456-def",
  "checkout_url": "https://checkout.stripe.com/pay/cs_live_...",
  "booking_id": "BOOK-001"
}
```

**Implementation**:
```python
# views/booking.py

class BookingViewSet(viewsets.ModelViewSet):
    permission_classes = [IsAuthenticated]
    
    @action(detail=True, methods=['post'])
    def pay(self, request, pk=None):
        """Initiate payment for booking"""
        booking = self.get_object()
        
        # Validate booking belongs to user
        if booking.guest.user != request.user:
            return Response(
                {'error': 'Not authorized'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        # Check booking status
        if booking.status != 'pending':
            return Response(
                {'error': f'Cannot pay for {booking.status} booking'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Get payment method
        payment_method_id = request.data.get('payment_method_id')
        try:
            payment_method = PaymentMethod.objects.get(
                id=payment_method_id,
                user=request.user,
                deleted_at__isnull=True
            )
        except PaymentMethod.DoesNotExist:
            return Response(
                {'error': 'Payment method not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        # Process payment
        transaction = self._process_payment(booking, payment_method)
        
        return Response({
            'status': transaction.status,
            'transaction_id': str(transaction.id),
            'booking_id': booking.booking_id,
            'amount': str(booking.total_price),
            'currency': 'USD',
            'checkout_url': getattr(transaction, 'checkout_url', None)
        })
    
    def _process_payment(self, booking, payment_method):
        """Process payment through provider"""
        # Create transaction record
        transaction = PaymentTransaction.objects.create(
            booking=booking,
            payment_method=payment_method,
            amount=booking.total_price,
            status='pending'
        )
        
        # Route to provider
        if payment_method.provider == 'stripe':
            self._process_stripe_payment(transaction, payment_method)
        elif payment_method.provider == 'paynow':
            self._process_paynow_payment(transaction, payment_method)
        elif payment_method.provider == 'flutterwave':
            self._process_flutterwave_payment(transaction, payment_method)
        elif payment_method.provider == 'paystack':
            self._process_paystack_payment(transaction, payment_method)
        
        return transaction
```

---

## Payment Provider Integration

### Stripe Implementation

```python
# services/payment_providers/stripe_service.py

import stripe
from django.conf import settings

stripe.api_key = settings.STRIPE_SECRET_KEY

class StripePaymentService:
    
    @staticmethod
    def create_payment_intent(transaction, payment_method):
        """Create Stripe PaymentIntent"""
        intent = stripe.PaymentIntent.create(
            amount=int(transaction.amount * 100),  # Convert to cents
            currency=transaction.currency.lower(),
            payment_method=payment_method.provider_token,
            confirm=True,
            off_session=True,  # Allow future payments
            metadata={
                'booking_id': str(transaction.booking.id),
                'transaction_id': str(transaction.id),
                'user_id': str(transaction.booking.guest.user.id),
            }
        )
        
        transaction.provider_transaction_id = intent.id
        transaction.status = 'processing'
        transaction.save()
        
        return intent
    
    @staticmethod
    def handle_webhook(event):
        """Handle Stripe webhook"""
        if event['type'] == 'payment_intent.succeeded':
            intent = event['data']['object']
            transaction_id = intent['metadata']['transaction_id']
            
            transaction = PaymentTransaction.objects.get(id=transaction_id)
            transaction.status = 'completed'
            transaction.completed_at = timezone.now()
            transaction.save()
            
            # Update booking
            transaction.booking.status = 'confirmed'
            transaction.booking.save()
        
        elif event['type'] == 'payment_intent.payment_failed':
            intent = event['data']['object']
            transaction_id = intent['metadata']['transaction_id']
            
            transaction = PaymentTransaction.objects.get(id=transaction_id)
            transaction.status = 'failed'
            transaction.error_message = intent['last_payment_error']['message']
            transaction.save()
```

### Webhook Endpoint

```python
# views/webhooks.py

from django.http import JsonResponse
from django.views.decorators.http import csrf_exempt
from django.views.decorators.csrf import csrf_exempt
import stripe

@csrf_exempt
def stripe_webhook(request):
    """Handle Stripe webhook"""
    payload = request.body
    sig_header = request.META.get('HTTP_STRIPE_SIGNATURE')
    
    try:
        event = stripe.Webhook.construct_event(
            payload,
            sig_header,
            settings.STRIPE_WEBHOOK_SECRET
        )
    except ValueError:
        return JsonResponse({'error': 'Invalid payload'}, status=400)
    except stripe.error.SignatureVerificationError:
        return JsonResponse({'error': 'Invalid signature'}, status=400)
    
    # Handle event
    StripePaymentService.handle_webhook(event)
    
    return JsonResponse({'success': True})
```

---

## Security Checklist

- [ ] **PCI Compliance**: Never store raw credit card data
  - Use tokenization services for all providers
  - Only store provider tokens, expiry, and last 4 digits

- [ ] **SSL/TLS**: All payment endpoints use HTTPS
  - Enable HSTS headers
  - Use strong TLS 1.2+

- [ ] **API Keys**: Secure storage
  - Use environment variables, not hardcoded
  - Implement key rotation
  - Grant minimal permissions

- [ ] **Webhook Verification**: Verify all provider webhooks
  - Check signatures before processing
  - Implement idempotency for retry handling

- [ ] **Rate Limiting**: Prevent abuse
  - Limit payment attempts per user
  - Implement CAPTCHA for repeated failures

- [ ] **Error Messages**: Don't expose sensitive data
  - Log full errors server-side
  - Return generic messages to client

- [ ] **Audit Logging**: Track all payment operations
  - Log payment initiation
  - Log webhook events
  - Log refunds/cancellations

---

## Testing

### Unit Tests

```python
# tests/test_payment.py

from django.test import TestCase
from rest_framework.test import APIClient
from .models import PaymentMethod, PaymentTransaction

class PaymentMethodTests(TestCase):
    def setUp(self):
        self.client = APIClient()
        self.user = User.objects.create_user(username='test', password='test')
        self.client.force_authenticate(user=self.user)
    
    def test_add_payment_method(self):
        """Test adding a payment method"""
        response = self.client.post(
            '/api/wallet/payment-methods/',
            {
                'provider': 'stripe',
                'method_type': 'card',
                'name': 'Test Card',
                'token': 'tok_visa',
                'last_four': '4242',
                'expiry_month': 12,
                'expiry_year': 2026,
            }
        )
        
        self.assertEqual(response.status_code, 201)
        self.assertEqual(PaymentMethod.objects.count(), 1)
    
    def test_list_payment_methods(self):
        """Test listing payment methods"""
        PaymentMethod.objects.create(
            user=self.user,
            provider='stripe',
            method_type='card',
            name='Test Card',
            provider_token='stripe_token',
            last_four='4242'
        )
        
        response = self.client.get('/api/wallet/payment-methods/')
        
        self.assertEqual(response.status_code, 200)
        self.assertEqual(len(response.data['results']), 1)
```

---

## Deployment Checklist

- [ ] Configure payment provider API keys in production
- [ ] Enable webhook handlers for all providers
- [ ] Set up payment provider signing keys for webhook verification
- [ ] Run database migrations for payment models
- [ ] Configure SSL certificates
- [ ] Test payment flow end-to-end
- [ ] Set up monitoring/alerts for failed payments
- [ ] Review security settings with payment team
- [ ] Enable audit logging
- [ ] Train support team on refund process

---

## Next Steps

1. **Backend Development**: Implement payment models and endpoints
2. **Provider Integration**: Set up Stripe, Paynow, Flutterwave, Paystack
3. **Webhook Handling**: Implement webhook receivers for each provider
4. **Testing**: End-to-end testing with test keys
5. **Deployment**: Roll out to production with monitoring

---

## Reference Documentation

- **Stripe**: https://stripe.com/docs/payments
- **Paynow**: https://www.paynow.co.zw/docs/
- **Flutterwave**: https://developer.flutterwave.com/docs/
- **Paystack**: https://paystack.com/docs/payments/

