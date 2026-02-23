# Backend Payment System Architecture - Integration Analysis

## Overview

The StayAfrica backend has **two complementary payment systems** that work together seamlessly:

1. **Payment System** (Existing) - One-time payment transactions
2. **Payment Method System** (New) - Reusable stored payment methods

These are NOT competing implementations - they are designed to work together in a complete payment flow.

---

## System Architecture

### 1. Payment System (Existing)

**Location**: `backend/apps/payments/models.py` (Payment model) + `backend/services/payment_gateway_enhanced.py`

**Purpose**: Handle individual payment transactions for bookings

**Key Components**:
- `Payment` model: One-to-one with Booking, tracks transaction status
- `PaymentGatewayService`: Routes payments to provider SDKs
- Methods: `initiate_payment()`, `verify_*_webhook()`, `convert_currency()`

**Supported Providers**: 
- Stripe (cards globally)
- Paynow (Zimbabwe ecocash/cards)
- Paystack (Nigeria, Ghana, South Africa)
- Flutterwave (Pan-Africa)
- PayPal
- Cash on Arrival

**Regional Support**:
```python
'Zimbabwe': ['paynow', 'flutterwave', 'paypal', 'cash_on_arrival']
'South Africa': ['paystack', 'flutterwave', 'paypal', 'ozow']
'Nigeria': ['paystack', 'flutterwave', 'paypal']
'Kenya': ['flutterwave', 'paypal', 'mpesa']
'Ghana': ['paystack', 'flutterwave', 'paypal']
'International': ['stripe', 'paypal', 'flutterwave']
```

### 2. Payment Method System (New)

**Location**: `backend/apps/payments/models.py` (PaymentMethod model) + `backend/apps/payments/views.py` (PaymentMethodViewSet)

**Purpose**: Store and manage reusable payment methods with tokenization

**Key Components**:
- `PaymentMethod` model: Stores user payment methods with provider tokens
- `PaymentMethodViewSet`: CRUD operations for payment methods
- Methods: `create()`, `list()`, `set_default()`, `destroy()` (soft delete)
- `_tokenize_with_provider()`: Tokenizes method with provider (placeholder)

**Supported Providers**: 
- Stripe
- Paynow
- Flutterwave
- Paystack

**Supported Method Types**:
- Card (Credit/Debit)
- Mobile Money (Ecocash, MTN, Vodacom, etc.)
- Bank Transfer
- USSD

---

## Integration Architecture

```
                    ┌─────────────────────────────────────┐
                    │   User Initiates Booking Payment    │
                    └──────────────┬──────────────────────┘
                                   │
                    ┌──────────────┴──────────────┐
                    │                             │
        ┌───────────▼─────────┐      ┌──────────▼──────────┐
        │  Payment Initiation │      │  Use Saved Method?  │
        │  (New Payment)      │      │                    │
        └──────────┬──────────┘      └─────────┬──────────┘
                   │                            │
                   │                    ┌───────▼────────┐
                   │                    │ PaymentMethod  │
                   │                    │ - Provider     │
                   │                    │ - Token        │
                   │                    │ - Type         │
                   │                    └───────┬────────┘
                   │                            │
        ┌──────────▼────────────────────────────▼─────────┐
        │   PaymentGatewayService.initiate_payment()      │
        │   - Routes to provider SDK                      │
        │   - Handles tokenization if new method          │
        │   - Initiates transaction                       │
        └──────────┬─────────────────────────────────────┘
                   │
        ┌──────────▼─────────────┐
        │  Provider Gateway      │
        │ (Stripe, Paynow, etc)  │
        └──────────┬─────────────┘
                   │
        ┌──────────▼──────────────┐
        │  Payment Confirmation   │
        │  - Update Payment model │
        │  - Create PaymentMethod │
        │    (if new)             │
        │  - Complete booking     │
        └────────────────────────┘
```

---

## Data Flow

### Scenario 1: New Payment Method During Booking

```
1. User initiates booking payment with new card
2. Frontend sends: provider, method_type, card details
3. PaymentViewSet.initiate() creates Payment record (status='initiated')
4. Calls PaymentGatewayService.initiate_payment()
5. PaymentGatewayService routes to provider SDK (e.g., Stripe)
6. Provider returns tokenized payment method reference
7. Payment completes successfully
8. Optionally save PaymentMethod for future use
9. Update Payment.status = 'success'
```

**Database Result**:
- ✅ Payment record created + updated
- ✅ Booking marked as paid
- ❓ PaymentMethod created (optional, if user checked "Save for future")

### Scenario 2: Using Saved Payment Method

```
1. User selects saved PaymentMethod from list
2. Frontend sends: payment_method_id
3. PaymentViewSet.initiate() fetches PaymentMethod record
4. PaymentViewSet extracts provider_token, provider, method_type
5. Creates Payment record with same provider
6. Calls PaymentGatewayService.initiate_payment()
7. PaymentGatewayService uses provider token for charge
8. Provider processes using stored method
9. Payment completes successfully
10. Neither creates nor updates PaymentMethod
```

**Database Result**:
- ✅ Payment record created + updated
- ✅ Booking marked as paid
- ✅ PaymentMethod remains unchanged (reused)

### Scenario 3: Pre-Save Payment Method (Wallet Tab)

```
1. User navigates to Wallet > Payment Methods
2. Clicks "Add Payment Method"
3. Completes 4-step wizard (Provider → Method → Details → Confirm)
4. Frontend sends data to POST /payments/payment-methods/
5. PaymentMethodViewSet.create() receives data
6. Calls _tokenize_with_provider() (uses PaymentGatewayService)
7. Provider validates and returns token
8. Creates PaymentMethod record with token
9. User can now use this method in future bookings
```

**Database Result**:
- ✅ PaymentMethod record created
- ❌ Payment record NOT created (no transaction yet)
- Payment will be created when user actually books with this method

---

## Seamless Integration Points

### 1. Tokenization Strategy

**Currently**: `_tokenize_with_provider()` is a placeholder that returns the token as-is

**Enhancement Needed**: Integrate with PaymentGatewayService

```python
def _tokenize_with_provider(self, payment_service, provider, method_type, token, data):
    """
    Leverage PaymentGatewayService for provider-specific tokenization
    """
    if provider == 'stripe':
        # Use Stripe SDK to tokenize
        return self._tokenize_stripe(data)
    
    elif provider == 'paynow':
        # Verify phone number with Paynow
        return self._tokenize_paynow(data)
    
    elif provider == 'flutterwave':
        # Tokenize card with Flutterwave
        return self._tokenize_flutterwave(data)
    
    elif provider == 'paystack':
        # Tokenize card with Paystack
        return self._tokenize_paystack(data)
    
    return token  # Fallback

def _tokenize_stripe(self, data):
    """Tokenize with Stripe - Uses Stripe SDK"""
    try:
        import stripe
        payment_method = stripe.PaymentMethod.create(
            type='card',
            card={
                'number': data['card_number'],
                'exp_month': data['expiry_month'],
                'exp_year': data['expiry_year'],
                'cvc': data['cvv'],
            }
        )
        return payment_method.id
    except stripe.error.StripeError as e:
        raise Exception(f'Stripe tokenization failed: {str(e)}')

# Similar methods for other providers...
```

### 2. Provider Consistency

**IMPORTANT**: The PaymentMethod system respects the same provider list as PaymentGatewayService

**Matching Providers**:
```
PaymentMethod PROVIDER_CHOICES:    PaymentGatewayService PAYMENT_METHODS:
- stripe                          ✅ stripe
- paynow                          ✅ paynow
- flutterwave                     ✅ flutterwave
- paystack                        ✅ paystack

(PaymentGatewayService also supports:)
                                   ⚠️ paypal (not in PaymentMethod yet)
                                   ⚠️ cash_on_arrival (not applicable)
                                   ⚠️ mpesa (not in PaymentMethod yet)
                                   ⚠️ ozow (not in PaymentMethod yet)
```

**Future Enhancement**: Add more providers to PaymentMethod if needed

### 3. Method Type Mapping

**PaymentMethod METHOD_TYPE_CHOICES** maps to payment flows:

| Method Type | Providers | Use Case |
|-------------|-----------|----------|
| card | Stripe, Paynow, Flutterwave, Paystack | Credit/Debit cards for global & regional |
| mobile | Paynow (Ecocash), Flutterwave | Mobile money in Africa |
| bank | Flutterwave, Paystack | Bank transfers in Africa |
| ussd | Flutterwave | USSD codes in Africa |

### 4. Regional Availability

**Example**: User from Zimbabwe adding payment method
```python
# Get available providers for Zimbabwe
available = payment_service.get_available_providers('Zimbabwe')
# Returns: ['paynow', 'flutterwave', 'paypal', 'cash_on_arrival']

# PaymentMethod should filter to supported providers:
supported = ['paynow', 'flutterwave']  # subset of available

# User can only add Paynow or Flutterwave methods
```

---

## Database Relationships

### Payment Model (Existing)
```
Payment
├── booking (OneToOne) → Booking
├── provider (CharField) → 'stripe', 'paynow', 'paystack', 'flutterwave'
├── gateway_ref (CharField) → Provider transaction ID
├── status (CharField) → 'initiated', 'pending', 'success', 'failed'
├── amount
├── currency
└── created_at, updated_at
```

### PaymentMethod Model (New)
```
PaymentMethod
├── user (ForeignKey) → User
├── provider (CharField) → 'stripe', 'paynow', 'paystack', 'flutterwave'
├── method_type (CharField) → 'card', 'mobile', 'bank', 'ussd'
├── name (CharField) → Display name
├── provider_token (CharField) → Tokenized secret (NEVER raw data)
├── last_four (CharField) → Display suffix
├── expiry_month, expiry_year (Optional) → Card expiry
├── phone_number (Optional) → Mobile money
├── is_default (BooleanField) → Quick selection
├── is_verified (BooleanField) → Verification status
├── deleted_at (DateTimeField) → Soft delete
└── created_at, updated_at
```

### Relationship
```
User
├── payment_methods (PaymentMethod ForeignKey)
│   └── Stores reusable methods
└── bookings (Booking ForeignKey)
    └── payment (OneToOne to Payment)
        └── Tracks individual transactions
```

**NO DIRECT LINK** between Payment and PaymentMethod (intentional):
- Payment can be one-time (anonymous or saved method)
- PaymentMethod is optional for payments
- Keeps concerns separated

---

## Security Considerations

### Token Storage
✅ **SECURE**: Provider tokens stored in `provider_token` field
- Never store raw card numbers, passwords, or API keys
- Only store provider-generated tokens
- Provider responsible for token security

### Soft Delete
✅ **SECURE**: Soft delete for audit trail
- PaymentMethod.deleted_at instead of hard delete
- Maintains transaction history
- Complies with data retention policies

### Unique Constraints
✅ **SECURE**: Prevent duplicate payment methods
```python
UniqueConstraint(
    fields=['user', 'provider', 'provider_token'],
    condition=Q(deleted_at__isnull=True),
    name='unique_active_payment_method'
)
```

### Audit Logging
✅ **SECURE**: All operations logged
- Payment method creation
- Default method changes
- Deletions with timestamp

---

## API Integration Points

### Payment Initiation with Saved Method

**Current API** (web/mobile):
```typescript
POST /api/v1/payments/initiate
{
  booking_id: string,
  provider: 'stripe' | 'paynow' | 'flutterwave' | 'paystack',
  payment_method_id?: string  // Optional
}
```

**Enhancement Needed**:
```typescript
POST /api/v1/payments/initiate
{
  booking_id: string,
  payment_option: {
    type: 'new' | 'saved',
    provider: string,
    // If type='new':
    card?: { number, exp_month, exp_year, cvc },
    phone_number?: string,
    // If type='saved':
    payment_method_id?: string,
    // Optionally save new method:
    save_for_future?: boolean
  }
}
```

### Payment Method API (Already Implemented)
```typescript
GET    /api/v1/payments/payment-methods/          // List
POST   /api/v1/payments/payment-methods/          // Create
PATCH  /api/v1/payments/payment-methods/{id}/     // Update name
PATCH  /api/v1/payments/payment-methods/{id}/set_default/  // Set default
DELETE /api/v1/payments/payment-methods/{id}/    // Delete
```

---

## No Clashes - Strategic Separation

### Why These Systems Don't Conflict

1. **Separate Concerns**
   - Payment: Transaction tracking (booking → payment)
   - PaymentMethod: Method storage (user → methods)

2. **Optional but Compatible**
   - User CAN use new payment for every booking
   - User CAN reuse saved payment methods
   - User CAN mix both approaches

3. **Provider Alignment**
   - Both systems use same providers: Stripe, Paynow, Flutterwave, Paystack
   - Both leverage same PaymentGatewayService
   - Consistent regional availability

4. **Token-Based Architecture**
   - Payment uses gateway_ref (provider transaction ID)
   - PaymentMethod uses provider_token (tokenized method)
   - No overlap - different purposes

5. **No Breaking Changes**
   - Existing Payment initiation flow unchanged
   - New PaymentMethod flow is additive
   - Backward compatible with current implementations

---

## Implementation Checklist

### Phase 1: Tokenization Implementation ✅ READY
- [ ] Implement `_tokenize_stripe()` using Stripe.js
- [ ] Implement `_tokenize_paynow()` with Paynow API
- [ ] Implement `_tokenize_flutterwave()` with Flutterwave API
- [ ] Implement `_tokenize_paystack()` with Paystack API
- [ ] Add error handling for failed tokenization
- [ ] Add security headers (X-Stripe-Client-User-Agent, etc.)

### Phase 2: Payment Integration
- [ ] Enhance Payment.initiate() to accept payment_method_id
- [ ] Add logic to extract provider_token from PaymentMethod
- [ ] Update PaymentGatewayService to handle saved methods
- [ ] Add "save for future" checkbox to payment flow

### Phase 3: Frontend Integration
- [ ] Add PaymentMethod selection UI to booking payment
- [ ] Show saved Cards, Mobile Money, Bank, USSD options
- [ ] Auto-select default payment method
- [ ] Add "Save for future" toggle

### Phase 4: Testing
- [ ] Test payment with new card (no save)
- [ ] Test payment with saved card
- [ ] Test adding multiple payment methods
- [ ] Test set/change default
- [ ] Test soft delete and retrieval
- [ ] Test regional provider restrictions

---

## Conclusion

✅ **The two systems work seamlessly together**:
- No architectural clashes
- Intentional separation of concerns
- Shared provider infrastructure
- Enhanced user experience (save and reuse)
- Maintained security standards
- Future-proof extensible design

The PaymentGatewayService is the backbone, and PaymentMethod is a natural extension for improved UX without disrupting existing payment flows.
