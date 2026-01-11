# Payment System Enhancement - Implementation Guide

## Changes Summary

### 1. Official Payment SDKs Integrated ✅
- **Stripe SDK** (stripe==10.15.0)
- **Paystack SDK** (paystack==2.0.0)  
- **Flutterwave SDK** (flutterwave-python==1.2.7)
- **Paynow SDK** (paynow==1.0.5)

### 2. Enhanced Pricing Model ✅

#### New Models Created:
- **PricingRule** - Dynamic pricing (seasonal, weekend, length discounts, early bird, last minute)
- **PropertyFee** - Additional fees (cleaning, service, pet, extra guest, resort, parking, linen)
- **PropertyTax** - Tax configuration (VAT, occupancy, tourism, city taxes)
- **CurrencyExchangeRate** - Multi-currency support

#### Features:
- Seasonal pricing (high/low season rates)
- Weekend premiums
- Length of stay discounts
- Early bird discounts
- Last-minute deals
- Per-night, per-booking, per-guest fee structures
- Tax on base price and/or fees
- Currency conversion

### 3. Payment Flow Improvements ✅

#### SDK-Based Initiation:
- **Stripe**: Creates checkout session, returns checkout URL
- **Paynow**: Uses official SDK for Ecocash/Visa payments
- **Flutterwave**: Supports card, mobile money, USSD across Africa

#### Enhanced Webhooks:
- Stripe webhook signature verification using SDK
- Provider-specific status mapping
- Proper error handling and logging

### 4. Database Migrations

Run these migrations:
```bash
cd backend
python manage.py makemigrations
python manage.py migrate
```

Migrations created:
- `0002_add_pricing_models.py` - Pricing/fee/tax tables
- `0003_add_pricing_breakdown.py` - Enhanced booking fields

### 5. Configuration Required

#### Environment Variables (.env):
```env
# Stripe
STRIPE_SECRET_KEY=sk_live_...
STRIPE_PUBLISHABLE_KEY=pk_live_...
STRIPE_WEBHOOK_SECRET=whsec_...

# Paynow (Zimbabwe)
PAYNOW_INTEGRATION_ID=...
PAYNOW_INTEGRATION_KEY=...
PAYNOW_WEBHOOK_SECRET=...

# Flutterwave (Africa-wide)
FLUTTERWAVE_PUBLIC_KEY=FLWPUBK-...
FLUTTERWAVE_SECRET_KEY=FLWSECK-...
FLUTTERWAVE_WEBHOOK_SECRET=...

# Paystack (Nigeria, Ghana, South Africa)
PAYSTACK_SECRET_KEY=sk_live_...
PAYSTACK_PUBLIC_KEY=pk_live_...
```

#### System Configuration (Admin Dashboard):
Navigate to Admin → System Configuration and add:
- Payment provider credentials
- Webhook secrets
- Commission rates
- Service fees

### 6. API Response Changes

#### Payment Initiation Response:
```json
{
  "id": "...",
  "gateway_ref": "BOOK-ABC-stripe-xyz",
  "status": "initiated",
  "amount": "250.00",
  "currency": "USD",
  "provider": "stripe",
  "checkout_url": "https://checkout.stripe.com/...",  // Stripe
  "redirect_url": "https://paynow.netone.co.zw/...",  // Paynow
  "payment_link": "https://checkout.flutterwave.com/..." // Flutterwave
}
```

### 7. Pricing Calculation API

New comprehensive pricing calculation in bookings:
```python
from services.payment_gateway_enhanced import PaymentGatewayService

service = PaymentGatewayService()
pricing = service.calculate_pricing(
    property_obj,
    check_in,
    check_out,
    guests,
    booking_date
)

# Returns detailed breakdown:
{
    'nights': 3,
    'base_price': 100.00,
    'base_total': 300.00,
    'pricing_adjustments': -30.00,  # 10% discount
    'adjusted_base': 270.00,
    'applied_rules': [
        {'name': 'Summer Discount', 'type': 'seasonal', 'adjustment': -30.00}
    ],
    'fees': [
        {'name': 'Cleaning Fee', 'type': 'cleaning', 'amount': 50.00}
    ],
    'total_fees': 50.00,
    'taxes': [
        {'name': 'VAT', 'type': 'vat', 'rate': 15.0, 'amount': 48.00}
    ],
    'total_tax': 48.00,
    'subtotal': 320.00,
    'grand_total': 368.00,
    'platform_commission': 19.60,
    'host_payout': 300.40,
    'currency': 'USD'
}
```

### 8. Webhook Endpoints

Configure these in payment provider dashboards:

- **Stripe**: `https://api.stayafrica.app/v1/payments/webhook/`
- **Paynow**: `https://api.stayafrica.app/v1/payments/webhook/?provider=paynow`
- **Flutterwave**: `https://api.stayafrica.app/v1/payments/webhook/?provider=flutterwave`

### 9. Testing

1. **Install dependencies**:
```bash
pip install -r requirements.txt
```

2. **Run migrations**:
```bash
python manage.py migrate
```

3. **Create test pricing rules** (Django admin or shell):
```python
from apps.properties.models import Property
from apps.payments.pricing_models import PricingRule, PropertyFee, PropertyTax

property = Property.objects.first()

# Add seasonal discount
PricingRule.objects.create(
    property=property,
    name="Summer 20% Off",
    rule_type='seasonal',
    adjustment_type='percentage',
    adjustment_value=-20,
    start_date='2026-06-01',
    end_date='2026-08-31',
    is_active=True
)

# Add cleaning fee
PropertyFee.objects.create(
    property=property,
    name="Cleaning Fee",
    fee_type='cleaning',
    amount=50.00,
    charge_type='per_booking',
    is_mandatory=True
)

# Add VAT
PropertyTax.objects.create(
    property=property,
    name="VAT",
    tax_type='vat',
    rate=15.00,
    applies_to_base_price=True,
    applies_to_fees=True
)
```

4. **Test payment flow**:
```bash
# Start server
python manage.py runserver

# Test endpoint
curl -X POST http://localhost:8000/api/v1/payments/initiate/ \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "booking_id": "...",
    "provider": "stripe"
  }'
```

### 10. Next Steps

1. ✅ Install dependencies: `pip install -r requirements.txt`
2. ✅ Run migrations
3. ⏳ Add payment credentials to system config
4. ⏳ Test each payment provider
5. ⏳ Configure webhooks in provider dashboards
6. ⏳ Create pricing rules for properties
7. ⏳ Test complete booking → payment → confirmation flow

All payment methods now use official SDKs with proper error handling, webhook verification, and comprehensive pricing calculations!
