"""
Payment Gateway Service
Handles payment provider switching and processing
"""
from decimal import Decimal
from django.conf import settings

class PaymentGatewayService:
    REGIONAL_PROVIDERS = {
        'Zimbabwe': ['paynow', 'cash_on_arrival'],
        'South Africa': ['payfast', 'ozow'],
        'International': ['stripe'],
    }
    
    PAYMENT_METHODS = {
        'paynow': 'Paynow (Ecocash/Visa)',
        'payfast': 'PayFast',
        'ozow': 'Ozow',
        'stripe': 'Stripe',
        'cash_on_arrival': 'Cash on Arrival',
    }
    
    def __init__(self):
        self.commission_rate = settings.COMMISSION_RATE
        self.service_fee = Decimal(str(settings.SERVICE_FEE))
    
    def get_available_providers(self, user_country):
        """Get available payment providers for a specific country"""
        return self.REGIONAL_PROVIDERS.get(user_country, ['stripe'])
    
    def get_provider_label(self, provider):
        """Get display name for provider"""
        return self.PAYMENT_METHODS.get(provider, provider)
    
    def calculate_fees(self, base_price, cleaning_fee=0):
        """
        Calculate commission and total fees
        Commission = 7% of (base_price + service_fee)
        """
        base_price = Decimal(str(base_price))
        cleaning_fee = Decimal(str(cleaning_fee or 0))
        
        commission = (base_price + self.service_fee) * Decimal(str(self.commission_rate))
        total_fees = commission + self.service_fee
        
        return {
            'base_price': base_price,
            'service_fee': self.service_fee,
            'commission_fee': commission,
            'cleaning_fee': cleaning_fee,
            'total_fees': total_fees,
            'host_payout': base_price + cleaning_fee - commission,
        }
    
    def initiate_payment(self, booking, provider):
        """Initiate payment with specified provider"""
        # This will be implemented with actual provider SDKs
        pass
    
    def handle_webhook(self, provider, data):
        """Handle webhook from payment provider"""
        # This will be implemented with webhook handlers
        pass
