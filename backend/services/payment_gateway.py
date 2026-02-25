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
        from apps.admin_dashboard.models import SystemConfiguration
        config = SystemConfiguration.get_config()
        self.commission_rate = config.commission_rate
        self.service_fee = Decimal(str(config.service_fee))
        self.config = config
    
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
    
    def get_provider_credentials(self, provider):
        """Get credentials for a specific payment provider"""
        credentials = {}
        
        if provider == 'paynow':
            credentials = {
                'integration_id': self.config.paynow_integration_id,
                'integration_key': self.config.paynow_integration_key,
            }
        elif provider == 'payfast':
            credentials = {
                'merchant_id': self.config.payfast_merchant_id,
                'merchant_key': self.config.payfast_merchant_key,
                'passphrase': self.config.payfast_passphrase,
                'webhook_secret': self.config.payfast_webhook_secret,
            }
        elif provider == 'stripe':
            credentials = {
                'secret_key': self.config.stripe_secret_key,
                'publishable_key': self.config.stripe_publishable_key,
                'webhook_secret': self.config.stripe_webhook_secret,
            }
        
        return credentials
    
    def initiate_payment(self, booking, provider):
        """Initiate payment with specified provider"""
        credentials = self.get_provider_credentials(provider)
        # This will be implemented with actual provider SDKs using credentials
        pass
    
    def handle_webhook(self, provider, data):
        """Handle webhook from payment provider"""
        credentials = self.get_provider_credentials(provider)
        # This will be implemented with webhook handlers using credentials
        pass
