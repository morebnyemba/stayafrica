"""
Enhanced Payment Gateway Service with Official SDKs
"""
from decimal import Decimal
from django.conf import settings
from typing import Dict, Optional, Tuple
import stripe
from paynow import Paynow as PaynowSDK
from flutterwave import Flutterwave as FlutterwaveSDK
import requests
import base64
import json
import logging

logger = logging.getLogger(__name__)


class PaymentGatewayService:
    """Enhanced payment gateway with official SDK support"""
    
    REGIONAL_PROVIDERS = {
        'Zimbabwe': ['paynow', 'flutterwave', 'paypal', 'cash_on_arrival'],
        'South Africa': ['paystack', 'flutterwave', 'paypal', 'ozow'],
        'Nigeria': ['paystack', 'flutterwave', 'paypal'],
        'Kenya': ['flutterwave', 'paypal', 'mpesa'],
        'Ghana': ['paystack', 'flutterwave', 'paypal'],
        'International': ['stripe', 'paypal', 'flutterwave'],
    }
    
    PAYMENT_METHODS = {
        'paynow': 'Paynow (Ecocash/Visa)',
        'paystack': 'Paystack',
        'flutterwave': 'Flutterwave',
        'stripe': 'Stripe',
        'paypal': 'PayPal',
        'cash_on_arrival': 'Cash on Arrival',
        'mpesa': 'M-Pesa',
        'ozow': 'Ozow',
    }
    
    def __init__(self):
        from apps.admin_dashboard.models import SystemConfiguration
        self.config = SystemConfiguration.get_config()
        self.commission_rate = self.config.commission_rate
        self.service_fee = Decimal(str(self.config.service_fee))
        self._init_sdks()
    
    def _init_sdks(self):
        """Initialize payment provider SDKs"""
        # Stripe
        if self.config.stripe_secret_key:
            stripe.api_key = self.config.stripe_secret_key
        
        # Paynow
        if self.config.paynow_integration_id and self.config.paynow_integration_key:
            self.paynow = PaynowSDK(
                self.config.paynow_integration_id,
                self.config.paynow_integration_key,
                return_url=f'{settings.SITE_URL}/payment/return',
                result_url=f'{settings.API_URL}/v1/payments/webhook/paynow/'
            )
        
        # Flutterwave
        if hasattr(self.config, 'flutterwave_secret_key') and self.config.flutterwave_secret_key:
            self.flutterwave = FlutterwaveSDK(
                public_key=getattr(self.config, 'flutterwave_public_key', ''),
                secret_key=self.config.flutterwave_secret_key
            )
        
        # PayPal - using REST API directly
        self.paypal_mode = getattr(self.config, 'paypal_mode', 'sandbox')  # 'sandbox' or 'live'
        self.paypal_client_id = getattr(self.config, 'paypal_client_id', '')
        self.paypal_client_secret = getattr(self.config, 'paypal_client_secret', '')
        self.paypal_base_url = 'https://api-m.sandbox.paypal.com' if self.paypal_mode == 'sandbox' else 'https://api-m.paypal.com'
    
    def get_available_providers(self, user_country: str) -> list:
        """Get available payment providers for a specific country"""
        return self.REGIONAL_PROVIDERS.get(user_country, self.REGIONAL_PROVIDERS['International'])
    
    def get_provider_label(self, provider: str) -> str:
        """Get display name for provider"""
        return self.PAYMENT_METHODS.get(provider, provider)
    
    def calculate_pricing(
        self, 
        property_obj, 
        check_in, 
        check_out, 
        guests: int,
        booking_date=None
    ) -> Dict[str, Decimal]:
        """
        Calculate comprehensive pricing with all fees, taxes, and discounts
        Returns breakdown of all costs
        """
        from datetime import date as date_class
        from apps.payments.pricing_models import PropertyFee, PropertyTax, PricingRule
        
        if booking_date is None:
            booking_date = date_class.today()
        
        nights = (check_out - check_in).days
        base_price = Decimal(str(property_obj.price_per_night)) * Decimal(str(nights))
        
        # Apply pricing rules (discounts/premiums)
        total_adjustment = Decimal('0')
        applied_rules = []
        
        pricing_rules = PricingRule.objects.filter(
            property=property_obj,
            is_active=True
        ).order_by('-priority')
        
        for rule in pricing_rules:
            if rule.applies_to_booking(check_in, check_out, booking_date):
                adjustment = rule.calculate_adjustment(base_price)
                total_adjustment += adjustment
                applied_rules.append({
                    'name': rule.name,
                    'type': rule.rule_type,
                    'adjustment': adjustment
                })
        
        adjusted_base = base_price + total_adjustment
        
        # Calculate fees
        total_fees = Decimal('0')
        fee_breakdown = []
        
        fees = PropertyFee.objects.filter(property=property_obj, is_active=True)
        for fee in fees:
            fee_amount = fee.calculate_fee(nights, guests)
            total_fees += fee_amount
            fee_breakdown.append({
                'name': fee.name,
                'type': fee.fee_type,
                'amount': fee_amount,
                'mandatory': fee.is_mandatory
            })
        
        # Calculate taxes
        total_tax = Decimal('0')
        tax_breakdown = []
        
        taxes = PropertyTax.objects.filter(property=property_obj, is_active=True)
        for tax in taxes:
            tax_amount = tax.calculate_tax(adjusted_base, total_fees if tax.applies_to_fees else Decimal('0'))
            total_tax += tax_amount
            tax_breakdown.append({
                'name': tax.name,
                'type': tax.tax_type,
                'rate': tax.rate,
                'amount': tax_amount
            })
        
        # Platform commission and service fee
        commission = (adjusted_base + self.service_fee) * Decimal(str(self.commission_rate))
        
        # Calculate totals
        subtotal = adjusted_base + total_fees
        grand_total = subtotal + total_tax
        host_payout = adjusted_base - commission + total_fees
        
        return {
            'nights': nights,
            'guests': guests,
            'base_price': property_obj.price_per_night,
            'base_total': base_price,
            'pricing_adjustments': total_adjustment,
            'adjusted_base': adjusted_base,
            'applied_rules': applied_rules,
            'fees': fee_breakdown,
            'total_fees': total_fees,
            'taxes': tax_breakdown,
            'total_tax': total_tax,
            'subtotal': subtotal,
            'grand_total': grand_total,
            'platform_service_fee': self.service_fee,
            'platform_commission': commission,
            'host_payout': host_payout,
            'currency': property_obj.currency,
        }
    
    def initiate_stripe_payment(
        self, 
        payment_obj, 
        booking,
        customer_email: str
    ) -> Dict:
        """Initiate Stripe payment using official SDK"""
        try:
            # Create Stripe checkout session
            session = stripe.checkout.Session.create(
                payment_method_types=['card'],
                line_items=[{
                    'price_data': {
                        'currency': payment_obj.currency.lower(),
                        'unit_amount': int(payment_obj.amount * 100),  # Convert to cents
                        'product_data': {
                            'name': f'Booking: {booking.rental_property.title}',
                            'description': f'{booking.check_in} to {booking.check_out}',
                        },
                    },
                    'quantity': 1,
                }],
                mode='payment',
                success_url=f'{settings.SITE_URL}/booking/success?session_id={{CHECKOUT_SESSION_ID}}',
                cancel_url=f'{settings.SITE_URL}/booking/cancel',
                customer_email=customer_email,
                metadata={
                    'payment_id': str(payment_obj.id),
                    'booking_id': str(booking.id),
                    'gateway_ref': payment_obj.gateway_ref,
                }
            )
            
            # Update payment with Stripe session ID
            payment_obj.gateway_ref = session.id
            payment_obj.save()
            
            logger.info(f'Stripe checkout session created: {session.id}')
            return {
                'success': True,
                'session_id': session.id,
                'checkout_url': session.url,
                'gateway_ref': session.id
            }
            
        except stripe.error.StripeError as e:
            logger.error(f'Stripe error: {str(e)}')
            return {
                'success': False,
                'error': str(e)
            }
    
    def initiate_paynow_payment(
        self, 
        payment_obj, 
        booking,
        customer_email: str
    ) -> Dict:
        """Initiate Paynow payment using official SDK"""
        try:
            # Create Paynow payment
            payment = self.paynow.create_payment(
                payment_obj.gateway_ref,
                customer_email
            )
            
            # Add item
            payment.add(
                f'Booking: {booking.rental_property.title}',
                float(payment_obj.amount)
            )
            
            # Initiate transaction
            response = self.paynow.send(payment)
            
            if response.success:
                logger.info(f'Paynow payment initiated: {payment_obj.gateway_ref}')
                return {
                    'success': True,
                    'poll_url': response.poll_url,
                    'redirect_url': response.redirect_url,
                    'gateway_ref': payment_obj.gateway_ref
                }
            else:
                logger.error(f'Paynow error: {response.errors}')
                return {
                    'success': False,
                    'error': response.errors
                }
                
        except Exception as e:
            logger.error(f'Paynow exception: {str(e)}')
            return {
                'success': False,
                'error': str(e)
            }
    
    def initiate_flutterwave_payment(
        self, 
        payment_obj, 
        booking,
        customer_email: str,
        customer_name: str = ''
    ) -> Dict:
        """Initiate Flutterwave payment using official SDK"""
        try:
            payload = {
                'tx_ref': payment_obj.gateway_ref,
                'amount': str(payment_obj.amount),
                'currency': payment_obj.currency,
                'redirect_url': f'{settings.SITE_URL}/payment/return',
                'payment_options': 'card,mobilemoney,ussd',
                'customer': {
                    'email': customer_email,
                    'name': customer_name or customer_email,
                },
                'customizations': {
                    'title': 'StayAfrica Booking',
                    'description': f'Booking: {booking.rental_property.title}',
                    'logo': f'{settings.SITE_URL}/static/logo.png',
                },
                'meta': {
                    'payment_id': str(payment_obj.id),
                    'booking_id': str(booking.id),
                }
            }
            
            response = self.flutterwave.payment.initiate(payload)
            
            if response.get('status') == 'success':
                link = response.get('data', {}).get('link')
                logger.info(f'Flutterwave payment initiated: {payment_obj.gateway_ref}')
                return {
                    'success': True,
                    'payment_link': link,
                    'gateway_ref': payment_obj.gateway_ref
                }
            else:
                logger.error(f'Flutterwave error: {response}')
                return {
                    'success': False,
                    'error': response.get('message', 'Payment initialization failed')
                }
                
        except Exception as e:
            logger.error(f'Flutterwave exception: {str(e)}')
            return {
                'success': False,
                'error': str(e)
            }
    
    def _get_paypal_access_token(self) -> Optional[str]:
        """Get PayPal OAuth2 access token"""
        try:
            auth = base64.b64encode(f"{self.paypal_client_id}:{self.paypal_client_secret}".encode()).decode()
            headers = {
                'Authorization': f'Basic {auth}',
                'Content-Type': 'application/x-www-form-urlencoded'
            }
            response = requests.post(
                f'{self.paypal_base_url}/v1/oauth2/token',
                headers=headers,
                data={'grant_type': 'client_credentials'}
            )
            if response.status_code == 200:
                return response.json()['access_token']
            return None
        except Exception as e:
            logger.error(f'PayPal auth error: {str(e)}')
            return None
    
    def initiate_paypal_payment(
        self, 
        payment_obj, 
        booking,
        customer_email: str,
        customer_name: str = ''
    ) -> Dict:
        """Initiate PayPal payment using REST API"""
        try:
            access_token = self._get_paypal_access_token()
            if not access_token:
                return {
                    'success': False,
                    'error': 'Failed to authenticate with PayPal'
                }
            
            headers = {
                'Content-Type': 'application/json',
                'Authorization': f'Bearer {access_token}'
            }
            
            order_data = {
                'intent': 'CAPTURE',
                'purchase_units': [{
                    'reference_id': payment_obj.gateway_ref,
                    'amount': {
                        'currency_code': payment_obj.currency,
                        'value': str(payment_obj.amount)
                    },
                    'description': f'StayAfrica booking from {booking.check_in} to {booking.check_out}'
                }],
                'application_context': {
                    'return_url': f'{settings.SITE_URL}/payment/success?gateway_ref={payment_obj.gateway_ref}',
                    'cancel_url': f'{settings.SITE_URL}/payment/cancel',
                    'brand_name': 'StayAfrica',
                    'user_action': 'PAY_NOW'
                }
            }
            
            response = requests.post(
                f'{self.paypal_base_url}/v2/checkout/orders',
                headers=headers,
                json=order_data
            )
            
            if response.status_code == 201:
                order = response.json()
                approval_url = None
                for link in order.get('links', []):
                    if link.get('rel') == 'approve':
                        approval_url = link.get('href')
                        break
                
                # Store PayPal order ID
                payment_obj.gateway_ref = order['id']
                payment_obj.save()
                
                logger.info(f'PayPal order created: {order["id"]}')
                return {
                    'success': True,
                    'payment_link': approval_url,
                    'paypal_order_id': order['id'],
                    'gateway_ref': order['id']
                }
            else:
                logger.error(f'PayPal API error: {response.text}')
                return {
                    'success': False,
                    'error': response.json().get('message', 'Failed to create PayPal order')
                }
                
        except Exception as e:
            logger.error(f'PayPal exception: {str(e)}')
            return {
                'success': False,
                'error': str(e)
            }
    
    def initiate_payment(
        self, 
        payment_obj, 
        booking, 
        provider: str,
        customer_email: str,
        customer_name: str = ''
    ) -> Dict:
        """
        Main payment initiation method - routes to appropriate provider SDK
        """
        if provider == 'stripe':
            return self.initiate_stripe_payment(payment_obj, booking, customer_email)
        elif provider == 'paynow':
            return self.initiate_paynow_payment(payment_obj, booking, customer_email)
        elif provider == 'flutterwave':
            return self.initiate_flutterwave_payment(payment_obj, booking, customer_email, customer_name)
        elif provider == 'paypal':
            return self.initiate_paypal_payment(payment_obj, booking, customer_email, customer_name)
        elif provider == 'cash_on_arrival':
            # No external gateway needed
            return {
                'success': True,
                'message': 'Cash on arrival selected',
                'gateway_ref': payment_obj.gateway_ref
            }
        else:
            return {
                'success': False,
                'error': f'Provider {provider} not supported'
            }
    
    def verify_stripe_webhook(self, payload: bytes, sig_header: str) -> Optional[dict]:
        """Verify Stripe webhook signature"""
        try:
            event = stripe.Webhook.construct_event(
                payload, sig_header, self.config.stripe_webhook_secret
            )
            return event
        except ValueError:
            logger.error('Invalid Stripe webhook payload')
            return None
        except stripe.error.SignatureVerificationError:
            logger.error('Invalid Stripe webhook signature')
            return None
    
    def verify_paypal_webhook(self, headers: dict, body: str) -> bool:
        """Verify PayPal webhook signature using REST API"""
        try:
            webhook_id = getattr(self.config, 'paypal_webhook_id', '')
            if not webhook_id:
                logger.warning('PayPal webhook ID not configured')
                return True  # Allow in development
            
            access_token = self._get_paypal_access_token()
            if not access_token:
                return False
            
            verify_data = {
                'transmission_id': headers.get('PAYPAL-TRANSMISSION-ID'),
                'transmission_time': headers.get('PAYPAL-TRANSMISSION-TIME'),
                'cert_url': headers.get('PAYPAL-CERT-URL'),
                'auth_algo': headers.get('PAYPAL-AUTH-ALGO'),
                'transmission_sig': headers.get('PAYPAL-TRANSMISSION-SIG'),
                'webhook_id': webhook_id,
                'webhook_event': json.loads(body)
            }
            
            response = requests.post(
                f'{self.paypal_base_url}/v1/notifications/verify-webhook-signature',
                headers={
                    'Content-Type': 'application/json',
                    'Authorization': f'Bearer {access_token}'
                },
                json=verify_data
            )
            
            if response.status_code == 200:
                result = response.json()
                return result.get('verification_status') == 'SUCCESS'
            return False
            
        except Exception as e:
            logger.error(f'PayPal webhook verification failed: {str(e)}')
            return False
    
    def convert_currency(
        self, 
        amount: Decimal, 
        from_currency: str, 
        to_currency: str
    ) -> Decimal:
        """Convert currency using stored exchange rates"""
        if from_currency == to_currency:
            return amount
        
        from apps.payments.pricing_models import CurrencyExchangeRate
        
        try:
            rate = CurrencyExchangeRate.objects.get(
                from_currency=from_currency,
                to_currency=to_currency,
                is_active=True
            )
            return amount * rate.rate
        except CurrencyExchangeRate.DoesNotExist:
            logger.warning(f'No exchange rate found for {from_currency} -> {to_currency}')
            return amount
