"""
Enhanced Payment Gateway Service with Official SDKs
"""
from decimal import Decimal
from django.conf import settings
from typing import Dict, Optional, Tuple
import stripe
from paynow import Paynow as PaynowSDK
import requests
import base64
import json
import time
import logging

# PayPal Server SDK imports
from paypalserversdk.http.auth.o_auth_2 import ClientCredentialsAuthCredentials
from paypalserversdk.logging.configuration.api_logging_configuration import (
    LoggingConfiguration,
    RequestLoggingConfiguration,
    ResponseLoggingConfiguration,
)
from paypalserversdk.paypal_serversdk_client import PaypalServersdkClient
from paypalserversdk.controllers.orders_controller import OrdersController
from paypalserversdk.controllers.payments_controller import PaymentsController
from paypalserversdk.models.amount_with_breakdown import AmountWithBreakdown
from paypalserversdk.models.checkout_payment_intent import CheckoutPaymentIntent
from paypalserversdk.models.order_request import OrderRequest
from paypalserversdk.models.purchase_unit_request import PurchaseUnitRequest
from paypalserversdk.models.paypal_wallet_context_shipping_preference import PaypalWalletContextShippingPreference
from paypalserversdk.models.paypal_experience_user_action import PaypalExperienceUserAction
from paypalserversdk.models.payment_source import PaymentSource
from paypalserversdk.models.paypal_wallet import PaypalWallet
from paypalserversdk.models.paypal_wallet_experience_context import PaypalWalletExperienceContext
from paypalserversdk.models.refund_request import RefundRequest
from paypalserversdk.models.money import Money
from paypalserversdk.api_helper import APIHelper

logger = logging.getLogger(__name__)


class PaymentGatewayService:
    """Enhanced payment gateway with official SDK support"""
    
    # Regional providers – these are specific to each country / zone.
    # Stripe and PayPal are handled separately as INTERNATIONAL_PROVIDERS.
    REGIONAL_PROVIDERS = {
        'Zimbabwe': ['paynow', 'flutterwave', 'cash_on_arrival'],
        'South Africa': ['paystack', 'flutterwave', 'ozow'],
        'Nigeria': ['paystack', 'flutterwave'],
        'Kenya': ['flutterwave', 'mpesa'],
        'Ghana': ['paystack', 'flutterwave'],
        'Uganda': ['flutterwave'],
        'Tanzania': ['flutterwave'],
        'International': [],   # no region-specific methods
    }

    # International providers shown in EVERY region alongside the regional ones.
    INTERNATIONAL_PROVIDERS = ['stripe', 'paypal']
    STRIPE_EXCLUDED_COUNTRIES = []  # Stripe available everywhere

    # ISO-3166 country code aliases → canonical name used in REGIONAL_PROVIDERS
    COUNTRY_ALIASES = {
        'ZW': 'Zimbabwe', 'ZWE': 'Zimbabwe',
        'ZA': 'South Africa', 'ZAF': 'South Africa',
        'NG': 'Nigeria', 'NGA': 'Nigeria',
        'KE': 'Kenya', 'KEN': 'Kenya',
        'GH': 'Ghana', 'GHA': 'Ghana',
        'UG': 'Uganda', 'UGA': 'Uganda',
        'TZ': 'Tanzania', 'TZA': 'Tanzania',
    }

    PAYMENT_METHODS = {
        'paynow': 'Paynow (Ecocash/Visa)',
        'paystack': 'Paystack',
        'flutterwave': 'Flutterwave',
        'stripe': 'Stripe (Card)',
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
        
        # Paynow SDK initialization
        self.paynow_integration_id = getattr(self.config, 'paynow_integration_id', '')
        self.paynow_integration_key = getattr(self.config, 'paynow_integration_key', '')
        if self.paynow_integration_id and self.paynow_integration_key:
            return_url = getattr(self.config, 'paynow_return_url', '') or f'{settings.SITE_URL}/payment/return'
            result_url = getattr(self.config, 'paynow_result_url', '') or f'{settings.BACKEND_URL}/api/v1/payments/webhook/?provider=paynow'
            
            self.paynow = PaynowSDK(
                self.paynow_integration_id,
                self.paynow_integration_key,
                return_url,
                result_url
            )
        
        # REST API configurations (Flutterwave, Paystack)
        self.flutterwave_secret_key = getattr(self.config, 'flutterwave_secret_key', '')
        self.paystack_secret_key = getattr(self.config, 'paystack_secret_key', '')

        # PayPal Server SDK initialization
        self.paypal_client_id = getattr(self.config, 'paypal_client_id', '')
        self.paypal_client_secret = getattr(self.config, 'paypal_client_secret', '')
        self.paypal_mode = getattr(self.config, 'paypal_mode', 'sandbox')
        self.paypal_base_url = (
            'https://api-m.sandbox.paypal.com'
            if self.paypal_mode == 'sandbox'
            else 'https://api-m.paypal.com'
        )
        self._paypal_token_cache = None  # (token, expiry_timestamp)

        if self.paypal_client_id and self.paypal_client_secret:
            from paypalserversdk.environment import Environment

            paypal_environment = (
                Environment.SANDBOX
                if self.paypal_mode == 'sandbox'
                else Environment.PRODUCTION
            )

            self.paypal_client = PaypalServersdkClient(
                client_credentials_auth_credentials=ClientCredentialsAuthCredentials(
                    o_auth_client_id=self.paypal_client_id,
                    o_auth_client_secret=self.paypal_client_secret,
                ),
                environment=paypal_environment,
                logging_configuration=LoggingConfiguration(
                    log_level=logging.INFO,
                    request_logging_config=RequestLoggingConfiguration(
                        log_body=True
                    ),
                    response_logging_config=ResponseLoggingConfiguration(
                        log_headers=True
                    ),
                ),
            )
            self.orders_controller = self.paypal_client.orders
            self.payments_controller = self.paypal_client.payments
            logger.info(f'PayPal Server SDK initialized ({self.paypal_mode} mode)')
    
    def _resolve_country(self, user_country: str) -> str:
        """Resolve a raw country input to a canonical name.

        Tries: exact match → case-insensitive → ISO-code alias → 'International'.
        """
        raw = (user_country or '').strip()
        country = raw.title()

        if country in self.REGIONAL_PROVIDERS:
            return country

        for key in self.REGIONAL_PROVIDERS:
            if key.lower() == country.lower():
                return key

        canonical = self.COUNTRY_ALIASES.get(raw.upper())
        if canonical and canonical in self.REGIONAL_PROVIDERS:
            return canonical

        return 'International'

    def _is_provider_configured(self, provider: str) -> bool:
        """Check whether a provider's API keys are set in SystemConfiguration."""
        checks = {
            'stripe': lambda: self.config.stripe_secret_key,
            'paynow': lambda: getattr(self.config, 'paynow_integration_id', ''),
            'payfast': lambda: getattr(self.config, 'payfast_merchant_id', ''),
            'ozow': lambda: getattr(self.config, 'ozow_site_code', ''),
            'flutterwave': lambda: getattr(self.config, 'flutterwave_secret_key', ''),
            'paystack': lambda: getattr(self.config, 'paystack_secret_key', ''),
            'paypal': lambda: getattr(self.config, 'paypal_client_id', ''),
        }
        check = checks.get(provider)
        if check:
            return bool(check())
        # Providers with no API keys (cash_on_arrival, mpesa) are always available
        return provider in ['cash_on_arrival', 'mpesa']

    def get_available_providers(self, user_country: str) -> list:
        """Return a flat list of configured provider IDs (kept for backward compat)."""
        result = self.get_available_providers_detailed(user_country)
        return [p['id'] for p in result]

    def get_available_providers_detailed(self, user_country: str) -> list:
        """Return providers with category metadata.

        Each entry: ``{"id": "stripe", "name": "...", "category": "international"}``

        Regional providers come from REGIONAL_PROVIDERS for the resolved
        country. International providers (Stripe, PayPal) are appended
        for every region, except Stripe is excluded for countries in
        STRIPE_EXCLUDED_COUNTRIES.
        """
        country = self._resolve_country(user_country)
        regional = list(self.REGIONAL_PROVIDERS.get(country, []))

        # Build international list
        international = []
        for ip in self.INTERNATIONAL_PROVIDERS:
            if ip == 'stripe' and country in self.STRIPE_EXCLUDED_COUNTRIES:
                continue
            international.append(ip)

        # Filter to only configured providers and attach metadata
        result = []
        for pid in regional:
            if self._is_provider_configured(pid):
                result.append({
                    'id': pid,
                    'name': self.get_provider_label(pid),
                    'category': 'regional',
                })
        for pid in international:
            if self._is_provider_configured(pid):
                result.append({
                    'id': pid,
                    'name': self.get_provider_label(pid),
                    'category': 'international',
                })

        return result

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
                # Let Stripe dynamically show all eligible payment methods
                # (cards, Apple Pay, Google Pay, bank transfers, etc.)
                automatic_payment_methods={'enabled': True},
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
                success_url=f'{settings.SITE_URL}/payment/return',
                cancel_url=f'{settings.SITE_URL}/booking/failure',
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
            # Check if Paynow is initialized
            if not hasattr(self, 'paynow'):
                logger.error('Paynow SDK not initialized - credentials missing')
                return {
                    'success': False,
                    'error': 'Paynow payment gateway is not configured'
                }
            
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
        """Initiate Flutterwave payment using REST API"""
        try:
            headers = {
                'Authorization': f'Bearer {self.flutterwave_secret_key}',
                'Content-Type': 'application/json'
            }
            
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
            
            response = requests.post(
                'https://api.flutterwave.com/v3/payments',
                headers=headers,
                json=payload
            )
            
            if response.status_code == 200:
                data = response.json()
                if data.get('status') == 'success':
                    link = data.get('data', {}).get('link')
                    logger.info(f'Flutterwave payment initiated: {payment_obj.gateway_ref}')
                    return {
                        'success': True,
                        'payment_link': link,
                        'gateway_ref': payment_obj.gateway_ref
                    }
            
            # Handle error response
            try:
                error_data = response.json()
                error_msg = error_data.get('message', 'Payment initialization failed')
            except ValueError:
                error_msg = f'HTTP {response.status_code}: {response.text[:100]}'
            
            logger.error(f'Flutterwave error: {error_msg}')
            return {
                'success': False,
                'error': error_msg
            }
                
        except Exception as e:
            logger.error(f'Flutterwave exception: {str(e)}')
            return {
                'success': False,
                'error': str(e)
            }
    
    def initiate_paystack_payment(
        self, 
        payment_obj, 
        booking,
        customer_email: str,
        customer_name: str = ''
    ) -> Dict:
        """Initiate Paystack payment using REST API"""
        try:
            headers = {
                'Authorization': f'Bearer {self.paystack_secret_key}',
                'Content-Type': 'application/json'
            }
            
            payload = {
                'reference': payment_obj.gateway_ref,
                'amount': int(payment_obj.amount * 100),  # Convert to kobo/cents (amount is already Decimal)
                'currency': payment_obj.currency,
                'email': customer_email,
                'callback_url': f'{settings.SITE_URL}/payment/return',
                'metadata': {
                    'payment_id': str(payment_obj.id),
                    'booking_id': str(booking.id),
                    'customer_name': customer_name or customer_email,
                    'custom_fields': [
                        {
                            'display_name': 'Booking',
                            'variable_name': 'booking',
                            'value': f'{booking.rental_property.title}'
                        }
                    ]
                }
            }
            
            response = requests.post(
                'https://api.paystack.co/transaction/initialize',
                headers=headers,
                json=payload
            )
            
            if response.status_code == 200:
                data = response.json()
                if data.get('status') == True:  # JSON boolean comparison
                    authorization_url = data.get('data', {}).get('authorization_url')
                    logger.info(f'Paystack payment initiated: {payment_obj.gateway_ref}')
                    return {
                        'success': True,
                        'payment_link': authorization_url,
                        'gateway_ref': payment_obj.gateway_ref
                    }
            
            # Handle error response
            try:
                error_data = response.json()
                error_msg = error_data.get('message', 'Payment initialization failed')
            except ValueError:
                error_msg = f'HTTP {response.status_code}: {response.text[:100]}'
            
            logger.error(f'Paystack error: {error_msg}')
            return {
                'success': False,
                'error': error_msg
            }
                
        except Exception as e:
            logger.error(f'Paystack exception: {str(e)}')
            return {
                'success': False,
                'error': str(e)
            }
    
    def _get_paypal_access_token(self) -> Optional[str]:
        """Get PayPal OAuth2 access token with caching (used for webhook verification)"""
        # Return cached token if still valid
        if self._paypal_token_cache:
            cached_token, expiry = self._paypal_token_cache
            if time.time() < expiry:
                return cached_token

        try:
            auth = base64.b64encode(
                f"{self.paypal_client_id}:{self.paypal_client_secret}".encode()
            ).decode()
            headers = {
                'Authorization': f'Basic {auth}',
                'Content-Type': 'application/x-www-form-urlencoded',
            }
            response = requests.post(
                f'{self.paypal_base_url}/v1/oauth2/token',
                headers=headers,
                data={'grant_type': 'client_credentials'},
            )
            if response.status_code == 200:
                data = response.json()
                token = data['access_token']
                expires_in = data.get('expires_in', 32400)  # default ~9 hours
                # Cache with 5-minute buffer before expiry
                self._paypal_token_cache = (token, time.time() + expires_in - 300)
                return token
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
        """Initiate PayPal payment using official Server SDK"""
        try:
            if not hasattr(self, 'orders_controller'):
                return {
                    'success': False,
                    'error': 'PayPal payment gateway is not configured'
                }

            order_request = OrderRequest(
                intent=CheckoutPaymentIntent.CAPTURE,
                purchase_units=[
                    PurchaseUnitRequest(
                        reference_id=payment_obj.gateway_ref,
                        amount=AmountWithBreakdown(
                            currency_code=payment_obj.currency,
                            value=str(payment_obj.amount),
                        ),
                        description=(
                            f'StayAfrica booking: {booking.rental_property.title} '
                            f'({booking.check_in} to {booking.check_out})'
                        ),
                    )
                ],
                payment_source=PaymentSource(
                    paypal=PaypalWallet(
                        experience_context=PaypalWalletExperienceContext(
                            brand_name='StayAfrica',
                            shipping_preference=PaypalWalletContextShippingPreference.NO_SHIPPING,
                            user_action=PaypalExperienceUserAction.PAY_NOW,
                            return_url=(
                                f'{settings.SITE_URL}/payment/return'
                                f'?provider=paypal&gateway_ref={payment_obj.gateway_ref}'
                            ),
                            cancel_url=f'{settings.SITE_URL}/booking/failure',
                        )
                    )
                ),
            )

            response = self.orders_controller.orders_create(
                {'body': order_request}
            )

            order = APIHelper.json_deserialize(
                APIHelper.json_serialize(response.body)
            )

            # Extract approval URL (payer-action for SDK, approve for REST)
            approval_url = None
            for link in order.get('links', []):
                if link.get('rel') in ('payer-action', 'approve'):
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
                'gateway_ref': order['id'],
            }

        except Exception as e:
            logger.error(f'PayPal exception: {str(e)}')
            return {
                'success': False,
                'error': str(e),
            }

    def capture_paypal_order(self, order_id: str) -> Dict:
        """Capture an approved PayPal order after customer approval"""
        try:
            if not hasattr(self, 'orders_controller'):
                return {
                    'success': False,
                    'error': 'PayPal payment gateway is not configured'
                }

            response = self.orders_controller.orders_capture(
                {'id': order_id}
            )

            result = APIHelper.json_deserialize(
                APIHelper.json_serialize(response.body)
            )

            status = result.get('status')
            capture_id = None

            # Extract the capture ID from the response
            purchase_units = result.get('purchase_units', [])
            if purchase_units:
                captures = (
                    purchase_units[0]
                    .get('payments', {})
                    .get('captures', [])
                )
                if captures:
                    capture_id = captures[0].get('id')

            logger.info(
                f'PayPal order captured: {order_id} '
                f'(status={status}, capture={capture_id})'
            )
            return {
                'success': status == 'COMPLETED',
                'status': status,
                'capture_id': capture_id,
                'order_id': order_id,
                'details': result,
            }

        except Exception as e:
            logger.error(f'PayPal capture exception: {str(e)}')
            return {
                'success': False,
                'error': str(e),
            }

    def refund_paypal_payment(
        self,
        capture_id: str,
        amount: Optional[Decimal] = None,
        currency: str = 'USD',
        note: str = '',
    ) -> Dict:
        """Refund a captured PayPal payment (full or partial)"""
        try:
            if not hasattr(self, 'payments_controller'):
                return {
                    'success': False,
                    'error': 'PayPal payment gateway is not configured'
                }

            refund_body = {}

            # If amount is provided, do a partial refund
            if amount is not None:
                refund_body['amount'] = Money(
                    currency_code=currency,
                    value=str(amount),
                )

            if note:
                refund_body['note_to_payer'] = note

            refund_request = (
                RefundRequest(**refund_body) if refund_body else RefundRequest()
            )

            response = self.payments_controller.captures_refund({
                'capture_id': capture_id,
                'body': refund_request,
            })

            result = APIHelper.json_deserialize(
                APIHelper.json_serialize(response.body)
            )

            logger.info(
                f'PayPal refund processed: capture={capture_id}, '
                f'refund_id={result.get("id")}'
            )
            return {
                'success': result.get('status') == 'COMPLETED',
                'refund_id': result.get('id'),
                'status': result.get('status'),
                'details': result,
            }

        except Exception as e:
            logger.error(f'PayPal refund exception: {str(e)}')
            return {
                'success': False,
                'error': str(e),
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
        elif provider == 'paystack':
            return self.initiate_paystack_payment(payment_obj, booking, customer_email, customer_name)
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
                logger.error('PayPal webhook ID not configured — rejecting webhook')
                return False  # Never accept unverified webhooks
            
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
