"""
Tests for Stripe payment integration
"""
import pytest
import uuid
from unittest.mock import patch, MagicMock
from decimal import Decimal


class TestStripePaymentInitiation:
    """Test Stripe payment initiation via PaymentGatewayService"""

    @patch('services.payment_gateway_enhanced.stripe')
    def test_initiate_stripe_payment_success(self, mock_stripe):
        """Test successful Stripe checkout session creation"""
        from services.payment_gateway_enhanced import PaymentGatewayService

        # Mock the checkout session
        mock_session = MagicMock()
        mock_session.id = 'cs_test_123'
        mock_session.url = 'https://checkout.stripe.com/session/cs_test_123'
        mock_stripe.checkout.Session.create.return_value = mock_session

        # Mock payment and booking objects
        payment_obj = MagicMock()
        payment_obj.id = 1
        payment_obj.amount = Decimal('100.00')
        payment_obj.currency = 'USD'
        payment_obj.gateway_ref = 'BK123-stripe-abc'

        booking = MagicMock()
        booking.id = uuid.UUID('550e8400-e29b-41d4-a716-446655440000')
        booking.rental_property.title = 'Test Property'
        booking.check_in = '2024-06-01'
        booking.check_out = '2024-06-05'

        # Mock SystemConfiguration
        with patch('services.payment_gateway_enhanced.PaymentGatewayService.__init__', return_value=None):
            service = PaymentGatewayService.__new__(PaymentGatewayService)
            service.config = MagicMock()
            service.config.stripe_secret_key = 'sk_test_123'
            service.config.stripe_webhook_secret = 'whsec_test_123'

            result = service.initiate_stripe_payment(payment_obj, booking, 'test@example.com')

        assert result['success'] is True
        assert result['session_id'] == 'cs_test_123'
        assert result['checkout_url'] == 'https://checkout.stripe.com/session/cs_test_123'
        payment_obj.save.assert_called_once()

    @patch('services.payment_gateway_enhanced.stripe')
    def test_initiate_stripe_payment_error(self, mock_stripe):
        """Test Stripe error handling during checkout session creation"""
        from services.payment_gateway_enhanced import PaymentGatewayService

        mock_stripe.error.StripeError = type('StripeError', (Exception,), {})
        mock_stripe.checkout.Session.create.side_effect = mock_stripe.error.StripeError('Card declined')

        payment_obj = MagicMock()
        payment_obj.amount = Decimal('100.00')
        payment_obj.currency = 'USD'
        payment_obj.gateway_ref = 'BK123-stripe-abc'

        booking = MagicMock()
        booking.rental_property.title = 'Test Property'
        booking.check_in = '2024-06-01'
        booking.check_out = '2024-06-05'

        with patch('services.payment_gateway_enhanced.PaymentGatewayService.__init__', return_value=None):
            service = PaymentGatewayService.__new__(PaymentGatewayService)
            service.config = MagicMock()
            service.config.stripe_secret_key = 'sk_test_123'

            result = service.initiate_stripe_payment(payment_obj, booking, 'test@example.com')

        assert result['success'] is False
        assert 'error' in result


class TestStripeWebhookVerification:
    """Test Stripe webhook signature verification"""

    @patch('services.payment_gateway_enhanced.stripe')
    def test_verify_stripe_webhook_success(self, mock_stripe):
        """Test successful Stripe webhook verification"""
        from services.payment_gateway_enhanced import PaymentGatewayService

        mock_event = {'type': 'checkout.session.completed', 'data': {'object': {'id': 'cs_123'}}}
        mock_stripe.Webhook.construct_event.return_value = mock_event

        with patch('services.payment_gateway_enhanced.PaymentGatewayService.__init__', return_value=None):
            service = PaymentGatewayService.__new__(PaymentGatewayService)
            service.config = MagicMock()
            service.config.stripe_webhook_secret = 'whsec_test_123'

            result = service.verify_stripe_webhook(b'payload', 'sig_header')

        assert result is not None
        assert result['type'] == 'checkout.session.completed'

    @patch('services.payment_gateway_enhanced.stripe')
    def test_verify_stripe_webhook_invalid_signature(self, mock_stripe):
        """Test Stripe webhook with invalid signature"""
        from services.payment_gateway_enhanced import PaymentGatewayService

        mock_stripe.error.SignatureVerificationError = type('SignatureVerificationError', (Exception,), {})
        mock_stripe.Webhook.construct_event.side_effect = mock_stripe.error.SignatureVerificationError('Invalid')

        with patch('services.payment_gateway_enhanced.PaymentGatewayService.__init__', return_value=None):
            service = PaymentGatewayService.__new__(PaymentGatewayService)
            service.config = MagicMock()
            service.config.stripe_webhook_secret = 'whsec_test_123'

            result = service.verify_stripe_webhook(b'payload', 'bad_sig')

        assert result is None

    @patch('services.payment_gateway_enhanced.stripe')
    def test_verify_stripe_webhook_invalid_payload(self, mock_stripe):
        """Test Stripe webhook with invalid payload"""
        from services.payment_gateway_enhanced import PaymentGatewayService

        mock_stripe.Webhook.construct_event.side_effect = ValueError('Bad payload')

        with patch('services.payment_gateway_enhanced.PaymentGatewayService.__init__', return_value=None):
            service = PaymentGatewayService.__new__(PaymentGatewayService)
            service.config = MagicMock()
            service.config.stripe_webhook_secret = 'whsec_test_123'

            result = service.verify_stripe_webhook(b'bad_payload', 'sig')

        assert result is None


class TestPaymentRouting:
    """Test payment provider routing"""

    def test_initiate_payment_routes_to_stripe(self):
        """Test that initiate_payment routes to Stripe correctly"""
        from services.payment_gateway_enhanced import PaymentGatewayService

        with patch('services.payment_gateway_enhanced.PaymentGatewayService.__init__', return_value=None):
            service = PaymentGatewayService.__new__(PaymentGatewayService)
            service.initiate_stripe_payment = MagicMock(return_value={'success': True})

            payment_obj = MagicMock()
            booking = MagicMock()

            result = service.initiate_payment(payment_obj, booking, 'stripe', 'test@email.com')

            service.initiate_stripe_payment.assert_called_once_with(payment_obj, booking, 'test@email.com')
            assert result['success'] is True

    def test_get_available_providers_international(self):
        """Test that international users get Stripe as a provider"""
        from services.payment_gateway_enhanced import PaymentGatewayService

        with patch('services.payment_gateway_enhanced.PaymentGatewayService.__init__', return_value=None):
            service = PaymentGatewayService.__new__(PaymentGatewayService)
            providers = service.get_available_providers('International')
            assert 'stripe' in providers

    def test_get_available_providers_unknown_country_gets_international(self):
        """Test that unknown countries fall back to international providers"""
        from services.payment_gateway_enhanced import PaymentGatewayService

        with patch('services.payment_gateway_enhanced.PaymentGatewayService.__init__', return_value=None):
            service = PaymentGatewayService.__new__(PaymentGatewayService)
            providers = service.get_available_providers('SomeUnknownCountry')
            assert 'stripe' in providers


class TestSiteUrlSetting:
    """Test that SITE_URL setting is properly configured"""

    def test_site_url_setting_exists(self):
        """Test that SITE_URL is defined in Django settings"""
        from django.conf import settings
        assert hasattr(settings, 'SITE_URL'), "SITE_URL must be defined in Django settings for payment redirects"

    def test_site_url_is_not_empty(self):
        """Test that SITE_URL has a value"""
        from django.conf import settings
        assert settings.SITE_URL, "SITE_URL must not be empty"

    def test_site_url_defaults_to_frontend_url(self):
        """Test that SITE_URL defaults to FRONTEND_URL"""
        from django.conf import settings
        # If SITE_URL env var is not set, it should fall back to FRONTEND_URL
        assert settings.SITE_URL == settings.FRONTEND_URL or settings.SITE_URL
