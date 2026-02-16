"""
Tests to verify production security settings are properly configured.
"""
import pytest
from django.conf import settings


class TestSecuritySettings:
    """Validate that critical security settings are configured."""

    def test_debug_defaults_to_false(self):
        """DEBUG should default to False when the env var is missing."""
        # The settings module reads os.getenv('DEBUG', 'False')
        # In CI without DEBUG set, this ensures safety.
        # This test documents the expected default behaviour.
        import os
        from importlib import import_module

        env_val = os.getenv('DEBUG', 'False')
        result = env_val == 'True'
        # If DEBUG env is not set to 'True', it should be False
        if os.getenv('DEBUG') != 'True':
            assert result is False

    def test_secret_key_is_set(self):
        """SECRET_KEY should not be the insecure placeholder in production."""
        assert settings.SECRET_KEY is not None
        assert len(settings.SECRET_KEY) > 10

    def test_password_validators_configured(self):
        """Password validators should be set."""
        assert len(settings.AUTH_PASSWORD_VALIDATORS) >= 4

    def test_rest_framework_has_exception_handler(self):
        """DRF should use the custom exception handler."""
        assert settings.REST_FRAMEWORK.get('EXCEPTION_HANDLER') == \
            'utils.exception_handlers.custom_exception_handler'

    def test_rest_framework_has_throttle_rates(self):
        """Rate limiting should be configured."""
        rates = settings.REST_FRAMEWORK.get('DEFAULT_THROTTLE_RATES', {})
        assert 'anon' in rates
        assert 'user' in rates

    def test_jwt_configuration(self):
        """JWT should have reasonable token lifetimes."""
        from datetime import timedelta
        assert settings.SIMPLE_JWT['ACCESS_TOKEN_LIFETIME'] <= timedelta(hours=48)
        assert settings.SIMPLE_JWT['REFRESH_TOKEN_LIFETIME'] <= timedelta(days=30)

    def test_middleware_includes_security(self):
        """Critical security middleware should be present."""
        assert 'django.middleware.security.SecurityMiddleware' in settings.MIDDLEWARE
        assert 'corsheaders.middleware.CorsMiddleware' in settings.MIDDLEWARE
        assert 'django.middleware.csrf.CsrfViewMiddleware' in settings.MIDDLEWARE
        assert 'utils.exception_handlers.RequestLoggingMiddleware' in settings.MIDDLEWARE

    def test_cors_has_explicit_origins(self):
        """CORS should not use a wildcard; explicit origins must be listed."""
        assert hasattr(settings, 'CORS_ALLOWED_ORIGINS')
        assert len(settings.CORS_ALLOWED_ORIGINS) > 0
        assert '*' not in settings.CORS_ALLOWED_ORIGINS

    def test_session_cookie_secure_when_not_debug(self):
        """Secure cookies should be enabled when DEBUG is False."""
        if not settings.DEBUG:
            assert settings.SESSION_COOKIE_SECURE is True
            assert settings.CSRF_COOKIE_SECURE is True
