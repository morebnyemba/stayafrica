"""
Email Service
High-level email API that delegates to Celery tasks for async delivery.
Supports both environment variable and database-based SMTP configuration.
"""
from django.conf import settings
import logging

logger = logging.getLogger(__name__)


class EmailService:
    """
    Convenience wrapper around email tasks.
    Prefer calling these methods instead of importing tasks directly —
    they handle serialization and graceful fallback when Celery is unavailable.
    """

    @staticmethod
    def send_verification_email(user):
        """Queue verification email for user."""
        try:
            from tasks.email_tasks import send_verification_email
            send_verification_email.delay(user.id)
        except Exception as e:
            logger.error(f"Could not queue verification email for {user.email}: {e}")

    @staticmethod
    def send_password_reset_email(user, reset_token):
        """Queue password reset email."""
        try:
            from tasks.email_tasks import send_password_reset_email
            send_password_reset_email.delay(user.id, reset_token)
        except Exception as e:
            logger.error(f"Could not queue password reset for {user.email}: {e}")

    @staticmethod
    def send_booking_confirmation(booking):
        """Queue booking confirmation emails (guest + host)."""
        try:
            from tasks.email_tasks import send_booking_confirmation_email
            send_booking_confirmation_email.delay(str(booking.id))
        except Exception as e:
            logger.error(f"Could not queue booking confirmation for {booking.booking_ref}: {e}")

    @staticmethod
    def send_payment_receipt(payment):
        """Queue payment receipt email."""
        try:
            from tasks.email_tasks import send_payment_receipt_email
            send_payment_receipt_email.delay(payment.id)
        except Exception as e:
            logger.error(f"Could not queue payment receipt for {payment.gateway_ref}: {e}")

    @staticmethod
    def send_identity_verification_email(user, status, rejection_reason=None):
        """Queue identity verification status email."""
        try:
            from tasks.email_tasks import send_identity_verification_email
            send_identity_verification_email.delay(user.id, status, rejection_reason)
        except Exception as e:
            logger.error(f"Could not queue identity verification email for {user.email}: {e}")
