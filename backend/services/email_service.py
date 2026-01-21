"""
Email Service
Handles sending emails for bookings, payments, verification
Supports both environment variable and database-based SMTP configuration.
"""
from django.core.mail import send_mail, EmailMultiAlternatives, get_connection
from django.conf import settings
from django.template.loader import render_to_string
from django.utils.html import strip_tags
from datetime import datetime
import logging

logger = logging.getLogger(__name__)


class EmailService:
    """Email service with HTML template support and database configuration"""
    
    @staticmethod
    def _get_email_config():
        """
        Get email configuration from database if available, 
        otherwise fall back to environment variables.
        """
        try:
            from apps.notifications.models import EmailConfiguration
            config = EmailConfiguration.objects.filter(is_active=True).first()
            if config:
                return config
        except Exception as e:
            logger.debug(f"Could not load email config from database: {e}")
        return None
    
    @staticmethod
    def _get_email_connection():
        """
        Get email connection using database config or fall back to Django settings.
        """
        config = EmailService._get_email_config()
        
        if config:
            return get_connection(
                backend=config.backend,
                host=config.host,
                port=config.port,
                username=config.username,
                password=config.password,
                use_tls=config.get_use_tls(),
                use_ssl=config.get_use_ssl(),
                fail_silently=config.fail_silently,
                timeout=config.timeout,
            )
        # Fall back to Django's default connection (uses settings.py / env vars)
        return None
    
    @staticmethod
    def _get_from_email():
        """Get the from email address from database config or settings."""
        config = EmailService._get_email_config()
        if config:
            return f'{config.default_from_name} <{config.default_from_email}>'
        return settings.EMAIL_HOST_USER
    
    @staticmethod
    def _get_base_context():
        """Get base context for email templates"""
        return {
            'frontend_url': getattr(settings, 'FRONTEND_URL', 'https://stayafrica.com'),
            'year': datetime.now().year,
        }
    
    @staticmethod
    def _send_html_email(subject, template_name, context, recipient_list):
        """Send HTML email with plain text fallback"""
        full_context = {**EmailService._get_base_context(), **context}
        
        try:
            html_content = render_to_string(f'emails/{template_name}.html', full_context)
            text_content = strip_tags(html_content)
            
            # Get connection and from email
            connection = EmailService._get_email_connection()
            from_email = EmailService._get_from_email()
            
            msg = EmailMultiAlternatives(
                subject,
                text_content,
                from_email,
                recipient_list,
                connection=connection
            )
            msg.attach_alternative(html_content, 'text/html')
            msg.send()
            logger.info(f"Email sent successfully: {subject} to {recipient_list}")
            return True
        except Exception as e:
            logger.error(f"Error sending email: {e}")
            return False
    
    @staticmethod
    def send_verification_email(user, verification_token):
        """Send email verification link with HTML template"""
        context = {
            'user_name': user.first_name or user.email.split('@')[0],
            'verification_url': f"{settings.FRONTEND_URL}/verify/{verification_token}",
        }
        return EmailService._send_html_email(
            'Verify your StayAfrica account',
            'verification_email',
            context,
            [user.email]
        )
    
    @staticmethod
    def send_password_reset_email(user, reset_token):
        """Send password reset email with HTML template"""
        context = {
            'user_name': user.first_name or user.email.split('@')[0],
            'reset_url': f"{settings.FRONTEND_URL}/reset-password/{reset_token}",
        }
        return EmailService._send_html_email(
            'Reset your StayAfrica password',
            'password_reset',
            context,
            [user.email]
        )
    
    @staticmethod
    def send_booking_confirmation(booking):
        """Send booking confirmation email with HTML template"""
        context = {
            'guest_name': booking.guest.first_name or booking.guest.email.split('@')[0],
            'booking_ref': booking.booking_ref,
            'property_title': booking.rental_property.title,
            'property_location': f"{booking.rental_property.city}, {booking.rental_property.country}",
            'check_in': booking.check_in.strftime('%B %d, %Y'),
            'check_out': booking.check_out.strftime('%B %d, %Y'),
            'num_guests': booking.number_of_guests,
            'currency': booking.currency,
            'total_amount': f"{booking.grand_total:,.2f}",
            'booking_url': f"{settings.FRONTEND_URL}/bookings/{booking.id}",
        }
        return EmailService._send_html_email(
            f'Booking Confirmed - {booking.booking_ref}',
            'booking_confirmation',
            context,
            [booking.guest.email]
        )
    
    @staticmethod
    def send_host_new_booking(booking):
        """Send new booking notification to host with HTML template"""
        # Calculate host payout
        host_payout = booking.nightly_total + booking.fees_total - getattr(booking, 'commission_fee', 0)
        
        context = {
            'host_name': booking.rental_property.host.first_name or booking.rental_property.host.email.split('@')[0],
            'booking_ref': booking.booking_ref,
            'property_title': booking.rental_property.title,
            'guest_name': booking.guest.get_full_name() or booking.guest.email,
            'guest_email': booking.guest.email,
            'check_in': booking.check_in.strftime('%B %d, %Y'),
            'check_out': booking.check_out.strftime('%B %d, %Y'),
            'num_nights': booking.nights,
            'num_guests': booking.number_of_guests,
            'currency': booking.currency,
            'host_payout': f"{host_payout:,.2f}",
            'booking_url': f"{settings.FRONTEND_URL}/host/bookings/{booking.id}",
            'message_url': f"{settings.FRONTEND_URL}/messages",
        }
        return EmailService._send_html_email(
            f'New Booking - {booking.rental_property.title}',
            'host_new_booking',
            context,
            [booking.rental_property.host.email]
        )
    
    @staticmethod
    def send_payment_receipt(payment):
        """Send payment receipt with HTML template"""
        booking = payment.booking
        context = {
            'user_name': booking.guest.first_name or booking.guest.email.split('@')[0],
            'payment_ref': payment.gateway_ref,
            'booking_ref': booking.booking_ref,
            'property_title': booking.rental_property.title,
            'currency': payment.currency,
            'amount': f"{payment.amount:,.2f}",
            'payment_method': payment.get_provider_display(),
            'payment_date': payment.created_at.strftime('%B %d, %Y at %H:%M'),
            'booking_url': f"{settings.FRONTEND_URL}/bookings/{booking.id}",
        }
        return EmailService._send_html_email(
            f'Payment Receipt - {payment.gateway_ref}',
            'payment_receipt',
            context,
            [booking.guest.email]
        )
    
    @staticmethod
    def send_identity_verification_approved(user):
        """Send identity verification approved email"""
        context = {
            'user_name': user.first_name or user.email.split('@')[0],
        }
        return EmailService._send_html_email(
            'Identity Verified! ✅',
            'identity_verification_approved',
            context,
            [user.email]
        )
    
    @staticmethod
    def send_identity_verification_rejected(user, rejection_reason):
        """Send identity verification rejected email"""
        context = {
            'user_name': user.first_name or user.email.split('@')[0],
            'rejection_reason': rejection_reason,
        }
        return EmailService._send_html_email(
            'Verification Update',
            'identity_verification_rejected',
            context,
            [user.email]
        )
