"""
Email tasks for async processing
"""
from celery import shared_task
from django.core.mail import send_mail, EmailMultiAlternatives
from django.conf import settings
from django.template.loader import render_to_string
from django.utils.html import strip_tags
from django.core.cache import cache
from datetime import datetime
import logging
import hashlib

logger = logging.getLogger(__name__)


def _get_base_context():
    """Get base context for email templates"""
    return {
        'frontend_url': getattr(settings, 'FRONTEND_URL', 'https://stayafrica.com'),
        'year': datetime.now().year,
    }


@shared_task(bind=True, max_retries=3)
def send_email_async(self, subject, message, recipient_list, html_message=None):
    """
    Async task for sending emails via Celery
    Retries up to 3 times on failure with exponential backoff
    """
    try:
        if html_message:
            msg = EmailMultiAlternatives(
                subject, 
                message, 
                settings.EMAIL_HOST_USER, 
                recipient_list
            )
            msg.attach_alternative(html_message, "text/html")
            msg.send()
        else:
            send_mail(subject, message, settings.EMAIL_HOST_USER, recipient_list)
        
        logger.info(f"Email sent successfully to {recipient_list}")
        return True
    except Exception as exc:
        logger.error(f"Error sending email to {recipient_list}: {str(exc)}")
        # Retry with exponential backoff
        raise self.retry(exc=exc, countdown=60 * (2 ** self.request.retries))


@shared_task(bind=True, max_retries=3)
def send_templated_email(self, template_name, subject, recipient_list, context):
    """
    Send an email using an HTML template
    """
    try:
        full_context = {**_get_base_context(), **context}
        html_content = render_to_string(f'emails/{template_name}.html', full_context)
        text_content = strip_tags(html_content)
        
        msg = EmailMultiAlternatives(
            subject,
            text_content,
            settings.EMAIL_HOST_USER,
            recipient_list
        )
        msg.attach_alternative(html_content, 'text/html')
        msg.send()
        
        logger.info(f"Templated email '{template_name}' sent to {recipient_list}")
        return True
    except Exception as exc:
        logger.error(f"Error sending templated email to {recipient_list}: {str(exc)}")
        raise self.retry(exc=exc, countdown=60 * (2 ** self.request.retries))


@shared_task
def send_verification_email(user_id):
    """Send email verification link to user using HTML template"""
    from apps.users.models import User
    from utils.helpers import generate_verification_token
    
    try:
        user = User.objects.get(id=user_id)
        token = generate_verification_token()
        
        # Create a hashed user identifier for security
        user_hash = hashlib.sha256(f"{user.id}{user.email}".encode()).hexdigest()[:16]
        verification_token = f"{user_hash}/{token}"
        
        # Store token in cache with 24-hour expiry
        cache_key = f'verify_{user_hash}'
        cache.set(cache_key, {'token': token, 'user_id': user.id}, timeout=86400)  # 24 hours
        
        context = {
            'user_name': user.first_name or user.email.split('@')[0],
            'verification_url': f"{settings.FRONTEND_URL}/verify/{verification_token}",
        }
        
        send_templated_email.delay(
            'verification_email',
            'Verify your StayAfrica account',
            [user.email],
            context
        )
        
        logger.info(f"Verification email queued for {user.email}")
    except User.DoesNotExist:
        logger.error(f"User with id {user_id} not found")
    except Exception as e:
        logger.error(f"Error sending verification email: {str(e)}")


@shared_task
def send_booking_confirmation_email(booking_id):
    """Send booking confirmation email to guest and host using HTML templates"""
    from apps.bookings.models import Booking
    
    try:
        booking = Booking.objects.select_related(
            'guest', 'rental_property', 'rental_property__host'
        ).get(id=booking_id)
        
        # Email to guest
        guest_context = {
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
        
        send_templated_email.delay(
            'booking_confirmation',
            f'Booking Confirmed - {booking.booking_ref}',
            [booking.guest.email],
            guest_context
        )
        
        # Email to host
        host_context = {
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
            'host_payout': f"{booking.host_payout:,.2f}" if hasattr(booking, 'host_payout') else f"{booking.grand_total:,.2f}",
            'booking_url': f"{settings.FRONTEND_URL}/host/bookings/{booking.id}",
            'message_url': f"{settings.FRONTEND_URL}/messages",
        }
        
        send_templated_email.delay(
            'host_new_booking',
            f'New Booking - {booking.rental_property.title}',
            [booking.rental_property.host.email],
            host_context
        )
        
        logger.info(f"Booking confirmation emails queued for {booking.booking_ref}")
    except Booking.DoesNotExist:
        logger.error(f"Booking with id {booking_id} not found")
    except Exception as e:
        logger.error(f"Error sending booking confirmation: {str(e)}")


@shared_task
def send_payment_receipt_email(payment_id):
    """Send payment receipt to guest using HTML template"""
    from apps.payments.models import Payment
    
    try:
        payment = Payment.objects.select_related(
            'booking__guest', 'booking__rental_property'
        ).get(id=payment_id)
        
        context = {
            'user_name': payment.booking.guest.first_name or payment.booking.guest.email.split('@')[0],
            'payment_ref': payment.gateway_ref,
            'booking_ref': payment.booking.booking_ref,
            'property_title': payment.booking.rental_property.title,
            'currency': payment.currency,
            'amount': f"{payment.amount:,.2f}",
            'payment_method': payment.get_provider_display(),
            'payment_date': payment.created_at.strftime('%B %d, %Y at %H:%M'),
            'booking_url': f"{settings.FRONTEND_URL}/bookings/{payment.booking.id}",
        }
        
        send_templated_email.delay(
            'payment_receipt',
            f'Payment Receipt - {payment.gateway_ref}',
            [payment.booking.guest.email],
            context
        )
        
        logger.info(f"Payment receipt email queued for {payment.gateway_ref}")
    except Payment.DoesNotExist:
        logger.error(f"Payment with id {payment_id} not found")
    except Exception as e:
        logger.error(f"Error sending payment receipt: {str(e)}")


@shared_task
def send_password_reset_email(user_id, reset_token):
    """Send password reset email using HTML template"""
    from apps.users.models import User
    
    try:
        user = User.objects.get(id=user_id)
        
        context = {
            'user_name': user.first_name or user.email.split('@')[0],
            'reset_url': f"{settings.FRONTEND_URL}/reset-password/{reset_token}",
        }
        
        send_templated_email.delay(
            'password_reset',
            'Reset your StayAfrica password',
            [user.email],
            context
        )
        
        logger.info(f"Password reset email queued for {user.email}")
    except User.DoesNotExist:
        logger.error(f"User with id {user_id} not found")
    except Exception as e:
        logger.error(f"Error sending password reset email: {str(e)}")


@shared_task
def send_identity_verification_email(user_id, status, rejection_reason=None):
    """Send identity verification status email"""
    from apps.users.models import User
    
    try:
        user = User.objects.get(id=user_id)
        
        if status == 'approved':
            context = {
                'user_name': user.first_name or user.email.split('@')[0],
            }
            send_templated_email.delay(
                'identity_verification_approved',
                'Identity Verified! ✅',
                [user.email],
                context
            )
        elif status == 'rejected':
            context = {
                'user_name': user.first_name or user.email.split('@')[0],
                'rejection_reason': rejection_reason or 'Please contact support for more information.',
            }
            send_templated_email.delay(
                'identity_verification_rejected',
                'Verification Update',
                [user.email],
                context
            )
        
        logger.info(f"Identity verification {status} email queued for {user.email}")
    except User.DoesNotExist:
        logger.error(f"User with id {user_id} not found")
    except Exception as e:
        logger.error(f"Error sending identity verification email: {str(e)}")


@shared_task
def send_pending_emails():
    """
    Periodic task to send any pending emails
    This is a placeholder for more complex email queue logic
    """
    logger.info("Checking for pending emails...")
    # Implementation for email queue if needed
    pass
