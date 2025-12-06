"""
Email tasks for async processing
"""
from celery import shared_task
from django.core.mail import send_mail, EmailMultiAlternatives
from django.conf import settings
from django.template.loader import render_to_string
import logging

logger = logging.getLogger(__name__)


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


@shared_task
def send_verification_email(user_id):
    """Send email verification link to user"""
    from apps.users.models import User
    from utils.helpers import generate_verification_token
    import hashlib
    
    try:
        user = User.objects.get(id=user_id)
        token = generate_verification_token()
        
        # Create a hashed user identifier for security
        user_hash = hashlib.sha256(f"{user.id}{user.email}".encode()).hexdigest()[:16]
        
        # Store token (in production, use Redis or database with expiry)
        # TODO: Implement token storage with Redis
        # cache.set(f'verify_{user_hash}', token, timeout=86400)  # 24 hours
        
        subject = 'Verify your StayAfrica account'
        message = f'Welcome to StayAfrica! Please verify your account by clicking the link below:\n\n'
        message += f'{settings.FRONTEND_URL}/verify/{user_hash}/{token}\n\n'
        message += 'This link will expire in 24 hours.\n\n'
        message += 'If you did not create this account, please ignore this email.'
        
        send_email_async.delay(subject, message, [user.email])
        logger.info(f"Verification email queued for {user.email}")
    except User.DoesNotExist:
        logger.error(f"User with id {user_id} not found")
    except Exception as e:
        logger.error(f"Error sending verification email: {str(e)}")


@shared_task
def send_booking_confirmation_email(booking_id):
    """Send booking confirmation email to guest and host"""
    from apps.bookings.models import Booking
    
    try:
        booking = Booking.objects.select_related('guest', 'property__host').get(id=booking_id)
        
        # Email to guest
        guest_subject = f'Booking Confirmed - {booking.booking_ref}'
        guest_message = f'Dear {booking.guest.first_name or booking.guest.email},\n\n'
        guest_message += f'Your booking for {booking.property.title} has been confirmed!\n\n'
        guest_message += f'Booking Reference: {booking.booking_ref}\n'
        guest_message += f'Check-in: {booking.check_in}\n'
        guest_message += f'Check-out: {booking.check_out}\n'
        guest_message += f'Total: {booking.currency} {booking.grand_total}\n\n'
        guest_message += 'Thank you for choosing StayAfrica!'
        
        send_email_async.delay(guest_subject, guest_message, [booking.guest.email])
        
        # Email to host
        host_subject = f'New Booking - {booking.property.title}'
        host_message = f'Dear {booking.property.host.first_name or booking.property.host.email},\n\n'
        host_message += f'You have a new booking for {booking.property.title}!\n\n'
        host_message += f'Booking Reference: {booking.booking_ref}\n'
        host_message += f'Guest: {booking.guest.email}\n'
        host_message += f'Check-in: {booking.check_in}\n'
        host_message += f'Check-out: {booking.check_out}\n'
        host_message += f'Nights: {booking.nights}\n\n'
        host_message += 'Please ensure the property is ready for your guest.'
        
        send_email_async.delay(host_subject, host_message, [booking.property.host.email])
        
        logger.info(f"Booking confirmation emails queued for {booking.booking_ref}")
    except Booking.DoesNotExist:
        logger.error(f"Booking with id {booking_id} not found")
    except Exception as e:
        logger.error(f"Error sending booking confirmation: {str(e)}")


@shared_task
def send_payment_receipt_email(payment_id):
    """Send payment receipt to guest"""
    from apps.payments.models import Payment
    
    try:
        payment = Payment.objects.select_related('booking__guest', 'booking__property').get(id=payment_id)
        
        subject = f'Payment Receipt - {payment.gateway_ref}'
        message = f'Dear {payment.booking.guest.first_name or payment.booking.guest.email},\n\n'
        message += f'Your payment has been processed successfully!\n\n'
        message += f'Payment Reference: {payment.gateway_ref}\n'
        message += f'Booking Reference: {payment.booking.booking_ref}\n'
        message += f'Amount: {payment.currency} {payment.amount}\n'
        message += f'Property: {payment.booking.property.title}\n'
        message += f'Payment Method: {payment.get_provider_display()}\n\n'
        message += 'Thank you for your payment!'
        
        send_email_async.delay(subject, message, [payment.booking.guest.email])
        logger.info(f"Payment receipt email queued for {payment.gateway_ref}")
    except Payment.DoesNotExist:
        logger.error(f"Payment with id {payment_id} not found")
    except Exception as e:
        logger.error(f"Error sending payment receipt: {str(e)}")


@shared_task
def send_password_reset_email(user_id, reset_token):
    """Send password reset email"""
    from apps.users.models import User
    
    try:
        user = User.objects.get(id=user_id)
        
        subject = 'Reset your StayAfrica password'
        message = f'Dear {user.first_name or user.email},\n\n'
        message += 'You requested to reset your password. Click the link below to reset it:\n\n'
        message += f'{settings.FRONTEND_URL}/reset-password/{reset_token}\n\n'
        message += 'This link will expire in 1 hour.\n\n'
        message += 'If you did not request this, please ignore this email.'
        
        send_email_async.delay(subject, message, [user.email])
        logger.info(f"Password reset email queued for {user.email}")
    except User.DoesNotExist:
        logger.error(f"User with id {user_id} not found")
    except Exception as e:
        logger.error(f"Error sending password reset email: {str(e)}")


@shared_task
def send_pending_emails():
    """
    Periodic task to send any pending emails
    This is a placeholder for more complex email queue logic
    """
    logger.info("Checking for pending emails...")
    # Implementation for email queue if needed
    pass
