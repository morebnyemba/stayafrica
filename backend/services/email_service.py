"""
Email Service
Handles sending emails for bookings, payments, verification
"""
from django.core.mail import send_mail
from django.conf import settings
from celery import shared_task

class EmailService:
    @staticmethod
    def send_verification_email(user):
        """Send email verification link"""
        subject = 'Verify your StayAfrica account'
        message = f'Click the link to verify your account: {settings.FRONTEND_URL}/verify/{user.id}'
        send_mail(subject, message, settings.EMAIL_HOST_USER, [user.email])
    
    @staticmethod
    def send_booking_confirmation(booking):
        """Send booking confirmation email"""
        subject = f'Booking Confirmed - {booking.booking_ref}'
        message = f'Your booking for {booking.rental_property.title} is confirmed'
        send_mail(subject, message, settings.EMAIL_HOST_USER, [booking.guest.email])
    
    @staticmethod
    def send_payment_receipt(payment):
        """Send payment receipt"""
        subject = f'Payment Receipt - {payment.gateway_ref}'
        message = f'Your payment of {payment.booking.grand_total} has been processed'
        send_mail(subject, message, settings.EMAIL_HOST_USER, [payment.booking.guest.email])
    
    @staticmethod
    def send_host_notification(booking):
        """Send host notification for new booking"""
        subject = f'New Booking Request - {booking.rental_property.title}'
        message = f'{booking.guest.email} has booked your property'
        send_mail(subject, message, settings.EMAIL_HOST_USER, [booking.rental_property.host.email])

@shared_task
def send_email_async(subject, message, recipient):
    """Async task for sending emails via Celery"""
    send_mail(subject, message, settings.EMAIL_HOST_USER, [recipient])
