from celery import shared_task
from django.utils import timezone
from datetime import timedelta
import logging

logger = logging.getLogger(__name__)


@shared_task
def expire_pending_bookings():
    """
    Expire bookings that have been pending for > 24 hours.
    Notify the guest about cancellation.
    """
    from apps.bookings.models import Booking
    from tasks.notification_tasks import send_push_notification
    
    # Cutoff time: 24 hours ago
    cutoff = timezone.now() - timedelta(hours=24)
    
    # Find bookings that are 'pending' (approved but unpaid) and older than 24h
    expired_bookings = Booking.objects.filter(
        status='pending',
        updated_at__lt=cutoff
    )
    
    count = 0
    for booking in expired_bookings:
        # Double check status in case it changed during processing
        if booking.status != 'pending':
            continue
            
        booking.status = 'cancelled'
        # update_fields includes updated_at automatically? No, usually auto_now does.
        # But we want to be explicit.
        booking.save(update_fields=['status', 'updated_at'])
        
        # Notify guest
        send_push_notification.delay(
            booking.guest.id,
            "Booking Cancelled",
            f"Your booking request {booking.booking_ref} was cancelled because payment was not received within 24 hours.",
            {'booking_id': str(booking.id), 'type': 'booking_cancelled'}
        )
        
        # Notify host that dates are free
        send_push_notification.delay(
            booking.rental_property.host.id,
            "Booking Request Expired",
            f"The booking request {booking.booking_ref} has expired due to non-payment. Dates are now available.",
            {'booking_id': str(booking.id), 'type': 'booking_cancelled'}
        )
        
        count += 1
        logger.info(f"Expired pending booking {booking.booking_ref}")
        
    if count > 0:
        logger.info(f"Expired {count} pending bookings")
        
    return f"Expired {count} bookings"


@shared_task
def send_pending_booking_reminders():
    """
    Send reminder to guests with pending bookings older than 20 hours.
    Verify against Notification history to avoid duplicate alerts.
    """
    from apps.bookings.models import Booking
    from apps.notifications.models import Notification
    from tasks.notification_tasks import send_push_notification

    # Look for bookings pending between 20 and 24 hours
    start_window = timezone.now() - timedelta(hours=24)
    end_window = timezone.now() - timedelta(hours=20)
    
    pending_bookings = Booking.objects.filter(
        status='pending',
        updated_at__gt=start_window,
        updated_at__lt=end_window
    )
    
    count = 0
    for booking in pending_bookings:
        # Check if we already sent a payment_required notification for this booking
        already_notified = Notification.objects.filter(
            user=booking.guest,
            notification_type='payment_required',
            data__booking_id=str(booking.id),
            created_at__gt=timezone.now() - timedelta(hours=24)
        ).exists()
        
        if not already_notified:
            send_push_notification.delay(
                booking.guest.id,
                "Action Required: Complete Payment",
                f"Your booking {booking.booking_ref} will be cancelled in 4 hours if payment is not completed.",
                {'booking_id': str(booking.id), 'type': 'payment_required'}
            )
            count += 1
            logger.info(f"Sent payment reminder for booking {booking.booking_ref}")
            
    if count > 0:
        logger.info(f"Sent {count} payment reminders")
            
    return f"Sent {count} reminders"
