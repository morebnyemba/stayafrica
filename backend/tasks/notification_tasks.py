"""
Notification tasks for push notifications and alerts
"""
from celery import shared_task
import logging

logger = logging.getLogger(__name__)


@shared_task
def send_push_notification(user_id, title, message, data=None):
    """
    Send push notification to user's mobile device
    Placeholder for future implementation with Firebase/Expo
    """
    logger.info(f"Push notification queued for user {user_id}: {title}")
    
    # TODO: Implement with Firebase Cloud Messaging or Expo Push Notifications
    # from firebase_admin import messaging
    # message = messaging.Message(
    #     notification=messaging.Notification(
    #         title=title,
    #         body=message,
    #     ),
    #     data=data or {},
    #     token=user_device_token,
    # )
    # response = messaging.send(message)
    
    return True


@shared_task
def send_booking_reminder(booking_id):
    """
    Send reminder to guest before check-in
    """
    from apps.bookings.models import Booking
    from datetime import date, timedelta
    
    try:
        booking = Booking.objects.get(id=booking_id)
        
        # Check if check-in is tomorrow
        if booking.check_in == date.today() + timedelta(days=1):
            title = "Check-in Tomorrow!"
            message = f"Your stay at {booking.rental_property.title} starts tomorrow. Safe travels!"
            
            # Send push notification
            send_push_notification.delay(
                booking.guest.id,
                title,
                message,
                {'booking_id': booking.id, 'type': 'check_in_reminder'}
            )
            
            logger.info(f"Check-in reminder sent for booking {booking.booking_ref}")
        
        return True
    except Booking.DoesNotExist:
        logger.error(f"Booking with id {booking_id} not found")
        return False
    except Exception as e:
        logger.error(f"Error sending booking reminder: {str(e)}")
        return False


@shared_task
def send_new_message_notification(message_id):
    """
    Send notification when user receives a new message
    """
    from apps.messaging.models import Message
    
    try:
        message = Message.objects.select_related('sender', 'receiver').get(id=message_id)
        
        title = f"New message from {message.sender.first_name or message.sender.email}"
        notification_message = message.text[:100]  # First 100 characters
        
        # Send push notification to receiver
        send_push_notification.delay(
            message.receiver.id,
            title,
            notification_message,
            {'message_id': message.id, 'sender_id': message.sender.id, 'type': 'new_message'}
        )
        
        logger.info(f"Message notification sent to user {message.receiver.id}")
        return True
    except Message.DoesNotExist:
        logger.error(f"Message with id {message_id} not found")
        return False
    except Exception as e:
        logger.error(f"Error sending message notification: {str(e)}")
        return False


@shared_task
def send_review_request(booking_id):
    """
    Send review request after checkout
    """
    from apps.bookings.models import Booking
    from datetime import date, timedelta
    
    try:
        booking = Booking.objects.get(id=booking_id)
        
        # Check if checkout was within last 7 days and no review exists
        if booking.status == 'completed':
            if booking.check_out <= date.today() <= booking.check_out + timedelta(days=7):
                # Check if review doesn't exist
                if not hasattr(booking, 'review'):
                    title = "How was your stay?"
                    message = f"Please share your experience at {booking.rental_property.title}"
                    
                    send_push_notification.delay(
                        booking.guest.id,
                        title,
                        message,
                        {'booking_id': booking.id, 'type': 'review_request'}
                    )
                    
                    logger.info(f"Review request sent for booking {booking.booking_ref}")
        
        return True
    except Booking.DoesNotExist:
        logger.error(f"Booking with id {booking_id} not found")
        return False
    except Exception as e:
        logger.error(f"Error sending review request: {str(e)}")
        return False


@shared_task
def send_daily_notifications():
    """
    Daily task to send various notifications
    - Check-in reminders
    - Review requests
    - Host notifications
    """
    from apps.bookings.models import Booking
    from datetime import date, timedelta
    
    logger.info("Running daily notification task...")
    
    try:
        # Send check-in reminders (1 day before)
        tomorrow_bookings = Booking.objects.filter(
            check_in=date.today() + timedelta(days=1),
            status='confirmed'
        )
        for booking in tomorrow_bookings:
            send_booking_reminder.delay(booking.id)
        
        # Send review requests (after checkout)
        recent_checkouts = Booking.objects.filter(
            check_out__gte=date.today() - timedelta(days=7),
            check_out__lt=date.today(),
            status='completed'
        )
        for booking in recent_checkouts:
            if not hasattr(booking, 'review'):
                send_review_request.delay(booking.id)
        
        logger.info("Daily notification task completed")
        return True
    except Exception as e:
        logger.error(f"Error in daily notification task: {str(e)}")
        return False
