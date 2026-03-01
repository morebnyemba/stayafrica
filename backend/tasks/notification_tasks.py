"""
Notification tasks for push notifications and alerts.
Supports Expo Push Notifications for mobile and stores in-app notifications.
"""
from celery import shared_task
import logging
import json

logger = logging.getLogger(__name__)


def _get_expo_push_tokens(user_id):
    """Retrieve Expo push tokens for a user's registered devices."""
    from apps.users.models import User
    try:
        user = User.objects.get(id=user_id)
        tokens = getattr(user, 'device_tokens', None)
        if tokens and isinstance(tokens, list):
            return [t for t in tokens if t.startswith('ExponentPushToken')]
        return []
    except Exception:
        return []


def _send_expo_push(tokens, title, body, data=None):
    """Send push notification via Expo Push API."""
    import requests
    
    messages = [
        {
            'to': token,
            'sound': 'default',
            'title': title,
            'body': body,
            'data': data or {},
        }
        for token in tokens
    ]
    
    try:
        response = requests.post(
            'https://exp.host/--/api/v2/push/send',
            json=messages,
            headers={
                'Accept': 'application/json',
                'Content-Type': 'application/json',
            },
            timeout=10,
        )
        result = response.json()
        logger.info(f"Expo push response: {response.status_code}")
        return result
    except requests.RequestException as e:
        logger.error(f"Expo push failed: {e}")
        return None


def _create_in_app_notification(user_id, title, message, data=None):
    """Store notification in database for in-app display."""
    from apps.notifications.models import Notification
    try:
        Notification.objects.create(
            recipient_id=user_id,
            title=title,
            message=message,
            notification_type=data.get('type', 'general') if data else 'general',
            data=data or {},
        )
    except Exception as e:
        logger.error(f"Failed to create in-app notification: {e}")


@shared_task(bind=True, max_retries=3, default_retry_delay=60)
def send_push_notification(self, user_id, title, message, data=None):
    """
    Send push notification to user's mobile device via Expo Push API.
    Falls back to in-app notification if no device tokens found.
    """
    logger.info(f"Push notification queued for user {user_id}: {title}")
    
    # Always create in-app notification
    _create_in_app_notification(user_id, title, message, data)
    
    # Try Expo push
    tokens = _get_expo_push_tokens(user_id)
    if tokens:
        try:
            result = _send_expo_push(tokens, title, message, data)
            if result is None:
                raise Exception("Expo push returned None")
            logger.info(f"Push sent to {len(tokens)} devices for user {user_id}")
        except Exception as exc:
            logger.warning(f"Push notification retry for user {user_id}: {exc}")
            self.retry(exc=exc)
    else:
        logger.info(f"No device tokens for user {user_id}, in-app notification created")
    
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
            send_booking_reminder.delay(str(booking.id))
        
        # Send review requests (after checkout)
        recent_checkouts = Booking.objects.filter(
            check_out__gte=date.today() - timedelta(days=7),
            check_out__lt=date.today(),
            status='completed'
        )
        for booking in recent_checkouts:
            if not hasattr(booking, 'review'):
                send_review_request.delay(str(booking.id))
        
        logger.info("Daily notification task completed")
        return True
    except Exception as e:
        logger.error(f"Error in daily notification task: {str(e)}")
        return False
