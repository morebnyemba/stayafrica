"""
Celery tasks for sending notifications asynchronously.
"""
import logging

from celery import shared_task

logger = logging.getLogger(__name__)


@shared_task(bind=True, max_retries=3, default_retry_delay=30)
def send_notification_task(self, user_id: int, notification_type: str, title: str, body: str,
                           data: dict | None = None, deep_link: str = ""):
    """
    Async task to send a push + in-app notification.
    Retries up to 3 times on failure.
    """
    from django.contrib.auth import get_user_model
    from apps.notifications.services import send_notification

    User = get_user_model()
    try:
        user = User.objects.get(pk=user_id)
    except User.DoesNotExist:
        logger.error(f"send_notification_task: user {user_id} not found")
        return

    try:
        notification = send_notification(
            user=user,
            notification_type=notification_type,
            title=title,
            body=body,
            data=data,
            deep_link=deep_link,
        )
        if notification:
            logger.info(f"Notification task completed: {notification.id} ({notification.status})")
        else:
            logger.info(f"Notification suppressed by preferences for user {user_id}")
    except Exception as exc:
        logger.error(f"send_notification_task failed: {exc}")
        raise self.retry(exc=exc)


@shared_task
def send_booking_confirmed_notification(user_id: int, booking_id: str, property_name: str):
    """Send a booking-confirmed push notification."""
    send_notification_task.delay(
        user_id=user_id,
        notification_type="booking_confirmed",
        title="Booking Confirmed!",
        body=f"Your booking at {property_name} has been confirmed.",
        data={"booking_id": booking_id},
        deep_link=f"/bookings/{booking_id}",
    )


@shared_task
def send_booking_cancelled_notification(user_id: int, booking_id: str, property_name: str):
    """Send a booking-cancelled push notification."""
    send_notification_task.delay(
        user_id=user_id,
        notification_type="booking_cancelled",
        title="Booking Cancelled",
        body=f"Your booking at {property_name} has been cancelled.",
        data={"booking_id": booking_id},
        deep_link=f"/bookings/{booking_id}",
    )


@shared_task
def send_new_message_notification(user_id: int, sender_name: str, conversation_id: str):
    """Send a new-message push notification."""
    send_notification_task.delay(
        user_id=user_id,
        notification_type="new_message",
        title="New Message",
        body=f"{sender_name} sent you a message.",
        data={"conversation_id": conversation_id},
        deep_link=f"/inbox/{conversation_id}",
    )


@shared_task
def send_payment_received_notification(user_id: int, amount: str, booking_id: str):
    """Send a payment-received push notification (for hosts)."""
    send_notification_task.delay(
        user_id=user_id,
        notification_type="payment_received",
        title="Payment Received",
        body=f"You received a payment of {amount}.",
        data={"booking_id": booking_id},
        deep_link=f"/bookings/{booking_id}",
    )


@shared_task
def send_review_received_notification(user_id: int, property_name: str, reviewer_name: str, property_id: str):
    """Send a review-received push notification (for hosts)."""
    send_notification_task.delay(
        user_id=user_id,
        notification_type="review_received",
        title="New Review",
        body=f"{reviewer_name} left a review on {property_name}.",
        data={"property_id": property_id},
        deep_link=f"/properties/{property_id}",
    )
