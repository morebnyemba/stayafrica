"""
Notification Service
Handles sending push notifications via Expo Push API and Firebase Cloud Messaging.
Also creates in-app notification records.
"""
import json
import logging
from typing import Optional

import requests
from django.conf import settings
from django.utils import timezone

from apps.notifications.models import Notification, NotificationPreference, PushToken

logger = logging.getLogger(__name__)

# Expo Push API endpoint
EXPO_PUSH_URL = "https://exp.host/--/api/v2/push/send"


def should_send_notification(user, notification_type: str) -> bool:
    """Check user's notification preferences to decide whether to send."""
    try:
        prefs = NotificationPreference.objects.get(user=user)
        return getattr(prefs, notification_type, True)
    except NotificationPreference.DoesNotExist:
        # No preferences set — default to sending
        return True


def send_push_to_expo(tokens: list[str], title: str, body: str, data: dict | None = None) -> list[dict]:
    """
    Send push notifications via the Expo Push API.
    Works for Expo Push Tokens (ExponentPushToken[xxx]).
    Returns a list of response tickets.
    """
    messages = []
    for token in tokens:
        msg = {
            "to": token,
            "sound": "default",
            "title": title,
            "body": body,
            "channelId": "default",
        }
        if data:
            msg["data"] = data
        messages.append(msg)

    if not messages:
        return []

    try:
        response = requests.post(
            EXPO_PUSH_URL,
            json=messages,
            headers={
                "Accept": "application/json",
                "Content-Type": "application/json",
            },
            timeout=15,
        )
        response.raise_for_status()
        result = response.json()
        tickets = result.get("data", [])
        logger.info(f"Expo push sent: {len(tickets)} tickets")
        return tickets
    except Exception as e:
        logger.error(f"Expo push failed: {e}")
        return []


def send_push_via_fcm(tokens: list[str], title: str, body: str, data: dict | None = None) -> list[dict]:
    """
    Send push notifications via Firebase Cloud Messaging (firebase-admin).
    Used for web push tokens and native FCM tokens.
    Requires firebase-admin to be installed and FIREBASE_CREDENTIALS_PATH configured.
    """
    results = []
    try:
        import firebase_admin
        from firebase_admin import credentials, messaging

        # Initialize Firebase app if not already done
        if not firebase_admin._apps:
            cred_path = getattr(settings, "FIREBASE_CREDENTIALS_PATH", None)
            if not cred_path:
                logger.warning("FIREBASE_CREDENTIALS_PATH not configured — skipping FCM push")
                return []
            cred = credentials.Certificate(cred_path)
            firebase_admin.initialize_app(cred)

        for token in tokens:
            try:
                message = messaging.Message(
                    notification=messaging.Notification(title=title, body=body),
                    data={k: str(v) for k, v in (data or {}).items()},
                    token=token,
                )
                msg_id = messaging.send(message)
                results.append({"token": token, "success": True, "message_id": msg_id})
                logger.info(f"FCM push sent: {msg_id}")
            except Exception as e:
                results.append({"token": token, "success": False, "error": str(e)})
                logger.error(f"FCM push failed for token {token[:20]}…: {e}")

    except ImportError:
        logger.warning("firebase-admin not installed — skipping FCM push")
    except Exception as e:
        logger.error(f"FCM initialization failed: {e}")

    return results


def send_notification(
    user,
    notification_type: str,
    title: str,
    body: str,
    data: dict | None = None,
    deep_link: str = "",
) -> Optional[Notification]:
    """
    Main entry point for sending a notification.
    1. Checks user preferences
    2. Creates an in-app Notification record
    3. Sends push notifications to all active devices (Expo + FCM)
    4. Updates the Notification record with delivery status
    """
    # 1. Check preferences (skip for system notifications)
    if notification_type != "system" and not should_send_notification(user, notification_type):
        logger.info(f"Notification '{notification_type}' suppressed by user preferences for {user.email}")
        return None

    # 2. Create in-app notification record
    notification = Notification.objects.create(
        user=user,
        notification_type=notification_type,
        title=title,
        body=body,
        data=data or {},
        deep_link=deep_link,
        status="pending",
    )

    # 3. Gather active push tokens
    push_tokens = PushToken.objects.filter(user=user, is_active=True)
    if not push_tokens.exists():
        notification.status = "sent"  # Saved in-app; no device to push to
        notification.sent_at = timezone.now()
        notification.save(update_fields=["status", "sent_at"])
        logger.info(f"Notification {notification.id} saved (no push tokens)")
        return notification

    expo_tokens = []
    fcm_tokens = []
    for pt in push_tokens:
        if pt.token.startswith("ExponentPushToken"):
            expo_tokens.append(pt.token)
        else:
            fcm_tokens.append(pt.token)

    push_data = data.copy() if data else {}
    if deep_link:
        push_data["deep_link"] = deep_link

    # 4. Send pushes
    all_success = True

    if expo_tokens:
        tickets = send_push_to_expo(expo_tokens, title, body, push_data)
        for ticket in tickets:
            if ticket.get("status") != "ok":
                all_success = False

    if fcm_tokens:
        results = send_push_via_fcm(fcm_tokens, title, body, push_data)
        for r in results:
            if not r.get("success"):
                all_success = False
            elif r.get("message_id"):
                notification.fcm_message_id = r["message_id"]

    # 5. Update notification status
    notification.status = "sent" if all_success else "failed"
    notification.sent_at = timezone.now()
    if not all_success:
        notification.error_message = "Some push deliveries failed"
    notification.save(update_fields=["status", "sent_at", "fcm_message_id", "error_message"])

    logger.info(f"Notification {notification.id} sent (expo={len(expo_tokens)}, fcm={len(fcm_tokens)})")
    return notification
