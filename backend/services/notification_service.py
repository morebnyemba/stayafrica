"""
Push Notification Service
Integrates with Firebase Cloud Messaging (FCM) for cross-platform notifications
"""
import logging
from typing import List, Dict, Optional
from django.conf import settings
from django.utils import timezone

logger = logging.getLogger(__name__)

# Try to import Firebase Admin SDK
try:
    import firebase_admin
    from firebase_admin import credentials, messaging
    FIREBASE_AVAILABLE = True
except ImportError:
    FIREBASE_AVAILABLE = False
    logger.warning("Firebase Admin SDK not installed. Push notifications will be disabled.")


class NotificationService:
    """Service for sending push notifications via FCM"""
    
    _initialized = False
    
    @classmethod
    def initialize(cls):
        """Initialize Firebase Admin SDK"""
        if cls._initialized or not FIREBASE_AVAILABLE:
            return
        
        try:
            # Check if Firebase is already initialized
            firebase_admin.get_app()
            cls._initialized = True
            logger.info("Firebase already initialized")
        except ValueError:
            # Initialize Firebase with credentials
            firebase_credentials_path = getattr(settings, 'FIREBASE_CREDENTIALS_PATH', None)
            if firebase_credentials_path:
                try:
                    cred = credentials.Certificate(firebase_credentials_path)
                    firebase_admin.initialize_app(cred)
                    cls._initialized = True
                    logger.info("Firebase initialized successfully")
                except Exception as e:
                    logger.error(f"Failed to initialize Firebase: {e}")
            else:
                logger.warning("FIREBASE_CREDENTIALS_PATH not set in settings")
    
    @classmethod
    def send_notification(
        cls,
        user,
        title: str,
        body: str,
        notification_type: str,
        data: Optional[Dict] = None,
        deep_link: Optional[str] = None,
    ) -> Dict:
        """
        Send push notification to a user
        
        Args:
            user: User instance
            title: Notification title
            body: Notification body text
            notification_type: Type of notification (from Notification.NOTIFICATION_TYPE_CHOICES)
            data: Additional data dictionary
            deep_link: Deep link URL for navigation
            
        Returns:
            dict with send results
        """
        from apps.notifications.models import Notification, NotificationPreference, PushToken
        
        # Check if user has push tokens
        active_tokens = PushToken.objects.filter(user=user, is_active=True)
        if not active_tokens.exists():
            logger.info(f"No active push tokens for user {user.email}")
            return {'success': False, 'reason': 'no_tokens'}
        
        # Check notification preferences
        try:
            preferences = user.notification_preferences
            pref_field = notification_type
            if hasattr(preferences, pref_field) and not getattr(preferences, pref_field):
                logger.info(f"User {user.email} has disabled {notification_type} notifications")
                return {'success': False, 'reason': 'preferences_disabled'}
        except NotificationPreference.DoesNotExist:
            # If no preferences, allow all notifications
            pass
        
        # Create notification record
        notification = Notification.objects.create(
            user=user,
            notification_type=notification_type,
            title=title,
            body=body,
            data=data or {},
            deep_link=deep_link or '',
            status='pending'
        )
        
        # Send to FCM if available
        if FIREBASE_AVAILABLE and cls._initialized:
            try:
                tokens = list(active_tokens.values_list('token', flat=True))
                fcm_data = data or {}
                fcm_data['notification_id'] = str(notification.id)
                fcm_data['type'] = notification_type
                if deep_link:
                    fcm_data['deep_link'] = deep_link
                
                # Create FCM message
                message = messaging.MulticastMessage(
                    notification=messaging.Notification(
                        title=title,
                        body=body,
                    ),
                    data={k: str(v) for k, v in fcm_data.items()},  # FCM requires string values
                    tokens=tokens,
                    android=messaging.AndroidConfig(
                        priority='high',
                        notification=messaging.AndroidNotification(
                            sound='default',
                            color='#3A5C50',  # StayAfrica brand color
                        ),
                    ),
                    apns=messaging.APNSConfig(
                        payload=messaging.APNSPayload(
                            aps=messaging.Aps(
                                sound='default',
                                badge=1,
                            ),
                        ),
                    ),
                )
                
                # Send message
                response = messaging.send_multicast(message)
                
                # Update notification status
                notification.status = 'sent' if response.success_count > 0 else 'failed'
                notification.sent_at = timezone.now()
                if response.success_count > 0:
                    notification.delivered_at = timezone.now()
                
                # Handle failed tokens
                if response.failure_count > 0:
                    for idx, send_response in enumerate(response.responses):
                        if not send_response.success:
                            error_code = send_response.exception.code if send_response.exception else 'unknown'
                            logger.warning(f"Failed to send to token {tokens[idx][:20]}: {error_code}")
                            
                            # Deactivate invalid tokens
                            if error_code in ['NOT_FOUND', 'INVALID_ARGUMENT', 'UNREGISTERED']:
                                PushToken.objects.filter(token=tokens[idx]).update(is_active=False)
                
                notification.save()
                
                logger.info(f"Sent notification to {response.success_count}/{len(tokens)} devices")
                return {
                    'success': True,
                    'sent_count': response.success_count,
                    'failed_count': response.failure_count,
                    'notification_id': str(notification.id)
                }
                
            except Exception as e:
                logger.error(f"Error sending FCM notification: {e}")
                notification.status = 'failed'
                notification.error_message = str(e)
                notification.save()
                return {'success': False, 'error': str(e)}
        else:
            # Firebase not available, just log
            logger.info(f"Firebase not initialized. Notification logged only: {title}")
            return {'success': False, 'reason': 'firebase_not_available'}
    
    @classmethod
    def send_booking_confirmation(cls, booking):
        """Send booking confirmation notification"""
        return cls.send_notification(
            user=booking.guest,
            title="Booking Confirmed! 🎉",
            body=f"Your booking at {booking.rental_property.title} is confirmed for {booking.check_in}.",
            notification_type='booking_confirmed',
            data={
                'booking_id': booking.id,
                'booking_ref': booking.booking_ref,
                'property_id': booking.rental_property.id,
            },
            deep_link=f"stayafrica://bookings/{booking.id}"
        )
    
    @classmethod
    def send_booking_cancelled(cls, booking, cancelled_by_host=False):
        """Send booking cancellation notification"""
        if cancelled_by_host:
            user = booking.guest
            body = f"Your booking at {booking.rental_property.title} has been cancelled by the host."
        else:
            user = booking.rental_property.host
            body = f"A booking at {booking.rental_property.title} has been cancelled."
        
        return cls.send_notification(
            user=user,
            title="Booking Cancelled",
            body=body,
            notification_type='booking_cancelled',
            data={
                'booking_id': booking.id,
                'booking_ref': booking.booking_ref,
            },
            deep_link=f"stayafrica://bookings/{booking.id}"
        )
    
    @classmethod
    def send_new_message(cls, message):
        """Send new message notification"""
        # Notify the recipient (not the sender)
        recipient = message.recipient
        return cls.send_notification(
            user=recipient,
            title=f"New message from {message.sender.get_full_name() or 'Guest'}",
            body=message.content[:100] + ('...' if len(message.content) > 100 else ''),
            notification_type='new_message',
            data={
                'message_id': message.id,
                'conversation_id': message.conversation_id,
                'sender_id': message.sender.id,
            },
            deep_link=f"stayafrica://messages/{message.conversation_id}"
        )
    
    @classmethod
    def send_payment_received(cls, payment):
        """Send payment received notification to host"""
        booking = payment.booking
        return cls.send_notification(
            user=booking.rental_property.host,
            title="Payment Received 💰",
            body=f"You received {payment.currency} {payment.amount:.2f} for booking {booking.booking_ref}.",
            notification_type='payment_received',
            data={
                'payment_id': payment.id,
                'booking_id': booking.id,
                'amount': float(payment.amount),
            },
            deep_link=f"stayafrica://payments/{payment.id}"
        )
    
    @classmethod
    def send_review_reminder(cls, booking):
        """Send review reminder after checkout"""
        return cls.send_notification(
            user=booking.guest,
            title="How was your stay?",
            body=f"Share your experience at {booking.rental_property.title}!",
            notification_type='review_reminder',
            data={
                'booking_id': booking.id,
                'property_id': booking.rental_property.id,
            },
            deep_link=f"stayafrica://reviews/create/{booking.id}"
        )
    
    @classmethod
    def send_price_drop(cls, user, property_obj, old_price, new_price):
        """Send price drop notification for wishlisted property"""
        discount_pct = ((old_price - new_price) / old_price) * 100
        return cls.send_notification(
            user=user,
            title="Price Drop! 📉",
            body=f"{property_obj.title} is now {int(discount_pct)}% off!",
            notification_type='price_drop',
            data={
                'property_id': property_obj.id,
                'old_price': float(old_price),
                'new_price': float(new_price),
            },
            deep_link=f"stayafrica://properties/{property_obj.id}"
        )
    
    @classmethod
    def send_bulk_notification(cls, user_ids: List[int], title: str, body: str, data: Optional[Dict] = None):
        """Send same notification to multiple users"""
        from apps.users.models import User
        
        results = []
        users = User.objects.filter(id__in=user_ids)
        
        for user in users:
            result = cls.send_notification(
                user=user,
                title=title,
                body=body,
                notification_type='system',
                data=data
            )
            results.append({'user_id': user.id, 'result': result})
        
        return results


# Initialize Firebase on module load
NotificationService.initialize()
