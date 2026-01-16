from django.db import models
from django.contrib.auth import get_user_model
from django.utils import timezone
import uuid

User = get_user_model()


class PushToken(models.Model):
    """Store device push notification tokens"""
    PLATFORM_CHOICES = [
        ('ios', 'iOS'),
        ('android', 'Android'),
        ('web', 'Web'),
    ]
    
    user = models.ForeignKey(User, on_delete=models.CASCADE, related_name='push_tokens')
    token = models.CharField(max_length=512, unique=True, db_index=True, help_text='FCM/APNS device token')
    platform = models.CharField(max_length=20, choices=PLATFORM_CHOICES)
    device_id = models.CharField(max_length=255, blank=True, help_text='Unique device identifier')
    is_active = models.BooleanField(default=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    last_used_at = models.DateTimeField(default=timezone.now)
    
    class Meta:
        ordering = ['-last_used_at']
        indexes = [
            models.Index(fields=['user', 'is_active']),
            models.Index(fields=['platform']),
        ]
        unique_together = ('user', 'token')
    
    def __str__(self):
        return f"{self.user.email} - {self.platform} ({self.token[:20]}...)"


class NotificationPreference(models.Model):
    """User notification preferences"""
    user = models.OneToOneField(User, on_delete=models.CASCADE, related_name='notification_preferences')
    
    # Booking notifications
    booking_confirmed = models.BooleanField(default=True, help_text='Notify when booking is confirmed')
    booking_cancelled = models.BooleanField(default=True, help_text='Notify when booking is cancelled')
    booking_reminder = models.BooleanField(default=True, help_text='Notify before check-in')
    
    # Message notifications
    new_message = models.BooleanField(default=True, help_text='Notify on new messages')
    
    # Payment notifications
    payment_received = models.BooleanField(default=True, help_text='Notify when payment is received (hosts)')
    payment_required = models.BooleanField(default=True, help_text='Notify when payment is required')
    
    # Review notifications
    review_reminder = models.BooleanField(default=True, help_text='Remind to leave review after checkout')
    review_received = models.BooleanField(default=True, help_text='Notify when property receives review (hosts)')
    
    # Price drop notifications
    price_drop = models.BooleanField(default=True, help_text='Notify on price drops for wishlisted properties')
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name_plural = 'Notification Preferences'
    
    def __str__(self):
        return f"Preferences for {self.user.email}"


class Notification(models.Model):
    """Notification history and tracking"""
    NOTIFICATION_TYPE_CHOICES = [
        ('booking_confirmed', 'Booking Confirmed'),
        ('booking_cancelled', 'Booking Cancelled'),
        ('booking_reminder', 'Booking Reminder'),
        ('new_message', 'New Message'),
        ('payment_received', 'Payment Received'),
        ('payment_required', 'Payment Required'),
        ('review_reminder', 'Review Reminder'),
        ('review_received', 'Review Received'),
        ('price_drop', 'Price Drop'),
        ('system', 'System'),
    ]
    
    STATUS_CHOICES = [
        ('pending', 'Pending'),
        ('sent', 'Sent'),
        ('delivered', 'Delivered'),
        ('failed', 'Failed'),
        ('read', 'Read'),
    ]
    
    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    user = models.ForeignKey(User, on_delete=models.CASCADE, related_name='notifications')
    notification_type = models.CharField(max_length=50, choices=NOTIFICATION_TYPE_CHOICES)
    
    title = models.CharField(max_length=255)
    body = models.TextField()
    data = models.JSONField(default=dict, blank=True, help_text='Additional data for notification')
    
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='pending')
    
    # Deep link for notification tap
    deep_link = models.CharField(max_length=512, blank=True, help_text='Deep link URL for app navigation')
    
    # FCM response tracking
    fcm_message_id = models.CharField(max_length=255, blank=True)
    error_message = models.TextField(blank=True)
    
    sent_at = models.DateTimeField(null=True, blank=True)
    delivered_at = models.DateTimeField(null=True, blank=True)
    read_at = models.DateTimeField(null=True, blank=True)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-created_at']
        indexes = [
            models.Index(fields=['user', 'status']),
            models.Index(fields=['notification_type']),
            models.Index(fields=['sent_at']),
        ]
    
    def __str__(self):
        return f"{self.notification_type} for {self.user.email} - {self.status}"
    
    def mark_as_read(self):
        """Mark notification as read"""
        if self.status != 'read':
            self.status = 'read'
            self.read_at = timezone.now()
            self.save(update_fields=['status', 'read_at', 'updated_at'])
