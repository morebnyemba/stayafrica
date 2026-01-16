"""
Enhanced Messaging Models for Automated Host Communication
"""
from django.db import models
from django.utils import timezone
from apps.users.models import User
from apps.messaging.models import MessageTemplate, Conversation
from apps.bookings.models import Booking


class HostMessageSettings(models.Model):
    """
    Host preferences for automated messaging
    """
    host = models.OneToOneField(
        User,
        on_delete=models.CASCADE,
        related_name='message_settings'
    )
    
    # Auto-response settings
    enable_auto_responses = models.BooleanField(
        default=False,
        help_text='Automatically send responses based on triggers'
    )
    
    # Quick replies
    enable_quick_replies = models.BooleanField(
        default=True,
        help_text='Show quick reply suggestions'
    )
    
    # Scheduled messages
    enable_scheduled_messages = models.BooleanField(
        default=False,
        help_text='Send scheduled messages at specific times'
    )
    
    # Away mode
    away_mode_enabled = models.BooleanField(
        default=False,
        help_text='Send away message when host is unavailable'
    )
    away_message = models.TextField(
        blank=True,
        help_text='Message to send when in away mode'
    )
    
    # Response time goal
    target_response_time_hours = models.IntegerField(
        default=24,
        help_text='Target response time in hours'
    )
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = 'Host Message Settings'
        verbose_name_plural = 'Host Message Settings'
    
    def __str__(self):
        return f"Message Settings for {self.host.email}"


class AutomatedMessage(models.Model):
    """
    Automated messages triggered by events
    """
    TRIGGER_TYPES = [
        ('booking_inquiry', 'Booking Inquiry Received'),
        ('booking_confirmed', 'Booking Confirmed'),
        ('booking_cancelled', 'Booking Cancelled'),
        ('check_in_reminder', 'Check-in Reminder'),
        ('check_out_reminder', 'Check-out Reminder'),
        ('review_request', 'Review Request'),
        ('no_response', 'No Response After X Hours'),
        ('custom', 'Custom Trigger'),
    ]
    
    host = models.ForeignKey(
        User,
        on_delete=models.CASCADE,
        related_name='automated_messages'
    )
    
    name = models.CharField(max_length=255)
    trigger_type = models.CharField(max_length=50, choices=TRIGGER_TYPES)
    template = models.ForeignKey(
        MessageTemplate,
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        help_text='Template to use for this automated message'
    )
    
    # Timing
    delay_hours = models.IntegerField(
        default=0,
        help_text='Hours to wait before sending (0 = immediate)'
    )
    
    # Custom message (alternative to template)
    custom_message = models.TextField(
        blank=True,
        help_text='Custom message text (if not using template)'
    )
    
    # Conditions
    is_active = models.BooleanField(default=True)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['host', 'trigger_type']
        indexes = [
            models.Index(fields=['host', 'trigger_type', 'is_active']),
        ]
    
    def __str__(self):
        return f"{self.name} ({self.get_trigger_type_display()})"


class ScheduledMessage(models.Model):
    """
    Messages scheduled to be sent at specific times
    """
    STATUS_CHOICES = [
        ('pending', 'Pending'),
        ('sent', 'Sent'),
        ('failed', 'Failed'),
        ('cancelled', 'Cancelled'),
    ]
    
    host = models.ForeignKey(
        User,
        on_delete=models.CASCADE,
        related_name='scheduled_messages'
    )
    conversation = models.ForeignKey(
        Conversation,
        on_delete=models.CASCADE,
        related_name='scheduled_messages'
    )
    
    message_text = models.TextField()
    scheduled_time = models.DateTimeField(
        help_text='When to send this message'
    )
    
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='pending')
    sent_at = models.DateTimeField(null=True, blank=True)
    
    # Related booking (optional)
    booking = models.ForeignKey(
        Booking,
        on_delete=models.CASCADE,
        null=True,
        blank=True,
        related_name='scheduled_messages'
    )
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['scheduled_time']
        indexes = [
            models.Index(fields=['status', 'scheduled_time']),
            models.Index(fields=['host', 'status']),
        ]
    
    def __str__(self):
        return f"Scheduled message for {self.conversation} at {self.scheduled_time}"
    
    def is_due(self):
        """Check if message is due to be sent"""
        return self.status == 'pending' and self.scheduled_time <= timezone.now()


class QuickReply(models.Model):
    """
    Quick reply shortcuts for common responses
    """
    host = models.ForeignKey(
        User,
        on_delete=models.CASCADE,
        related_name='quick_replies'
    )
    
    shortcut = models.CharField(
        max_length=50,
        help_text='Shortcut text (e.g., /checkin, /wifi)'
    )
    message_text = models.TextField(help_text='Full message text')
    
    # Categorization
    category = models.CharField(
        max_length=50,
        blank=True,
        help_text='Category for organization'
    )
    
    use_count = models.IntegerField(
        default=0,
        help_text='Number of times this quick reply has been used'
    )
    
    is_active = models.BooleanField(default=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-use_count', 'shortcut']
        unique_together = ('host', 'shortcut')
        indexes = [
            models.Index(fields=['host', 'is_active']),
        ]
    
    def __str__(self):
        return f"{self.shortcut}: {self.message_text[:50]}"
    
    def increment_use_count(self):
        """Increment usage counter"""
        self.use_count += 1
        self.save(update_fields=['use_count'])


class MessageAnalytics(models.Model):
    """
    Track messaging analytics for hosts
    """
    host = models.ForeignKey(
        User,
        on_delete=models.CASCADE,
        related_name='message_analytics'
    )
    
    date = models.DateField(help_text='Date for these analytics')
    
    # Metrics
    messages_sent = models.IntegerField(default=0)
    messages_received = models.IntegerField(default=0)
    conversations_started = models.IntegerField(default=0)
    conversations_resolved = models.IntegerField(default=0)
    
    # Response time (minutes)
    avg_response_time_minutes = models.IntegerField(
        null=True,
        blank=True,
        help_text='Average response time in minutes'
    )
    
    # Quick replies used
    quick_replies_used = models.IntegerField(default=0)
    automated_messages_sent = models.IntegerField(default=0)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-date']
        unique_together = ('host', 'date')
        indexes = [
            models.Index(fields=['host', '-date']),
        ]
    
    def __str__(self):
        return f"Analytics for {self.host.email} on {self.date}"
