from django.db import models
from django.utils import timezone
from apps.users.models import User
from apps.properties.models import Property
from apps.bookings.models import Booking


class Conversation(models.Model):
    """
    Conversation thread between users
    Can be related to a specific property or booking
    """
    participants = models.ManyToManyField(User, related_name='conversations')
    property = models.ForeignKey(
        Property,
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name='conversations',
        help_text='Property this conversation is about'
    )
    booking = models.ForeignKey(
        Booking,
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name='conversations',
        help_text='Booking this conversation is about'
    )
    subject = models.CharField(max_length=255, blank=True, help_text='Conversation subject')
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    archived_by = models.ManyToManyField(
        User,
        related_name='archived_conversations',
        blank=True,
        help_text='Users who have archived this conversation'
    )
    
    class Meta:
        ordering = ['-updated_at']
        indexes = [
            models.Index(fields=['-updated_at']),
            models.Index(fields=['property']),
            models.Index(fields=['booking']),
        ]
    
    def __str__(self):
        participant_emails = ', '.join([u.email for u in self.participants.all()[:2]])
        return f"Conversation: {participant_emails}"
    
    def get_last_message(self):
        """Get the most recent message in this conversation"""
        return self.messages.order_by('-created_at').first()
    
    def get_unread_count(self, user):
        """Get unread message count for a specific user"""
        return self.messages.filter(is_read=False).exclude(sender=user).count()
    
    def mark_as_read(self, user):
        """Mark all messages in conversation as read for a user"""
        self.messages.filter(is_read=False, receiver=user).update(is_read=True, read_at=timezone.now())


class Message(models.Model):
    """
    Individual message in a conversation
    """
    MESSAGE_TYPES = [
        ('text', 'Text Message'),
        ('system', 'System Notification'),
        ('booking_request', 'Booking Request'),
        ('booking_confirmation', 'Booking Confirmation'),
        ('review_reminder', 'Review Reminder'),
    ]
    
    conversation = models.ForeignKey(
        Conversation,
        on_delete=models.CASCADE,
        related_name='messages',
        help_text='Conversation this message belongs to'
    )
    sender = models.ForeignKey(User, on_delete=models.CASCADE, related_name='messages_sent')
    receiver = models.ForeignKey(User, on_delete=models.CASCADE, related_name='messages_received')
    text = models.TextField()
    message_type = models.CharField(max_length=30, choices=MESSAGE_TYPES, default='text')
    is_read = models.BooleanField(default=False)
    read_at = models.DateTimeField(null=True, blank=True)
    created_at = models.DateTimeField(auto_now_add=True)
    edited_at = models.DateTimeField(null=True, blank=True)
    deleted_by_sender = models.BooleanField(default=False)
    deleted_by_receiver = models.BooleanField(default=False)
    
    # Attachments/metadata
    metadata = models.JSONField(
        default=dict,
        blank=True,
        help_text='Additional message data (links, booking info, etc.)'
    )
    
    class Meta:
        ordering = ['created_at']
        indexes = [
            models.Index(fields=['conversation', 'created_at']),
            models.Index(fields=['sender']),
            models.Index(fields=['receiver']),
            models.Index(fields=['is_read']),
            models.Index(fields=['message_type']),
        ]
    
    def __str__(self):
        return f'{self.sender.email} -> {self.receiver.email}: {self.text[:50]}'
    
    def mark_as_read(self):
        """Mark message as read"""
        if not self.is_read:
            self.is_read = True
            self.read_at = timezone.now()
            self.save(update_fields=['is_read', 'read_at'])


class MessageTemplate(models.Model):
    """
    Reusable message templates for common scenarios
    """
    TEMPLATE_TYPES = [
        ('booking_inquiry', 'Booking Inquiry'),
        ('booking_confirmation', 'Booking Confirmation'),
        ('cancellation', 'Cancellation Message'),
        ('review_request', 'Review Request'),
        ('welcome', 'Welcome Message'),
        ('custom', 'Custom Template'),
    ]
    
    name = models.CharField(max_length=100)
    template_type = models.CharField(max_length=30, choices=TEMPLATE_TYPES)
    subject = models.CharField(max_length=255)
    body = models.TextField(help_text='Use {variable} for dynamic content')
    is_active = models.BooleanField(default=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['template_type', 'name']
    
    def __str__(self):
        return f"{self.template_type}: {self.name}"
    
    def render(self, context):
        """Render template with context variables"""
        subject = self.subject.format(**context)
        body = self.body.format(**context)
        return {'subject': subject, 'body': body}
