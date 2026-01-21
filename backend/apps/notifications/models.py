from django.db import models
from django.contrib.auth import get_user_model
from django.utils import timezone
from django.core.validators import MinValueValidator, MaxValueValidator
import uuid

User = get_user_model()


class EmailConfiguration(models.Model):
    """
    Database-based email/SMTP configuration for the application.
    This is a singleton model - only one configuration should exist.
    Allows admins to configure email settings through the Django admin panel.
    """
    BACKEND_CHOICES = [
        ('django.core.mail.backends.smtp.EmailBackend', 'SMTP'),
        ('django.core.mail.backends.console.EmailBackend', 'Console (Development)'),
        ('django_ses.SESBackend', 'Amazon SES'),
        ('anymail.backends.mailgun.EmailBackend', 'Mailgun'),
        ('anymail.backends.sendgrid.EmailBackend', 'SendGrid'),
        ('anymail.backends.mailjet.EmailBackend', 'Mailjet'),
    ]
    
    ENCRYPTION_CHOICES = [
        ('none', 'None'),
        ('tls', 'TLS (Port 587)'),
        ('ssl', 'SSL (Port 465)'),
    ]
    
    # Basic SMTP settings
    name = models.CharField(
        max_length=100,
        default='Primary Email',
        help_text='Friendly name for this email configuration'
    )
    backend = models.CharField(
        max_length=100,
        choices=BACKEND_CHOICES,
        default='django.core.mail.backends.smtp.EmailBackend',
        help_text='Email backend to use'
    )
    host = models.CharField(
        max_length=255,
        default='smtp.gmail.com',
        help_text='SMTP server hostname'
    )
    port = models.PositiveIntegerField(
        default=587,
        validators=[MinValueValidator(1), MaxValueValidator(65535)],
        help_text='SMTP server port (587 for TLS, 465 for SSL)'
    )
    encryption = models.CharField(
        max_length=10,
        choices=ENCRYPTION_CHOICES,
        default='tls',
        help_text='Connection encryption type'
    )
    
    # Authentication
    username = models.CharField(
        max_length=255,
        blank=True,
        help_text='SMTP username (usually your email address)'
    )
    password = models.CharField(
        max_length=255,
        blank=True,
        help_text='SMTP password or app-specific password'
    )
    
    # From address settings
    default_from_email = models.EmailField(
        default='noreply@stayafrica.com',
        help_text='Default sender email address'
    )
    default_from_name = models.CharField(
        max_length=100,
        default='StayAfrica',
        help_text='Default sender name'
    )
    
    # Optional settings
    timeout = models.PositiveIntegerField(
        default=30,
        help_text='Connection timeout in seconds'
    )
    fail_silently = models.BooleanField(
        default=False,
        help_text='If True, errors during sending will be silently ignored'
    )
    
    # Status
    is_active = models.BooleanField(
        default=True,
        help_text='Whether this email configuration is active'
    )
    last_tested_at = models.DateTimeField(
        null=True,
        blank=True,
        help_text='Last time a test email was sent'
    )
    last_test_success = models.BooleanField(
        default=False,
        help_text='Whether the last test email was successful'
    )
    last_test_error = models.TextField(
        blank=True,
        help_text='Error message from last test if it failed'
    )
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = 'Email Configuration'
        verbose_name_plural = 'Email Configuration'
    
    def __str__(self):
        return f"{self.name} ({self.host}:{self.port})"
    
    def save(self, *args, **kwargs):
        # Ensure only one EmailConfiguration exists (singleton pattern)
        if not self.pk and EmailConfiguration.objects.exists():
            # Update the existing one instead of creating a new one
            existing = EmailConfiguration.objects.first()
            self.pk = existing.pk
        super().save(*args, **kwargs)
    
    @classmethod
    def get_config(cls):
        """Get the email configuration, creating a default one if none exists"""
        config, created = cls.objects.get_or_create(
            defaults={
                'name': 'Primary Email',
                'host': 'smtp.gmail.com',
                'port': 587,
                'encryption': 'tls',
            }
        )
        return config
    
    def get_use_tls(self):
        """Return True if TLS should be used"""
        return self.encryption == 'tls'
    
    def get_use_ssl(self):
        """Return True if SSL should be used"""
        return self.encryption == 'ssl'
    
    def test_connection(self):
        """
        Test the SMTP connection with current settings.
        Returns (success: bool, error_message: str)
        """
        from django.core.mail import get_connection
        from django.core.mail import EmailMessage
        
        try:
            connection = get_connection(
                backend=self.backend,
                host=self.host,
                port=self.port,
                username=self.username,
                password=self.password,
                use_tls=self.get_use_tls(),
                use_ssl=self.get_use_ssl(),
                fail_silently=False,
                timeout=self.timeout,
            )
            connection.open()
            connection.close()
            
            self.last_tested_at = timezone.now()
            self.last_test_success = True
            self.last_test_error = ''
            self.save(update_fields=['last_tested_at', 'last_test_success', 'last_test_error'])
            
            return True, 'Connection successful'
        except Exception as e:
            self.last_tested_at = timezone.now()
            self.last_test_success = False
            self.last_test_error = str(e)
            self.save(update_fields=['last_tested_at', 'last_test_success', 'last_test_error'])
            
            return False, str(e)
    
    def send_test_email(self, to_email):
        """
        Send a test email using current settings.
        Returns (success: bool, error_message: str)
        """
        from django.core.mail import EmailMessage, get_connection
        
        try:
            connection = get_connection(
                backend=self.backend,
                host=self.host,
                port=self.port,
                username=self.username,
                password=self.password,
                use_tls=self.get_use_tls(),
                use_ssl=self.get_use_ssl(),
                fail_silently=False,
                timeout=self.timeout,
            )
            
            email = EmailMessage(
                subject='StayAfrica - Test Email',
                body='This is a test email from StayAfrica to verify your SMTP configuration is working correctly.',
                from_email=f'{self.default_from_name} <{self.default_from_email}>',
                to=[to_email],
                connection=connection,
            )
            email.send()
            
            self.last_tested_at = timezone.now()
            self.last_test_success = True
            self.last_test_error = ''
            self.save(update_fields=['last_tested_at', 'last_test_success', 'last_test_error'])
            
            return True, f'Test email sent to {to_email}'
        except Exception as e:
            self.last_tested_at = timezone.now()
            self.last_test_success = False
            self.last_test_error = str(e)
            self.save(update_fields=['last_tested_at', 'last_test_success', 'last_test_error'])
            
            return False, str(e)


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
