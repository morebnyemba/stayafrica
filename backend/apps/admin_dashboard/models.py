from django.db import models
from django.contrib.auth import get_user_model
from django.contrib.contenttypes.models import ContentType
from django.contrib.contenttypes.fields import GenericForeignKey
from django.core.cache import cache

User = get_user_model()


class SystemConfiguration(models.Model):
    """
    System-wide configuration settings manageable via Django admin.
    Uses singleton pattern - only one instance should exist.
    """
    # Pricing Configuration
    commission_rate = models.DecimalField(
        max_digits=5, 
        decimal_places=4, 
        default=0.07,
        help_text="Commission rate (e.g., 0.07 for 7%)"
    )
    service_fee = models.DecimalField(
        max_digits=10, 
        decimal_places=2, 
        default=3.00,
        help_text="Service fee per booking"
    )
    default_currency = models.CharField(
        max_length=3, 
        default='USD',
        help_text="Default currency code (e.g., USD, ZAR)"
    )
    
    # Payment Gateway Configuration
    paynow_integration_id = models.CharField(
        max_length=255, 
        blank=True,
        help_text="Paynow Integration ID"
    )
    paynow_integration_key = models.CharField(
        max_length=255, 
        blank=True,
        help_text="Paynow Integration Key"
    )
    paynow_webhook_secret = models.CharField(
        max_length=255, 
        blank=True,
        help_text="Paynow Webhook Secret"
    )
    
    payfast_merchant_id = models.CharField(
        max_length=255, 
        blank=True,
        help_text="PayFast Merchant ID"
    )
    payfast_merchant_key = models.CharField(
        max_length=255, 
        blank=True,
        help_text="PayFast Merchant Key"
    )
    payfast_passphrase = models.CharField(
        max_length=255, 
        blank=True,
        help_text="PayFast Passphrase"
    )
    payfast_webhook_secret = models.CharField(
        max_length=255, 
        blank=True,
        help_text="PayFast Webhook Secret"
    )
    
    stripe_secret_key = models.CharField(
        max_length=255, 
        blank=True,
        help_text="Stripe Secret Key"
    )
    stripe_publishable_key = models.CharField(
        max_length=255, 
        blank=True,
        help_text="Stripe Publishable Key"
    )
    stripe_webhook_secret = models.CharField(
        max_length=255, 
        blank=True,
        help_text="Stripe Webhook Secret"
    )
    
    # Business Rules
    max_advance_booking_days = models.IntegerField(
        default=365,
        help_text="Maximum days in advance for bookings"
    )
    max_stay_duration_days = models.IntegerField(
        default=90,
        help_text="Maximum stay duration in days"
    )
    review_window_days = models.IntegerField(
        default=30,
        help_text="Days after checkout to submit review"
    )
    review_edit_window_days = models.IntegerField(
        default=7,
        help_text="Days to edit review after submission"
    )
    
    # Email Configuration
    admin_email = models.EmailField(
        blank=True,
        help_text="Admin notification email"
    )
    support_email = models.EmailField(
        blank=True,
        help_text="Support email for user inquiries"
    )
    
    # Maintenance
    maintenance_mode = models.BooleanField(
        default=False,
        help_text="Enable maintenance mode (API read-only)"
    )
    maintenance_message = models.TextField(
        blank=True,
        help_text="Message displayed during maintenance"
    )
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = 'System Configuration'
        verbose_name_plural = 'System Configuration'
    
    def __str__(self):
        return f'System Configuration (Updated: {self.updated_at})'
    
    def save(self, *args, **kwargs):
        """Ensure only one instance exists (singleton pattern)"""
        self.pk = 1
        super().save(*args, **kwargs)
        # Clear cache when settings are updated
        cache.delete('system_config')
    
    def delete(self, *args, **kwargs):
        """Prevent deletion of configuration"""
        pass
    
    @classmethod
    def get_config(cls):
        """Get the system configuration (cached)"""
        config = cache.get('system_config')
        if config is None:
            config, created = cls.objects.get_or_create(pk=1)
            cache.set('system_config', config, timeout=3600)  # Cache for 1 hour
        return config


class AuditLog(models.Model):
    user = models.ForeignKey(User, on_delete=models.SET_NULL, null=True)
    action = models.CharField(max_length=100)
    content_type = models.ForeignKey(ContentType, on_delete=models.SET_NULL, null=True, blank=True)
    object_id = models.PositiveIntegerField(null=True, blank=True)
    content_object = GenericForeignKey('content_type', 'object_id')
    changes = models.JSONField(default=dict, blank=True)
    timestamp = models.DateTimeField(auto_now_add=True, db_index=True)
    
    class Meta:
        ordering = ['-timestamp']
        indexes = [
            models.Index(fields=['user']),
            models.Index(fields=['action']),
            models.Index(fields=['timestamp']),
        ]
    
    def __str__(self):
        return f'{self.action} by {self.user} at {self.timestamp}'

class AdminStats(models.Model):
    """Cached stats for admin dashboard"""
    total_revenue = models.DecimalField(max_digits=12, decimal_places=2, default=0)
    total_bookings = models.IntegerField(default=0)
    total_users = models.IntegerField(default=0)
    active_hosts = models.IntegerField(default=0)
    total_properties = models.IntegerField(default=0)
    last_updated = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name_plural = 'Admin Stats'
    
    def __str__(self):
        return f'Stats (Updated: {self.last_updated})'
