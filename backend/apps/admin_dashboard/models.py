from django.db import models
from django.contrib.auth import get_user_model
from django.contrib.contenttypes.models import ContentType
from django.contrib.contenttypes.fields import GenericForeignKey

User = get_user_model()

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
