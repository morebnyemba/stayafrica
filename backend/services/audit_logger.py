"""
Audit Logger Service
Tracks all important user actions and data changes
"""
from django.contrib.contenttypes.models import ContentType
from apps.admin_dashboard.models import AuditLog

class AuditLoggerService:
    @staticmethod
    def log_action(user, action, content_type, object_id, changes=None):
        """
        Log user action
        action: 'create', 'update', 'delete', 'login', 'payment', etc.
        """
        AuditLog.objects.create(
            user=user,
            action=action,
            content_type=content_type,
            object_id=object_id,
            changes=changes or {}
        )
    
    @staticmethod
    def log_booking_action(user, booking, action):
        """Log booking-related action"""
        from django.contrib.contenttypes.models import ContentType
        content_type = ContentType.objects.get_for_model(booking.__class__)
        AuditLoggerService.log_action(user, action, content_type, booking.id)
    
    @staticmethod
    def log_payment_action(user, payment, action):
        """Log payment-related action"""
        from django.contrib.contenttypes.models import ContentType
        content_type = ContentType.objects.get_for_model(payment.__class__)
        AuditLoggerService.log_action(user, action, content_type, payment.id)
