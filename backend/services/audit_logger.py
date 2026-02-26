"""
Audit Logger Service
Tracks all important user actions and data changes
"""
import logging
from django.contrib.contenttypes.models import ContentType
from apps.admin_dashboard.models import AuditLog

logger = logging.getLogger(__name__)


class AuditLoggerService:
    @staticmethod
    def log_action(user, action, content_type=None, object_id=None, changes=None,
                   *, model=None, resource_type=None, resource_id=None, details=None):
        """
        Log user action.

        Accepts multiple calling conventions for backwards-compatibility:
          - content_type + object_id              (canonical)
          - model + object_id                     (auto-resolves ContentType)
          - resource_type + resource_id + details  (legacy / payment-method style)
        """
        try:
            # Resolve content_type from a raw model class if provided
            if content_type is None and model is not None:
                content_type = ContentType.objects.get_for_model(model)

            # Legacy callers that pass resource_type / resource_id / details
            if content_type is None and resource_type is not None:
                # Try to look up the ContentType by model name
                try:
                    content_type = ContentType.objects.get(model=resource_type.lower())
                except ContentType.DoesNotExist:
                    content_type = None

            if object_id is None and resource_id is not None:
                object_id = resource_id

            if changes is None and details is not None:
                changes = details

            AuditLog.objects.create(
                user=user,
                action=action,
                content_type=content_type,
                object_id=str(object_id) if object_id else '',
                changes=changes or {}
            )
        except Exception as e:
            # Never let audit logging crash the actual request
            logger.error(f"AuditLoggerService.log_action failed: {e}")
    
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
