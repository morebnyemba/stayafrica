from django.contrib import admin
from django.utils.html import format_html
from django.utils.translation import gettext_lazy as _
from django.contrib import messages
from unfold.admin import ModelAdmin as UnfoldModelAdmin
from unfold.decorators import display, action
from apps.notifications.models import PushToken, NotificationPreference, Notification, EmailConfiguration


@admin.register(EmailConfiguration)
class EmailConfigurationAdmin(UnfoldModelAdmin):
    """Admin interface for email/SMTP configuration"""
    
    list_display = ['name', 'host', 'port', 'encryption', 'active_badge', 'test_status', 'updated_at']
    readonly_fields = ['last_tested_at', 'last_test_success', 'last_test_error', 'created_at', 'updated_at']
    
    fieldsets = (
        (_('General'), {
            'fields': ('name', 'is_active'),
        }),
        (_('SMTP Server Settings'), {
            'fields': ('backend', 'host', 'port', 'encryption', 'timeout'),
            'description': _('Configure your SMTP server connection settings'),
        }),
        (_('Authentication'), {
            'fields': ('username', 'password'),
            'description': _('SMTP server credentials. For Gmail, use an App Password.'),
        }),
        (_('Sender Information'), {
            'fields': ('default_from_email', 'default_from_name'),
            'description': _('Default sender information for outgoing emails'),
        }),
        (_('Advanced'), {
            'fields': ('fail_silently',),
            'classes': ['collapse'],
        }),
        (_('Test Status'), {
            'fields': ('last_tested_at', 'last_test_success', 'last_test_error'),
            'classes': ['collapse'],
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    actions = ['test_smtp_connection']
    
    @display(description=_('Active'), label=True)
    def active_badge(self, obj):
        return {
            'value': 'Active' if obj.is_active else 'Inactive',
            'color': 'success' if obj.is_active else 'secondary',
        }
    
    @display(description=_('Test Status'), label=True)
    def test_status(self, obj):
        if obj.last_tested_at is None:
            return {'value': 'Not Tested', 'color': 'secondary'}
        return {
            'value': 'Passed' if obj.last_test_success else 'Failed',
            'color': 'success' if obj.last_test_success else 'danger',
        }
    
    @action(description=_('Test SMTP Connection'))
    def test_smtp_connection(self, request, queryset):
        for config in queryset:
            success, message = config.test_connection()
            if success:
                self.message_user(request, f'{config.name}: {message}', messages.SUCCESS)
            else:
                self.message_user(request, f'{config.name}: {message}', messages.ERROR)
    
    def has_add_permission(self, request):
        # Only allow adding if no configuration exists (singleton)
        if EmailConfiguration.objects.exists():
            return False
        return super().has_add_permission(request)
    
    def has_delete_permission(self, request, obj=None):
        # Prevent deletion of the only configuration
        return False


@admin.register(PushToken)
class PushTokenAdmin(UnfoldModelAdmin):
    """Admin interface for push tokens"""
    
    list_display = ['user_email', 'platform', 'masked_token', 'active_badge', 'last_used_at', 'created_at']
    list_filter = ['platform', 'is_active', 'created_at']
    search_fields = ['user__email', 'device_id', 'token']
    list_select_related = ['user']
    readonly_fields = ['token', 'created_at', 'updated_at', 'last_used_at']
    
    fieldsets = (
        (_('User Information'), {
            'fields': ('user',),
        }),
        (_('Token Details'), {
            'fields': ('token', 'platform', 'device_id', 'is_active'),
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at', 'last_used_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('User'))
    def user_email(self, obj):
        return obj.user.email
    
    @display(description=_('Token'))
    def masked_token(self, obj):
        return f"{obj.token[:20]}..." if len(obj.token) > 20 else obj.token
    
    @display(description=_('Active'), label=True)
    def active_badge(self, obj):
        return {
            'value': 'Active' if obj.is_active else 'Inactive',
            'color': 'success' if obj.is_active else 'secondary',
        }


@admin.register(NotificationPreference)
class NotificationPreferenceAdmin(UnfoldModelAdmin):
    """Admin interface for notification preferences"""
    
    list_display = ['user_email', 'booking_status', 'message_status', 'payment_status', 'updated_at']
    search_fields = ['user__email']
    list_select_related = ['user']
    
    fieldsets = (
        (_('User'), {
            'fields': ('user',),
        }),
        (_('Booking Notifications'), {
            'fields': ('booking_confirmed', 'booking_cancelled', 'booking_reminder'),
        }),
        (_('Message Notifications'), {
            'fields': ('new_message',),
        }),
        (_('Payment Notifications'), {
            'fields': ('payment_received', 'payment_required'),
        }),
        (_('Review Notifications'), {
            'fields': ('review_reminder', 'review_received'),
        }),
        (_('Other'), {
            'fields': ('price_drop',),
        }),
    )
    
    @display(description=_('User'))
    def user_email(self, obj):
        return obj.user.email
    
    @display(description=_('Bookings'), label=True)
    def booking_status(self, obj):
        enabled = any([obj.booking_confirmed, obj.booking_cancelled, obj.booking_reminder])
        return {
            'value': 'Enabled' if enabled else 'Disabled',
            'color': 'success' if enabled else 'secondary',
        }
    
    @display(description=_('Messages'), label=True)
    def message_status(self, obj):
        return {
            'value': 'Enabled' if obj.new_message else 'Disabled',
            'color': 'success' if obj.new_message else 'secondary',
        }
    
    @display(description=_('Payments'), label=True)
    def payment_status(self, obj):
        enabled = any([obj.payment_received, obj.payment_required])
        return {
            'value': 'Enabled' if enabled else 'Disabled',
            'color': 'success' if enabled else 'secondary',
        }


@admin.register(Notification)
class NotificationAdmin(UnfoldModelAdmin):
    """Admin interface for notifications"""
    
    list_display = ['user_email', 'title_truncated', 'notification_type', 'status_badge', 
                    'sent_at', 'read_at']
    list_filter = ['notification_type', 'status', 'sent_at', 'created_at']
    search_fields = ['user__email', 'title', 'body']
    list_select_related = ['user']
    readonly_fields = ['id', 'sent_at', 'delivered_at', 'read_at', 'created_at', 'updated_at', 
                       'fcm_message_id']
    date_hierarchy = 'created_at'
    
    fieldsets = (
        (_('User'), {
            'fields': ('user',),
        }),
        (_('Notification Content'), {
            'fields': ('notification_type', 'title', 'body', 'deep_link'),
        }),
        (_('Status'), {
            'fields': ('status', 'fcm_message_id', 'error_message'),
        }),
        (_('Additional Data'), {
            'fields': ('data',),
            'classes': ['collapse'],
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'sent_at', 'delivered_at', 'read_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('User'))
    def user_email(self, obj):
        return obj.user.email
    
    @display(description=_('Title'))
    def title_truncated(self, obj):
        return obj.title[:50] + '...' if len(obj.title) > 50 else obj.title
    
    @display(description=_('Status'), label=True)
    def status_badge(self, obj):
        colors = {
            'pending': 'warning',
            'sent': 'info',
            'delivered': 'success',
            'failed': 'danger',
            'read': 'secondary',
        }
        return {
            'value': obj.get_status_display(),
            'color': colors.get(obj.status, 'secondary'),
        }
