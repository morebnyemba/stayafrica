from django.contrib import admin
from django.utils.html import format_html
from django.utils.translation import gettext_lazy as _
from unfold.admin import ModelAdmin as UnfoldModelAdmin
from unfold.decorators import display
from apps.messaging.automated_models import (
    HostMessageSettings, AutomatedMessage, ScheduledMessage,
    QuickReply, MessageAnalytics
)


@admin.register(HostMessageSettings)
class HostMessageSettingsAdmin(UnfoldModelAdmin):
    """Admin interface for host messaging settings"""
    
    list_display = ['host_display', 'auto_reply_enabled_badge', 'greeting_enabled_badge', 
                    'check_in_enabled_badge', 'updated_at']
    list_filter = ['enable_auto_reply', 'enable_greeting_message', 'enable_check_in_reminder', 'updated_at']
    search_fields = ['host__email', 'host__username']
    readonly_fields = ['host', 'created_at', 'updated_at']
    list_select_related = ['host']
    list_per_page = 25
    
    fieldsets = (
        (_('Host'), {
            'fields': ('host',),
        }),
        (_('Auto-Reply Settings'), {
            'fields': ('enable_auto_reply', 'auto_reply_message', 'auto_reply_delay_minutes'),
        }),
        (_('Greeting Message'), {
            'fields': ('enable_greeting_message', 'greeting_message_template', 'greeting_delay_hours'),
        }),
        (_('Check-in Reminder'), {
            'fields': ('enable_check_in_reminder', 'check_in_reminder_template', 'check_in_reminder_hours'),
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Host'))
    def host_display(self, obj):
        return obj.host.get_full_name() or obj.host.email
    
    @display(description=_('Auto Reply'), label=True)
    def auto_reply_enabled_badge(self, obj):
        return {
            'value': 'Enabled' if obj.enable_auto_reply else 'Disabled',
            'color': 'success' if obj.enable_auto_reply else 'secondary',
        }
    
    @display(description=_('Greeting'), label=True)
    def greeting_enabled_badge(self, obj):
        return {
            'value': 'Enabled' if obj.enable_greeting_message else 'Disabled',
            'color': 'success' if obj.enable_greeting_message else 'secondary',
        }
    
    @display(description=_('Check-in'), label=True)
    def check_in_enabled_badge(self, obj):
        return {
            'value': 'Enabled' if obj.enable_check_in_reminder else 'Disabled',
            'color': 'success' if obj.enable_check_in_reminder else 'secondary',
        }


@admin.register(AutomatedMessage)
class AutomatedMessageAdmin(UnfoldModelAdmin):
    """Admin interface for automated messages"""
    
    list_display = ['host_display', 'message_type', 'trigger_event', 'active_badge', 'created_at']
    list_filter = ['message_type', 'trigger_event', 'is_active', 'created_at']
    search_fields = ['host__email', 'name', 'message_content']
    readonly_fields = ['host', 'created_at', 'updated_at']
    list_select_related = ['host']
    list_per_page = 25
    actions = ['activate_messages', 'deactivate_messages']
    
    fieldsets = (
        (_('Basic Information'), {
            'fields': ('host', 'name', 'message_type', 'is_active'),
        }),
        (_('Trigger'), {
            'fields': ('trigger_event', 'trigger_delay_hours'),
        }),
        (_('Message Content'), {
            'fields': ('message_content',),
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Host'))
    def host_display(self, obj):
        return obj.host.get_full_name() or obj.host.email
    
    @display(description=_('Active'), label=True)
    def active_badge(self, obj):
        return {
            'value': 'Active' if obj.is_active else 'Inactive',
            'color': 'success' if obj.is_active else 'secondary',
        }
    
    @admin.action(description=_('Activate selected messages'))
    def activate_messages(self, request, queryset):
        updated = queryset.update(is_active=True)
        self.message_user(request, f'{updated} automated message(s) activated.')
    
    @admin.action(description=_('Deactivate selected messages'))
    def deactivate_messages(self, request, queryset):
        updated = queryset.update(is_active=False)
        self.message_user(request, f'{updated} automated message(s) deactivated.')


@admin.register(ScheduledMessage)
class ScheduledMessageAdmin(UnfoldModelAdmin):
    """Admin interface for scheduled messages"""
    
    list_display = ['host_display', 'recipient_display', 'scheduled_for', 
                    'status_badge', 'sent_at']
    list_filter = ['status', 'scheduled_for', 'sent_at']
    search_fields = ['host__email', 'recipient__email', 'message_content']
    readonly_fields = ['host', 'recipient', 'booking', 'automated_message', 
                       'created_at', 'sent_at', 'error_message']
    list_select_related = ['host', 'recipient', 'booking', 'automated_message']
    list_per_page = 25
    date_hierarchy = 'scheduled_for'
    actions = ['mark_sent', 'mark_cancelled']
    
    fieldsets = (
        (_('Participants'), {
            'fields': ('host', 'recipient', 'booking'),
        }),
        (_('Message'), {
            'fields': ('automated_message', 'message_content'),
        }),
        (_('Scheduling'), {
            'fields': ('scheduled_for', 'status', 'sent_at'),
        }),
        (_('Error'), {
            'fields': ('error_message',),
            'classes': ['collapse'],
        }),
        (_('Timestamp'), {
            'fields': ('created_at',),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Host'))
    def host_display(self, obj):
        return obj.host.get_full_name() or obj.host.email
    
    @display(description=_('Recipient'))
    def recipient_display(self, obj):
        return obj.recipient.get_full_name() or obj.recipient.email
    
    @display(description=_('Status'), label=True)
    def status_badge(self, obj):
        colors = {
            'pending': 'warning',
            'sent': 'success',
            'failed': 'danger',
            'cancelled': 'secondary',
        }
        return {
            'value': obj.get_status_display(),
            'color': colors.get(obj.status, 'secondary'),
        }
    
    @admin.action(description=_('Mark as sent'))
    def mark_sent(self, request, queryset):
        from django.utils import timezone
        updated = queryset.filter(status='pending').update(status='sent', sent_at=timezone.now())
        self.message_user(request, f'{updated} message(s) marked as sent.')
    
    @admin.action(description=_('Mark as cancelled'))
    def mark_cancelled(self, request, queryset):
        updated = queryset.filter(status='pending').update(status='cancelled')
        self.message_user(request, f'{updated} message(s) cancelled.')


@admin.register(QuickReply)
class QuickReplyAdmin(UnfoldModelAdmin):
    """Admin interface for quick reply templates"""
    
    list_display = ['host_display', 'shortcut', 'title', 'active_badge', 'usage_count', 'created_at']
    list_filter = ['is_active', 'created_at']
    search_fields = ['host__email', 'shortcut', 'title', 'message_content']
    readonly_fields = ['host', 'usage_count', 'created_at', 'updated_at']
    list_select_related = ['host']
    list_per_page = 25
    actions = ['activate_replies', 'deactivate_replies']
    
    fieldsets = (
        (_('Basic Information'), {
            'fields': ('host', 'shortcut', 'title', 'is_active'),
        }),
        (_('Message Content'), {
            'fields': ('message_content',),
        }),
        (_('Statistics'), {
            'fields': ('usage_count',),
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Host'))
    def host_display(self, obj):
        return obj.host.get_full_name() or obj.host.email
    
    @display(description=_('Active'), label=True)
    def active_badge(self, obj):
        return {
            'value': 'Active' if obj.is_active else 'Inactive',
            'color': 'success' if obj.is_active else 'secondary',
        }
    
    @admin.action(description=_('Activate selected quick replies'))
    def activate_replies(self, request, queryset):
        updated = queryset.update(is_active=True)
        self.message_user(request, f'{updated} quick reply(ies) activated.')
    
    @admin.action(description=_('Deactivate selected quick replies'))
    def deactivate_replies(self, request, queryset):
        updated = queryset.update(is_active=False)
        self.message_user(request, f'{updated} quick reply(ies) deactivated.')


@admin.register(MessageAnalytics)
class MessageAnalyticsAdmin(UnfoldModelAdmin):
    """Admin interface for message analytics"""
    
    list_display = ['host_display', 'period_display', 'total_sent', 'total_received',
                    'avg_response_time_display', 'response_rate_display']
    list_filter = ['period_type', 'year', 'month']
    search_fields = ['host__email']
    readonly_fields = ['host', 'period_type', 'year', 'month', 'quarter', 'week',
                       'total_sent', 'total_received', 'total_automated', 
                       'avg_response_time_minutes', 'response_rate', 'created_at', 'updated_at']
    list_select_related = ['host']
    list_per_page = 25
    
    fieldsets = (
        (_('Host & Period'), {
            'fields': ('host', 'period_type', 'year', 'month', 'quarter', 'week'),
        }),
        (_('Message Counts'), {
            'fields': ('total_sent', 'total_received', 'total_automated'),
        }),
        (_('Performance'), {
            'fields': ('avg_response_time_minutes', 'response_rate'),
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Host'))
    def host_display(self, obj):
        return obj.host.get_full_name() or obj.host.email
    
    @display(description=_('Period'))
    def period_display(self, obj):
        if obj.period_type == 'monthly':
            return f"{obj.year}-{obj.month:02d}"
        elif obj.period_type == 'quarterly':
            return f"{obj.year} Q{obj.quarter}"
        elif obj.period_type == 'weekly':
            return f"{obj.year} W{obj.week}"
        return f"{obj.year}"
    
    @display(description=_('Avg Response Time'))
    def avg_response_time_display(self, obj):
        if obj.avg_response_time_minutes:
            hours = obj.avg_response_time_minutes / 60
            if hours < 1:
                return f"{obj.avg_response_time_minutes:.0f} min"
            return f"{hours:.1f} hrs"
        return '-'
    
    @display(description=_('Response Rate'))
    def response_rate_display(self, obj):
        if obj.response_rate:
            return f"{obj.response_rate:.1f}%"
        return '-'
    
    def has_add_permission(self, request):
        return False
    
    def has_change_permission(self, request, obj=None):
        return False
