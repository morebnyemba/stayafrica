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
    
    list_display = ['host_display', 'auto_responses_badge', 'quick_replies_badge', 
                    'scheduled_messages_badge', 'updated_at']
    list_filter = ['enable_auto_responses', 'enable_quick_replies', 'enable_scheduled_messages', 'updated_at']
    search_fields = ['host__email', 'host__username']
    readonly_fields = ['host', 'created_at', 'updated_at']
    list_select_related = ['host']
    list_per_page = 25
    
    fieldsets = (
        (_('Host'), {
            'fields': ('host',),
        }),
        (_('Feature Toggles'), {
            'fields': ('enable_auto_responses', 'enable_quick_replies', 'enable_scheduled_messages'),
        }),
        (_('Away Mode'), {
            'fields': ('away_mode_enabled', 'away_message'),
        }),
        (_('Response Settings'), {
            'fields': ('target_response_time_hours',),
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Host'))
    def host_display(self, obj):
        return obj.host.get_full_name() or obj.host.email
    
    @display(description=_('Auto Responses'), label=True)
    def auto_responses_badge(self, obj):
        return {
            'value': 'Enabled' if obj.enable_auto_responses else 'Disabled',
            'color': 'success' if obj.enable_auto_responses else 'secondary',
        }
    
    @display(description=_('Quick Replies'), label=True)
    def quick_replies_badge(self, obj):
        return {
            'value': 'Enabled' if obj.enable_quick_replies else 'Disabled',
            'color': 'success' if obj.enable_quick_replies else 'secondary',
        }
    
    @display(description=_('Scheduled'), label=True)
    def scheduled_messages_badge(self, obj):
        return {
            'value': 'Enabled' if obj.enable_scheduled_messages else 'Disabled',
            'color': 'success' if obj.enable_scheduled_messages else 'secondary',
        }


@admin.register(AutomatedMessage)
class AutomatedMessageAdmin(UnfoldModelAdmin):
    """Admin interface for automated messages"""
    
    list_display = ['host_display', 'name', 'trigger_type', 'active_badge', 'created_at']
    list_filter = ['trigger_type', 'is_active', 'created_at']
    search_fields = ['host__email', 'name', 'custom_message']
    readonly_fields = ['host', 'created_at', 'updated_at']
    list_select_related = ['host', 'template']
    list_per_page = 25
    actions = ['activate_messages', 'deactivate_messages']
    
    fieldsets = (
        (_('Basic Information'), {
            'fields': ('host', 'name', 'trigger_type', 'is_active'),
        }),
        (_('Template & Delay'), {
            'fields': ('template', 'delay_hours'),
        }),
        (_('Custom Message'), {
            'fields': ('custom_message',),
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
    
    list_display = ['host_display', 'message_preview', 'scheduled_time', 
                    'status_badge', 'sent_at']
    list_filter = ['status', 'scheduled_time', 'sent_at']
    search_fields = ['host__email', 'message_text']
    readonly_fields = ['host', 'conversation', 'booking', 
                       'created_at', 'sent_at']
    list_select_related = ['host', 'conversation', 'booking']
    list_per_page = 25
    date_hierarchy = 'scheduled_time'
    actions = ['mark_sent', 'mark_cancelled']
    
    fieldsets = (
        (_('Participants'), {
            'fields': ('host', 'conversation', 'booking'),
        }),
        (_('Message'), {
            'fields': ('message_text',),
        }),
        (_('Scheduling'), {
            'fields': ('scheduled_time', 'status', 'sent_at'),
        }),
        (_('Timestamp'), {
            'fields': ('created_at',),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Host'))
    def host_display(self, obj):
        return obj.host.get_full_name() or obj.host.email
    
    @display(description=_('Message'))
    def message_preview(self, obj):
        if obj.message_text:
            return obj.message_text[:60] + '...' if len(obj.message_text) > 60 else obj.message_text
        return '-'
    
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
    
    list_display = ['host_display', 'shortcut', 'category', 'active_badge', 'use_count', 'created_at']
    list_filter = ['is_active', 'category', 'created_at']
    search_fields = ['host__email', 'shortcut', 'message_text']
    readonly_fields = ['host', 'use_count', 'created_at', 'updated_at']
    list_select_related = ['host']
    list_per_page = 25
    actions = ['activate_replies', 'deactivate_replies']
    
    fieldsets = (
        (_('Basic Information'), {
            'fields': ('host', 'shortcut', 'category', 'is_active'),
        }),
        (_('Message Content'), {
            'fields': ('message_text',),
        }),
        (_('Statistics'), {
            'fields': ('use_count',),
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
    
    list_display = ['host_display', 'date', 'messages_sent', 'messages_received',
                    'avg_response_time_display', 'automated_messages_sent']
    list_filter = ['date']
    search_fields = ['host__email']
    readonly_fields = ['host', 'date', 'messages_sent', 'messages_received',
                       'conversations_started', 'conversations_resolved',
                       'avg_response_time_minutes', 'quick_replies_used',
                       'automated_messages_sent', 'created_at', 'updated_at']
    list_select_related = ['host']
    list_per_page = 25
    date_hierarchy = 'date'
    
    fieldsets = (
        (_('Host & Date'), {
            'fields': ('host', 'date'),
        }),
        (_('Message Counts'), {
            'fields': ('messages_sent', 'messages_received', 'automated_messages_sent'),
        }),
        (_('Conversations'), {
            'fields': ('conversations_started', 'conversations_resolved'),
        }),
        (_('Performance'), {
            'fields': ('avg_response_time_minutes', 'quick_replies_used'),
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Host'))
    def host_display(self, obj):
        return obj.host.get_full_name() or obj.host.email
    
    @display(description=_('Avg Response Time'))
    def avg_response_time_display(self, obj):
        if obj.avg_response_time_minutes:
            hours = obj.avg_response_time_minutes / 60
            if hours < 1:
                return f"{obj.avg_response_time_minutes:.0f} min"
            return f"{hours:.1f} hrs"
        return '-'
    
    def has_add_permission(self, request):
        return False
    
    def has_change_permission(self, request, obj=None):
        return False
