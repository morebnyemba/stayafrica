from django.contrib import admin
from django.utils.html import format_html
from django.utils.translation import gettext_lazy as _
from unfold.admin import ModelAdmin as UnfoldModelAdmin
from unfold.decorators import display
from apps.messaging.models import Conversation, Message, MessageTemplate


@admin.register(Conversation)
class ConversationAdmin(UnfoldModelAdmin):
    """Enhanced admin interface for Conversation management"""
    
    list_display = ['id', 'subject', 'property_display', 'booking_display', 
                    'participant_count', 'message_count', 'updated_at']
    list_filter = ['property', 'booking', 'updated_at', 'created_at']
    search_fields = ['subject', 'property__title', 'booking__booking_ref', 'participants__email']
    filter_horizontal = ['participants', 'archived_by']
    readonly_fields = ['created_at', 'updated_at', 'conversation_summary']
    date_hierarchy = 'updated_at'
    list_select_related = ['property', 'booking']
    list_per_page = 25
    
    fieldsets = (
        (_('Conversation Details'), {
            'fields': ('subject', 'conversation_summary'),
        }),
        (_('Participants'), {
            'fields': ('participants', 'archived_by'),
        }),
        (_('Related'), {
            'fields': ('property', 'booking'),
            'classes': ['collapse'],
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )

    @display(description=_('Participants'))
    def participant_count(self, obj):
        return obj.participants.count()

    @display(description=_('Messages'))
    def message_count(self, obj):
        return obj.messages.count()

    @display(description=_('Property'))
    def property_display(self, obj):
        if obj.property:
            return obj.property.title
        return '-'

    @display(description=_('Booking'))
    def booking_display(self, obj):
        if obj.booking:
            return obj.booking.booking_ref
        return '-'

    @display(description=_('Summary'))
    def conversation_summary(self, obj):
        if obj.id:
            participants = ", ".join([p.get_full_name() or p.email for p in obj.participants.all()[:3]])
            if obj.participants.count() > 3:
                participants += f" and {obj.participants.count() - 3} more"
            
            summary = f"""
            <div style="padding: 10px; background: #F4F1EA; border-left: 3px solid #3A5C50; border-radius: 4px;">
                <strong style="color: #122F26;">Conversation Summary</strong><br/>
                <span style="color: #3A5C50;">
                    Subject: {obj.subject}<br/>
                    Participants ({obj.participants.count()}): {participants}<br/>
                    Messages: {obj.messages.count()}<br/>
                    {'Property: ' + obj.property.title if obj.property else ''}<br/>
                    {'Booking: ' + obj.booking.booking_ref if obj.booking else ''}
                </span>
            </div>
            """
            return format_html(summary)
        return "Save to see summary"


@admin.register(Message)
class MessageAdmin(UnfoldModelAdmin):
    """Enhanced admin interface for Message management"""
    
    list_display = ['id', 'sender_display', 'receiver_display', 'message_type', 
                    'read_badge', 'text_preview', 'created_at']
    list_filter = ['message_type', 'is_read', 'created_at']
    search_fields = ['sender__email', 'receiver__email', 'text', 'conversation__subject']
    readonly_fields = ['created_at', 'read_at', 'edited_at', 'message_preview']
    date_hierarchy = 'created_at'
    list_select_related = ['conversation', 'sender', 'receiver']
    list_per_page = 25
    actions = ['mark_read', 'mark_unread', 'moderate_messages']
    
    fieldsets = (
        (_('Message Information'), {
            'fields': ('conversation', 'message_preview', 'message_type'),
        }),
        (_('Sender & Receiver'), {
            'fields': ('sender', 'receiver'),
        }),
        (_('Content'), {
            'fields': ('text', 'attachment'),
        }),
        (_('Status'), {
            'fields': ('is_read', 'read_at', 'edited_at'),
        }),
        (_('Metadata'), {
            'fields': ('metadata',),
            'classes': ['collapse'],
        }),
        (_('Timestamps'), {
            'fields': ('created_at',),
            'classes': ['collapse'],
        }),
    )

    @display(description=_('Read'), label={"Read": "success", "Unread": "warning"})
    def read_badge(self, obj):
        return "Read" if obj.is_read else "Unread"

    @display(description=_('Sender'))
    def sender_display(self, obj):
        return obj.sender.get_full_name() or obj.sender.email

    @display(description=_('Receiver'))
    def receiver_display(self, obj):
        return obj.receiver.get_full_name() or obj.receiver.email

    @display(description=_('Message'))
    def text_preview(self, obj):
        return obj.text[:50] + '...' if len(obj.text) > 50 else obj.text

    @display(description=_('Preview'))
    def message_preview(self, obj):
        if obj.id:
            preview = f"""
            <div style="padding: 10px; background: #F4F1EA; border-left: 3px solid #D9B168; border-radius: 4px;">
                <strong style="color: #122F26;">From:</strong> {obj.sender.get_full_name() or obj.sender.email}<br/>
                <strong style="color: #122F26;">To:</strong> {obj.receiver.get_full_name() or obj.receiver.email}<br/>
                <strong style="color: #122F26;">Type:</strong> {obj.get_message_type_display()}<br/>
                <strong style="color: #122F26;">Status:</strong> {'Read' if obj.is_read else 'Unread'}<br/>
                <div style="margin-top: 8px; padding: 8px; background: white; border-radius: 4px;">
                    <em style="color: #3A5C50;">{obj.text[:200]}{'...' if len(obj.text) > 200 else ''}</em>
                </div>
            </div>
            """
            return format_html(preview)
        return "Save to see preview"

    @admin.action(description=_('Mark selected as read'))
    def mark_read(self, request, queryset):
        from django.utils import timezone
        updated = queryset.update(is_read=True, read_at=timezone.now())
        self.message_user(request, f'{updated} message(s) marked as read.')

    @admin.action(description=_('Mark selected as unread'))
    def mark_unread(self, request, queryset):
        updated = queryset.update(is_read=False, read_at=None)
        self.message_user(request, f'{updated} message(s) marked as unread.')

    @admin.action(description=_('Hide/Moderate selected messages'))
    def moderate_messages(self, request, queryset):
        updated = queryset.update(text='[MODERATED]', metadata={})
        self.message_user(request, f'{updated} message(s) moderated.')


@admin.register(MessageTemplate)
class MessageTemplateAdmin(UnfoldModelAdmin):
    """Enhanced admin interface for Message Template management"""
    
    list_display = ['name', 'template_type', 'active_badge', 'updated_at']
    list_filter = ['template_type', 'is_active', 'updated_at']
    search_fields = ['name', 'subject', 'body']
    readonly_fields = ['created_at', 'updated_at', 'template_preview']
    date_hierarchy = 'updated_at'
    actions = ['activate_templates', 'deactivate_templates']
    
    fieldsets = (
        (_('Template Information'), {
            'fields': ('name', 'template_type', 'is_active', 'template_preview'),
        }),
        (_('Content'), {
            'fields': ('subject', 'body'),
            'description': 'Use {variable} syntax for dynamic content',
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )

    @display(description=_('Active'), label={"Active": "success", "Inactive": "danger"})
    def active_badge(self, obj):
        return "Active" if obj.is_active else "Inactive"

    @display(description=_('Preview'))
    def template_preview(self, obj):
        if obj.id:
            preview = f"""
            <div style="padding: 10px; background: #F4F1EA; border-left: 3px solid #3A5C50; border-radius: 4px;">
                <strong style="color: #122F26;">Template Preview</strong><br/>
                <div style="margin-top: 8px;">
                    <strong style="color: #3A5C50;">Type:</strong> {obj.get_template_type_display()}<br/>
                    <strong style="color: #3A5C50;">Subject:</strong> {obj.subject}<br/>
                    <div style="margin-top: 8px; padding: 8px; background: white; border-radius: 4px;">
                        <em style="color: #3A5C50;">{obj.body[:200]}{'...' if len(obj.body) > 200 else ''}</em>
                    </div>
                </div>
            </div>
            """
            return format_html(preview)
        return "Save to see preview"

    @admin.action(description=_('Activate selected templates'))
    def activate_templates(self, request, queryset):
        updated = queryset.update(is_active=True)
        self.message_user(request, f'{updated} template(s) activated.')

    @admin.action(description=_('Deactivate selected templates'))
    def deactivate_templates(self, request, queryset):
        updated = queryset.update(is_active=False)
        self.message_user(request, f'{updated} template(s) deactivated.')


# ── Automated Messaging Admin ─────────────────────────────────────────────

from apps.messaging.automated_models import (
    HostMessageSettings, AutomatedMessage, ScheduledMessage,
    QuickReply, MessageAnalytics
)


@admin.register(HostMessageSettings)
class HostMessageSettingsAdmin(UnfoldModelAdmin):
    list_display = [
        'host_display', 'auto_responses_badge', 'quick_replies_badge',
        'scheduled_badge', 'away_badge', 'target_response_time_hours'
    ]
    search_fields = ['host__email', 'host__first_name', 'host__last_name']
    list_select_related = ['host']

    @display(description=_('Host'))
    def host_display(self, obj):
        return obj.host.get_full_name() or obj.host.email

    @display(description=_('Auto-Responses'), label={"On": "success", "Off": "secondary"})
    def auto_responses_badge(self, obj):
        return "On" if obj.enable_auto_responses else "Off"

    @display(description=_('Quick Replies'), label={"On": "success", "Off": "secondary"})
    def quick_replies_badge(self, obj):
        return "On" if obj.enable_quick_replies else "Off"

    @display(description=_('Scheduled'), label={"On": "success", "Off": "secondary"})
    def scheduled_badge(self, obj):
        return "On" if obj.enable_scheduled_messages else "Off"

    @display(description=_('Away'), label={"Away": "warning", "Available": "success"})
    def away_badge(self, obj):
        return "Away" if obj.away_mode_enabled else "Available"


@admin.register(AutomatedMessage)
class AutomatedMessageAdmin(UnfoldModelAdmin):
    list_display = [
        'name', 'host_display', 'trigger_badge', 'delay_display',
        'active_badge', 'updated_at'
    ]
    list_filter = ['trigger_type', 'is_active']
    search_fields = ['name', 'host__email', 'custom_message']
    list_select_related = ['host', 'template']

    @display(description=_('Host'))
    def host_display(self, obj):
        return obj.host.email

    @display(description=_('Trigger'), label={
        "Booking Inquiry Received": "info",
        "Booking Confirmed": "success",
        "Booking Cancelled": "danger",
        "Check-in Reminder": "warning",
        "Check-out Reminder": "warning",
        "Review Request": "info",
        "No Response After X Hours": "danger",
        "Custom Trigger": "secondary",
    })
    def trigger_badge(self, obj):
        return obj.get_trigger_type_display()

    @display(description=_('Delay'))
    def delay_display(self, obj):
        if obj.delay_hours == 0:
            return "Immediate"
        return f"{obj.delay_hours}h"

    @display(description=_('Active'), label={"Active": "success", "Inactive": "secondary"})
    def active_badge(self, obj):
        return "Active" if obj.is_active else "Inactive"


@admin.register(ScheduledMessage)
class ScheduledMessageAdmin(UnfoldModelAdmin):
    list_display = [
        'host_display', 'conversation_display', 'message_preview',
        'scheduled_time', 'status_badge', 'sent_at'
    ]
    list_filter = ['status', 'scheduled_time']
    search_fields = ['host__email', 'message_text']
    list_select_related = ['host', 'conversation', 'booking']
    date_hierarchy = 'scheduled_time'

    @display(description=_('Host'))
    def host_display(self, obj):
        return obj.host.email

    @display(description=_('Conversation'))
    def conversation_display(self, obj):
        return str(obj.conversation)[:40]

    @display(description=_('Message'))
    def message_preview(self, obj):
        return obj.message_text[:60] + '...' if len(obj.message_text) > 60 else obj.message_text

    @display(description=_('Status'), label={"Pending": "warning", "Sent": "success", "Failed": "danger", "Cancelled": "secondary"})
    def status_badge(self, obj):
        return obj.get_status_display()


@admin.register(QuickReply)
class QuickReplyAdmin(UnfoldModelAdmin):
    list_display = [
        'shortcut', 'host_display', 'category', 'message_preview',
        'use_count', 'active_badge'
    ]
    list_filter = ['category', 'is_active']
    search_fields = ['shortcut', 'message_text', 'host__email']
    list_select_related = ['host']

    @display(description=_('Host'))
    def host_display(self, obj):
        return obj.host.email

    @display(description=_('Message'))
    def message_preview(self, obj):
        return obj.message_text[:60] + '...' if len(obj.message_text) > 60 else obj.message_text

    @display(description=_('Active'), label={"Active": "success", "Inactive": "secondary"})
    def active_badge(self, obj):
        return "Active" if obj.is_active else "Inactive"


@admin.register(MessageAnalytics)
class MessageAnalyticsAdmin(UnfoldModelAdmin):
    list_display = [
        'host_display', 'date', 'messages_sent', 'messages_received',
        'conversations_started', 'avg_response_display',
        'quick_replies_used', 'automated_messages_sent'
    ]
    list_filter = ['date']
    search_fields = ['host__email']
    list_select_related = ['host']
    date_hierarchy = 'date'
    ordering = ['-date']

    @display(description=_('Host'))
    def host_display(self, obj):
        return obj.host.email

    @display(description=_('Avg Response'))
    def avg_response_display(self, obj):
        if obj.avg_response_time_minutes:
            if obj.avg_response_time_minutes < 60:
                return f"{obj.avg_response_time_minutes} min"
            return f"{obj.avg_response_time_minutes // 60}h {obj.avg_response_time_minutes % 60}m"
        return '-'

    def has_add_permission(self, request):
        return False
