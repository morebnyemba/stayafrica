from django.contrib import admin
from django.utils.html import format_html
from unfold.admin import ModelAdmin as UnfoldModelAdmin
from apps.messaging.models import Conversation, Message, MessageTemplate


@admin.register(Conversation)
class ConversationAdmin(UnfoldModelAdmin):
    list_display = ['id', 'subject', 'property', 'booking', 'participant_count', 'updated_at']
    list_filter = ['property', 'booking', 'updated_at']
    search_fields = ['subject', 'property__title', 'booking__booking_ref', 'participants__email']
    filter_horizontal = ['participants', 'archived_by']
    readonly_fields = ['created_at', 'updated_at']
    date_hierarchy = 'updated_at'
    list_select_related = ['property', 'booking']

    @admin.display(description='Participants')
    def participant_count(self, obj):
        return obj.participants.count()


@admin.register(Message)
class MessageAdmin(UnfoldModelAdmin):
    list_display = ['conversation', 'sender', 'receiver', 'message_type', 'is_read_badge', 'created_at']
    list_filter = ['message_type', 'is_read', 'created_at']
    search_fields = ['sender__email', 'receiver__email', 'text', 'conversation__subject']
    readonly_fields = ['created_at', 'read_at', 'edited_at']
    raw_id_fields = ['conversation', 'sender', 'receiver']
    date_hierarchy = 'created_at'
    list_select_related = ['conversation', 'sender', 'receiver']
    actions = ['mark_read', 'mark_unread', 'moderate_messages']

    @admin.display(description='Read')
    def is_read_badge(self, obj):
        klass = 'bg-green-100 text-green-700' if obj.is_read else 'bg-amber-100 text-amber-700'
        label = 'Read' if obj.is_read else 'Unread'
        return format_html('<span class="{} px-2 py-1 rounded">{}</span>', klass, label)

    def mark_read(self, request, queryset):
        queryset.update(is_read=True, read_at=None)
    mark_read.short_description = 'Mark selected as read'

    def mark_unread(self, request, queryset):
        queryset.update(is_read=False, read_at=None)
    mark_unread.short_description = 'Mark selected as unread'

    def moderate_messages(self, request, queryset):
        queryset.update(text='[MODERATED]', metadata={})
    moderate_messages.short_description = 'Hide/Moderate selected messages'


@admin.register(MessageTemplate)
class MessageTemplateAdmin(UnfoldModelAdmin):
    list_display = ['name', 'template_type', 'is_active_badge', 'updated_at']
    list_filter = ['template_type', 'is_active', 'updated_at']
    search_fields = ['name', 'subject', 'body']
    readonly_fields = ['created_at', 'updated_at']
    date_hierarchy = 'updated_at'
    actions = ['activate_templates', 'deactivate_templates']

    @admin.display(description='Active')
    def is_active_badge(self, obj):
        klass = 'bg-green-100 text-green-700' if obj.is_active else 'bg-red-100 text-red-700'
        label = 'Active' if obj.is_active else 'Inactive'
        return format_html('<span class="{} px-2 py-1 rounded">{}</span>', klass, label)

    def activate_templates(self, request, queryset):
        queryset.update(is_active=True)
    activate_templates.short_description = 'Activate selected templates'

    def deactivate_templates(self, request, queryset):
        queryset.update(is_active=False)
    deactivate_templates.short_description = 'Deactivate selected templates'
