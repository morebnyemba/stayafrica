"""
Serializers for Automated Messaging
"""
from rest_framework import serializers
from apps.messaging.automated_models import (
    HostMessageSettings,
    AutomatedMessage,
    ScheduledMessage,
    QuickReply,
    MessageAnalytics
)
from apps.messaging.models import MessageTemplate


class HostMessageSettingsSerializer(serializers.ModelSerializer):
    """Serializer for host message settings"""
    
    class Meta:
        model = HostMessageSettings
        fields = [
            'enable_auto_responses', 'enable_quick_replies',
            'enable_scheduled_messages', 'away_mode_enabled',
            'away_message', 'target_response_time_hours',
            'created_at', 'updated_at'
        ]
        read_only_fields = ['created_at', 'updated_at']


class AutomatedMessageSerializer(serializers.ModelSerializer):
    """Serializer for automated messages"""
    trigger_type_display = serializers.CharField(source='get_trigger_type_display', read_only=True)
    template_name = serializers.CharField(source='template.name', read_only=True, allow_null=True)
    
    class Meta:
        model = AutomatedMessage
        fields = [
            'id', 'name', 'trigger_type', 'trigger_type_display',
            'template', 'template_name', 'delay_hours',
            'custom_message', 'is_active',
            'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'created_at', 'updated_at']


class ScheduledMessageSerializer(serializers.ModelSerializer):
    """Serializer for scheduled messages"""
    status_display = serializers.CharField(source='get_status_display', read_only=True)
    conversation_id = serializers.IntegerField(source='conversation.id', read_only=True)
    
    class Meta:
        model = ScheduledMessage
        fields = [
            'id', 'conversation', 'conversation_id', 'message_text',
            'scheduled_time', 'status', 'status_display',
            'sent_at', 'booking', 'created_at'
        ]
        read_only_fields = ['id', 'status', 'sent_at', 'created_at']


class ScheduledMessageCreateSerializer(serializers.ModelSerializer):
    """Serializer for creating scheduled messages"""
    
    class Meta:
        model = ScheduledMessage
        fields = ['conversation', 'message_text', 'scheduled_time', 'booking']
    
    def validate_scheduled_time(self, value):
        """Ensure scheduled time is in the future"""
        from django.utils import timezone
        if value <= timezone.now():
            raise serializers.ValidationError("Scheduled time must be in the future")
        return value


class QuickReplySerializer(serializers.ModelSerializer):
    """Serializer for quick replies"""
    
    class Meta:
        model = QuickReply
        fields = [
            'id', 'shortcut', 'message_text', 'category',
            'use_count', 'is_active', 'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'use_count', 'created_at', 'updated_at']


class MessageAnalyticsSerializer(serializers.ModelSerializer):
    """Serializer for message analytics"""
    
    class Meta:
        model = MessageAnalytics
        fields = [
            'date', 'messages_sent', 'messages_received',
            'conversations_started', 'conversations_resolved',
            'avg_response_time_minutes', 'quick_replies_used',
            'automated_messages_sent'
        ]
        read_only_fields = fields


class MessageTemplateSerializer(serializers.ModelSerializer):
    """Serializer for message templates"""
    template_type_display = serializers.CharField(source='get_template_type_display', read_only=True)
    
    class Meta:
        model = MessageTemplate
        fields = [
            'id', 'name', 'template_type', 'template_type_display',
            'subject', 'body', 'is_active',
            'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'created_at', 'updated_at']
