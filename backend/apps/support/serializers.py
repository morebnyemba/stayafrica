from rest_framework import serializers
from apps.support.models import SupportTicket, SupportTicketEvent, CannedResponse, BugReport
from apps.messaging.serializers import ConversationSerializer
from django.contrib.auth import get_user_model

User = get_user_model()

class SupportTicketEventSerializer(serializers.ModelSerializer):
    actor_name = serializers.SerializerMethodField()
    
    class Meta:
        model = SupportTicketEvent
        fields = ['id', 'ticket', 'actor', 'actor_name', 'event_type', 'old_value', 'new_value', 'note', 'created_at']
        read_only_fields = ['id', 'created_at']
        
    def get_actor_name(self, obj):
        return obj.actor.get_full_name() or obj.actor.email


class SupportTicketSerializer(serializers.ModelSerializer):
    requester_name = serializers.SerializerMethodField()
    assigned_agent_name = serializers.SerializerMethodField()
    conversation_summary = ConversationSerializer(source='conversation', read_only=True)
    events = SupportTicketEventSerializer(many=True, read_only=True)
    
    class Meta:
        model = SupportTicket
        fields = [
            'id', 'conversation', 'conversation_summary', 'requester', 'requester_name', 
            'assigned_agent', 'assigned_agent_name', 'category', 'priority', 'status', 
            'subject', 'related_booking', 'related_property', 'resolution_notes', 
            'created_at', 'updated_at', 'resolved_at', 'satisfaction_rating', 'events'
        ]
        read_only_fields = ['id', 'created_at', 'updated_at', 'requester', 'conversation']

    def get_requester_name(self, obj):
        return obj.requester.get_full_name() or obj.requester.email

    def get_assigned_agent_name(self, obj):
        if obj.assigned_agent:
            return obj.assigned_agent.get_full_name() or obj.assigned_agent.email
        return None


class CannedResponseSerializer(serializers.ModelSerializer):
    class Meta:
        model = CannedResponse
        fields = ['id', 'title', 'body', 'category', 'is_active']


class BugReportSerializer(serializers.ModelSerializer):
    reporter_name = serializers.SerializerMethodField()
    assigned_name = serializers.SerializerMethodField()
    
    class Meta:
        model = BugReport
        fields = [
            'id', 'reporter', 'reporter_name', 'title', 'description', 'steps_to_reproduce',
            'expected_behavior', 'actual_behavior', 'severity', 'status', 'browser_info',
            'page_url', 'console_logs', 'network_errors', 'screenshot', 'support_ticket',
            'assigned_to', 'assigned_name', 'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'reporter', 'created_at', 'updated_at']

    def get_reporter_name(self, obj):
        return obj.reporter.get_full_name() or obj.reporter.email
        
    def get_assigned_name(self, obj):
        if obj.assigned_to:
            return obj.assigned_to.get_full_name() or obj.assigned_to.email
        return None
