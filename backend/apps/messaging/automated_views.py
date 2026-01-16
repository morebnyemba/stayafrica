"""
Views for Automated Messaging
"""
from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from apps.messaging.automated_models import (
    HostMessageSettings,
    AutomatedMessage,
    ScheduledMessage,
    QuickReply,
    MessageAnalytics
)
from apps.messaging.models import MessageTemplate
from apps.messaging.automated_serializers import (
    HostMessageSettingsSerializer,
    AutomatedMessageSerializer,
    ScheduledMessageSerializer,
    ScheduledMessageCreateSerializer,
    QuickReplySerializer,
    MessageAnalyticsSerializer,
    MessageTemplateSerializer
)
import logging

logger = logging.getLogger(__name__)


class HostMessageSettingsViewSet(viewsets.ViewSet):
    """ViewSet for host message settings"""
    permission_classes = [IsAuthenticated]
    
    def list(self, request):
        """Get host message settings"""
        settings, created = HostMessageSettings.objects.get_or_create(
            host=request.user
        )
        serializer = HostMessageSettingsSerializer(settings)
        return Response(serializer.data)
    
    def update(self, request, pk=None):
        """Update host message settings"""
        settings, created = HostMessageSettings.objects.get_or_create(
            host=request.user
        )
        serializer = HostMessageSettingsSerializer(settings, data=request.data, partial=True)
        serializer.is_valid(raise_exception=True)
        serializer.save()
        return Response(serializer.data)


class AutomatedMessageViewSet(viewsets.ModelViewSet):
    """ViewSet for automated messages"""
    serializer_class = AutomatedMessageSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        """Return automated messages for current host"""
        return AutomatedMessage.objects.filter(host=self.request.user)
    
    def perform_create(self, serializer):
        """Create automated message for current host"""
        serializer.save(host=self.request.user)


class ScheduledMessageViewSet(viewsets.ModelViewSet):
    """ViewSet for scheduled messages"""
    permission_classes = [IsAuthenticated]
    
    def get_serializer_class(self):
        if self.action == 'create':
            return ScheduledMessageCreateSerializer
        return ScheduledMessageSerializer
    
    def get_queryset(self):
        """Return scheduled messages for current host"""
        return ScheduledMessage.objects.filter(host=self.request.user)
    
    def perform_create(self, serializer):
        """Create scheduled message for current host"""
        serializer.save(host=self.request.user)
    
    @action(detail=True, methods=['post'])
    def cancel(self, request, pk=None):
        """Cancel a scheduled message"""
        scheduled_msg = self.get_object()
        
        if scheduled_msg.status != 'pending':
            return Response(
                {'error': 'Only pending messages can be cancelled'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        scheduled_msg.status = 'cancelled'
        scheduled_msg.save()
        
        return Response({'message': 'Scheduled message cancelled'})


class QuickReplyViewSet(viewsets.ModelViewSet):
    """ViewSet for quick replies"""
    serializer_class = QuickReplySerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        """Return quick replies for current host"""
        return QuickReply.objects.filter(host=self.request.user)
    
    def perform_create(self, serializer):
        """Create quick reply for current host"""
        serializer.save(host=self.request.user)
    
    @action(detail=True, methods=['post'])
    def use(self, request, pk=None):
        """Use a quick reply (increments counter)"""
        quick_reply = self.get_object()
        quick_reply.increment_use_count()
        
        serializer = QuickReplySerializer(quick_reply)
        return Response(serializer.data)


class MessageAnalyticsViewSet(viewsets.ReadOnlyModelViewSet):
    """ViewSet for message analytics (read-only)"""
    serializer_class = MessageAnalyticsSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        """Return analytics for current host"""
        return MessageAnalytics.objects.filter(host=self.request.user)
    
    @action(detail=False, methods=['get'])
    def summary(self, request):
        """Get analytics summary"""
        from django.db.models import Sum, Avg
        from datetime import timedelta
        from django.utils import timezone
        
        # Last 30 days
        thirty_days_ago = timezone.now().date() - timedelta(days=30)
        
        analytics = MessageAnalytics.objects.filter(
            host=request.user,
            date__gte=thirty_days_ago
        )
        
        summary = analytics.aggregate(
            total_messages_sent=Sum('messages_sent'),
            total_messages_received=Sum('messages_received'),
            total_quick_replies_used=Sum('quick_replies_used'),
            total_automated_messages=Sum('automated_messages_sent'),
            avg_response_time=Avg('avg_response_time_minutes')
        )
        
        return Response(summary)


class MessageTemplateViewSet(viewsets.ModelViewSet):
    """ViewSet for message templates"""
    serializer_class = MessageTemplateSerializer
    permission_classes = [IsAuthenticated]
    queryset = MessageTemplate.objects.all()
    
    def get_queryset(self):
        """Return all active templates"""
        return MessageTemplate.objects.filter(is_active=True)
