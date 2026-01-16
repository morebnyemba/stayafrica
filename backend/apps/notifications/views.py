from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from django.shortcuts import get_object_or_404
from apps.notifications.models import PushToken, NotificationPreference, Notification
from apps.notifications.serializers import (
    PushTokenSerializer,
    NotificationPreferenceSerializer,
    NotificationSerializer
)
import logging

logger = logging.getLogger(__name__)


class PushTokenViewSet(viewsets.ModelViewSet):
    """Manage push notification tokens"""
    serializer_class = PushTokenSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        return PushToken.objects.filter(user=self.request.user)
    
    def create(self, request, *args, **kwargs):
        """Register or update a push token"""
        serializer = self.get_serializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        push_token = serializer.save()
        
        logger.info(f"Push token registered for user {request.user.email}")
        return Response(
            PushTokenSerializer(push_token).data,
            status=status.HTTP_201_CREATED
        )
    
    @action(detail=True, methods=['post'])
    def deactivate(self, request, pk=None):
        """Deactivate a push token"""
        push_token = self.get_object()
        push_token.is_active = False
        push_token.save()
        
        logger.info(f"Push token deactivated for user {request.user.email}")
        return Response({'status': 'deactivated'})


class NotificationPreferenceViewSet(viewsets.ViewSet):
    """Manage notification preferences"""
    permission_classes = [IsAuthenticated]
    
    def list(self, request):
        """Get user's notification preferences"""
        preferences, created = NotificationPreference.objects.get_or_create(
            user=request.user
        )
        serializer = NotificationPreferenceSerializer(preferences)
        return Response(serializer.data)
    
    def update(self, request, pk=None):
        """Update notification preferences"""
        preferences, created = NotificationPreference.objects.get_or_create(
            user=request.user
        )
        serializer = NotificationPreferenceSerializer(
            preferences,
            data=request.data,
            partial=True
        )
        serializer.is_valid(raise_exception=True)
        serializer.save()
        
        logger.info(f"Notification preferences updated for user {request.user.email}")
        return Response(serializer.data)


class NotificationViewSet(viewsets.ReadOnlyModelViewSet):
    """View user notifications"""
    serializer_class = NotificationSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        return Notification.objects.filter(user=self.request.user)
    
    @action(detail=False, methods=['get'])
    def unread_count(self, request):
        """Get count of unread notifications"""
        count = Notification.objects.filter(
            user=request.user,
            status__in=['sent', 'delivered']
        ).count()
        return Response({'unread_count': count})
    
    @action(detail=True, methods=['post'])
    def mark_read(self, request, pk=None):
        """Mark a notification as read"""
        notification = self.get_object()
        notification.mark_as_read()
        
        return Response({'status': 'read'})
    
    @action(detail=False, methods=['post'])
    def mark_all_read(self, request):
        """Mark all notifications as read"""
        updated = Notification.objects.filter(
            user=request.user,
            status__in=['sent', 'delivered']
        ).update(status='read')
        
        return Response({'marked_read': updated})
