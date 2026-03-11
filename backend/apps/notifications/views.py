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
        if getattr(self.request.user, 'is_admin_user', False):
            return PushToken.objects.all()
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

    @action(detail=False, methods=['post'], url_path='deactivate-all')
    def deactivate_all(self, request):
        """Deactivate all push tokens for the current user (used on logout)"""
        count = self.get_queryset().filter(is_active=True).update(is_active=False)
        logger.info(f"Deactivated {count} push tokens for user {request.user.email}")
        return Response({'status': 'deactivated', 'count': count})


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


from rest_framework.mixins import DestroyModelMixin


class NotificationViewSet(DestroyModelMixin, viewsets.ReadOnlyModelViewSet):
    """View and manage user notifications"""
    serializer_class = NotificationSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        if getattr(self.request.user, 'is_admin_user', False):
            return Notification.objects.all()
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
    
    @action(detail=False, methods=['post'])
    def clear_all(self, request):
        """Delete all notifications for the user"""
        count, _ = Notification.objects.filter(user=request.user).delete()
        return Response({'deleted': count})


from rest_framework import serializers
from apps.notifications.models import EmailConfiguration


class EmailConfigurationSerializer(serializers.ModelSerializer):
    """Serializer for email configuration singleton"""
    class Meta:
        model = EmailConfiguration
        fields = '__all__'
        extra_kwargs = {'password': {'write_only': True}}


class EmailConfigurationViewSet(viewsets.ModelViewSet):
    """Manage email configurations"""
    permission_classes = [IsAuthenticated]
    serializer_class = EmailConfigurationSerializer
    
    def get_queryset(self):
        if getattr(self.request.user, 'is_admin_user', False):
            # Ensure at least one config exists before listing
            EmailConfiguration.get_config()
            return EmailConfiguration.objects.all()
        return EmailConfiguration.objects.none()
        
    @action(detail=True, methods=['post'])
    def test_connection(self, request, pk=None):
        if not getattr(request.user, 'is_admin_user', False):
            return Response({'error': 'Admin only'}, status=status.HTTP_403_FORBIDDEN)
        config = self.get_object()
        success, message = config.test_connection()
        return Response({'success': success, 'message': message})
