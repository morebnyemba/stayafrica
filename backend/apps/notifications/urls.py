"""
Notifications app URLs for user notifications and alerts
"""
from django.urls import path, include
from rest_framework.routers import DefaultRouter
from apps.notifications.views import (
    PushTokenViewSet,
    NotificationPreferenceViewSet,
    NotificationViewSet
)

app_name = 'notifications'

router = DefaultRouter()
router.register('tokens', PushTokenViewSet, basename='push-token')
router.register('notifications', NotificationViewSet, basename='notification')

urlpatterns = [
    path('', include(router.urls)),
    path('preferences/', NotificationPreferenceViewSet.as_view({'get': 'list', 'put': 'update'}), name='notification-preferences'),
]
