"""
Notifications app URLs for user notifications and alerts
"""
from django.urls import path, include
from rest_framework.routers import DefaultRouter
from . import views

app_name = 'notifications'

router = DefaultRouter()
router.register(r'notifications', views.NotificationViewSet, basename='notification')

urlpatterns = [
    path('', include(router.urls)),
    path('unread/', views.UnreadNotificationsView.as_view(), name='unread-notifications'),
    path('mark-read/<int:notification_id>/', views.MarkNotificationReadView.as_view(), name='mark-read'),
    path('mark-all-read/', views.MarkAllReadView.as_view(), name='mark-all-read'),
]
