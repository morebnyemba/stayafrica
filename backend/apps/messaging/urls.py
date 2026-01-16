from django.urls import path, include
from rest_framework.routers import DefaultRouter
from apps.messaging.views import (
    MessageViewSet, ConversationViewSet, MessageTemplateViewSet,
    erlang_persist_messages, erlang_health
)
from apps.messaging.automated_views import (
    HostMessageSettingsViewSet,
    AutomatedMessageViewSet,
    ScheduledMessageViewSet,
    QuickReplyViewSet,
    MessageAnalyticsViewSet
)

app_name = 'messaging'

router = DefaultRouter()
router.register(r'conversations', ConversationViewSet, basename='conversation')
router.register(r'messages', MessageViewSet, basename='message')
router.register(r'templates', MessageTemplateViewSet, basename='template')
router.register(r'automated-messages', AutomatedMessageViewSet, basename='automated-message')
router.register(r'scheduled-messages', ScheduledMessageViewSet, basename='scheduled-message')
router.register(r'quick-replies', QuickReplyViewSet, basename='quick-reply')
router.register(r'analytics', MessageAnalyticsViewSet, basename='message-analytics')
router.register(r'settings', HostMessageSettingsViewSet, basename='message-settings')

urlpatterns = [
    path('', include(router.urls)),
    path('erlang/persist/', erlang_persist_messages, name='erlang-persist'),
    path('erlang/health/', erlang_health, name='erlang-health'),
]
