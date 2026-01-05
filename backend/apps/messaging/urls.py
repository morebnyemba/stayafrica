from django.urls import path, include
from rest_framework.routers import DefaultRouter
from apps.messaging.views import (
    MessageViewSet, ConversationViewSet, MessageTemplateViewSet,
    erlang_persist_messages, erlang_health
)

app_name = 'messaging'

router = DefaultRouter()
router.register(r'messaging/conversations', ConversationViewSet, basename='conversation')
router.register(r'messaging/messages', MessageViewSet, basename='message')
router.register(r'messaging/templates', MessageTemplateViewSet, basename='template')

urlpatterns = [
    path('', include(router.urls)),
    path('messaging/erlang/persist/', erlang_persist_messages, name='erlang-persist'),
    path('messaging/erlang/health/', erlang_health, name='erlang-health'),
]
