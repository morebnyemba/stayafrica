from django.urls import path, include
from rest_framework.routers import DefaultRouter
from apps.messaging.views import MessageViewSet, ConversationViewSet, MessageTemplateViewSet

app_name = 'messaging'

router = DefaultRouter()
router.register(r'conversations', ConversationViewSet, basename='conversation')
router.register(r'messages', MessageViewSet, basename='message')
router.register(r'templates', MessageTemplateViewSet, basename='template')

urlpatterns = [
    path('', include(router.urls)),
]
