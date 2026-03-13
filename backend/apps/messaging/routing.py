"""
WebSocket routing for messaging
"""
from django.urls import re_path
from apps.messaging import consumers, support_consumer

websocket_urlpatterns = [
    re_path(r'ws/chat/$', consumers.ChatConsumer.as_asgi()),
    re_path(r'ws/support/$', support_consumer.SupportConsumer.as_asgi()),
]
