from django.urls import path, include
from rest_framework.routers import DefaultRouter
from apps.messaging.views import MessageViewSet

app_name = 'messaging'

router = DefaultRouter()
router.register(r'messages', MessageViewSet, basename='message')

urlpatterns = [
    path('', include(router.urls)),
]
