from django.urls import path, include
from rest_framework.routers import DefaultRouter
from .views import SupportTicketViewSet, CannedResponseViewSet, BugReportViewSet

app_name = 'support'

router = DefaultRouter()
router.register(r'tickets', SupportTicketViewSet, basename='ticket')
router.register(r'canned-responses', CannedResponseViewSet, basename='canned-response')
router.register(r'bugs', BugReportViewSet, basename='bug')

urlpatterns = [
    path('', include(router.urls)),
]
