from django.urls import path, include
from rest_framework.routers import DefaultRouter
from apps.admin_dashboard.views import (
    AuditLogViewSet,
    AdminStatsViewSet,
    SystemConfigurationViewSet
)

app_name = 'admin'

router = DefaultRouter()
router.register(r'audit-logs', AuditLogViewSet, basename='audit-log')
router.register(r'stats', AdminStatsViewSet, basename='stats')
router.register(r'config', SystemConfigurationViewSet, basename='config')

urlpatterns = [
    path('', include(router.urls)),
]
