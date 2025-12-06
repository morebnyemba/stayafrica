"""Health check URL patterns"""
from django.urls import path
from apps.health import views

urlpatterns = [
    path('', views.health_check, name='health_check'),
    path('detailed/', views.health_check_detailed, name='health_check_detailed'),
    path('ready/', views.readiness_check, name='readiness_check'),
    path('live/', views.liveness_check, name='liveness_check'),
]
