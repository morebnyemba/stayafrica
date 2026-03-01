from django.urls import path
from utils.metrics_view import prometheus_metrics

urlpatterns = [
    path('', prometheus_metrics, name='prometheus-metrics'),
]
