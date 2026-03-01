"""
Prometheus metrics endpoint view.
Exposes /metrics for Prometheus scraping.
"""
from django.http import HttpResponse
from django.views.decorators.http import require_GET
from django.views.decorators.csrf import csrf_exempt


@csrf_exempt
@require_GET
def prometheus_metrics(request):
    """
    Expose Prometheus metrics for scraping.
    Should be restricted to internal/monitoring network in production.
    """
    from utils.metrics import get_metrics
    content, content_type = get_metrics()
    return HttpResponse(content, content_type=content_type)
