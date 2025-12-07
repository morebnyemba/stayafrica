"""
Health check endpoints for monitoring and observability
"""
from rest_framework.decorators import api_view, permission_classes
from rest_framework.permissions import AllowAny
from rest_framework.response import Response
from rest_framework import status
from django.db import connection
from django.core.cache import cache
import logging

logger = logging.getLogger(__name__)


@api_view(['GET'])
@permission_classes([AllowAny])
def health_check(request):
    """Simple health check endpoint"""
    return Response({
        'status': 'healthy',
        'service': 'StayAfrica API',
        'version': '1.0.0'
    })


@api_view(['GET'])
@permission_classes([AllowAny])
def health_check_detailed(request):
    """Detailed health check with database and cache connectivity"""
    health_status = {
        'status': 'healthy',
        'service': 'StayAfrica API',
        'version': '1.0.0',
        'checks': {}
    }
    
    is_healthy = True
    
    # Check database
    try:
        with connection.cursor() as cursor:
            cursor.execute("SELECT 1")
            cursor.fetchone()
        health_status['checks']['database'] = {'status': 'healthy'}
    except Exception as e:
        logger.error(f"Database health check failed: {str(e)}")
        health_status['checks']['database'] = {'status': 'unhealthy'}
        is_healthy = False
    
    # Check cache
    try:
        cache_key = 'health_check_test'
        cache.set(cache_key, 'test', 10)
        if cache.get(cache_key) == 'test':
            health_status['checks']['cache'] = {'status': 'healthy'}
        else:
            raise Exception("Cache mismatch")
    except Exception as e:
        logger.error(f"Cache health check failed: {str(e)}")
        health_status['checks']['cache'] = {'status': 'unhealthy'}
        is_healthy = False
    
    health_status['status'] = 'healthy' if is_healthy else 'unhealthy'
    status_code = status.HTTP_200_OK if is_healthy else status.HTTP_503_SERVICE_UNAVAILABLE
    return Response(health_status, status=status_code)


@api_view(['GET'])
@permission_classes([AllowAny])
def readiness_check(request):
    """Kubernetes readiness probe"""
    try:
        with connection.cursor() as cursor:
            cursor.execute("SELECT 1")
            cursor.fetchone()
        return Response({'status': 'ready'}, status=status.HTTP_200_OK)
    except Exception as e:
        logger.error(f"Readiness check failed: {str(e)}")
        return Response({'status': 'not_ready'}, status=status.HTTP_503_SERVICE_UNAVAILABLE)


@api_view(['GET'])
@permission_classes([AllowAny])
def liveness_check(request):
    """Kubernetes liveness probe"""
    return Response({'status': 'alive'}, status=status.HTTP_200_OK)
