import logging
import traceback

from django.http import JsonResponse
from rest_framework.response import Response
from rest_framework.views import exception_handler
from rest_framework import status

logger = logging.getLogger('apps')


def custom_exception_handler(exc, context):
    """
    Custom DRF exception handler that provides consistent error responses
    and logs unexpected errors for monitoring.
    """
    response = exception_handler(exc, context)

    if response is not None:
        response.data = {
            'success': False,
            'error': {
                'status_code': response.status_code,
                'detail': response.data,
            },
        }
    else:
        # Unhandled exception — log full traceback
        logger.error(
            'Unhandled exception in %s: %s',
            context.get('view', 'unknown'),
            traceback.format_exc(),
        )
        response_data = {
            'success': False,
            'error': {
                'status_code': 500,
                'detail': 'An unexpected error occurred. Please try again later.',
            },
        }
        response = Response(response_data, status=status.HTTP_500_INTERNAL_SERVER_ERROR)

    return response


class RequestLoggingMiddleware:
    """
    Middleware that logs request details and catches unhandled exceptions
    outside of DRF views (e.g. admin, static files, non-API views).
    """

    def __init__(self, get_response):
        self.get_response = get_response

    def __call__(self, request):
        response = self.get_response(request)

        if response.status_code >= 500:
            logger.error(
                'Server error: %s %s -> %s',
                request.method,
                request.get_full_path(),
                response.status_code,
            )
        elif response.status_code >= 400:
            logger.warning(
                'Client error: %s %s -> %s',
                request.method,
                request.get_full_path(),
                response.status_code,
            )

        return response

    def process_exception(self, request, exception):
        logger.error(
            'Unhandled exception on %s %s: %s',
            request.method,
            request.get_full_path(),
            traceback.format_exc(),
        )
        return JsonResponse(
            {
                'success': False,
                'error': {
                    'status_code': 500,
                    'detail': 'An unexpected error occurred.',
                },
            },
            status=500,
        )
