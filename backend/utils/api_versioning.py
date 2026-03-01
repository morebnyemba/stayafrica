"""
API Versioning Middleware
Handles API version detection and deprecation warnings.
"""
import logging
from django.utils.deprecation import MiddlewareMixin

logger = logging.getLogger(__name__)

CURRENT_API_VERSION = 'v1'
SUPPORTED_VERSIONS = ['v1']
DEPRECATED_VERSIONS = []


class APIVersionMiddleware(MiddlewareMixin):
    """
    Middleware that adds API version headers and deprecation warnings.
    Supports future v2 migration by detecting version from URL path.
    """

    def process_request(self, request):
        # Extract version from URL
        path = request.path_info
        if '/api/' in path:
            parts = path.split('/api/')
            if len(parts) > 1:
                version_part = parts[1].split('/')[0]
                if version_part in SUPPORTED_VERSIONS or version_part in DEPRECATED_VERSIONS:
                    request.api_version = version_part
                else:
                    request.api_version = CURRENT_API_VERSION
            else:
                request.api_version = CURRENT_API_VERSION
        else:
            request.api_version = None

    def process_response(self, request, response):
        api_version = getattr(request, 'api_version', None)

        if api_version:
            response['X-API-Version'] = api_version
            response['X-API-Current-Version'] = CURRENT_API_VERSION

            if api_version in DEPRECATED_VERSIONS:
                response['X-API-Deprecated'] = 'true'
                response['Sunset'] = '2025-12-31T23:59:59Z'
                response['X-API-Deprecation-Notice'] = (
                    f'API {api_version} is deprecated. '
                    f'Please migrate to {CURRENT_API_VERSION}. '
                    f'See https://docs.stayafrica.app/api/migration'
                )
                logger.warning(
                    f'Deprecated API {api_version} accessed: {request.path}',
                    extra={'api_version': api_version, 'path': request.path}
                )

        return response
