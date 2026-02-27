"""
Custom decorators for StayAfrica backend
"""
from functools import wraps
from rest_framework.response import Response
from rest_framework import status
from django_ratelimit.decorators import ratelimit
from django.core.cache import cache
import logging

logger = logging.getLogger(__name__)


def require_host(view_func):
    """Decorator to ensure user is a host"""
    @wraps(view_func)
    def wrapper(request, *args, **kwargs):
        if not request.user.is_authenticated:
            return Response(
                {'error': 'Authentication required'},
                status=status.HTTP_401_UNAUTHORIZED
            )
        if not request.user.is_host and not request.user.is_admin_user:
            return Response(
                {'error': 'Host privileges required'},
                status=status.HTTP_403_FORBIDDEN
            )
        return view_func(request, *args, **kwargs)
    return wrapper


def require_verified(view_func):
    """Decorator to ensure user is verified"""
    @wraps(view_func)
    def wrapper(request, *args, **kwargs):
        if not request.user.is_authenticated:
            return Response(
                {'error': 'Authentication required'},
                status=status.HTTP_401_UNAUTHORIZED
            )
        if not request.user.is_verified:
            return Response(
                {'error': 'Account verification required'},
                status=status.HTTP_403_FORBIDDEN
            )
        return view_func(request, *args, **kwargs)
    return wrapper


def log_action(action_name):
    """Decorator to log actions"""
    def decorator(view_func):
        @wraps(view_func)
        def wrapper(*args, **kwargs):
            # Handle both ViewSet methods (self, request, ...) and function-based views (request, ...)
            if len(args) > 0:
                if hasattr(args[0], 'user'):
                    # Function-based view: first arg is request
                    request = args[0]
                    user = request.user
                elif len(args) > 1 and hasattr(args[1], 'user'):
                    # ViewSet method: second arg is request
                    request = args[1]
                    user = request.user
                else:
                    user = 'Unknown'
            else:
                user = 'Unknown'
            
            logger.info(f"Action: {action_name}, User: {user}")
            try:
                response = view_func(*args, **kwargs)
                logger.info(f"Action {action_name} completed successfully")
                return response
            except Exception as e:
                logger.error(f"Action {action_name} failed: {str(e)}")
                raise
        return wrapper
    return decorator


def cache_response(timeout=300):
    """Decorator to cache API responses"""
    def decorator(view_func):
        @wraps(view_func)
        def wrapper(request, *args, **kwargs):
            # Create cache key from function name and args
            cache_key = f"{view_func.__name__}:{args}:{kwargs}"
            
            # Try to get from cache
            cached_response = cache.get(cache_key)
            if cached_response is not None:
                return cached_response
            
            # Get fresh response
            response = view_func(request, *args, **kwargs)
            
            # Cache the response
            cache.set(cache_key, response, timeout)
            
            return response
        return wrapper
    return decorator


def api_ratelimit(key='ip', rate='100/h', method='ALL'):
    """
    Rate limiting decorator for API endpoints.
    Works with both function-based views and ViewSet methods.
    Usage: @api_ratelimit(rate='10/m')
    """
    django_decorator = ratelimit(key=key, rate=rate, method=method)

    def decorator(view_func):
        @wraps(view_func)
        def wrapper(*args, **kwargs):
            # ViewSet method: args = (self, request, ...)
            # Function-based view: args = (request, ...)
            if args and not hasattr(args[0], 'method') and len(args) > 1:
                # First arg is `self` (ViewSet), second is `request`
                self_arg = args[0]
                request = args[1]
                # Apply django_ratelimit to a thin function whose 1st arg is request
                @django_decorator
                def _inner(request, *a, **kw):
                    return view_func(self_arg, request, *a, **kw)
                return _inner(request, *args[2:], **kwargs)
            else:
                # Function-based view — apply directly
                decorated = django_decorator(view_func)
                return decorated(*args, **kwargs)
        return wrapper
    return decorator
