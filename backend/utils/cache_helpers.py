"""
Redis Cache Utilities for StayAfrica

These utilities help you leverage Redis caching effectively throughout the application.

Examples:
    # Cache expensive database queries
    properties = cache_query_result('hot_properties', lambda: Property.objects.filter(is_featured=True), timeout=3600)
    
    # Cache API responses
    @cache_view_result(timeout=600)
    def property_list_view(request):
        ...
    
    # Rate limiting
    if is_rate_limited(f'booking_create:{user.id}', limit=10, period=3600):
        return Response({'error': 'Too many booking attempts'}, status=429)
"""

from functools import wraps
from django.core.cache import cache, caches
from django.conf import settings
import hashlib
import json
from typing import Any, Callable, Optional


def cache_query_result(cache_key: str, query_func: Callable, timeout: int = 300) -> Any:
    """
    Cache the result of an expensive database query.
    
    Args:
        cache_key: Unique identifier for the cached data
        query_func: Function that executes the query (called only if cache misses)
        timeout: Cache timeout in seconds (default 5 minutes)
    
    Returns:
        Query result (from cache or fresh query)
    
    Example:
        properties = cache_query_result(
            'featured_properties',
            lambda: list(Property.objects.filter(is_featured=True).values()),
            timeout=3600
        )
    """
    result = cache.get(cache_key)
    if result is None:
        result = query_func()
        cache.set(cache_key, result, timeout)
    return result


def invalidate_cache_pattern(pattern: str):
    """
    Delete all cache keys matching a pattern.
    
    Args:
        pattern: Cache key pattern (e.g., 'property:*')
    
    Example:
        # After updating a property, invalidate all property-related caches
        invalidate_cache_pattern('property:*')
    """
    try:
        from django_redis import get_redis_connection
        redis_conn = get_redis_connection('default')
        keys = redis_conn.keys(f'{settings.CACHES["default"]["KEY_PREFIX"]}:{pattern}')
        if keys:
            redis_conn.delete(*keys)
    except Exception as e:
        print(f'Cache invalidation failed: {e}')


def cache_view_result(timeout: int = 300, key_prefix: str = 'view'):
    """
    Decorator to cache view results based on request parameters.
    
    Args:
        timeout: Cache timeout in seconds
        key_prefix: Prefix for cache key
    
    Example:
        @cache_view_result(timeout=600, key_prefix='property_list')
        def property_list_view(request):
            properties = Property.objects.all()
            serializer = PropertySerializer(properties, many=True)
            return Response(serializer.data)
    """
    def decorator(func):
        @wraps(func)
        def wrapper(request, *args, **kwargs):
            # Generate cache key from request parameters
            cache_key_parts = [
                key_prefix,
                request.path,
                request.method,
                str(sorted(request.GET.items())),
            ]
            cache_key_str = ':'.join(cache_key_parts)
            cache_key = hashlib.md5(cache_key_str.encode()).hexdigest()
            
            # Try to get from cache
            result = cache.get(cache_key)
            if result is not None:
                return result
            
            # Execute view and cache result
            result = func(request, *args, **kwargs)
            cache.set(cache_key, result, timeout)
            return result
        return wrapper
    return decorator


def is_rate_limited(key: str, limit: int = 10, period: int = 60) -> bool:
    """
    Check if an action should be rate limited.
    
    Args:
        key: Unique identifier for the rate limit (e.g., 'login:user123')
        limit: Maximum number of actions allowed
        period: Time period in seconds
    
    Returns:
        True if rate limit exceeded, False otherwise
    
    Example:
        if is_rate_limited(f'booking:{user.id}', limit=5, period=3600):
            return Response({'error': 'Too many bookings'}, status=429)
        
        # Continue with booking creation
    """
    rl_cache = caches['ratelimit']
    current = rl_cache.get(key, 0)
    
    if current >= limit:
        return True
    
    rl_cache.set(key, current + 1, period)
    return False


def cache_user_data(user_id: int, data: dict, timeout: int = 3600):
    """
    Cache user-specific data (e.g., preferences, dashboard data).
    
    Args:
        user_id: User ID
        data: Data to cache
        timeout: Cache timeout in seconds (default 1 hour)
    
    Example:
        cache_user_data(user.id, {'bookings': bookings_data, 'stats': stats_data})
    """
    cache_key = f'user_data:{user_id}'
    cache.set(cache_key, data, timeout)


def get_user_data(user_id: int) -> Optional[dict]:
    """
    Retrieve cached user data.
    
    Args:
        user_id: User ID
    
    Returns:
        Cached user data or None
    
    Example:
        user_data = get_user_data(request.user.id)
        if user_data is None:
            # Fetch from database
            user_data = fetch_user_dashboard_data(request.user)
            cache_user_data(request.user.id, user_data)
    """
    cache_key = f'user_data:{user_id}'
    return cache.get(cache_key)


def invalidate_user_cache(user_id: int):
    """
    Invalidate all cache for a specific user.
    
    Args:
        user_id: User ID
    
    Example:
        # After user updates their profile
        invalidate_user_cache(user.id)
    """
    invalidate_cache_pattern(f'user_data:{user_id}*')


def cache_temporary_data(key: str, data: Any, timeout: int = 300):
    """
    Cache temporary data like OTPs, verification codes, etc.
    
    Args:
        key: Cache key
        data: Data to cache
        timeout: Expiration time in seconds (default 5 minutes)
    
    Example:
        # Store OTP for 5 minutes
        otp = generate_otp()
        cache_temporary_data(f'otp:{user.email}', otp, timeout=300)
        
        # Later, verify OTP
        stored_otp = cache.get(f'otp:{user.email}')
        if stored_otp == submitted_otp:
            # Valid OTP
    """
    cache.set(key, data, timeout)


def increment_counter(key: str, amount: int = 1) -> int:
    """
    Atomic counter increment using Redis.
    
    Args:
        key: Counter key
        amount: Increment amount (default 1)
    
    Returns:
        New counter value
    
    Example:
        # Track property views
        views = increment_counter(f'property_views:{property.id}')
    """
    try:
        from django_redis import get_redis_connection
        redis_conn = get_redis_connection('default')
        full_key = f'{settings.CACHES["default"]["KEY_PREFIX"]}:{key}'
        return redis_conn.incr(full_key, amount)
    except Exception:
        return 0


def get_or_set_cached(key: str, default_func: Callable, timeout: int = 300) -> Any:
    """
    Get cached value or set it using the default function.
    
    Args:
        key: Cache key
        default_func: Function to generate value if not cached
        timeout: Cache timeout in seconds
    
    Returns:
        Cached or newly generated value
    
    Example:
        stats = get_or_set_cached(
            'platform_stats',
            lambda: calculate_platform_statistics(),
            timeout=3600
        )
    """
    result = cache.get(key)
    if result is None:
        result = default_func()
        cache.set(key, result, timeout)
    return result
