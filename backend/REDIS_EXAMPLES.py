"""
Example: Using Redis Cache in Property Views

This demonstrates practical Redis caching implementation for the StayAfrica property module.
"""

from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from django.core.cache import cache
from django.utils.decorators import method_decorator
from django.views.decorators.cache import cache_page

from apps.properties.models import Property
from apps.properties.serializers import PropertySerializer
from utils.cache_helpers import (
    cache_query_result,
    invalidate_cache_pattern,
    is_rate_limited,
    increment_counter,
    get_or_set_cached,
)


class PropertyViewSet(viewsets.ModelViewSet):
    """
    Property ViewSet with Redis caching examples.
    """
    queryset = Property.objects.all()
    serializer_class = PropertySerializer
    
    def list(self, request, *args, **kwargs):
        """
        List properties with caching.
        Cache key varies by query parameters (filters, search, etc.)
        """
        # Generate cache key based on request parameters
        query_params = sorted(request.GET.items())
        cache_key = f'property:list:{hash(str(query_params))}'
        
        # Try to get from cache
        cached_data = cache.get(cache_key)
        if cached_data:
            return Response(cached_data)
        
        # Cache miss - fetch from database
        queryset = self.filter_queryset(self.get_queryset())
        page = self.paginate_queryset(queryset)
        
        if page is not None:
            serializer = self.get_serializer(page, many=True)
            result = self.get_paginated_response(serializer.data)
            # Cache for 10 minutes
            cache.set(cache_key, result.data, timeout=600)
            return result
        
        serializer = self.get_serializer(queryset, many=True)
        # Cache for 10 minutes
        cache.set(cache_key, serializer.data, timeout=600)
        return Response(serializer.data)
    
    def retrieve(self, request, *args, **kwargs):
        """
        Retrieve single property with caching and view tracking.
        """
        property_id = kwargs.get('pk')
        cache_key = f'property:detail:{property_id}'
        
        # Try cache first
        cached_property = cache.get(cache_key)
        if cached_property:
            # Still increment view counter
            increment_counter(f'property:views:{property_id}')
            return Response(cached_property)
        
        # Cache miss
        instance = self.get_object()
        serializer = self.get_serializer(instance)
        
        # Cache for 1 hour
        cache.set(cache_key, serializer.data, timeout=3600)
        
        # Increment view counter
        views = increment_counter(f'property:views:{property_id}')
        
        # Update database every 100 views
        if views % 100 == 0:
            instance.view_count = views
            instance.save(update_fields=['view_count'])
        
        return Response(serializer.data)
    
    def create(self, request, *args, **kwargs):
        """
        Create property and invalidate list cache.
        """
        # Rate limit property creation
        if is_rate_limited(f'property:create:{request.user.id}', limit=5, period=3600):
            return Response(
                {'error': 'Too many properties created. Please try again later.'},
                status=status.HTTP_429_TOO_MANY_REQUESTS
            )
        
        response = super().create(request, *args, **kwargs)
        
        # Invalidate property list caches
        invalidate_cache_pattern('property:list:*')
        
        return response
    
    def update(self, request, *args, **kwargs):
        """
        Update property and invalidate related caches.
        """
        property_id = kwargs.get('pk')
        response = super().update(request, *args, **kwargs)
        
        # Invalidate specific property cache
        cache.delete(f'property:detail:{property_id}')
        
        # Invalidate list caches
        invalidate_cache_pattern('property:list:*')
        
        return response
    
    def destroy(self, request, *args, **kwargs):
        """
        Delete property and invalidate caches.
        """
        property_id = kwargs.get('pk')
        response = super().destroy(request, *args, **kwargs)
        
        # Invalidate caches
        cache.delete(f'property:detail:{property_id}')
        cache.delete(f'property:views:{property_id}')
        invalidate_cache_pattern('property:list:*')
        
        return response
    
    @action(detail=False, methods=['get'])
    def featured(self, request):
        """
        Get featured properties (heavily cached).
        """
        # Cache featured properties for 1 hour
        featured_properties = cache_query_result(
            'property:featured',
            lambda: list(Property.objects.filter(is_featured=True).values()),
            timeout=3600
        )
        
        serializer = self.get_serializer(featured_properties, many=True)
        return Response(serializer.data)
    
    @action(detail=False, methods=['get'])
    def statistics(self, request):
        """
        Get platform statistics (cached for 24 hours).
        """
        stats = get_or_set_cached(
            'property:statistics',
            lambda: {
                'total_properties': Property.objects.count(),
                'total_active': Property.objects.filter(status='active').count(),
                'total_featured': Property.objects.filter(is_featured=True).count(),
                'average_price': Property.objects.aggregate(avg_price=models.Avg('price_per_night'))['avg_price'],
            },
            timeout=86400  # 24 hours
        )
        
        return Response(stats)
    
    @action(detail=True, methods=['post'])
    def favorite(self, request, pk=None):
        """
        Add property to favorites with rate limiting.
        """
        # Rate limit favorites
        if is_rate_limited(f'favorite:{request.user.id}', limit=20, period=60):
            return Response(
                {'error': 'Too many requests. Slow down!'},
                status=status.HTTP_429_TOO_MANY_REQUESTS
            )
        
        property = self.get_object()
        request.user.favorite_properties.add(property)
        
        # Invalidate user's favorite cache
        cache.delete(f'user:favorites:{request.user.id}')
        
        return Response({'status': 'Property added to favorites'})


# Function-based view example
from rest_framework.decorators import api_view, permission_classes
from rest_framework.permissions import IsAuthenticated

@api_view(['GET'])
@permission_classes([IsAuthenticated])
def user_dashboard(request):
    """
    Get user dashboard data with caching.
    """
    user_id = request.user.id
    cache_key = f'user:dashboard:{user_id}'
    
    # Check cache first
    dashboard_data = cache.get(cache_key)
    
    if dashboard_data is None:
        # Fetch from database
        from apps.bookings.models import Booking
        from apps.properties.models import Property
        
        dashboard_data = {
            'user': {
                'id': request.user.id,
                'email': request.user.email,
                'full_name': request.user.get_full_name(),
            },
            'bookings': {
                'active': Booking.objects.filter(user=request.user, status='confirmed').count(),
                'pending': Booking.objects.filter(user=request.user, status='pending').count(),
                'completed': Booking.objects.filter(user=request.user, status='completed').count(),
            },
            'properties': {
                'owned': Property.objects.filter(host=request.user).count(),
                'favorites': request.user.favorite_properties.count(),
            },
        }
        
        # Cache for 15 minutes
        cache.set(cache_key, dashboard_data, timeout=900)
    
    return Response(dashboard_data)


@api_view(['GET'])
def property_search(request):
    """
    Search properties with query caching.
    """
    query = request.GET.get('q', '')
    location = request.GET.get('location', '')
    
    # Generate cache key from search parameters
    cache_key = f'search:{hash(f"{query}:{location}")}'
    
    # Check cache
    results = cache.get(cache_key)
    
    if results is None:
        # Perform search
        results = Property.objects.filter(
            title__icontains=query,
            location__icontains=location,
            status='active'
        ).values('id', 'title', 'location', 'price_per_night')[:20]
        
        results = list(results)
        
        # Cache for 30 minutes
        cache.set(cache_key, results, timeout=1800)
    
    return Response(results)
