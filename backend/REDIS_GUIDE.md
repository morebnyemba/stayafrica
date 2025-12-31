# Redis Usage Guide for StayAfrica

## What Redis is Now Used For

StayAfrica now leverages Redis for multiple performance-critical features:

### 1. **Django Caching** (DB 1)
- Query result caching
- Expensive computations
- API response caching
- General application cache

### 2. **Session Storage** (DB 2)
- User sessions stored in Redis (faster than database)
- Reduced database load
- Better scalability

### 3. **Rate Limiting** (DB 3)
- API rate limiting per user/IP
- Prevents abuse
- Protects resources

### 4. **Celery** (DB 0)
- Message broker for async tasks
- Task result storage

## Configuration Changes Made

### settings.py Updates
```python
# Three Redis databases for different purposes
CACHES = {
    'default': {...},      # DB 1: General caching
    'session': {...},      # DB 2: Session storage  
    'ratelimit': {...},    # DB 3: Rate limiting
}

# Sessions now stored in Redis
SESSION_ENGINE = 'django.contrib.sessions.backends.cache'
SESSION_CACHE_ALIAS = 'session'

# API rate limiting enabled
REST_FRAMEWORK = {
    'DEFAULT_THROTTLE_CLASSES': [...],
    'DEFAULT_THROTTLE_RATES': {
        'anon': '100/hour',
        'user': '1000/hour',
    },
}
```

## How to Use Redis Caching

### 1. Cache Expensive Database Queries
```python
from utils.cache_helpers import cache_query_result

# Cache featured properties for 1 hour
featured_properties = cache_query_result(
    'featured_properties',
    lambda: list(Property.objects.filter(is_featured=True).values()),
    timeout=3600
)
```

### 2. Cache API View Results
```python
from utils.cache_helpers import cache_view_result
from rest_framework.decorators import api_view

@api_view(['GET'])
@cache_view_result(timeout=600, key_prefix='property_list')
def property_list(request):
    properties = Property.objects.all()
    serializer = PropertySerializer(properties, many=True)
    return Response(serializer.data)
```

### 3. Implement Custom Rate Limiting
```python
from utils.cache_helpers import is_rate_limited

def create_booking(request):
    # Limit to 5 bookings per hour per user
    if is_rate_limited(f'booking:{request.user.id}', limit=5, period=3600):
        return Response({'error': 'Too many booking attempts'}, status=429)
    
    # Continue with booking creation
    ...
```

### 4. Cache User Dashboard Data
```python
from utils.cache_helpers import cache_user_data, get_user_data

def get_user_dashboard(request):
    user_data = get_user_data(request.user.id)
    
    if user_data is None:
        # Fetch from database
        user_data = {
            'bookings': fetch_user_bookings(request.user),
            'stats': calculate_user_stats(request.user),
            'recommendations': get_recommendations(request.user),
        }
        cache_user_data(request.user.id, user_data, timeout=3600)
    
    return Response(user_data)
```

### 5. Store Temporary Data (OTPs, Tokens)
```python
from utils.cache_helpers import cache_temporary_data

# Generate and store OTP for 5 minutes
otp = generate_otp()
cache_temporary_data(f'otp:{user.email}', otp, timeout=300)

# Send OTP to user
send_email(user.email, f'Your OTP is: {otp}')

# Later, verify OTP
from django.core.cache import cache
stored_otp = cache.get(f'otp:{user.email}')
if stored_otp == submitted_otp:
    # Valid OTP
    cache.delete(f'otp:{user.email}')  # Remove after use
```

### 6. Track Counters (Views, Likes, etc.)
```python
from utils.cache_helpers import increment_counter

def property_detail(request, property_id):
    # Increment view counter
    views = increment_counter(f'property_views:{property_id}')
    
    # Fetch property
    property = get_object_or_404(Property, id=property_id)
    
    # Update database periodically (every 100 views)
    if views % 100 == 0:
        property.view_count = views
        property.save(update_fields=['view_count'])
    
    return Response(PropertySerializer(property).data)
```

### 7. Invalidate Cache After Updates
```python
from utils.cache_helpers import invalidate_cache_pattern, invalidate_user_cache

def update_property(request, property_id):
    property = get_object_or_404(Property, id=property_id)
    serializer = PropertySerializer(property, data=request.data)
    
    if serializer.is_valid():
        serializer.save()
        
        # Invalidate all property-related caches
        invalidate_cache_pattern('property:*')
        invalidate_cache_pattern(f'property_detail:{property_id}')
        
        return Response(serializer.data)
    
    return Response(serializer.errors, status=400)
```

## Best Practices

### 1. Cache Key Naming Convention
```python
# Use descriptive, hierarchical keys
'property:list:featured'
'property:detail:123'
'user:dashboard:456'
'stats:platform:daily'
```

### 2. Set Appropriate Timeouts
```python
# Frequently changing data: 5-10 minutes
cache.set('property_list', data, timeout=300)

# Stable data: 1 hour - 1 day
cache.set('property_categories', data, timeout=3600)

# Temporary data: seconds to minutes
cache.set(f'otp:{email}', otp, timeout=300)
```

### 3. Handle Cache Misses Gracefully
```python
from django.core.cache import cache

def get_property_stats(property_id):
    stats = cache.get(f'property:stats:{property_id}')
    
    if stats is None:
        # Cache miss - fetch from database
        stats = calculate_property_stats(property_id)
        cache.set(f'property:stats:{property_id}', stats, timeout=3600)
    
    return stats
```

### 4. Use Cache for Hot Data Only
Cache data that is:
- Expensive to compute
- Accessed frequently
- Doesn't change often
- Can tolerate slight staleness

Don't cache:
- User-specific sensitive data
- Real-time critical data
- Data that changes constantly

## Monitoring Redis Usage

### Check Redis Connection
```bash
docker exec -it stayafrica_redis redis-cli ping
# Expected output: PONG
```

### View Cached Keys
```bash
docker exec -it stayafrica_redis redis-cli
> SELECT 1  # Default cache DB
> KEYS stayafrica:*
> GET stayafrica:some_key
```

### Monitor Memory Usage
```bash
docker exec -it stayafrica_redis redis-cli INFO memory
```

### View Cache Statistics
```bash
docker exec -it stayafrica_redis redis-cli INFO stats
```

## Common Redis Commands

```bash
# Clear all cache (DB 1)
docker exec -it stayafrica_redis redis-cli -n 1 FLUSHDB

# Clear session cache (DB 2)
docker exec -it stayafrica_redis redis-cli -n 2 FLUSHDB

# Clear all Redis databases
docker exec -it stayafrica_redis redis-cli FLUSHALL

# View specific key
docker exec -it stayafrica_redis redis-cli -n 1 GET "stayafrica:property:list"

# Delete specific key
docker exec -it stayafrica_redis redis-cli -n 1 DEL "stayafrica:property:list"

# View TTL (time to live) for a key
docker exec -it stayafrica_redis redis-cli -n 1 TTL "stayafrica:property:list"
```

## Required Package

Add to `requirements.txt`:
```
django-redis==5.4.0
hiredis==2.3.2  # Optional: faster parser
```

Install:
```bash
pip install django-redis hiredis
```

## Performance Benefits

With Redis caching enabled, you'll see:
- **50-90% reduction** in database queries for cached data
- **Faster API responses** (ms instead of seconds for complex queries)
- **Better scalability** (handle more concurrent users)
- **Reduced database load** (sessions in Redis, not DB)
- **Protection against abuse** (rate limiting)

## Next Steps

1. Install `django-redis`: `pip install django-redis hiredis`
2. Update `requirements.txt`
3. Test caching with management command
4. Monitor Redis memory usage
5. Implement caching in views and queries
6. Set up cache invalidation after updates
