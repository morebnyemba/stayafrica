# Geocoding Service

Enhanced geocoding service with multiple providers, caching, rate limiting, and async support.

## Features

- **Multi-provider fallback**: Nominatim (free) → Google Maps (requires API key)
- **Automatic caching**: 7-day cache for geocoding results
- **Rate limiting**: Respects Nominatim 1 req/sec policy
- **Async support**: Celery tasks for bulk operations
- **Address geocoding**: Convert addresses to coordinates
- **Reverse geocoding**: Convert coordinates to addresses
- **Bulk geocoding**: Process multiple addresses efficiently
- **Country filtering**: Improve accuracy with country hints

## Providers

### 1. Nominatim (OpenStreetMap)
- **Cost**: Free
- **Setup**: No API key required
- **Rate limit**: 1 request per second (automatically enforced)
- **Limitations**: Lower accuracy for some addresses
- **Best for**: General addresses, European/North American addresses

### 2. Google Maps API (Fallback)
- **Cost**: Pay-per-use (free tier available)
- **Setup**: Requires API key
- **Rate limit**: No strict limit (depends on billing)
- **Benefits**: Higher accuracy, better coverage for African addresses
- **Best for**: Production use, critical geocoding

## Setup

### Google Maps API (Optional but Recommended)

1. Go to [Google Cloud Console](https://console.cloud.google.com/apis/credentials)
2. Create a new project or select existing one
3. Enable **Geocoding API**
4. Create API credentials (API key)
5. Restrict the API key:
   - Application restrictions: Set to your server IP
   - API restrictions: Select "Geocoding API"
6. Add to `.env.prod`:
   ```bash
   GOOGLE_MAPS_API_KEY=your_api_key_here
   ```

### Pricing (Google Maps)
- First $200/month: Free
- After: $5 per 1000 requests
- 1000 free requests = ~33/day with monthly credit
- [Pricing details](https://developers.google.com/maps/documentation/geocoding/usage-and-billing)

## Usage

### Synchronous (Request/Response)

The service is automatically used by property creation endpoints:

```python
from services.geocoding_service import GeocodingService

# Geocode an address
result = GeocodingService.geocode_address(
    address="2789 Dada Crescent, Budiriro 2, Harare",
    country="Zimbabwe"
)
# Returns: {'latitude': -17.xx, 'longitude': 30.xx, 'city': 'Harare', ...}

# Reverse geocode coordinates
result = GeocodingService.reverse_geocode(
    latitude=-17.8252,
    longitude=31.0335
)
# Returns: {'address': 'Main Street', 'city': 'Harare', ...}

# Bulk geocoding (with rate limiting)
addresses = [
    {'address': 'Harare, Zimbabwe', 'country': 'ZW'},
    {'address': 'Cape Town, South Africa', 'country': 'ZA'},
]
results = GeocodingService.bulk_geocode(addresses)
```

### Asynchronous (Celery Tasks)

For bulk operations or non-blocking geocoding:

```python
from tasks.geocoding_tasks import geocode_address_async, bulk_geocode_async

# Async single geocoding
task = geocode_address_async.delay("Harare, Zimbabwe", "ZW")
result = task.get()  # Wait for result

# Async bulk geocoding (recommended for >10 addresses)
addresses = [
    {'address': 'Harare, Zimbabwe', 'country': 'ZW'},
    {'address': 'Cape Town, South Africa', 'country': 'ZA'},
]
task = bulk_geocode_async.delay(addresses)
results = task.get()  # Returns list of geocoding results
```

### Scheduled Tasks

Refresh property geocodes automatically:

```python
# In stayafrica/celery.py, add to beat_schedule:
app.conf.beat_schedule = {
    'refresh-property-geocodes': {
        'task': 'tasks.geocoding_tasks.refresh_property_geocodes',
        'schedule': crontab(hour=2, minute=0),  # Daily at 2 AM
    },
}
```

## Caching

Results are automatically cached for 7 days to:
- Reduce API costs
- Improve response times
- Respect rate limits

Cache keys:
- Geocode: `geocode:{address}:{country}`
- Reverse: `reverse_geocode:{lat}:{lon}`

## Rate Limiting

- **Nominatim**: Automatically enforced at 1 request/second
- **Google Maps**: No hard limit (depends on billing tier)
- Thread-safe implementation with locks

## Monitoring

Check logs for provider usage:
```bash
# View geocoding logs
docker compose -f docker-compose.prod.yml logs backend | grep geocoding

# See which provider was used
# "provider": "nominatim" or "provider": "google"
```

## Best Practices

1. **Always provide country parameter** for better accuracy
2. **Use async tasks for bulk operations** (>10 addresses)
3. **Validate addresses on frontend** before geocoding
4. **Monitor Google Maps usage** to control costs
5. **Cache results aggressively** for repeated addresses
6. **Use bulk_geocode()** instead of loops for multiple addresses

## Troubleshooting

### "Geocoding failed" warnings
- Normal when address doesn't exist in OSM database
- Will automatically try Google Maps if API key is set
- Consider adding manual coordinate input as fallback

### High API costs
- Review usage in Google Cloud Console
- Increase cache timeout if addresses are frequently repeated
- Implement frontend validation to reduce invalid requests
- Use async bulk tasks instead of individual requests

### Rate limit errors
- Nominatim rate limiting is automatic (1 req/sec)
- For high volume, use Celery tasks which respect limits
- Consider upgrading to Google Maps for faster throughput

### Rate limiting (Nominatim)
- Nominatim has 1 req/sec limit
- Implement queue system for bulk operations
- Use Google Maps for high-volume production use
