# Geocoding Service

Enhanced geocoding service with multiple providers and caching.

## Features

- **Multi-provider fallback**: Nominatim (free) → Google Maps (requires API key)
- **Automatic caching**: 7-day cache for geocoding results
- **Address geocoding**: Convert addresses to coordinates
- **Reverse geocoding**: Convert coordinates to addresses
- **Country filtering**: Improve accuracy with country hints

## Providers

### 1. Nominatim (OpenStreetMap)
- **Cost**: Free
- **Setup**: No API key required
- **Limitations**: Lower accuracy for some addresses, rate limits apply
- **Best for**: General addresses, European/North American addresses

### 2. Google Maps API (Fallback)
- **Cost**: Pay-per-use (free tier available)
- **Setup**: Requires API key
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
```

## Caching

Results are automatically cached for 7 days to:
- Reduce API costs
- Improve response times
- Respect rate limits

Cache keys:
- Geocode: `geocode:{address}:{country}`
- Reverse: `reverse_geocode:{lat}:{lon}`

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
2. **Validate addresses on frontend** before geocoding
3. **Monitor Google Maps usage** to control costs
4. **Use batch operations** for bulk geocoding via Celery tasks

## Troubleshooting

### "Geocoding failed" warnings
- Normal when address doesn't exist in OSM database
- Will automatically try Google Maps if API key is set
- Consider adding manual coordinate input as fallback

### High API costs
- Review usage in Google Cloud Console
- Increase cache timeout if addresses are frequently repeated
- Implement frontend validation to reduce invalid requests

### Rate limiting (Nominatim)
- Nominatim has 1 req/sec limit
- Implement queue system for bulk operations
- Use Google Maps for high-volume production use
