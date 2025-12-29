"""
Celery tasks for async geocoding operations
Use these for bulk operations or non-blocking geocoding
"""
from celery import shared_task
from services.geocoding_service import GeocodingService
from typing import List, Dict, Optional
import logging

logger = logging.getLogger(__name__)


@shared_task(bind=True, max_retries=3)
def geocode_address_async(self, address: str, country: Optional[str] = None) -> Optional[Dict]:
    """
    Asynchronous geocoding task
    
    Args:
        address: Address to geocode
        country: Optional country code
    
    Returns:
        Geocoding result dict or None
    
    Example:
        from tasks.geocoding_tasks import geocode_address_async
        result = geocode_address_async.delay("Harare, Zimbabwe", "ZW")
        geocoding_data = result.get()  # Wait for result
    """
    try:
        result = GeocodingService.geocode_address(address, country)
        logger.info(f"Async geocoding completed for: {address}")
        return result
    except Exception as e:
        logger.error(f"Async geocoding failed for '{address}': {str(e)}")
        # Retry with exponential backoff
        raise self.retry(exc=e, countdown=60 * (2 ** self.request.retries))


@shared_task(bind=True)
def bulk_geocode_async(self, addresses: List[Dict[str, str]]) -> List[Optional[Dict]]:
    """
    Asynchronous bulk geocoding task
    Best for large batches that would block the request/response cycle
    
    Args:
        addresses: List of address dicts with 'address' and optional 'country'
    
    Returns:
        List of geocoding results
    
    Example:
        from tasks.geocoding_tasks import bulk_geocode_async
        addresses = [
            {'address': 'Harare, Zimbabwe', 'country': 'ZW'},
            {'address': 'Cape Town, South Africa', 'country': 'ZA'},
        ]
        task = bulk_geocode_async.delay(addresses)
        results = task.get()  # Wait for completion
    """
    try:
        logger.info(f"Starting bulk geocoding for {len(addresses)} addresses")
        results = GeocodingService.bulk_geocode(addresses)
        successful = sum(1 for r in results if r)
        logger.info(f"Bulk geocoding completed: {successful}/{len(addresses)} successful")
        return results
    except Exception as e:
        logger.error(f"Bulk geocoding task failed: {str(e)}")
        return [None] * len(addresses)


@shared_task(bind=True)
def reverse_geocode_async(self, latitude: float, longitude: float) -> Optional[Dict]:
    """
    Asynchronous reverse geocoding task
    
    Args:
        latitude: Latitude coordinate
        longitude: Longitude coordinate
    
    Returns:
        Reverse geocoding result or None
    """
    try:
        result = GeocodingService.reverse_geocode(latitude, longitude)
        logger.info(f"Async reverse geocoding completed for: {latitude}, {longitude}")
        return result
    except Exception as e:
        logger.error(f"Async reverse geocoding failed: {str(e)}")
        raise self.retry(exc=e, countdown=30)


@shared_task
def refresh_property_geocodes():
    """
    Periodic task to refresh geocodes for properties with missing or outdated location data
    Should be scheduled in Celery beat configuration
    
    Example celerybeat schedule:
        'refresh-property-geocodes': {
            'task': 'tasks.geocoding_tasks.refresh_property_geocodes',
            'schedule': crontab(hour=2, minute=0),  # Run daily at 2 AM
        }
    """
    from apps.properties.models import Property
    
    try:
        # Find properties with missing city or suburb
        properties_to_update = Property.objects.filter(
            city__isnull=True
        ) | Property.objects.filter(
            city=''
        )
        
        count = properties_to_update.count()
        logger.info(f"Found {count} properties needing geocode refresh")
        
        updated = 0
        for prop in properties_to_update[:100]:  # Limit to 100 per run
            try:
                if prop.location:
                    lat = prop.location.y
                    lon = prop.location.x
                    result = GeocodingService.reverse_geocode(lat, lon)
                    
                    if result:
                        prop.city = result.get('city', prop.city)
                        prop.suburb = result.get('suburb', prop.suburb)
                        prop.country = result.get('country', prop.country)
                        prop.save(update_fields=['city', 'suburb', 'country'])
                        updated += 1
                        
            except Exception as e:
                logger.error(f"Failed to refresh geocode for property {prop.id}: {str(e)}")
                continue
        
        logger.info(f"Geocode refresh completed: {updated}/{count} properties updated")
        return {'total': count, 'updated': updated}
        
    except Exception as e:
        logger.error(f"Geocode refresh task failed: {str(e)}")
        return {'error': str(e)}
