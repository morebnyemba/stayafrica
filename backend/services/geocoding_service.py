"""
Geocoding Service for StayAfrica
Provides accurate location services using GDAL/PostGIS with async support,
caching, and rate limiting.
"""
from django.contrib.gis.geos import Point
from django.contrib.gis.measure import D
from django.core.cache import cache
from typing import Dict, Tuple, Optional, List
import logging
import requests
import os
import time
from functools import wraps
from threading import Lock

logger = logging.getLogger(__name__)


def rate_limit(max_calls: int = 1, period: float = 1.0):
    """
    Rate limiting decorator to respect Nominatim's usage policy (1 req/sec)
    
    Args:
        max_calls: Maximum number of calls allowed
        period: Time period in seconds
    """
    min_interval = period / max_calls
    last_called = [0.0]
    lock = Lock()
    
    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            with lock:
                elapsed = time.time() - last_called[0]
                left_to_wait = min_interval - elapsed
                if left_to_wait > 0:
                    time.sleep(left_to_wait)
                ret = func(*args, **kwargs)
                last_called[0] = time.time()
                return ret
        return wrapper
    return decorator


class GeocodingService:
    """
    Service for geocoding and reverse geocoding using multiple providers
    Primary: Nominatim (OpenStreetMap) - free, no API key, rate limited
    Fallback: Google Maps API - requires API key, more accurate
    
    Features:
    - Automatic caching (7 days TTL)
    - Rate limiting for Nominatim (1 req/sec)
    - Multi-provider fallback
    - Bulk geocoding support
    """
    
    NOMINATIM_BASE_URL = "https://nominatim.openstreetmap.org"
    GOOGLE_MAPS_BASE_URL = "https://maps.googleapis.com/maps/api"
    USER_AGENT = "StayAfrica/1.0"
    GOOGLE_API_KEY = os.getenv('GOOGLE_MAPS_API_KEY', '')
    CACHE_TIMEOUT = 60 * 60 * 24 * 7  # 7 days
    
    # Rate limiting: Nominatim allows 1 request per second
    _nominatim_rate_limit = rate_limit(max_calls=1, period=1.0)
    
    @classmethod
    def geocode_address(cls, address: str, country: Optional[str] = None) -> Optional[Dict]:
        """
        Convert an address to coordinates (lat, lon)
        Uses Nominatim first, falls back to Google Maps if available
        
        Args:
            address: Street address or location description
            country: Optional country filter for better accuracy
            
        Returns:
            Dict with 'latitude', 'longitude', 'display_name', 'city', 'country'
            or None if not found
        """
        # Check cache first
        cache_key = f"geocode:{address}:{country or 'none'}"
        cached_result = cache.get(cache_key)
        if cached_result:
            logger.debug(f"Cache hit for address: {address}")
            return cached_result
        
        # Try Nominatim first (free, no API key required)
        result = cls._geocode_with_nominatim(address, country)
        
        # Fallback to Google Maps if Nominatim fails and API key is available
        if not result and cls.GOOGLE_API_KEY:
            logger.info(f"Nominatim failed, trying Google Maps for: {address}")
            result = cls._geocode_with_google(address, country)
        
        # Cache successful results
        if result:
            cache.set(cache_key, result, cls.CACHE_TIMEOUT)
        
        return result
    
    @classmethod
    @rate_limit(max_calls=1, period=1.0)
    def _geocode_with_nominatim(cls, address: str, country: Optional[str] = None) -> Optional[Dict]:
        """Geocode using Nominatim (OpenStreetMap) with rate limiting"""
        try:
            params = {
                'q': address,
                'format': 'json',
                'limit': 1,
                'addressdetails': 1
            }
            
            if country:
                # Map country name to ISO code if needed
                country_code_map = {
                    'Zimbabwe': 'ZW',
                    'South Africa': 'ZA',
                    'Botswana': 'BW',
                    'Namibia': 'NA',
                    'Zambia': 'ZM',
                }
                country_code = country_code_map.get(country, country)
                params['countrycodes'] = country_code.upper()[:2]  # ISO country code
            
            response = requests.get(
                f"{cls.NOMINATIM_BASE_URL}/search",
                params=params,
                headers={'User-Agent': cls.USER_AGENT},
                timeout=5
            )
            
            if response.status_code == 200:
                data = response.json()
                if data and len(data) > 0:
                    result = data[0]
                    address_details = result.get('address', {})
                    
                    return {
                        'latitude': float(result['lat']),
                        'longitude': float(result['lon']),
                        'display_name': result.get('display_name', ''),
                        'city': address_details.get('city') or address_details.get('town') or address_details.get('village', ''),
                        'suburb': address_details.get('suburb', ''),
                        'country': address_details.get('country', ''),
                        'country_code': address_details.get('country_code', '').upper(),
                        'postcode': address_details.get('postcode', ''),
                        'provider': 'nominatim'
                    }
            
            logger.warning(f"Geocoding failed for address: {address}")
            return None
            
        except Exception as e:
            logger.error(f"Nominatim geocoding error for '{address}': {str(e)}")
            return None
    
    @classmethod
    def _geocode_with_google(cls, address: str, country: Optional[str] = None) -> Optional[Dict]:
        """Geocode using Google Maps Geocoding API"""
        try:
            params = {
                'address': address,
                'key': cls.GOOGLE_API_KEY
            }
            
            if country:
                params['components'] = f'country:{country}'
            
            response = requests.get(
                f"{cls.GOOGLE_MAPS_BASE_URL}/geocode/json",
                params=params,
                timeout=5
            )
            
            if response.status_code == 200:
                data = response.json()
                if data.get('status') == 'OK' and data.get('results'):
                    result = data['results'][0]
                    location = result['geometry']['location']
                    
                    # Extract address components
                    address_components = result.get('address_components', [])
                    city = suburb = country_name = country_code = postcode = ''
                    
                    for component in address_components:
                        types = component.get('types', [])
                        if 'locality' in types:
                            city = component.get('long_name', '')
                        elif 'sublocality' in types or 'neighborhood' in types:
                            suburb = component.get('long_name', '')
                        elif 'country' in types:
                            country_name = component.get('long_name', '')
                            country_code = component.get('short_name', '')
                        elif 'postal_code' in types:
                            postcode = component.get('long_name', '')
                    
                    return {
                        'latitude': location['lat'],
                        'longitude': location['lng'],
                        'display_name': result.get('formatted_address', ''),
                        'city': city,
                        'suburb': suburb,
                        'country': country_name,
                        'country_code': country_code,
                        'postcode': postcode,
                        'provider': 'google'
                    }
            
            logger.warning(f"Google Maps geocoding failed for address: {address}")
            return None
            
        except Exception as e:
            logger.error(f"Google Maps geocoding error for '{address}': {str(e)}")
            return None
    
    @classmethod
    def reverse_geocode(cls, latitude: float, longitude: float) -> Optional[Dict]:
        """
        Convert coordinates to an address
        Uses Nominatim first, falls back to Google Maps if available
        
        Args:
            latitude: Latitude coordinate
            longitude: Longitude coordinate
            
        Returns:
            Dict with address components or None if not found
        """
        # Check cache first
        cache_key = f"reverse_geocode:{latitude:.6f}:{longitude:.6f}"
        cached_result = cache.get(cache_key)
        if cached_result:
            logger.debug(f"Cache hit for coordinates: {latitude}, {longitude}")
            return cached_result
        
        # Try Nominatim first
        result = cls._reverse_geocode_nominatim(latitude, longitude)
        
        # Fallback to Google Maps if available
        if not result and cls.GOOGLE_API_KEY:
            logger.info(f"Nominatim reverse geocode failed, trying Google Maps for: {latitude}, {longitude}")
            result = cls._reverse_geocode_google(latitude, longitude)
        
        # Cache successful results
        if result:
            cache.set(cache_key, result, cls.CACHE_TIMEOUT)
        
        return result
    
    @classmethod
    @rate_limit(max_calls=1, period=1.0)
    def _reverse_geocode_nominatim(cls, latitude: float, longitude: float) -> Optional[Dict]:
        """Reverse geocode using Nominatim with rate limiting"""
        try:
            params = {
                'lat': latitude,
                'lon': longitude,
                'format': 'json',
                'addressdetails': 1
            }
            
            response = requests.get(
                f"{cls.NOMINATIM_BASE_URL}/reverse",
                params=params,
                headers={'User-Agent': cls.USER_AGENT},
                timeout=5
            )
            
            if response.status_code == 200:
                data = response.json()
                address = data.get('address', {})
                
                return {
                    'display_name': data.get('display_name', ''),
                    'address': address.get('road', ''),
                    'city': address.get('city') or address.get('town') or address.get('village', ''),
                    'suburb': address.get('suburb', ''),
                    'country': address.get('country', ''),
                    'country_code': address.get('country_code', '').upper(),
                    'postcode': address.get('postcode', ''),
                    'state': address.get('state', ''),
                    'provider': 'nominatim'
                }
            
            logger.warning(f"Reverse geocoding failed for: {latitude}, {longitude}")
            return None
            
        except Exception as e:
            logger.error(f"Nominatim reverse geocoding error for {latitude}, {longitude}: {str(e)}")
            return None
    
    @classmethod
    def _reverse_geocode_google(cls, latitude: float, longitude: float) -> Optional[Dict]:
        """Reverse geocode using Google Maps"""
        try:
            params = {
                'latlng': f"{latitude},{longitude}",
                'key': cls.GOOGLE_API_KEY
            }
            
            response = requests.get(
                f"{cls.GOOGLE_MAPS_BASE_URL}/geocode/json",
                params=params,
                timeout=5
            )
            
            if response.status_code == 200:
                data = response.json()
                if data.get('status') == 'OK' and data.get('results'):
                    result = data['results'][0]
                    
                    # Extract address components
                    address_components = result.get('address_components', [])
                    road = city = suburb = country = country_code = postcode = state = ''
                    
                    for component in address_components:
                        types = component.get('types', [])
                        if 'route' in types:
                            road = component.get('long_name', '')
                        elif 'locality' in types:
                            city = component.get('long_name', '')
                        elif 'sublocality' in types or 'neighborhood' in types:
                            suburb = component.get('long_name', '')
                        elif 'country' in types:
                            country = component.get('long_name', '')
                            country_code = component.get('short_name', '')
                        elif 'postal_code' in types:
                            postcode = component.get('long_name', '')
                        elif 'administrative_area_level_1' in types:
                            state = component.get('long_name', '')
                    
                    return {
                        'display_name': result.get('formatted_address', ''),
                        'address': road,
                        'city': city,
                        'suburb': suburb,
                        'country': country,
                        'country_code': country_code,
                        'postcode': postcode,
                        'state': state,
                        'provider': 'google'
                    }
            
            return None
            
        except Exception as e:
            logger.error(f"Google reverse geocoding error for {latitude}, {longitude}: {str(e)}")
            return None
            logger.error(f"Reverse geocoding error for {latitude}, {longitude}: {str(e)}")
            return None
    
    @classmethod
    def validate_coordinates(cls, latitude: float, longitude: float) -> bool:
        """
        Validate if coordinates are within valid ranges
        
        Args:
            latitude: Latitude (-90 to 90)
            longitude: Longitude (-180 to 180)
            
        Returns:
            True if valid, False otherwise
        """
        return -90 <= latitude <= 90 and -180 <= longitude <= 180
    
    @classmethod
    def calculate_distance(cls, point1: Point, point2: Point) -> float:
        """
        Calculate distance between two points in kilometers using PostGIS
        
        Args:
            point1: First GIS Point
            point2: Second GIS Point
            
        Returns:
            Distance in kilometers
        """
        try:
            # Transform to a metric projection for accurate distance calculation
            # SRID 4326 is WGS84 (lat/lon), we use geodetic distance
            return point1.distance(point2) * 111.32  # Approximate km per degree
        except Exception as e:
            logger.error(f"Distance calculation error: {str(e)}")
            return 0.0
    
    @classmethod
    def is_within_country_bounds(cls, latitude: float, longitude: float, country_code: str) -> bool:
        """
        Check if coordinates are within a country's approximate bounds
        Useful for validating property locations
        
        Args:
            latitude: Latitude coordinate
            longitude: Longitude coordinate
            country_code: ISO country code (e.g., 'ZW', 'ZA', 'BW')
            
        Returns:
            True if within bounds, False otherwise
        """
        # Approximate bounds for target African countries
        country_bounds = {
            'ZW': {'lat': (-22.5, -15.5), 'lon': (25.0, 33.1)},  # Zimbabwe
            'ZA': {'lat': (-35.0, -22.0), 'lon': (16.5, 33.0)},  # South Africa
            'BW': {'lat': (-27.0, -17.8), 'lon': (20.0, 29.4)},  # Botswana
            'NA': {'lat': (-29.0, -17.0), 'lon': (11.7, 25.3)},  # Namibia
            'ZM': {'lat': (-18.1, -8.2), 'lon': (21.9, 33.7)},   # Zambia
        }
        
        bounds = country_bounds.get(country_code.upper())
        if not bounds:
            # If country not in our list, just validate coordinates are valid
            return cls.validate_coordinates(latitude, longitude)
        
        lat_in_range = bounds['lat'][0] <= latitude <= bounds['lat'][1]
        lon_in_range = bounds['lon'][0] <= longitude <= bounds['lon'][1]
        
        return lat_in_range and lon_in_range
    
    @classmethod
    def search_nearby_addresses(cls, latitude: float, longitude: float, radius_km: float = 5) -> List[Dict]:
        """
        Search for nearby addresses/points of interest
        
        Args:
            latitude: Center latitude
            longitude: Center longitude
            radius_km: Search radius in kilometers
            
        Returns:
            List of nearby locations
        """
        # This would typically query a POI database or use a specialized geocoding API
        # For now, returning empty list as this requires external data source
        logger.info(f"Nearby search requested for {latitude}, {longitude} within {radius_km}km")
        return []
    
    @classmethod
    def get_location_suggestions(cls, query: str, country: Optional[str] = None, limit: int = 5) -> List[Dict]:
        """
        Get location autocomplete suggestions
        
        Args:
            query: Partial address or location name
            country: Optional country filter
            limit: Maximum number of suggestions
            
        Returns:
            List of location suggestions
        """
        try:
            params = {
                'q': query,
                'format': 'json',
                'limit': limit,
                'addressdetails': 1
            }
            
            if country:
                params['countrycodes'] = country.lower()[:2]
            
            response = requests.get(
                f"{cls.NOMINATIM_BASE_URL}/search",
                params=params,
                headers={'User-Agent': cls.USER_AGENT},
                timeout=5
            )
            
            if response.status_code == 200:
                data = response.json()
                suggestions = []
                
                for item in data:
                    address = item.get('address', {})
                    suggestions.append({
                        'display_name': item.get('display_name', ''),
                        'latitude': float(item['lat']),
                        'longitude': float(item['lon']),
                        'city': address.get('city') or address.get('town') or address.get('village', ''),
                        'country': address.get('country', ''),
                        'type': item.get('type', 'location'),
                    })
                
                return suggestions
            
            return []
            
        except Exception as e:
            logger.error(f"Location suggestions error for '{query}': {str(e)}")
            return []    
    @classmethod
    def bulk_geocode(cls, addresses: List[Dict[str, str]], use_cache: bool = True) -> List[Optional[Dict]]:
        """
        Geocode multiple addresses in batch
        Optimized for bulk operations with caching
        
        Args:
            addresses: List of dicts with 'address' and optional 'country' keys
            use_cache: Whether to use cache (default: True)
            
        Returns:
            List of geocoding results (None for failed addresses)
            
        Example:
            addresses = [
                {'address': 'Harare, Zimbabwe', 'country': 'ZW'},
                {'address': 'Cape Town, South Africa', 'country': 'ZA'},
            ]
            results = GeocodingService.bulk_geocode(addresses)
        """
        results = []
        
        for addr_data in addresses:
            address = addr_data.get('address')
            country = addr_data.get('country')
            
            if not address:
                results.append(None)
                continue
            
            try:
                result = cls.geocode_address(address, country)
                results.append(result)
                
                # Add small delay between requests to respect rate limits
                if len(addresses) > 1:
                    time.sleep(0.1)
                    
            except Exception as e:
                logger.error(f"Bulk geocoding failed for '{address}': {str(e)}")
                results.append(None)
        
        logger.info(f"Bulk geocoded {len(addresses)} addresses: {sum(1 for r in results if r)} successful")
        return results
    
    @classmethod
    def get_cache_stats(cls) -> Dict[str, int]:
        """
        Get geocoding cache statistics
        Useful for monitoring cache effectiveness
        
        Returns:
            Dict with cache hit/miss statistics
        """
        # Note: This is a simplified version
        # For production, consider using Redis cache with built-in stats
        return {
            'cache_timeout_seconds': cls.CACHE_TIMEOUT,
            'cache_backend': 'django.core.cache'
        }
    
    @classmethod
    def clear_cache(cls, pattern: Optional[str] = None) -> int:
        """
        Clear geocoding cache entries
        
        Args:
            pattern: Optional pattern to match keys (e.g., 'geocode:*')
                    If None, clears all geocoding cache
        
        Returns:
            Number of keys deleted
        """
        try:
            if pattern:
                # Django cache doesn't support pattern matching by default
                # This would require Redis cache backend
                logger.warning("Pattern-based cache clearing requires Redis backend")
                return 0
            else:
                # Clear specific geocoding keys would require tracking
                # For now, log the request
                logger.info("Cache clear requested - manual intervention needed")
                return 0
        except Exception as e:
            logger.error(f"Cache clear error: {str(e)}")
            return 0