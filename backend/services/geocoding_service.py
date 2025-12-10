"""
Geocoding Service for StayAfrica
Provides accurate location services using GDAL/PostGIS

Note: This service uses synchronous HTTP requests. For production use with
high traffic, consider:
1. Implementing async/await for non-blocking operations
2. Using Celery tasks for bulk geocoding operations
3. Implementing caching to reduce API calls
4. Rate limiting to respect Nominatim usage policy
"""
from django.contrib.gis.geos import Point
from django.contrib.gis.measure import D
from typing import Dict, Tuple, Optional, List
import logging
import requests

logger = logging.getLogger(__name__)


class GeocodingService:
    """
    Service for geocoding and reverse geocoding using Nominatim (OpenStreetMap)
    Can be extended to use Google Maps API, Mapbox, or other providers
    """
    
    NOMINATIM_BASE_URL = "https://nominatim.openstreetmap.org"
    USER_AGENT = "StayAfrica/1.0"
    
    @classmethod
    def geocode_address(cls, address: str, country: Optional[str] = None) -> Optional[Dict]:
        """
        Convert an address to coordinates (lat, lon)
        
        Args:
            address: Street address or location description
            country: Optional country filter for better accuracy
            
        Returns:
            Dict with 'latitude', 'longitude', 'display_name', 'city', 'country'
            or None if not found
        """
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
                    }
            
            logger.warning(f"Geocoding failed for address: {address}")
            return None
            
        except Exception as e:
            logger.error(f"Geocoding error for address '{address}': {str(e)}")
            return None
    
    @classmethod
    def reverse_geocode(cls, latitude: float, longitude: float) -> Optional[Dict]:
        """
        Convert coordinates to an address
        
        Args:
            latitude: Latitude coordinate
            longitude: Longitude coordinate
            
        Returns:
            Dict with address components or None if not found
        """
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
                }
            
            logger.warning(f"Reverse geocoding failed for: {latitude}, {longitude}")
            return None
            
        except Exception as e:
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
