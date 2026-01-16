"""
POI Service
Find and manage points of interest near properties
"""
from django.contrib.gis.geos import Point
from django.contrib.gis.db.models.functions import Distance
from django.contrib.gis.measure import D
import logging

logger = logging.getLogger(__name__)


class POIService:
    """Service for managing points of interest"""
    
    @staticmethod
    def find_nearby_pois(property_obj, radius_km=5, poi_types=None, limit=50):
        """
        Find POIs near a property
        
        Args:
            property_obj: Property instance
            radius_km: Search radius in kilometers
            poi_types: List of POI types to filter (optional)
            limit: Maximum number of results
            
        Returns:
            QuerySet of PointOfInterest with distance annotation
        """
        from apps.properties.poi_models import PointOfInterest
        
        # Query POIs within radius
        pois = PointOfInterest.objects.filter(
            location__distance_lte=(property_obj.location, D(km=radius_km)),
            is_active=True
        ).annotate(
            distance=Distance('location', property_obj.location)
        ).order_by('distance')
        
        # Filter by POI types if specified
        if poi_types:
            pois = pois.filter(poi_type__in=poi_types)
        
        return pois[:limit]
    
    @staticmethod
    def associate_pois_with_property(property_obj, radius_km=5, auto_calculate_time=True):
        """
        Find and associate POIs with a property
        
        Args:
            property_obj: Property instance
            radius_km: Search radius in kilometers
            auto_calculate_time: Calculate walking/driving times
            
        Returns:
            Number of POIs associated
        """
        from apps.properties.poi_models import PointOfInterest, PropertyPOI
        
        # Find nearby POIs
        pois = POIService.find_nearby_pois(property_obj, radius_km=radius_km)
        
        count = 0
        for poi in pois:
            distance_meters = poi.distance.m
            
            # Calculate estimated times (rough estimates)
            walking_time = None
            driving_time = None
            
            if auto_calculate_time:
                # Walking: ~5 km/h = 83 m/min
                walking_time = int(distance_meters / 83)
                # Driving: ~30 km/h in city = 500 m/min
                driving_time = int(distance_meters / 500)
            
            # Create or update association
            PropertyPOI.objects.update_or_create(
                property=property_obj,
                poi=poi,
                defaults={
                    'distance_meters': distance_meters,
                    'walking_time_minutes': walking_time,
                    'driving_time_minutes': driving_time,
                }
            )
            count += 1
        
        logger.info(f"Associated {count} POIs with property {property_obj.id}")
        return count
    
    @staticmethod
    def get_property_pois(property_obj, poi_types=None, max_distance_km=None, recommended_only=False):
        """
        Get POIs associated with a property
        
        Args:
            property_obj: Property instance
            poi_types: Filter by POI types
            max_distance_km: Maximum distance in kilometers
            recommended_only: Only return host-recommended POIs
            
        Returns:
            QuerySet of PropertyPOI
        """
        from apps.properties.poi_models import PropertyPOI
        
        pois = PropertyPOI.objects.filter(
            property=property_obj
        ).select_related('poi', 'poi__category')
        
        if poi_types:
            pois = pois.filter(poi__poi_type__in=poi_types)
        
        if max_distance_km:
            pois = pois.filter(distance_meters__lte=max_distance_km * 1000)
        
        if recommended_only:
            pois = pois.filter(is_recommended=True)
        
        return pois.order_by('distance_meters')
    
    @staticmethod
    def get_pois_by_category(property_obj, radius_km=5):
        """
        Get POIs grouped by category
        
        Args:
            property_obj: Property instance
            radius_km: Search radius in kilometers
            
        Returns:
            dict with POIs grouped by type
        """
        from apps.properties.poi_models import PropertyPOI
        
        pois = POIService.get_property_pois(
            property_obj,
            max_distance_km=radius_km
        )
        
        # Group by POI type
        grouped = {}
        for property_poi in pois:
            poi_type = property_poi.poi.poi_type
            if poi_type not in grouped:
                grouped[poi_type] = []
            
            grouped[poi_type].append({
                'id': str(property_poi.poi.id),
                'name': property_poi.poi.name,
                'poi_type': property_poi.poi.poi_type,
                'distance_meters': property_poi.distance_meters,
                'distance_display': property_poi.distance_display,
                'walking_time_minutes': property_poi.walking_time_minutes,
                'driving_time_minutes': property_poi.driving_time_minutes,
                'rating': float(property_poi.poi.rating) if property_poi.poi.rating else None,
                'price_level': property_poi.poi.price_level,
                'address': property_poi.poi.address,
                'is_recommended': property_poi.is_recommended,
                'host_notes': property_poi.host_notes,
            })
        
        return grouped
    
    @staticmethod
    def import_from_google_places(property_obj, api_key, radius_meters=5000):
        """
        Import POIs from Google Places API
        
        Args:
            property_obj: Property instance
            api_key: Google Places API key
            radius_meters: Search radius in meters
            
        Returns:
            Number of POIs imported
        """
        try:
            import googlemaps
            from apps.properties.poi_models import PointOfInterest, PropertyPOI, POICategory
            
            gmaps = googlemaps.Client(key=api_key)
            
            # Get nearby places
            places_result = gmaps.places_nearby(
                location=(property_obj.location.y, property_obj.location.x),
                radius=radius_meters
            )
            
            count = 0
            for place in places_result.get('results', []):
                # Map Google place types to our POI types
                place_types = place.get('types', [])
                poi_type = POIService._map_google_type_to_poi_type(place_types)
                
                # Get or create POI
                poi, created = PointOfInterest.objects.get_or_create(
                    external_id=place['place_id'],
                    source='google_places',
                    defaults={
                        'name': place['name'],
                        'poi_type': poi_type,
                        'location': Point(
                            place['geometry']['location']['lng'],
                            place['geometry']['location']['lat']
                        ),
                        'address': place.get('vicinity', ''),
                        'city': property_obj.city,
                        'country': property_obj.country,
                        'rating': place.get('rating'),
                        'review_count': place.get('user_ratings_total', 0),
                        'price_level': place.get('price_level'),
                    }
                )
                
                if created:
                    logger.info(f"Imported POI: {poi.name}")
                
                # Associate with property
                distance = property_obj.location.distance(poi.location) * 111000  # Convert to meters
                PropertyPOI.objects.update_or_create(
                    property=property_obj,
                    poi=poi,
                    defaults={
                        'distance_meters': distance,
                        'walking_time_minutes': int(distance / 83),
                        'driving_time_minutes': int(distance / 500),
                    }
                )
                count += 1
            
            logger.info(f"Imported {count} POIs from Google Places for property {property_obj.id}")
            return count
            
        except ImportError:
            logger.error("googlemaps library not installed. Run: pip install googlemaps")
            return 0
        except Exception as e:
            logger.error(f"Error importing from Google Places: {e}")
            return 0
    
    @staticmethod
    def _map_google_type_to_poi_type(google_types):
        """Map Google Places types to our POI types"""
        type_mapping = {
            'restaurant': 'restaurant',
            'cafe': 'cafe',
            'bar': 'bar',
            'night_club': 'bar',
            'supermarket': 'grocery',
            'grocery_or_supermarket': 'grocery',
            'pharmacy': 'pharmacy',
            'hospital': 'hospital',
            'doctor': 'hospital',
            'tourist_attraction': 'attraction',
            'museum': 'museum',
            'park': 'park',
            'beach': 'beach',
            'bus_station': 'transport',
            'train_station': 'transport',
            'subway_station': 'transport',
            'shopping_mall': 'shopping',
            'store': 'shopping',
            'movie_theater': 'entertainment',
            'spa': 'services',
            'gym': 'services',
        }
        
        for gtype in google_types:
            if gtype in type_mapping:
                return type_mapping[gtype]
        
        return 'other'
