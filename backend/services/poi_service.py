"""
POI Service
Find and manage points of interest near properties
"""
from django.contrib.gis.geos import Point
from django.contrib.gis.db.models.functions import Distance
from django.contrib.gis.measure import D
import logging
import requests

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
        Find and associate POIs with a property.
        If no POIs exist in the DB nearby, auto-imports from OpenStreetMap first.
        """
        from apps.properties.poi_models import PointOfInterest, PropertyPOI
        
        # Check if any POIs exist near this location
        nearby_count = PointOfInterest.objects.filter(
            location__distance_lte=(property_obj.location, D(km=radius_km)),
            is_active=True
        ).count()
        
        # Auto-import from OSM if database is empty for this area
        if nearby_count == 0:
            logger.info(f"No POIs in DB near property {property_obj.id}, importing from OpenStreetMap...")
            POIService.import_from_openstreetmap(property_obj, radius_meters=int(radius_km * 1000))
        
        # Find nearby POIs
        pois = POIService.find_nearby_pois(property_obj, radius_km=radius_km)
        
        count = 0
        for poi in pois:
            distance_meters = poi.distance.m
            
            walking_time = None
            driving_time = None
            
            if auto_calculate_time:
                walking_time = int(distance_meters / 83)
                driving_time = int(distance_meters / 500)
            
            PropertyPOI.objects.update_or_create(
                linked_property=property_obj,
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
            linked_property=property_obj
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
                'id': str(property_poi.id),
                'poi': {
                    'id': str(property_poi.poi.id),
                    'name': property_poi.poi.name,
                    'poi_type': property_poi.poi.poi_type,
                    'description': property_poi.poi.description,
                    'address': property_poi.poi.address,
                    'latitude': property_poi.poi.location.y,
                    'longitude': property_poi.poi.location.x,
                    'rating': float(property_poi.poi.rating) if property_poi.poi.rating else None,
                    'price_level': property_poi.poi.price_level,
                    'phone': property_poi.poi.phone,
                    'website': property_poi.poi.website,
                    'opening_hours': property_poi.poi.opening_hours,
                    'image_url': property_poi.poi.image_url,
                },
                'distance_meters': property_poi.distance_meters,
                'distance_display': property_poi.distance_display,
                'walking_time_minutes': property_poi.walking_time_minutes,
                'driving_time_minutes': property_poi.driving_time_minutes,
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
                    linked_property=property_obj,
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

    @staticmethod
    def import_from_openstreetmap(property_obj, radius_meters=5000):
        """
        Import POIs from OpenStreetMap via the Overpass API (free, no key needed).
        Queries for common amenities, tourism, leisure, and transport features.
        """
        from apps.properties.poi_models import PointOfInterest

        lat = property_obj.location.y
        lon = property_obj.location.x

        # Overpass QL: grab amenities, tourism, leisure, shops, transport within radius
        overpass_query = f"""
        [out:json][timeout:25];
        (
          node["amenity"~"restaurant|cafe|bar|pharmacy|hospital|clinic|bank|fuel|marketplace"](around:{radius_meters},{lat},{lon});
          node["tourism"~"attraction|museum|hotel|viewpoint|artwork|information"](around:{radius_meters},{lat},{lon});
          node["leisure"~"park|garden|beach_resort|swimming_pool|sports_centre"](around:{radius_meters},{lat},{lon});
          node["shop"~"supermarket|convenience|mall"](around:{radius_meters},{lat},{lon});
          node["public_transport"~"station|stop_position"](around:{radius_meters},{lat},{lon});
          node["railway"="station"](around:{radius_meters},{lat},{lon});
          node["aeroway"="aerodrome"](around:{radius_meters},{lat},{lon});
        );
        out body;
        """

        try:
            resp = requests.post(
                'https://overpass-api.de/api/interpreter',
                data={'data': overpass_query},
                timeout=30,
            )
            resp.raise_for_status()
            elements = resp.json().get('elements', [])
        except Exception as e:
            logger.error(f"Overpass API error: {e}")
            return 0

        count = 0
        for el in elements:
            tags = el.get('tags', {})
            name = tags.get('name')
            if not name:
                continue

            el_lat = el.get('lat')
            el_lon = el.get('lon')
            if el_lat is None or el_lon is None:
                continue

            osm_id = f"osm_node_{el['id']}"
            poi_type = POIService._map_osm_tags_to_poi_type(tags)

            poi, created = PointOfInterest.objects.update_or_create(
                external_id=osm_id,
                source='openstreetmap',
                defaults={
                    'name': name,
                    'poi_type': poi_type,
                    'location': Point(el_lon, el_lat, srid=4326),
                    'address': tags.get('addr:street', ''),
                    'city': tags.get('addr:city', property_obj.city),
                    'country': tags.get('addr:country', property_obj.country),
                    'phone': tags.get('phone', ''),
                    'website': tags.get('website', ''),
                    'opening_hours': {'raw': tags.get('opening_hours', '')},
                    'is_active': True,
                }
            )

            if created:
                count += 1

        logger.info(f"Imported {count} POIs from OpenStreetMap near property {property_obj.id}")
        return count

    @staticmethod
    def _map_osm_tags_to_poi_type(tags):
        """Map OpenStreetMap tags to our POI types"""
        amenity = tags.get('amenity', '')
        tourism = tags.get('tourism', '')
        leisure = tags.get('leisure', '')
        shop = tags.get('shop', '')
        transport = tags.get('public_transport', '')
        railway = tags.get('railway', '')
        aeroway = tags.get('aeroway', '')

        mapping = {
            # amenity
            'restaurant': 'restaurant', 'cafe': 'cafe', 'bar': 'bar',
            'pharmacy': 'pharmacy', 'hospital': 'hospital', 'clinic': 'hospital',
            'bank': 'services', 'fuel': 'services', 'marketplace': 'shopping',
            # tourism
            'attraction': 'attraction', 'museum': 'museum', 'viewpoint': 'attraction',
            'artwork': 'attraction', 'information': 'services', 'hotel': 'services',
            # leisure
            'park': 'park', 'garden': 'park', 'beach_resort': 'beach',
            'swimming_pool': 'entertainment', 'sports_centre': 'entertainment',
            # shop
            'supermarket': 'grocery', 'convenience': 'grocery', 'mall': 'shopping',
        }

        for tag_val in [amenity, tourism, leisure, shop]:
            if tag_val in mapping:
                return mapping[tag_val]

        if transport or railway or aeroway:
            return 'transport'

        return 'other'
