from rest_framework import serializers
from django.contrib.gis.geos import Point
from apps.properties.models import Property, Amenity, PropertyImage, SavedProperty
from apps.properties.validators import validate_image_file

class AmenitySerializer(serializers.ModelSerializer):
    class Meta:
        model = Amenity
        fields = ['id', 'name', 'icon', 'description']

class PropertyImageSerializer(serializers.ModelSerializer):
    image_url = serializers.SerializerMethodField()

    class Meta:
        model = PropertyImage
        fields = ['id', 'image', 'image_url', 'caption', 'order']

    def get_image_url(self, obj):
        if not obj.image:
            return None
        request = self.context.get('request') if hasattr(self, 'context') else None
        url = getattr(obj.image, 'url', None)
        if not url:
            return None
        return request.build_absolute_uri(url) if request else url

class PropertySerializer(serializers.ModelSerializer):
    amenities = AmenitySerializer(many=True, read_only=True)
    images = PropertyImageSerializer(many=True, read_only=True)
    host_email = serializers.CharField(source='host.email', read_only=True)
    main_image_url = serializers.SerializerMethodField()
    
    class Meta:
        model = Property
        fields = [
            'id', 'host', 'host_email', 'title', 'description', 'property_type',
            'location', 'country', 'city', 'suburb', 'address', 'price_per_night',
            'currency', 'amenities', 'status', 'main_image', 'main_image_url', 'max_guests',
            'bedrooms', 'bathrooms', 'images', 'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'host', 'created_at', 'updated_at']

    def _absolute_media_url(self, file_field):
        if not file_field:
            return None
        url = getattr(file_field, 'url', None)
        if not url:
            return None
        request = self.context.get('request') if hasattr(self, 'context') else None
        return request.build_absolute_uri(url) if request else url

    def _coerce_location(self, value):
        if value is None:
            return None
        # Accept GeoJSON format: {"type": "Point", "coordinates": [lng, lat]}
        if isinstance(value, dict):
            if value.get('type') == 'Point' and 'coordinates' in value:
                coords = value['coordinates']
                if isinstance(coords, (list, tuple)) and len(coords) == 2:
                    lng, lat = coords
                    return Point(float(lng), float(lat))
            # Accept dicts with lat/lng or latitude/longitude
            lat = value.get('lat') or value.get('latitude')
            lng = value.get('lng') or value.get('lon') or value.get('longitude')
            if lat is not None and lng is not None:
                return Point(float(lng), float(lat))
            raise serializers.ValidationError({'location': f'Invalid location dict format: {value}'})
        # Accept [lng, lat] or (lng, lat) - Note: GeoJSON order is [longitude, latitude]
        if isinstance(value, (list, tuple)) and len(value) == 2:
            lng, lat = value
            return Point(float(lng), float(lat))
        # Pass through if already a GEOS Point
        if isinstance(value, Point):
            return value
        raise serializers.ValidationError({'location': f'Invalid location format: {type(value)}'})

    def create(self, validated_data):
        loc = validated_data.get('location')
        if loc is not None:
            validated_data['location'] = self._coerce_location(loc)
        return super().create(validated_data)

    def update(self, instance, validated_data):
        loc = validated_data.get('location')
        if loc is not None:
            validated_data['location'] = self._coerce_location(loc)
        return super().update(instance, validated_data)

    def validate_main_image(self, value):
        if value is None:
            return value
        validate_image_file(value)
        return value

    def get_main_image_url(self, obj):
        return self._absolute_media_url(obj.main_image)

class PropertyDetailSerializer(PropertySerializer):
    average_rating = serializers.SerializerMethodField()
    review_count = serializers.SerializerMethodField()
    host = serializers.SerializerMethodField()
    
    class Meta(PropertySerializer.Meta):
        fields = PropertySerializer.Meta.fields + ['average_rating', 'review_count']
    
    def _get_review_stats(self, obj):
        """Cache review stats to avoid duplicate queries"""
        if not hasattr(self, '_review_stats_cache'):
            self._review_stats_cache = {}
        if obj.id not in self._review_stats_cache:
            from apps.properties.utils import get_property_review_stats
            self._review_stats_cache[obj.id] = get_property_review_stats(obj)
        return self._review_stats_cache[obj.id]
    
    def get_average_rating(self, obj):
        """Calculate average rating from reviews"""
        return self._get_review_stats(obj)['average_rating']
    
    def get_review_count(self, obj):
        """Count total reviews for this property"""
        return self._get_review_stats(obj)['review_count']
    
    def get_host(self, obj):
        """Include host details"""
        if not obj.host:
            return None
        return {
            'id': obj.host.id,
            'first_name': getattr(obj.host, 'first_name', ''),
            'last_name': getattr(obj.host, 'last_name', ''),
            'email': obj.host.email,
        }

class PropertyListSerializer(serializers.ModelSerializer):
    amenities = AmenitySerializer(many=True, read_only=True)
    images = PropertyImageSerializer(many=True, read_only=True)
    main_image_url = serializers.SerializerMethodField()
    average_rating = serializers.SerializerMethodField()
    review_count = serializers.SerializerMethodField()
    
    class Meta:
        model = Property
        fields = [
            'id', 'title', 'location', 'country', 'city', 'price_per_night',
            'currency', 'main_image', 'main_image_url', 'bedrooms', 'bathrooms', 'max_guests',
            'amenities', 'images', 'status', 'average_rating', 'review_count'
        ]

    def _absolute_media_url(self, file_field):
        if not file_field:
            return None
        url = getattr(file_field, 'url', None)
        if not url:
            return None
        request = self.context.get('request') if hasattr(self, 'context') else None
        return request.build_absolute_uri(url) if request else url

    def get_main_image_url(self, obj):
        return self._absolute_media_url(obj.main_image)
    
    def _get_review_stats(self, obj):
        """Cache review stats to avoid duplicate queries"""
        if not hasattr(self, '_review_stats_cache'):
            self._review_stats_cache = {}
        if obj.id not in self._review_stats_cache:
            from apps.properties.utils import get_property_review_stats
            self._review_stats_cache[obj.id] = get_property_review_stats(obj)
        return self._review_stats_cache[obj.id]
    
    def get_average_rating(self, obj):
        """Calculate average rating from reviews"""
        return self._get_review_stats(obj)['average_rating']
    
    def get_review_count(self, obj):
        """Count total reviews for this property"""
        return self._get_review_stats(obj)['review_count']


class HostPropertyListSerializer(PropertyListSerializer):
    """Extended serializer for host property listings with performance metrics"""
    total_bookings = serializers.SerializerMethodField()
    total_earnings = serializers.SerializerMethodField()
    
    class Meta(PropertyListSerializer.Meta):
        fields = PropertyListSerializer.Meta.fields + ['total_bookings', 'total_earnings']
    
    def get_total_bookings(self, obj):
        """Count total bookings for this property"""
        from apps.bookings.models import Booking
        return Booking.objects.filter(rental_property=obj).count()
    
    def get_total_earnings(self, obj):
        """Calculate total earnings from completed bookings"""
        from django.db.models import Sum, F
        from apps.bookings.models import Booking
        
        earnings = Booking.objects.filter(
            rental_property=obj,
            status='completed'
        ).aggregate(
            total=Sum(F('nightly_total') + F('cleaning_fee') - F('commission_fee'))
        )['total'] or 0
        
        return float(earnings)


class SavedPropertySerializer(serializers.ModelSerializer):
    property = PropertyListSerializer(read_only=True)
    property_id = serializers.CharField(max_length=10, write_only=True)
    
    class Meta:
        model = SavedProperty
        fields = ['id', 'user', 'property', 'property_id', 'created_at']
        read_only_fields = ['id', 'user', 'created_at']
    
    def create(self, validated_data):
        validated_data['user'] = self.context['request'].user
        return super().create(validated_data)
