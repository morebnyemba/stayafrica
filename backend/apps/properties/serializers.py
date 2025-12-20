from rest_framework import serializers
from django.contrib.gis.geos import Point
from apps.properties.models import Property, Amenity, PropertyImage, SavedProperty

class AmenitySerializer(serializers.ModelSerializer):
    class Meta:
        model = Amenity
        fields = ['id', 'name', 'icon', 'description']

class PropertyImageSerializer(serializers.ModelSerializer):
    class Meta:
        model = PropertyImage
        fields = ['id', 'image', 'caption', 'order']

class PropertySerializer(serializers.ModelSerializer):
    amenities = AmenitySerializer(many=True, read_only=True)
    images = PropertyImageSerializer(many=True, read_only=True)
    host_email = serializers.CharField(source='host.email', read_only=True)
    
    class Meta:
        model = Property
        fields = [
            'id', 'host', 'host_email', 'title', 'description', 'property_type',
            'location', 'country', 'city', 'suburb', 'address', 'price_per_night',
            'currency', 'amenities', 'status', 'main_image', 'max_guests',
            'bedrooms', 'bathrooms', 'images', 'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'host', 'created_at', 'updated_at']

    def _coerce_location(self, value):
        if value is None:
            return None
        # Accept dicts with lat/lng or latitude/longitude
        if isinstance(value, dict):
            lat = value.get('lat') or value.get('latitude')
            lng = value.get('lng') or value.get('lon') or value.get('longitude')
            if lat is not None and lng is not None:
                return Point(float(lng), float(lat))
            raise serializers.ValidationError({'location': 'Invalid location dict; expected lat/lng.'})
        # Accept [lat, lng] or (lat, lng)
        if isinstance(value, (list, tuple)) and len(value) == 2:
            lat, lng = value
            return Point(float(lng), float(lat))
        # Pass through if already a GEOS Point
        if isinstance(value, Point):
            return value
        raise serializers.ValidationError({'location': 'Invalid location format.'})

    def create(self, validated_data):
        loc = validated_data.pop('location', None)
        if loc is not None:
            validated_data['location'] = self._coerce_location(loc)
        return super().create(validated_data)

    def update(self, instance, validated_data):
        loc = validated_data.pop('location', None)
        if loc is not None:
            validated_data['location'] = self._coerce_location(loc)
        return super().update(instance, validated_data)

class PropertyDetailSerializer(PropertySerializer):
    pass

class PropertyListSerializer(serializers.ModelSerializer):
    amenities = AmenitySerializer(many=True, read_only=True)
    
    class Meta:
        model = Property
        fields = [
            'id', 'title', 'location', 'country', 'city', 'price_per_night',
            'currency', 'main_image', 'bedrooms', 'bathrooms', 'max_guests',
            'amenities', 'status'
        ]


class SavedPropertySerializer(serializers.ModelSerializer):
    property = PropertyListSerializer(read_only=True)
    property_id = serializers.IntegerField(write_only=True)
    
    class Meta:
        model = SavedProperty
        fields = ['id', 'user', 'property', 'property_id', 'created_at']
        read_only_fields = ['id', 'user', 'created_at']
    
    def create(self, validated_data):
        validated_data['user'] = self.context['request'].user
        return super().create(validated_data)
