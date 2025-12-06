from rest_framework import serializers
from apps.properties.models import Property, Amenity, PropertyImage

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
