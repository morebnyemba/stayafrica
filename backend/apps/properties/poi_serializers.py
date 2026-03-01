from rest_framework import serializers
from apps.properties.poi_models import POICategory, PointOfInterest


class POICategorySerializer(serializers.ModelSerializer):
    class Meta:
        model = POICategory
        fields = ['id', 'name', 'icon', 'color', 'description', 'display_order']


class PointOfInterestSerializer(serializers.ModelSerializer):
    category_name = serializers.CharField(source='category.name', read_only=True, default=None)

    class Meta:
        model = PointOfInterest
        fields = [
            'id', 'name', 'category', 'category_name', 'poi_type',
            'description', 'address', 'city', 'country',
            'phone', 'website', 'rating', 'review_count',
            'price_level', 'opening_hours', 'image_url',
            'source', 'is_active', 'created_at', 'updated_at',
        ]
        read_only_fields = ['id', 'created_at', 'updated_at']
