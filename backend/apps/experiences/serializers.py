from rest_framework import serializers
from apps.experiences.models import (
    Experience, ExperienceCategory, ExperienceImage,
    ExperienceBooking, ExperienceAvailability
)


class ExperienceCategorySerializer(serializers.ModelSerializer):
    class Meta:
        model = ExperienceCategory
        fields = ['id', 'name', 'description', 'icon']


class ExperienceImageSerializer(serializers.ModelSerializer):
    class Meta:
        model = ExperienceImage
        fields = ['id', 'image', 'caption', 'order']


class ExperienceSerializer(serializers.ModelSerializer):
    host_name = serializers.CharField(source='host.username', read_only=True)
    images = ExperienceImageSerializer(many=True, read_only=True)
    category_name = serializers.CharField(source='category.name', read_only=True)
    latitude = serializers.SerializerMethodField()
    longitude = serializers.SerializerMethodField()
    
    class Meta:
        model = Experience
        fields = [
            'id', 'host', 'host_name', 'title', 'description', 'category', 'category_name',
            'latitude', 'longitude', 'country', 'city', 'address',
            'price_per_person', 'currency', 'duration', 'duration_hours', 'difficulty',
            'min_participants', 'max_participants', 'included_items', 'requirements',
            'cancellation_policy', 'main_image', 'images', 'status', 'is_available',
            'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'host', 'created_at', 'updated_at', 'is_available']
    
    def get_latitude(self, obj):
        return obj.location.y if obj.location else None
    
    def get_longitude(self, obj):
        return obj.location.x if obj.location else None


class ExperienceBookingSerializer(serializers.ModelSerializer):
    guest_name = serializers.CharField(source='guest.username', read_only=True)
    experience_title = serializers.CharField(source='experience.title', read_only=True)
    
    class Meta:
        model = ExperienceBooking
        fields = [
            'id', 'booking_ref', 'guest', 'guest_name', 'experience', 'experience_title',
            'booking_date', 'booking_time', 'num_participants',
            'price_per_person', 'service_fee', 'total_amount', 'currency',
            'status', 'special_requests', 'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'booking_ref', 'guest', 'total_amount', 'created_at', 'updated_at']


class ExperienceAvailabilitySerializer(serializers.ModelSerializer):
    class Meta:
        model = ExperienceAvailability
        fields = [
            'id', 'experience', 'weekday', 'specific_date',
            'start_time', 'end_time', 'is_available'
        ]
