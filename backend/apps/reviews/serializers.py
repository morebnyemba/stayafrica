from rest_framework import serializers
from apps.reviews.models import Review, ReviewVote


class ReviewVoteSerializer(serializers.ModelSerializer):
    class Meta:
        model = ReviewVote
        fields = ['id', 'review', 'user', 'vote_type', 'created_at']
        read_only_fields = ['id', 'user', 'created_at']


class ReviewSerializer(serializers.ModelSerializer):
    guest_name = serializers.CharField(source='guest.username', read_only=True)
    guest_first_name = serializers.CharField(source='guest.first_name', read_only=True)
    guest_last_name = serializers.CharField(source='guest.last_name', read_only=True)
    booking_ref = serializers.CharField(source='booking.booking_ref', read_only=True)
    property_title = serializers.SerializerMethodField()
    experience_title = serializers.SerializerMethodField()
    
    class Meta:
        model = Review
        fields = [
            'id', 'booking', 'booking_ref', 'guest', 'guest_name', 'guest_first_name', 'guest_last_name', 'host',
            'review_type', 'property_id', 'property_title', 'experience_id', 'experience_title',
            'rating', 'text', 'host_response', 'host_response_date',
            'helpful_count', 'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'guest', 'host', 'helpful_count', 'created_at', 'updated_at']
    
    def get_property_title(self, obj):
        if obj.property_id:
            from apps.properties.models import Property
            try:
                property_obj = Property.objects.get(id=obj.property_id)
                return property_obj.title
            except Property.DoesNotExist:
                pass
        
        # Fallback to checking the booking
        if hasattr(obj, 'booking') and obj.booking:
            if hasattr(obj.booking, 'rental_property') and obj.booking.rental_property:
                return obj.booking.rental_property.title
        return None
    
    def get_experience_title(self, obj):
        if obj.experience_id:
            from apps.experiences.models import Experience
            try:
                experience_obj = Experience.objects.get(id=obj.experience_id)
                return experience_obj.title
            except Experience.DoesNotExist:
                pass
        
        # Fallback to checking the booking
        if hasattr(obj, 'booking') and obj.booking:
            if hasattr(obj.booking, 'experience') and obj.booking.experience:
                return obj.booking.experience.title
        return None
