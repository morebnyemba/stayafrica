from rest_framework import serializers
from apps.reviews.models import Review, ReviewVote


class ReviewVoteSerializer(serializers.ModelSerializer):
    class Meta:
        model = ReviewVote
        fields = ['id', 'review', 'user', 'vote_type', 'created_at']
        read_only_fields = ['id', 'user', 'created_at']


class ReviewSerializer(serializers.ModelSerializer):
    guest_name = serializers.CharField(source='guest.username', read_only=True)
    booking_ref = serializers.CharField(source='booking.booking_ref', read_only=True)
    property_title = serializers.SerializerMethodField()
    experience_title = serializers.SerializerMethodField()
    
    class Meta:
        model = Review
        fields = [
            'id', 'booking', 'booking_ref', 'guest', 'guest_name', 'host',
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
                return None
        return None
    
    def get_experience_title(self, obj):
        if obj.experience_id:
            from apps.experiences.models import Experience
            try:
                experience_obj = Experience.objects.get(id=obj.experience_id)
                return experience_obj.title
            except Experience.DoesNotExist:
                return None
        return None
