from rest_framework import serializers
from apps.reviews.models import Review

class ReviewSerializer(serializers.ModelSerializer):
    guest_name = serializers.CharField(source='guest.username', read_only=True)
    booking_ref = serializers.CharField(source='booking.booking_ref', read_only=True)
    
    class Meta:
        model = Review
        fields = [
            'id', 'booking', 'booking_ref', 'guest', 'guest_name', 'host',
            'rating', 'text', 'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'guest', 'host', 'created_at', 'updated_at']
