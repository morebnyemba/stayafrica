from rest_framework import serializers
from apps.bookings.models import Booking
from apps.properties.serializers import PropertyListSerializer

class BookingSerializer(serializers.ModelSerializer):
    property_title = serializers.CharField(source='rental_property.title', read_only=True)
    property = PropertyListSerializer(source='rental_property', read_only=True)
    guest_email = serializers.CharField(source='guest.email', read_only=True)
    nights = serializers.IntegerField(read_only=True)
    
    class Meta:
        model = Booking
        fields = [
            'id', 'booking_ref', 'guest', 'guest_email', 'rental_property', 'property', 'property_title',
            'check_in', 'check_out', 'nights', 'nightly_total', 'service_fee',
            'commission_fee', 'cleaning_fee', 'grand_total', 'currency', 'status',
            'special_requests', 'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'booking_ref', 'guest', 'nightly_total', 'service_fee', 'commission_fee', 'grand_total', 'currency', 'created_at', 'updated_at']
