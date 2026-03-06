from rest_framework import serializers
from apps.bookings.models import Booking
from apps.properties.serializers import PropertyListSerializer

class BookingSerializer(serializers.ModelSerializer):
    property_title = serializers.CharField(source='rental_property.title', read_only=True)
    property = PropertyListSerializer(source='rental_property', read_only=True)
    guest_email = serializers.CharField(source='guest.email', read_only=True)
    guest_first_name = serializers.CharField(source='guest.first_name', read_only=True)
    guest_last_name = serializers.CharField(source='guest.last_name', read_only=True)
    nights = serializers.IntegerField(read_only=True)
    payment_status = serializers.SerializerMethodField()
    
    class Meta:
        model = Booking
        fields = [
            'id', 'booking_ref', 'guest', 'guest_email', 'guest_first_name', 'guest_last_name',
            'rental_property', 'property', 'property_title',
            'check_in', 'check_out', 'number_of_guests', 'nights', 'nightly_total', 'service_fee',
            'commission_fee', 'cleaning_fee', 'taxes', 'grand_total', 'currency', 'status',
            'checked_in_at', 'checked_out_at', 'check_in_instructions', 'access_code',
            'payment_status', 'special_requests', 'created_at', 'updated_at'
        ]
        read_only_fields = [
            'id', 'booking_ref', 'guest', 'nightly_total', 'service_fee', 'commission_fee',
            'grand_total', 'currency', 'checked_in_at', 'checked_out_at', 'created_at', 'updated_at',
        ]

    def get_payment_status(self, obj):
        try:
            return obj.payment.status
        except Exception:
            return None

    def to_representation(self, instance):
        data = super().to_representation(instance)
        request = self.context.get('request')
        # Hide access_code from non-guests and non-hosts unless booking is confirmed+
        if request and request.user:
            is_guest = instance.guest_id == request.user.id
            is_host = instance.rental_property.host_id == request.user.id
            is_admin = request.user.is_staff
            visible_statuses = {'confirmed', 'checked_in', 'checked_out', 'completed'}
            if not (is_admin or is_host or (is_guest and instance.status in visible_statuses)):
                data.pop('access_code', None)
                data.pop('check_in_instructions', None)
        return data
