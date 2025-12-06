from rest_framework import serializers
from apps.payments.models import Payment

class PaymentSerializer(serializers.ModelSerializer):
    booking_ref = serializers.CharField(source='booking.booking_ref', read_only=True)
    
    class Meta:
        model = Payment
        fields = [
            'id', 'booking', 'booking_ref', 'provider', 'gateway_ref', 'status',
            'amount', 'currency', 'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'gateway_ref', 'created_at', 'updated_at']
