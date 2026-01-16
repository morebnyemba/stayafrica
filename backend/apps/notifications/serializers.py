from rest_framework import serializers
from apps.notifications.models import PushToken, NotificationPreference, Notification


class PushTokenSerializer(serializers.ModelSerializer):
    """Serializer for push tokens"""
    
    class Meta:
        model = PushToken
        fields = ['id', 'token', 'platform', 'device_id', 'is_active', 'created_at', 'last_used_at']
        read_only_fields = ['id', 'created_at', 'last_used_at']
    
    def create(self, validated_data):
        # Get or update existing token
        token = validated_data.get('token')
        user = self.context['request'].user
        
        push_token, created = PushToken.objects.update_or_create(
            user=user,
            token=token,
            defaults={
                'platform': validated_data.get('platform'),
                'device_id': validated_data.get('device_id', ''),
                'is_active': True,
            }
        )
        return push_token


class NotificationPreferenceSerializer(serializers.ModelSerializer):
    """Serializer for notification preferences"""
    
    class Meta:
        model = NotificationPreference
        fields = [
            'booking_confirmed', 'booking_cancelled', 'booking_reminder',
            'new_message', 'payment_received', 'payment_required',
            'review_reminder', 'review_received', 'price_drop',
            'updated_at'
        ]
        read_only_fields = ['updated_at']


class NotificationSerializer(serializers.ModelSerializer):
    """Serializer for notifications"""
    
    class Meta:
        model = Notification
        fields = [
            'id', 'notification_type', 'title', 'body', 'data',
            'status', 'deep_link', 'sent_at', 'read_at', 'created_at'
        ]
        read_only_fields = ['id', 'status', 'sent_at', 'read_at', 'created_at']
