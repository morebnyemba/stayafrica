from rest_framework import serializers
from apps.messaging.models import Message

class MessageSerializer(serializers.ModelSerializer):
    sender_email = serializers.CharField(source='sender.email', read_only=True)
    receiver_email = serializers.CharField(source='receiver.email', read_only=True)
    
    class Meta:
        model = Message
        fields = [
            'id', 'sender', 'sender_email', 'receiver', 'receiver_email',
            'text', 'is_read', 'created_at'
        ]
        read_only_fields = ['id', 'sender', 'created_at']
