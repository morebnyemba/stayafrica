from rest_framework import serializers
from apps.messaging.models import Message, Conversation, MessageTemplate
from apps.users.models import User


class MessageSerializer(serializers.ModelSerializer):
    """Serializer for individual messages"""
    sender_name = serializers.SerializerMethodField()
    receiver_name = serializers.SerializerMethodField()
    is_own_message = serializers.SerializerMethodField()
    
    class Meta:
        model = Message
        fields = [
            'id', 'conversation', 'sender', 'sender_name',
            'receiver', 'receiver_name', 'text',
            'message_type', 'is_read', 'read_at', 'created_at', 'edited_at',
            'metadata', 'is_own_message'
        ]
        read_only_fields = ['id', 'sender', 'created_at', 'read_at', 'edited_at']
    
    def get_sender_name(self, obj):
        name = obj.sender.first_name.strip()
        return name if name else obj.sender.email.split('@')[0]
    
    def get_receiver_name(self, obj):
        if obj.receiver is None:
            return 'Support Team'
        name = obj.receiver.first_name.strip()
        return name if name else obj.receiver.email.split('@')[0]
    
    def get_is_own_message(self, obj):
        request = self.context.get('request')
        if request and request.user:
            return obj.sender == request.user
        return False


class ConversationSerializer(serializers.ModelSerializer):
    """Serializer for conversations with last message preview"""
    participants = serializers.SerializerMethodField()
    last_message = serializers.SerializerMethodField()
    unread_count = serializers.SerializerMethodField()
    other_participant = serializers.SerializerMethodField()
    property_title = serializers.CharField(source='property.title', read_only=True)
    booking_id = serializers.CharField(source='booking.id', read_only=True)
    
    # Support ticket extensions
    ticket_id = serializers.SerializerMethodField()
    ticket_status = serializers.SerializerMethodField()
    
    class Meta:
        model = Conversation
        fields = [
            'id', 'participants', 'other_participant', 'property', 'property_title',
            'booking', 'booking_id', 'subject', 'last_message', 'unread_count',
            'conversation_type', 'ticket_id', 'ticket_status',
            'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'created_at', 'updated_at']
    
    def get_ticket_id(self, obj):
        if obj.conversation_type == 'support' and hasattr(obj, 'support_ticket'):
            return obj.support_ticket.id
        return None

    def get_ticket_status(self, obj):
        if obj.conversation_type == 'support' and hasattr(obj, 'support_ticket'):
            return obj.support_ticket.status
        return None

    def get_participants(self, obj):
        return [
            {
                'id': user.id,
                'name': user.first_name.strip() or user.email.split('@')[0]
            }
            for user in obj.participants.all()
        ]
    
    def get_other_participant(self, obj):
        """Get the other participant (not the current user)"""
        request = self.context.get('request')
        if request and request.user:
            other_users = obj.participants.exclude(id=request.user.id)
            if other_users.exists():
                user = other_users.first()
                return {
                    'id': user.id,
                    'name': user.first_name.strip() or user.email.split('@')[0],
                    'is_host': user.is_host
                }
        return None
    
    def get_last_message(self, obj):
        last_msg = obj.get_last_message()
        if last_msg:
            return {
                'id': last_msg.id,
                'text': last_msg.text[:100],
                'sender_name': last_msg.sender.first_name.strip() or last_msg.sender.email.split('@')[0],
                'created_at': last_msg.created_at,
                'is_read': last_msg.is_read
            }
        return None
    
    def get_unread_count(self, obj):
        request = self.context.get('request')
        if request and request.user:
            return obj.get_unread_count(request.user)
        return 0


class ConversationDetailSerializer(ConversationSerializer):
    """Detailed conversation serializer with all messages"""
    messages = MessageSerializer(many=True, read_only=True)
    
    class Meta(ConversationSerializer.Meta):
        fields = ConversationSerializer.Meta.fields + ['messages']


class MessageCreateSerializer(serializers.ModelSerializer):
    """Serializer for creating new messages"""
    
    class Meta:
        model = Message
        fields = ['conversation', 'receiver', 'text', 'message_type', 'metadata']
    
    def validate(self, data):
        """Ensure sender and receiver are in the conversation"""
        conversation = data.get('conversation')
        receiver = data.get('receiver')
        sender = self.context['request'].user
        
        # Validate message text is not empty
        if not data.get('text', '').strip():
            raise serializers.ValidationError({"text": "Message text cannot be empty."})
        
        # Validate conversation exists and sender is participant
        if conversation:
            participants = conversation.participants.all()
            if sender not in participants:
                raise serializers.ValidationError({"conversation": "Sender must be a participant in the conversation."})
            # Support conversations don't require a receiver
            if conversation.conversation_type == 'support':
                pass  # receiver can be None
            elif not receiver:
                raise serializers.ValidationError({"receiver": "Receiver is required."})
            elif receiver not in participants:
                raise serializers.ValidationError({"receiver": "Receiver must be a participant in the conversation."})
            elif receiver == sender:
                raise serializers.ValidationError({"receiver": "You cannot send a message to yourself."})
        else:
            raise serializers.ValidationError({"conversation": "Conversation is required."})
        
        return data


class MessageTemplateSerializer(serializers.ModelSerializer):
    """Serializer for message templates"""
    
    class Meta:
        model = MessageTemplate
        fields = ['id', 'name', 'template_type', 'subject', 'body', 'is_active', 'created_at', 'updated_at']
        read_only_fields = ['id', 'created_at', 'updated_at']
