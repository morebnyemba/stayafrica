from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from django.db.models import Q, Prefetch
from django.utils import timezone
from apps.messaging.models import Message, Conversation, MessageTemplate
from apps.messaging.serializers import (
    MessageSerializer, ConversationSerializer, ConversationDetailSerializer,
    MessageCreateSerializer, MessageTemplateSerializer
)
import logging

logger = logging.getLogger(__name__)


class ConversationViewSet(viewsets.ModelViewSet):
    """ViewSet for managing conversations"""
    permission_classes = [IsAuthenticated]
    
    def get_serializer_class(self):
        if self.action == 'retrieve':
            return ConversationDetailSerializer
        return ConversationSerializer
    
    def get_queryset(self):
        """Get conversations for the current user"""
        user = self.request.user
        
        # Filter out archived conversations
        queryset = Conversation.objects.filter(
            participants=user
        ).prefetch_related(
            'participants',
            'property',
            'booking',
            Prefetch('messages', queryset=Message.objects.select_related('sender', 'receiver'))
        ).distinct()
        
        # Filter archived conversations
        if self.request.query_params.get('archived') != 'true':
            queryset = queryset.exclude(archived_by=user)
        
        return queryset.order_by('-updated_at')
    
    def create(self, request, *args, **kwargs):
        """Create a new conversation"""
        try:
            participants = request.data.get('participants', [])
            if request.user.id not in participants:
                participants.append(request.user.id)
            
            # Check if conversation already exists
            property_id = request.data.get('property')
            booking_id = request.data.get('booking')
            
            if len(participants) == 2:
                # Try to find existing conversation
                existing_conv = Conversation.objects.filter(
                    participants__id=participants[0]
                ).filter(
                    participants__id=participants[1]
                ).filter(
                    property_id=property_id,
                    booking_id=booking_id
                ).first()
                
                if existing_conv:
                    serializer = self.get_serializer(existing_conv)
                    return Response(serializer.data, status=status.HTTP_200_OK)
            
            # Create new conversation
            conversation = Conversation.objects.create(
                property_id=property_id,
                booking_id=booking_id,
                subject=request.data.get('subject', '')
            )
            conversation.participants.set(participants)
            
            serializer = self.get_serializer(conversation)
            return Response(serializer.data, status=status.HTTP_201_CREATED)
            
        except Exception as e:
            logger.error(f"Error creating conversation: {str(e)}")
            return Response(
                {'error': str(e)},
                status=status.HTTP_400_BAD_REQUEST
            )
    
    @action(detail=True, methods=['post'])
    def mark_as_read(self, request, pk=None):
        """Mark all messages in conversation as read"""
        conversation = self.get_object()
        count = conversation.mark_as_read(request.user)
        return Response({'marked_as_read': count})
    
    @action(detail=True, methods=['post'])
    def archive(self, request, pk=None):
        """Archive conversation for current user"""
        conversation = self.get_object()
        if request.user in conversation.archived_by.all():
            conversation.archived_by.remove(request.user)
            return Response({'archived': False})
        else:
            conversation.archived_by.add(request.user)
            return Response({'archived': True})
    
    @action(detail=False, methods=['get'])
    def unread_count(self, request):
        """Get total unread message count"""
        conversations = self.get_queryset()
        total_unread = sum(conv.get_unread_count(request.user) for conv in conversations)
        return Response({'unread_count': total_unread})


class MessageViewSet(viewsets.ModelViewSet):
    """ViewSet for managing messages"""
    permission_classes = [IsAuthenticated]
    
    def get_serializer_class(self):
        if self.action == 'create':
            return MessageCreateSerializer
        return MessageSerializer
    
    def get_queryset(self):
        """Get messages for the current user"""
        user = self.request.user
        queryset = Message.objects.filter(
            Q(sender=user) | Q(receiver=user)
        ).filter(
            deleted_by_sender__isnull=True,
            deleted_by_receiver__isnull=True
        ).select_related('sender', 'receiver', 'conversation')
        
        # Filter by conversation
        conversation_id = self.request.query_params.get('conversation')
        if conversation_id:
            queryset = queryset.filter(conversation_id=conversation_id)
        
        # Filter by message type
        message_type = self.request.query_params.get('message_type')
        if message_type:
            queryset = queryset.filter(message_type=message_type)
        
        # Filter unread
        if self.request.query_params.get('unread') == 'true':
            queryset = queryset.filter(receiver=user, is_read=False)
        
        return queryset.order_by('-created_at')
    
    def perform_create(self, serializer):
        """Set the sender to the current user and route through Erlang if available"""
        message = serializer.save(sender=self.request.user)
        
        # Try to route through Erlang for real-time delivery
        try:
            from services.erlang_messaging import erlang_client
            erlang_client.send_message(
                conversation_id=message.conversation.id,
                sender_id=message.sender.id,
                receiver_id=message.receiver.id,
                text=message.text,
                message_type=message.message_type,
                priority='high' if message.message_type == 'system' else 'normal',
                metadata=message.metadata
            )
        except Exception as e:
            logger.warning(f"Failed to route message through Erlang: {str(e)}")
    
    def destroy(self, request, *args, **kwargs):
        """Soft delete message"""
        message = self.get_object()
        
        if message.sender == request.user:
            message.deleted_by_sender = timezone.now()
        if message.receiver == request.user:
            message.deleted_by_receiver = timezone.now()
        
        message.save()
        return Response(status=status.HTTP_204_NO_CONTENT)
    
    @action(detail=True, methods=['post'])
    def mark_as_read(self, request, pk=None):
        """Mark a message as read"""
        message = self.get_object()
        
        if message.receiver != request.user:
            return Response(
                {'error': 'You can only mark messages sent to you as read'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        if not message.is_read:
            message.is_read = True
            message.read_at = timezone.now()
            message.save()
        
        return Response({'status': 'marked as read'})
    
    @action(detail=True, methods=['put', 'patch'])
    def edit(self, request, pk=None):
        """Edit message text"""
        message = self.get_object()
        
        if message.sender != request.user:
            return Response(
                {'error': 'You can only edit your own messages'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        new_text = request.data.get('text')
        if not new_text:
            return Response(
                {'error': 'Text is required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        message.text = new_text
        message.edited_at = timezone.now()
        message.save()
        
        serializer = self.get_serializer(message)
        return Response(serializer.data)
    
    @action(detail=False, methods=['get'])
    def unread(self, request):
        """Get count of unread messages"""
        count = Message.objects.filter(
            receiver=request.user,
            is_read=False,
            deleted_by_receiver__isnull=True
        ).count()
        return Response({'unread_count': count})


class MessageTemplateViewSet(viewsets.ReadOnlyModelViewSet):
    """ViewSet for viewing message templates"""
    queryset = MessageTemplate.objects.filter(is_active=True)
    serializer_class = MessageTemplateSerializer
    permission_classes = [IsAuthenticated]
    
    @action(detail=True, methods=['post'])
    def render(self, request, pk=None):
        """Render template with provided variables"""
        template = self.get_object()
        variables = request.data.get('variables', {})
        
        try:
            rendered = template.render(variables)
            return Response(rendered)
        except KeyError as e:
            return Response(
                {'error': f'Missing variable: {str(e)}'},
                status=status.HTTP_400_BAD_REQUEST
            )


from rest_framework.decorators import api_view, permission_classes
from rest_framework.permissions import AllowAny


@api_view(['POST'])
@permission_classes([AllowAny])  # Protected by custom header check
def erlang_persist_messages(request):
    """
    Endpoint for Erlang service to persist messages back to Django
    Protected by X-Erlang-Service header
    """
    # Verify request is from Erlang service
    if request.headers.get('X-Erlang-Service') != 'messaging':
        return Response(
            {'error': 'Unauthorized'},
            status=status.HTTP_403_FORBIDDEN
        )
    
    messages = request.data.get('messages', [])
    
    if not messages:
        return Response(
            {'error': 'No messages provided'},
            status=status.HTTP_400_BAD_REQUEST
        )
    
    persisted_count = 0
    errors = []
    
    for msg_data in messages:
        try:
            # Get or create conversation
            conversation_id = msg_data.get('conversation_id')
            conversation = Conversation.objects.get(id=conversation_id)
            
            # Get users
            sender = User.objects.get(id=msg_data.get('sender_id'))
            receiver = User.objects.get(id=msg_data.get('receiver_id'))
            
            # Create message if it doesn't exist
            Message.objects.get_or_create(
                conversation=conversation,
                sender=sender,
                receiver=receiver,
                text=msg_data.get('text'),
                message_type=msg_data.get('message_type', 'text'),
                defaults={
                    'metadata': msg_data.get('metadata', {})
                }
            )
            persisted_count += 1
            
        except Exception as e:
            errors.append({
                'message': msg_data,
                'error': str(e)
            })
            logger.error(f"Failed to persist message from Erlang: {str(e)}")
    
    return Response({
        'persisted': persisted_count,
        'errors': errors
    })


@api_view(['GET'])
def erlang_health(request):
    """Check Erlang service health"""
    from services.erlang_messaging import erlang_client
    
    is_healthy = erlang_client.health_check()
    
    if is_healthy:
        stats = erlang_client.get_stats()
        return Response({
            'status': 'healthy',
            'erlang_stats': stats
        })
    else:
        return Response({
            'status': 'unhealthy',
            'message': 'Erlang service is not responding'
        }, status=status.HTTP_503_SERVICE_UNAVAILABLE)
