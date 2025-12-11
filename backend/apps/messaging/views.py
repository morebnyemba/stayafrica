from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from django.db.models import Q
from apps.messaging.models import Message
from apps.messaging.serializers import MessageSerializer

class MessageViewSet(viewsets.ModelViewSet):
    serializer_class = MessageSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        """Return messages for current user"""
        user = self.request.user
        return Message.objects.filter(Q(sender=user) | Q(receiver=user))
    
    def perform_create(self, serializer):
        """Create message from current user"""
        serializer.save(sender=self.request.user)
    
    @action(detail=False, methods=['get'])
    def conversations(self, request):
        """Get list of conversations for current user"""
        user = request.user
        messages = Message.objects.filter(Q(sender=user) | Q(receiver=user))
        
        # Get unique conversation partners
        conversation_partners = set()
        for msg in messages:
            if msg.sender == user:
                conversation_partners.add(msg.receiver)
            else:
                conversation_partners.add(msg.sender)
        
        return Response([{'id': p.id, 'email': p.email} for p in conversation_partners])
    
    @action(detail=False, methods=['get'])
    def unread(self, request):
        """Get unread messages count"""
        unread_count = Message.objects.filter(receiver=request.user, is_read=False).count()
        return Response({'unread_count': unread_count})
