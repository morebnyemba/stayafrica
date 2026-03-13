from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from .models import SupportTicket, SupportTicketEvent, CannedResponse, BugReport
from .serializers import SupportTicketSerializer, CannedResponseSerializer, BugReportSerializer
from apps.messaging.models import Conversation, Message
from django.db.models import Q
from django.utils import timezone

class SupportTicketViewSet(viewsets.ModelViewSet):
    permission_classes = [IsAuthenticated]
    serializer_class = SupportTicketSerializer

    def get_queryset(self):
        user = self.request.user
        if getattr(user, 'is_support_agent', False):
            # Agents see all tickets
            queryset = SupportTicket.objects.all()
            
            # Filter by assignment
            assignee = self.request.query_params.get('assigned_to')
            if assignee == 'me':
                queryset = queryset.filter(assigned_agent=user)
            elif assignee == 'unassigned':
                queryset = queryset.filter(assigned_agent__isnull=True)
                
            # Filter by status
            status_param = self.request.query_params.get('status')
            if status_param:
                queryset = queryset.filter(status=status_param)
        else:
            # Users only see their own tickets
            queryset = SupportTicket.objects.filter(requester=user)
            
        return queryset.select_related('requester', 'assigned_agent', 'conversation')

    def perform_create(self, serializer):
        user = self.request.user
        subject = serializer.validated_data.get('subject', 'Support Request')
        initial_message = self.request.data.get('initial_message')
        
        # 1. Create a support conversation
        conversation = Conversation.objects.create(
            subject=subject,
            conversation_type='support'
        )
        conversation.participants.add(user)
        
        # 2. Create the ticket linked to conversation
        ticket = serializer.save(requester=user, conversation=conversation)
        
        # 3. Create initial message if provided
        if initial_message:
            Message.objects.create(
                conversation=conversation,
                sender=user,
                # receiver is technically empty until an agent joins and responds
                # The ChatConsumer will broadcast to the support_agents group
                receiver=user, # Temp fallback for DB constraints
                text=initial_message,
                message_type='support_request'
            )
            
        SupportTicketEvent.objects.create(
            ticket=ticket,
            actor=user,
            event_type='created',
            note='Ticket created via API'
        )

    @action(detail=True, methods=['post'])
    def assign(self, request, pk=None):
        if not getattr(request.user, 'is_support_agent', False):
            return Response({'error': 'Permission denied'}, status=status.HTTP_403_FORBIDDEN)
            
        ticket = self.get_object()
        user_id = request.data.get('user_id')
        
        if user_id == 'me':
            agent = request.user
        else:
            return Response({'error': 'Explicit assignment not yet implemented'}, status=status.HTTP_400_BAD_REQUEST)
            
        old_agent = ticket.assigned_agent
        ticket.assigned_agent = agent
        if ticket.status == 'open':
            ticket.status = 'assigned'
        ticket.save()
        
        # Add agent to conversation participants
        ticket.conversation.participants.add(agent)
        
        # Send system message
        Message.objects.create(
            conversation=ticket.conversation,
            sender=agent,
            receiver=ticket.requester,
            text=f"An agent ({agent.first_name or agent.email}) has joined the chat.",
            message_type='system'
        )
        
        SupportTicketEvent.objects.create(
            ticket=ticket,
            actor=request.user,
            event_type='assigned',
            old_value=str(old_agent.id) if old_agent else 'unassigned',
            new_value=str(agent.id),
            note='Agent claimed ticket'
        )
        
        return Response(self.get_serializer(ticket).data)

    @action(detail=True, methods=['post'])
    def change_status(self, request, pk=None):
        if not getattr(request.user, 'is_support_agent', False):
            return Response({'error': 'Permission denied'}, status=status.HTTP_403_FORBIDDEN)
            
        ticket = self.get_object()
        new_status = request.data.get('status')
        
        if new_status not in dict(SupportTicket.STATUS_CHOICES):
            return Response({'error': 'Invalid status'}, status=status.HTTP_400_BAD_REQUEST)
            
        old_status = ticket.status
        ticket.status = new_status
        if new_status in ['resolved', 'closed']:
            ticket.resolved_at = timezone.now()
        ticket.save()
        
        SupportTicketEvent.objects.create(
            ticket=ticket,
            actor=request.user,
            event_type='status_change',
            old_value=old_status,
            new_value=new_status,
            note=request.data.get('note', '')
        )
        
        return Response(self.get_serializer(ticket).data)


class CannedResponseViewSet(viewsets.ModelViewSet):
    permission_classes = [IsAuthenticated]
    serializer_class = CannedResponseSerializer
    queryset = CannedResponse.objects.filter(is_active=True)
    
    def check_permissions(self, request):
        super().check_permissions(request)
        if request.method not in ['GET', 'HEAD', 'OPTIONS'] and not getattr(request.user, 'is_support_agent', False):
            self.permission_denied(request, message="Only support agents can edit canned responses.")

    def get_queryset(self):
        category = self.request.query_params.get('category')
        if category:
            return self.queryset.filter(category=category)
        return self.queryset


class BugReportViewSet(viewsets.ModelViewSet):
    permission_classes = [IsAuthenticated]
    serializer_class = BugReportSerializer
    
    def get_queryset(self):
        user = self.request.user
        if getattr(user, 'is_support_agent', False):
            queryset = BugReport.objects.all()
            status_param = self.request.query_params.get('status')
            if status_param:
                queryset = queryset.filter(status=status_param)
        else:
            queryset = BugReport.objects.filter(reporter=user)
        return queryset

    def perform_create(self, serializer):
        serializer.save(reporter=self.request.user)
        
    @action(detail=True, methods=['post'])
    def escalate(self, request, pk=None):
        if not getattr(request.user, 'is_support_agent', False):
            return Response({'error': 'Permission denied'}, status=status.HTTP_403_FORBIDDEN)
            
        bug = self.get_object()
        if bug.support_ticket:
            return Response({'error': 'Bug is already escalated'}, status=status.HTTP_400_BAD_REQUEST)
            
        # 1. Create a support conversation
        conversation = Conversation.objects.create(
            subject=f"Bug Escalation: {bug.title}",
            conversation_type='support'
        )
        conversation.participants.add(bug.reporter)
        conversation.participants.add(request.user)
        
        # 2. Create Support Ticket
        ticket = SupportTicket.objects.create(
            conversation=conversation,
            requester=bug.reporter,
            assigned_agent=request.user,
            category='technical',
            priority='high' if bug.severity in ['critical', 'major'] else 'medium',
            subject=f"Bug Escalation: {bug.title}"
        )
        
        # 3. Link back
        bug.support_ticket = ticket
        bug.status = 'in_progress'
        bug.assigned_to = request.user
        bug.save()
        
        return Response({
            'status': 'Escalated',
            'ticket_id': ticket.id
        })
