"""
Automated Messaging Service
Handles automated responses, scheduled messages, and quick replies
"""
from django.utils import timezone
from datetime import timedelta
import logging

logger = logging.getLogger(__name__)


class AutomatedMessagingService:
    """Service for automated host messaging"""
    
    @staticmethod
    def trigger_automated_message(trigger_type, booking=None, conversation=None, context=None):
        """
        Trigger automated messages based on event type
        
        Args:
            trigger_type: Type of trigger (e.g., 'booking_confirmed')
            booking: Optional Booking instance
            conversation: Optional Conversation instance
            context: Optional dict with additional context variables
        """
        from apps.messaging.automated_models import AutomatedMessage, ScheduledMessage
        from apps.messaging.models import Message
        from apps.users.models import User
        
        if not conversation and booking:
            # Find or create conversation for booking
            conversation = AutomatedMessagingService._get_or_create_conversation(booking)
        
        if not conversation:
            logger.warning(f"No conversation found for trigger {trigger_type}")
            return
        
        # Get host from conversation
        host = conversation.property.host if hasattr(conversation, 'property') else None
        if not host:
            # Try to get host from booking
            if booking:
                host = booking.rental_property.host
        
        if not host:
            logger.warning(f"No host found for trigger {trigger_type}")
            return
        
        # Check if host has automated messages enabled
        try:
            settings = host.message_settings
            if not settings.enable_auto_responses:
                return
        except:
            # No settings configured
            return
        
        # Find matching automated messages
        automated_messages = AutomatedMessage.objects.filter(
            host=host,
            trigger_type=trigger_type,
            is_active=True
        )
        
        for auto_msg in automated_messages:
            # Prepare context
            msg_context = context or {}
            if booking:
                msg_context.update({
                    'guest_name': booking.guest.get_full_name() or booking.guest.email,
                    'property_name': booking.rental_property.title,
                    'check_in': booking.check_in.strftime('%Y-%m-%d'),
                    'check_out': booking.check_out.strftime('%Y-%m-%d'),
                    'booking_ref': booking.booking_ref,
                })
            
            # Get message text
            if auto_msg.template:
                rendered = auto_msg.template.render(msg_context)
                message_text = rendered['body']
            elif auto_msg.custom_message:
                message_text = auto_msg.custom_message.format(**msg_context)
            else:
                logger.warning(f"Automated message {auto_msg.id} has no template or custom message")
                continue
            
            # Determine sender and receiver
            participants = list(conversation.participants.all())
            if len(participants) < 2:
                logger.warning(f"Conversation {conversation.id} has insufficient participants")
                continue
            
            receiver = participants[0] if participants[0] != host else participants[1]
            
            # Schedule or send immediately
            if auto_msg.delay_hours > 0:
                scheduled_time = timezone.now() + timedelta(hours=auto_msg.delay_hours)
                ScheduledMessage.objects.create(
                    host=host,
                    conversation=conversation,
                    message_text=message_text,
                    scheduled_time=scheduled_time,
                    booking=booking,
                    status='pending'
                )
                logger.info(f"Scheduled automated message for {scheduled_time}")
            else:
                # Send immediately
                Message.objects.create(
                    conversation=conversation,
                    sender=host,
                    receiver=receiver,
                    text=message_text,
                    message_type='text'
                )
                logger.info(f"Sent automated message for trigger {trigger_type}")
                
                # Update analytics
                AutomatedMessagingService._update_analytics(host, 'automated_message_sent')
    
    @staticmethod
    def process_scheduled_messages():
        """
        Process and send due scheduled messages (run via Celery task)
        """
        from apps.messaging.automated_models import ScheduledMessage
        from apps.messaging.models import Message
        
        due_messages = ScheduledMessage.objects.filter(
            status='pending',
            scheduled_time__lte=timezone.now()
        ).select_related('host', 'conversation')
        
        count = 0
        for scheduled_msg in due_messages:
            try:
                # Get conversation participants
                participants = list(scheduled_msg.conversation.participants.all())
                if len(participants) < 2:
                    scheduled_msg.status = 'failed'
                    scheduled_msg.save()
                    continue
                
                receiver = participants[0] if participants[0] != scheduled_msg.host else participants[1]
                
                # Send message
                Message.objects.create(
                    conversation=scheduled_msg.conversation,
                    sender=scheduled_msg.host,
                    receiver=receiver,
                    text=scheduled_msg.message_text,
                    message_type='text'
                )
                
                # Mark as sent
                scheduled_msg.status = 'sent'
                scheduled_msg.sent_at = timezone.now()
                scheduled_msg.save()
                
                count += 1
                logger.info(f"Sent scheduled message {scheduled_msg.id}")
                
                # Update analytics
                AutomatedMessagingService._update_analytics(
                    scheduled_msg.host,
                    'automated_message_sent'
                )
                
            except Exception as e:
                logger.error(f"Error sending scheduled message {scheduled_msg.id}: {e}")
                scheduled_msg.status = 'failed'
                scheduled_msg.save()
        
        return count
    
    @staticmethod
    def get_quick_replies(host, search_term=None):
        """
        Get quick replies for a host
        
        Args:
            host: User instance (host)
            search_term: Optional search term to filter shortcuts
            
        Returns:
            QuerySet of QuickReply
        """
        from apps.messaging.automated_models import QuickReply
        
        quick_replies = QuickReply.objects.filter(
            host=host,
            is_active=True
        )
        
        if search_term:
            quick_replies = quick_replies.filter(
                shortcut__icontains=search_term
            )
        
        return quick_replies.order_by('-use_count', 'shortcut')
    
    @staticmethod
    def use_quick_reply(quick_reply_id, host):
        """
        Use a quick reply and increment its counter
        
        Args:
            quick_reply_id: QuickReply ID
            host: User instance (host)
            
        Returns:
            QuickReply instance or None
        """
        from apps.messaging.automated_models import QuickReply
        
        try:
            quick_reply = QuickReply.objects.get(id=quick_reply_id, host=host)
            quick_reply.increment_use_count()
            
            # Update analytics
            AutomatedMessagingService._update_analytics(host, 'quick_reply_used')
            
            return quick_reply
        except QuickReply.DoesNotExist:
            return None
    
    @staticmethod
    def calculate_response_time(host, start_date=None, end_date=None):
        """
        Calculate average response time for a host
        
        Args:
            host: User instance
            start_date: Optional start date
            end_date: Optional end date
            
        Returns:
            Average response time in minutes
        """
        from apps.messaging.models import Message
        from django.db.models import Avg, F, ExpressionWrapper, fields
        
        # Get messages where host is replying to guest inquiries
        messages = Message.objects.filter(
            sender=host
        )
        
        if start_date:
            messages = messages.filter(created_at__gte=start_date)
        if end_date:
            messages = messages.filter(created_at__lte=end_date)
        
        # Calculate time between receiving message and replying
        # This is a simplified version - in production, you'd want more sophisticated tracking
        response_times = []
        
        for msg in messages.select_related('conversation'):
            # Find previous message from guest in same conversation
            prev_msg = Message.objects.filter(
                conversation=msg.conversation,
                receiver=host,
                created_at__lt=msg.created_at
            ).order_by('-created_at').first()
            
            if prev_msg:
                time_diff = (msg.created_at - prev_msg.created_at).total_seconds() / 60
                response_times.append(time_diff)
        
        if response_times:
            return sum(response_times) / len(response_times)
        return None
    
    @staticmethod
    def _get_or_create_conversation(booking):
        """Get or create conversation for a booking"""
        from apps.messaging.models import Conversation
        
        # Try to find existing conversation for this booking
        conversation = Conversation.objects.filter(booking=booking).first()
        
        if not conversation:
            # Create new conversation
            conversation = Conversation.objects.create(
                booking=booking,
                property=booking.rental_property,
                subject=f"Booking {booking.booking_ref}"
            )
            conversation.participants.add(booking.guest, booking.rental_property.host)
        
        return conversation
    
    @staticmethod
    def _update_analytics(host, metric_type):
        """Update messaging analytics for host"""
        from apps.messaging.automated_models import MessageAnalytics
        
        today = timezone.now().date()
        
        analytics, created = MessageAnalytics.objects.get_or_create(
            host=host,
            date=today
        )
        
        if metric_type == 'automated_message_sent':
            analytics.automated_messages_sent += 1
        elif metric_type == 'quick_reply_used':
            analytics.quick_replies_used += 1
        elif metric_type == 'message_sent':
            analytics.messages_sent += 1
        elif metric_type == 'message_received':
            analytics.messages_received += 1
        
        analytics.save()
