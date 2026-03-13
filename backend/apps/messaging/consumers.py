"""
WebSocket Consumers for Real-time Messaging
"""
import json
import logging
from channels.generic.websocket import AsyncWebsocketConsumer
from channels.db import database_sync_to_async
from django.utils import timezone

logger = logging.getLogger(__name__)


class ChatConsumer(AsyncWebsocketConsumer):
    """
    WebSocket consumer for real-time chat messaging
    Handles: message sending, typing indicators, read receipts
    """
    
    async def connect(self):
        """Handle WebSocket connection"""
        self.user = self.scope["user"]
        
        # Reject unauthenticated users
        if not self.user or not self.user.is_authenticated:
            await self.close()
            return
        
        # Each user has a personal channel for receiving messages
        self.user_group_name = f"user_{self.user.id}"
        
        # Join user's personal group
        await self.channel_layer.group_add(
            self.user_group_name,
            self.channel_name
        )
        
        await self.accept()
        logger.info(f"WebSocket connected for user {self.user.email}")
        
        # Send connection confirmation
        await self.send(text_data=json.dumps({
            'type': 'connection_established',
            'message': 'Connected to chat server'
        }))
    
    async def disconnect(self, close_code):
        """Handle WebSocket disconnection"""
        if hasattr(self, 'user_group_name'):
            await self.channel_layer.group_discard(
                self.user_group_name,
                self.channel_name
            )
        
        user_identifier = self.user.email if self.user and self.user.is_authenticated else 'unknown'
        logger.info(f"WebSocket disconnected for user {user_identifier}")
    
    async def receive(self, text_data):
        """Handle incoming WebSocket messages"""
        try:
            data = json.loads(text_data)
            message_type = data.get('type')
            
            if message_type == 'chat_message':
                await self.handle_chat_message(data)
            elif message_type == 'typing_indicator':
                await self.handle_typing_indicator(data)
            elif message_type == 'read_receipt':
                await self.handle_read_receipt(data)
            elif message_type == 'join_conversation':
                await self.handle_join_conversation(data)
            elif message_type == 'leave_conversation':
                await self.handle_leave_conversation(data)
            elif message_type == 'support_request':
                await self.handle_support_request(data)
            elif message_type == 'support_agent_join':
                await self.handle_agent_join(data)
            else:
                logger.warning(f"Unknown message type: {message_type}")
                
        except json.JSONDecodeError:
            logger.error("Invalid JSON received")
            await self.send(text_data=json.dumps({
                'type': 'error',
                'message': 'Invalid message format'
            }))
        except Exception as e:
            logger.error(f"Error handling message: {e}")
            await self.send(text_data=json.dumps({
                'type': 'error',
                'message': 'Failed to process message'
            }))
    
    async def handle_chat_message(self, data):
        """Handle sending a chat message.
        
        If receiver_id is provided → direct user-to-user message.
        If receiver_id is None/missing → support-ticket mode:
            message is saved and broadcast to the conversation group so all
            participants (user + assigned agent) receive it in real time.
        """
        conversation_id = data.get('conversation_id')
        receiver_id = data.get('receiver_id')  # May be None for support chats
        text = data.get('text', '').strip()

        if not conversation_id or not text:
            await self.send(text_data=json.dumps({
                'type': 'error',
                'message': 'Missing required fields: conversation_id, text'
            }))
            return

        if receiver_id:
            # ── Normal DM flow ──────────────────────────────────────────────
            message = await self.save_message(
                conversation_id=conversation_id,
                sender_id=self.user.id,
                receiver_id=receiver_id,
                text=text
            )
            if not message:
                await self.send(text_data=json.dumps({'type': 'error', 'message': 'Failed to save message'}))
                return

            await self.channel_layer.group_send(
                f"user_{receiver_id}",
                {
                    'type': 'chat_message_handler',
                    'message_id': message['id'],
                    'conversation_id': conversation_id,
                    'sender_id': self.user.id,
                    'sender_name': message['sender_name'],
                    'text': text,
                    'created_at': message['created_at'],
                }
            )

            await self.send(text_data=json.dumps({
                'type': 'message_sent',
                'message_id': message['id'],
                'conversation_id': conversation_id,
                'created_at': message['created_at'],
                'status': 'delivered'
            }))

            try:
                from services.notification_service import NotificationService
                from apps.messaging.models import Message
                message_obj = await database_sync_to_async(Message.objects.get)(id=message['id'])
                await database_sync_to_async(NotificationService.send_new_message)(message_obj)
            except Exception as e:
                logger.warning(f"Failed to send push notification: {e}")

        else:
            # ── Support-ticket / conversation-group flow ────────────────────
            message = await self.save_support_message(
                conversation_id=conversation_id,
                sender_id=self.user.id,
                text=text
            )
            if not message:
                await self.send(text_data=json.dumps({'type': 'error', 'message': 'Failed to save message'}))
                return

            payload = {
                'type': 'chat_message_handler',
                'message_id': message['id'],
                'conversation_id': conversation_id,
                'sender_id': self.user.id,
                'sender_name': message['sender_name'],
                'text': text,
                'created_at': message['created_at'],
            }

            # Broadcast to everyone joined to this conversation room
            await self.channel_layer.group_send(
                f"conversation_{conversation_id}",
                payload
            )

            # Also notify all other participants via their personal user groups
            # as a fallback (they may not have joined the conversation room)
            participant_ids = await self.get_conversation_participant_ids(conversation_id)
            for pid in participant_ids:
                if pid != self.user.id:
                    await self.channel_layer.group_send(
                        f"user_{pid}",
                        payload
                    )

            await self.send(text_data=json.dumps({
                'type': 'message_sent',
                'message_id': message['id'],
                'conversation_id': conversation_id,
                'created_at': message['created_at'],
                'status': 'delivered'
            }))
    
    async def handle_typing_indicator(self, data):
        """Handle typing indicator"""
        conversation_id = data.get('conversation_id')
        receiver_id = data.get('receiver_id')
        is_typing = data.get('is_typing', True)
        
        if not conversation_id or not receiver_id:
            return
        
        # Send typing indicator to receiver
        await self.channel_layer.group_send(
            f"user_{receiver_id}",
            {
                'type': 'typing_indicator_handler',
                'conversation_id': conversation_id,
                'user_id': self.user.id,
                'user_name': self.user.get_full_name() or self.user.email,
                'is_typing': is_typing,
            }
        )
    
    async def handle_read_receipt(self, data):
        """Handle read receipt"""
        message_ids = data.get('message_ids', [])
        
        if not message_ids:
            return
        
        # Mark messages as read
        await self.mark_messages_as_read(message_ids, self.user.id)
        
        # Get sender IDs for these messages
        sender_ids = await self.get_message_senders(message_ids)
        
        # Send read receipts to senders
        for sender_id in sender_ids:
            await self.channel_layer.group_send(
                f"user_{sender_id}",
                {
                    'type': 'read_receipt_handler',
                    'message_ids': message_ids,
                    'read_by_id': self.user.id,
                    'read_at': timezone.now().isoformat(),
                }
            )
    
    async def handle_join_conversation(self, data):
        """Handle joining a conversation room"""
        conversation_id = data.get('conversation_id')
        
        if not conversation_id:
            return
        
        # Verify user is participant in conversation
        is_participant = await self.verify_conversation_participant(conversation_id, self.user.id)
        
        if not is_participant:
            await self.send(text_data=json.dumps({
                'type': 'error',
                'message': 'You are not a participant in this conversation'
            }))
            return
        
        # Join conversation group
        self.conversation_group_name = f"conversation_{conversation_id}"
        await self.channel_layer.group_add(
            self.conversation_group_name,
            self.channel_name
        )
        
        await self.send(text_data=json.dumps({
            'type': 'conversation_joined',
            'conversation_id': conversation_id
        }))
    
    async def handle_leave_conversation(self, data):
        """Handle leaving a conversation room"""
        conversation_id = data.get('conversation_id')
        
        if not conversation_id:
            return
        
        conversation_group_name = f"conversation_{conversation_id}"
        await self.channel_layer.group_discard(
            conversation_group_name,
            self.channel_name
        )
        
        await self.send(text_data=json.dumps({
            'type': 'conversation_left',
            'conversation_id': conversation_id
        }))
    
    async def handle_support_request(self, data):
        """Handle incoming support requests from users"""
        text = data.get('text', '').strip()
        category = data.get('category', 'other')
        
        if not text:
            await self.send(text_data=json.dumps({
                'type': 'error',
                'message': 'Message text is required for support requests'
            }))
            return
            
        # Broadcast the new request to the global support agents group
        # Real logic for creating the SupportTicket will happen via a REST API, 
        # but this allows real-time UI updates for actively connected agents.
        await self.channel_layer.group_send(
            "support_agents",
            {
                'type': 'new_support_request_handler',
                'user_id': self.user.id,
                'user_name': self.user.get_full_name() or self.user.email,
                'text': text,
                'category': category
            }
        )
        
    async def handle_agent_join(self, data):
        """Handle support agent joining a user's support ticket"""
        conversation_id = data.get('conversation_id')
        user_id = data.get('user_id')  # The guest/host who made the request
        
        if not conversation_id or not user_id:
            return
            
        # Notify the specific user that an agent has joined
        await self.channel_layer.group_send(
            f"user_{user_id}",
            {
                'type': 'support_agent_joined_handler',
                'conversation_id': conversation_id,
                'agent_id': self.user.id,
                'agent_name': self.user.get_full_name() or self.user.email,
            }
        )

    
    # Handler methods called by channel layer
    async def chat_message_handler(self, event):
        """Send chat message to WebSocket — skip self-echo to avoid duplicates"""
        if event.get('sender_id') == self.user.id:
            return

        await self.send(text_data=json.dumps({
            'type': 'new_message',
            'message_id': event['message_id'],
            'conversation_id': event['conversation_id'],
            'sender_id': event['sender_id'],
            'sender_name': event['sender_name'],
            'text': event['text'],
            'created_at': event['created_at'],
        }))
    
    async def typing_indicator_handler(self, event):
        """Send typing indicator to WebSocket"""
        await self.send(text_data=json.dumps({
            'type': 'typing_indicator',
            'conversation_id': event['conversation_id'],
            'user_id': event['user_id'],
            'user_name': event['user_name'],
            'is_typing': event['is_typing'],
        }))
    
    async def read_receipt_handler(self, event):
        """Send read receipt to WebSocket"""
        await self.send(text_data=json.dumps({
            'type': 'read_receipt',
            'message_ids': event['message_ids'],
            'read_by_id': event['read_by_id'],
            'read_at': event['read_at'],
        }))
        
    async def new_support_request_handler(self, event):
        """Send new support request notification to agents"""
        await self.send(text_data=json.dumps({
            'type': 'new_support_request',
            'user_id': event['user_id'],
            'user_name': event['user_name'],
            'text': event['text'],
            'category': event['category']
        }))
        
    async def support_agent_joined_handler(self, event):
        """Send agent join notification to user"""
        await self.send(text_data=json.dumps({
            'type': 'support_agent_joined',
            'conversation_id': event['conversation_id'],
            'agent_id': event['agent_id'],
            'agent_name': event['agent_name']
        }))
    
    # Database operations
    @database_sync_to_async
    def save_message(self, conversation_id, sender_id, receiver_id, text):
        """Save message to database"""
        try:
            from apps.messaging.models import Message, Conversation
            from apps.users.models import User
            
            conversation = Conversation.objects.get(id=conversation_id)
            sender = User.objects.get(id=sender_id)
            receiver = User.objects.get(id=receiver_id)
            
            message = Message.objects.create(
                conversation=conversation,
                sender=sender,
                receiver=receiver,
                text=text,
                message_type='text'
            )
            
            # Update conversation timestamp
            conversation.updated_at = timezone.now()
            conversation.save(update_fields=['updated_at'])
            
            return {
                'id': message.id,
                'sender_name': sender.get_full_name() or sender.email,
                'created_at': message.created_at.isoformat(),
            }
        except Exception as e:
            logger.error(f"Error saving message: {e}")
            return None

    @database_sync_to_async
    def save_support_message(self, conversation_id, sender_id, text):
        """Save a support-chat message (no explicit receiver — support ticket mode)."""
        try:
            from apps.messaging.models import Message, Conversation
            from apps.users.models import User

            conversation = Conversation.objects.get(id=conversation_id)
            sender = User.objects.get(id=sender_id)

            message = Message.objects.create(
                conversation=conversation,
                sender=sender,
                receiver=None,  # Support messages have no single receiver
                text=text,
                message_type='support_request' if not sender.is_support_agent else 'support_response'
            )

            conversation.updated_at = timezone.now()
            conversation.save(update_fields=['updated_at'])

            return {
                'id': message.id,
                'sender_name': sender.get_full_name() or sender.email,
                'created_at': message.created_at.isoformat(),
            }
        except Exception as e:
            logger.error(f"Error saving support message: {e}")
            return None

    @database_sync_to_async
    def mark_messages_as_read(self, message_ids, user_id):
        """Mark messages as read — handles both DM and support messages"""
        try:
            from apps.messaging.models import Message
            from django.db.models import Q
            Message.objects.filter(
                id__in=message_ids,
                is_read=False
            ).filter(
                Q(receiver_id=user_id) |
                Q(receiver__isnull=True, conversation__participants__id=user_id)
            ).exclude(sender_id=user_id).update(is_read=True, read_at=timezone.now())
        except Exception as e:
            logger.error(f"Error marking messages as read: {e}")
    
    @database_sync_to_async
    def get_message_senders(self, message_ids):
        """Get sender IDs for messages"""
        try:
            from apps.messaging.models import Message
            return list(Message.objects.filter(
                id__in=message_ids
            ).values_list('sender_id', flat=True).distinct())
        except Exception as e:
            logger.error(f"Error getting message senders: {e}")
            return []
    
    @database_sync_to_async
    def verify_conversation_participant(self, conversation_id, user_id):
        """Verify user is participant in conversation"""
        try:
            from apps.messaging.models import Conversation
            return Conversation.objects.filter(
                id=conversation_id,
                participants__id=user_id
            ).exists()
        except Exception as e:
            logger.error(f"Error verifying participant: {e}")
            return False

    @database_sync_to_async
    def get_conversation_participant_ids(self, conversation_id):
        """Get all participant IDs for a conversation"""
        try:
            from apps.messaging.models import Conversation
            conversation = Conversation.objects.get(id=conversation_id)
            return list(conversation.participants.values_list('id', flat=True))
        except Exception as e:
            logger.error(f"Error getting conversation participants: {e}")
            return []
