import json
import logging
from channels.generic.websocket import AsyncWebsocketConsumer
from channels.db import database_sync_to_async
from django.utils import timezone

logger = logging.getLogger(__name__)


class SupportConsumer(AsyncWebsocketConsumer):
    """
    WebSocket consumer for real-time support agent dashboard.
    Handles: receiving new support requests, ticket updates, and agent chat.
    """

    async def connect(self):
        """Handle WebSocket connection"""
        self.user = self.scope.get("user")

        # Reject unauthenticated users or non-agents
        if not self.user or not self.user.is_authenticated or not getattr(self.user, 'is_support_agent', False):
            await self.close()
            return

        # Track multiple conversation groups the agent has joined
        self.joined_conversations = set()

        # Join global support agents group
        await self.channel_layer.group_add(
            "support_agents",
            self.channel_name
        )

        await self.accept()
        logger.info(f"Support WebSocket connected for agent {self.user.email}")

    async def disconnect(self, close_code):
        """Handle WebSocket disconnection"""
        if hasattr(self, 'user') and self.user and self.user.is_authenticated:
            await self.channel_layer.group_discard(
                "support_agents",
                self.channel_name
            )
            # Leave all joined conversation groups
            for conv_group in getattr(self, 'joined_conversations', set()):
                await self.channel_layer.group_discard(
                    conv_group,
                    self.channel_name
                )
            logger.info(f"Support WebSocket disconnected for agent {self.user.email}")

    async def receive(self, text_data):
        """Handle incoming WebSocket messages from agents"""
        try:
            data = json.loads(text_data)
            message_type = data.get('type')

            if message_type == 'chat_message':
                await self.handle_chat_message(data)
            elif message_type == 'join_conversation':
                await self.handle_join_conversation(data)
            elif message_type == 'leave_conversation':
                await self.handle_leave_conversation(data)
            else:
                logger.warning(f"SupportConsumer ignored unknown message type: {message_type}")

        except Exception as e:
            logger.error(f"Error handling support message: {e}")
            await self.send(text_data=json.dumps({
                'type': 'error',
                'message': 'Failed to process message'
            }))

    async def handle_join_conversation(self, data):
        """Handle agent joining a specific ticket conversation room"""
        conversation_id = data.get('conversation_id')
        if not conversation_id:
            return

        group_name = f"conversation_{conversation_id}"
        self.joined_conversations.add(group_name)
        await self.channel_layer.group_add(
            group_name,
            self.channel_name
        )

        await self.send(text_data=json.dumps({
            'type': 'conversation_joined',
            'conversation_id': conversation_id
        }))

    async def handle_leave_conversation(self, data):
        """Handle agent leaving a conversation room"""
        conversation_id = data.get('conversation_id')
        if not conversation_id:
            return

        group_name = f"conversation_{conversation_id}"
        self.joined_conversations.discard(group_name)
        await self.channel_layer.group_discard(
            group_name,
            self.channel_name
        )

        await self.send(text_data=json.dumps({
            'type': 'conversation_left',
            'conversation_id': conversation_id
        }))

    async def handle_chat_message(self, data):
        """Handle agent sending a reply to a support ticket"""
        conversation_id = data.get('conversation_id')
        text = data.get('text', '').strip()

        if not conversation_id or not text:
            await self.send(text_data=json.dumps({
                'type': 'error',
                'message': 'Missing required fields: conversation_id, text'
            }))
            return

        # Save the message directly (no dummy consumer instantiation)
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

        # Broadcast to everyone in the conversation room (user + agent)
        await self.channel_layer.group_send(
            f"conversation_{conversation_id}",
            payload
        )

        # Also notify the requester's personal user group as a fallback
        # (in case they're connected but haven't joined the conversation room)
        requester_id = await self.get_conversation_requester(conversation_id)
        if requester_id and requester_id != self.user.id:
            await self.channel_layer.group_send(
                f"user_{requester_id}",
                payload
            )

        await self.send(text_data=json.dumps({
            'type': 'message_sent',
            'message_id': message['id'],
            'conversation_id': conversation_id,
            'created_at': message['created_at'],
            'status': 'delivered'
        }))

    async def chat_message_handler(self, event):
        """Receive broadcasted chat messages and forward to WebSocket"""
        # Avoid echoing back the agent's own messages (they get message_sent)
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

    # Handlers for messages broadcast to "support_agents" group

    async def new_support_request_handler(self, event):
        """Broadcast new support requests to connected agents"""
        await self.send(text_data=json.dumps({
            'type': 'new_support_request',
            'user_id': event['user_id'],
            'user_name': event['user_name'],
            'text': event['text'],
            'category': event['category']
        }))

    async def ticket_status_updated_handler(self, event):
        """Broadcast ticket status changes"""
        await self.send(text_data=json.dumps({
            'type': 'ticket_status_updated',
            'ticket_id': event['ticket_id'],
            'status': event['status'],
            'updated_by': event.get('updated_by')
        }))

    # Database operations

    @database_sync_to_async
    def save_support_message(self, conversation_id, sender_id, text):
        """Save a support-chat message (no explicit receiver)."""
        try:
            from apps.messaging.models import Message, Conversation
            from apps.users.models import User

            conversation = Conversation.objects.get(id=conversation_id)
            sender = User.objects.get(id=sender_id)

            message = Message.objects.create(
                conversation=conversation,
                sender=sender,
                receiver=None,
                text=text,
                message_type='support_response' if getattr(sender, 'is_support_agent', False) else 'support_request'
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
    def get_conversation_requester(self, conversation_id):
        """Get the non-agent participant (requester) of a support conversation."""
        try:
            from apps.messaging.models import Conversation
            conversation = Conversation.objects.get(id=conversation_id)
            # The requester is the participant who is NOT a support agent
            for participant in conversation.participants.all():
                if not getattr(participant, 'is_support_agent', False):
                    return participant.id
            # Fallback: return first participant
            first = conversation.participants.first()
            return first.id if first else None
        except Exception as e:
            logger.error(f"Error getting conversation requester: {e}")
            return None
