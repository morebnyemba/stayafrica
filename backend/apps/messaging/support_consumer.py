import json
import logging
from channels.generic.websocket import AsyncWebsocketConsumer

logger = logging.getLogger(__name__)

class SupportConsumer(AsyncWebsocketConsumer):
    """
    WebSocket consumer for real-time support agent dashboard
    Handles: receiving new support requests, ticket updates
    """
    
    async def connect(self):
        """Handle WebSocket connection"""
        self.user = self.scope.get("user")
        
        # Reject unauthenticated users or non-agents
        if not self.user or not self.user.is_authenticated or not hasattr(self.user, 'is_support_agent') or not self.user.is_support_agent:
            await self.close()
            return
            
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
            
        self.conversation_group_name = f"conversation_{conversation_id}"
        await self.channel_layer.group_add(
            self.conversation_group_name,
            self.channel_name
        )
        
        await self.send(text_data=json.dumps({
            'type': 'conversation_joined',
            'conversation_id': conversation_id
        }))

    async def handle_chat_message(self, data):
        """Handle agent sending a reply to a support ticket"""
        from apps.messaging.consumers import ChatConsumer

        conversation_id = data.get('conversation_id')
        text = data.get('text', '').strip()

        if not conversation_id or not text:
            await self.send(text_data=json.dumps({
                'type': 'error',
                'message': 'Missing required fields: conversation_id, text'
            }))
            return

        # Reuse the ChatConsumer's database save logic for support messages
        # Agents saving a message will have message_type='support_response'
        dummy_consumer = ChatConsumer()
        message = await dummy_consumer.save_support_message(
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

        # Broadcast to everyone joined to this ticket (user + agent)
        await self.channel_layer.group_send(
            f"conversation_{conversation_id}",
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
