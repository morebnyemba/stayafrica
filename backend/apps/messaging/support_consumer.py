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
            # Agents might send typing indicators or ticket claims through here in the future
        except Exception as e:
            logger.error(f"Error handling support message: {e}")
            
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
