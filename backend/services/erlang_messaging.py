"""
Erlang Messaging Service Client
Interfaces Django with the Erlang messaging service
"""
import requests
import logging
from django.conf import settings
from typing import Dict, List, Optional

logger = logging.getLogger(__name__)


class ErlangMessagingClient:
    """
    Client for interacting with Erlang messaging service
    Provides high-level API for sending messages through Erlang
    """
    
    def __init__(self, base_url: str = None):
        self.base_url = base_url or getattr(
            settings, 
            'ERLANG_MESSAGING_URL', 
            'http://localhost:8765'
        )
        self.timeout = 5  # seconds
    
    def health_check(self) -> bool:
        """Check if Erlang service is healthy"""
        try:
            response = requests.get(
                f"{self.base_url}/health",
                timeout=self.timeout
            )
            return response.status_code == 200
        except Exception as e:
            logger.error(f"Erlang health check failed: {str(e)}")
            return False
    
    def send_message(
        self, 
        conversation_id: int,
        sender_id: int,
        receiver_id: int,
        text: str,
        message_type: str = 'text',
        priority: str = 'normal',
        metadata: Optional[Dict] = None
    ) -> bool:
        """
        Send a message through Erlang service
        
        Args:
            conversation_id: ID of the conversation
            sender_id: User ID of sender
            receiver_id: User ID of receiver
            text: Message text
            message_type: Type of message (text, system, etc.)
            priority: Message priority (high, normal, low)
            metadata: Additional message metadata
        
        Returns:
            True if message was successfully queued, False otherwise
        """
        message = {
            'conversation_id': conversation_id,
            'sender_id': sender_id,
            'receiver_id': receiver_id,
            'text': text,
            'message_type': message_type,
            'priority': priority,
            'metadata': metadata or {}
        }
        
        try:
            response = requests.post(
                f"{self.base_url}/api/messages/send",
                json=message,
                timeout=self.timeout
            )
            
            if response.status_code == 200:
                logger.info(
                    f"Message queued in Erlang: {sender_id} -> {receiver_id}"
                )
                return True
            else:
                logger.error(
                    f"Erlang message queueing failed: {response.status_code} - {response.text}"
                )
                return False
                
        except Exception as e:
            logger.error(f"Error sending message to Erlang: {str(e)}")
            return False
    
    def get_user_messages(self, user_id: int, max_count: int = 10) -> List[Dict]:
        """
        Get pending messages for a user from Erlang queue
        
        Args:
            user_id: User ID
            max_count: Maximum number of messages to retrieve
        
        Returns:
            List of message dictionaries
        """
        try:
            response = requests.get(
                f"{self.base_url}/api/messages/queue/{user_id}",
                params={'max_count': max_count},
                timeout=self.timeout
            )
            
            if response.status_code == 200:
                data = response.json()
                return data.get('messages', [])
            else:
                logger.error(
                    f"Failed to get messages from Erlang: {response.status_code}"
                )
                return []
                
        except Exception as e:
            logger.error(f"Error getting messages from Erlang: {str(e)}")
            return []
    
    def broadcast_message(
        self,
        sender_id: int,
        user_ids: List[int],
        text: str,
        message_type: str = 'system',
        metadata: Optional[Dict] = None
    ) -> bool:
        """
        Broadcast a message to multiple users
        
        Args:
            sender_id: User ID of sender
            user_ids: List of receiver user IDs
            text: Message text
            message_type: Type of message
            metadata: Additional metadata
        
        Returns:
            True if broadcast was successful, False otherwise
        """
        message = {
            'sender_id': sender_id,
            'text': text,
            'message_type': message_type,
            'metadata': metadata or {}
        }
        
        try:
            response = requests.post(
                f"{self.base_url}/api/messages/broadcast",
                json={
                    'message': message,
                    'user_ids': user_ids
                },
                timeout=self.timeout
            )
            
            if response.status_code == 200:
                logger.info(
                    f"Broadcast message sent to {len(user_ids)} users"
                )
                return True
            else:
                logger.error(
                    f"Erlang broadcast failed: {response.status_code}"
                )
                return False
                
        except Exception as e:
            logger.error(f"Error broadcasting message: {str(e)}")
            return False
    
    def get_stats(self) -> Optional[Dict]:
        """Get Erlang service statistics"""
        try:
            response = requests.get(
                f"{self.base_url}/api/stats",
                timeout=self.timeout
            )
            
            if response.status_code == 200:
                return response.json()
            else:
                return None
                
        except Exception as e:
            logger.error(f"Error getting Erlang stats: {str(e)}")
            return None


# Global client instance
erlang_client = ErlangMessagingClient()
