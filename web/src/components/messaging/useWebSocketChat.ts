import { useState, useEffect, useCallback, useRef } from 'react';
import { Message, WebSocketMessage } from '@/types/messaging-types';

interface UseWebSocketChatProps {
  conversationId: string;
  userId: string;
  onMessage?: (message: Message) => void;
  onTyping?: (userId: string, isTyping: boolean) => void;
}

const WS_BASE_URL = process.env.NEXT_PUBLIC_WS_BASE_URL || 
  (typeof window !== 'undefined' && window.location.protocol === 'https:' ? 'wss:' : 'ws:') + 
  '//' + (typeof window !== 'undefined' ? window.location.host : 'localhost:8000');

export const useWebSocketChat = ({ 
  conversationId, 
  userId, 
  onMessage, 
  onTyping 
}: UseWebSocketChatProps) => {
  const [isConnected, setIsConnected] = useState(false);
  const [messages, setMessages] = useState<Message[]>([]);
  const [error, setError] = useState<string | null>(null);
  const wsRef = useRef<WebSocket | null>(null);
  const reconnectTimeoutRef = useRef<NodeJS.Timeout>();
  const messageQueueRef = useRef<string[]>([]);
  const reconnectAttempts = useRef(0);
  const maxReconnectAttempts = 5;

  const connect = useCallback(() => {
    if (typeof window === 'undefined') return;

    try {
      const token = localStorage.getItem('access_token');
      const wsUrl = `${WS_BASE_URL}/ws/chat/${conversationId}/?token=${token}`;
      
      const ws = new WebSocket(wsUrl);
      wsRef.current = ws;

      ws.onopen = () => {
        setIsConnected(true);
        setError(null);
        reconnectAttempts.current = 0;
        
        // Send queued messages
        while (messageQueueRef.current.length > 0) {
          const queuedMessage = messageQueueRef.current.shift();
          if (queuedMessage) {
            ws.send(queuedMessage);
          }
        }
      };

      ws.onmessage = (event) => {
        try {
          const data: WebSocketMessage = JSON.parse(event.data);
          
          if (data.type === 'message' && 'id' in data.data) {
            const message = data.data as Message;
            setMessages(prev => [...prev, message]);
            onMessage?.(message);
          } else if (data.type === 'typing' && 'user_id' in data.data) {
            const typingData = data.data as { user_id: string; is_typing: boolean };
            onTyping?.(typingData.user_id, typingData.is_typing);
          } else if (data.type === 'error') {
            setError('error' in data.data ? data.data.error : 'Unknown error');
          }
        } catch (err) {
          console.error('Error parsing WebSocket message:', err);
        }
      };

      ws.onerror = (event) => {
        console.error('WebSocket error:', event);
        setError('Connection error occurred');
      };

      ws.onclose = () => {
        setIsConnected(false);
        
        // Attempt to reconnect with exponential backoff
        if (reconnectAttempts.current < maxReconnectAttempts) {
          const delay = Math.min(1000 * Math.pow(2, reconnectAttempts.current), 10000);
          reconnectTimeoutRef.current = setTimeout(() => {
            reconnectAttempts.current++;
            connect();
          }, delay);
        } else {
          setError('Failed to establish connection. Please refresh the page.');
        }
      };
    } catch (err) {
      console.error('Error creating WebSocket:', err);
      setError('Failed to connect to chat server');
    }
  }, [conversationId, onMessage, onTyping]);

  useEffect(() => {
    connect();

    return () => {
      if (reconnectTimeoutRef.current) {
        clearTimeout(reconnectTimeoutRef.current);
      }
      if (wsRef.current) {
        wsRef.current.close();
      }
    };
  }, [connect]);

  const sendMessage = useCallback((content: string) => {
    const messageData = JSON.stringify({
      type: 'message',
      content,
      sender: userId,
    });

    if (wsRef.current?.readyState === WebSocket.OPEN) {
      wsRef.current.send(messageData);
    } else {
      // Queue message if not connected
      messageQueueRef.current.push(messageData);
      // Try to reconnect if not already attempting
      if (!isConnected && reconnectAttempts.current < maxReconnectAttempts) {
        connect();
      }
    }
  }, [userId, isConnected, connect]);

  const sendTypingStatus = useCallback((isTyping: boolean) => {
    if (wsRef.current?.readyState === WebSocket.OPEN) {
      wsRef.current.send(JSON.stringify({
        type: 'typing',
        is_typing: isTyping,
        user_id: userId,
      }));
    }
  }, [userId]);

  return {
    isConnected,
    messages,
    error,
    sendMessage,
    sendTypingStatus,
    reconnect: connect,
  };
};
