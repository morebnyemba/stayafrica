'use client';

import { useEffect, useRef, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import axios from 'axios';
import { MessageBubble } from './MessageBubble';
import { MessageInput } from './MessageInput';
import { TypingIndicator } from './TypingIndicator';
import { useWebSocketChat } from './useWebSocketChat';
import { Message } from '@/types/messaging-types';
import { Loader2, WifiOff } from 'lucide-react';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8000';

interface ChatWindowProps {
  conversationId: string;
  userId: string;
  otherUserName?: string;
}

export const ChatWindow = ({ conversationId, userId, otherUserName }: ChatWindowProps) => {
  const [typingUsers, setTypingUsers] = useState<Map<string, string>>(new Map());
  const messagesEndRef = useRef<HTMLDivElement>(null);
  const messagesContainerRef = useRef<HTMLDivElement>(null);

  // Fetch message history
  const { data: historyMessages = [], isLoading } = useQuery<Message[]>({
    queryKey: ['messages', conversationId],
    queryFn: async () => {
      const token = localStorage.getItem('access_token');
      const response = await axios.get(
        `${API_BASE_URL}/api/v1/messages/?conversation=${conversationId}`,
        {
          headers: {
            Authorization: `Bearer ${token}`,
          },
        }
      );
      return response.data.results || response.data;
    },
  });

  const handleTypingChange = (typingUserId: string, isTyping: boolean) => {
    setTypingUsers(prev => {
      const next = new Map(prev);
      if (isTyping) {
        next.set(typingUserId, otherUserName || typingUserId);
      } else {
        next.delete(typingUserId);
      }
      return next;
    });
  };

  const { 
    isConnected, 
    messages: wsMessages, 
    error, 
    sendMessage,
    sendTypingStatus,
    reconnect 
  } = useWebSocketChat({
    conversationId,
    userId,
    onTyping: handleTypingChange,
  });

  // Combine history and WebSocket messages
  const allMessages = [...historyMessages, ...wsMessages];

  // Auto-scroll to bottom on new messages
  useEffect(() => {
    if (messagesEndRef.current) {
      messagesEndRef.current.scrollIntoView({ behavior: 'smooth' });
    }
  }, [allMessages]);

  const handleSendMessage = async (content: string) => {
    sendMessage(content);
  };

  const handleTypingStatusChange = (isTyping: boolean) => {
    sendTypingStatus(isTyping);
  };

  if (isLoading) {
    return (
      <div className="flex items-center justify-center h-full">
        <Loader2 className="h-8 w-8 animate-spin text-gray-400" />
      </div>
    );
  }

  return (
    <div className="flex flex-col h-full bg-white">
      {/* Connection status */}
      {!isConnected && (
        <div className="bg-yellow-50 border-b border-yellow-200 px-4 py-2 flex items-center justify-between">
          <div className="flex items-center gap-2 text-sm text-yellow-800">
            <WifiOff className="h-4 w-4" />
            <span>Connecting to chat server...</span>
          </div>
          <button
            onClick={reconnect}
            className="text-sm text-yellow-800 underline hover:text-yellow-900"
          >
            Retry
          </button>
        </div>
      )}

      {error && (
        <div className="bg-red-50 border-b border-red-200 px-4 py-2 text-sm text-red-800">
          {error}
        </div>
      )}

      {/* Messages */}
      <div
        ref={messagesContainerRef}
        className="flex-1 overflow-y-auto p-4 space-y-2"
      >
        {allMessages.length === 0 ? (
          <div className="flex items-center justify-center h-full text-gray-500">
            <p className="text-sm">No messages yet. Start the conversation!</p>
          </div>
        ) : (
          allMessages.map((message) => (
            <MessageBubble
              key={message.id}
              message={message}
              isOwn={message.sender === userId}
            />
          ))
        )}

        {/* Typing indicator */}
        <TypingIndicator
          userNames={Array.from(typingUsers.values())}
          isVisible={typingUsers.size > 0}
        />

        <div ref={messagesEndRef} />
      </div>

      {/* Input */}
      <MessageInput
        onSend={handleSendMessage}
        onTypingChange={handleTypingStatusChange}
        disabled={!isConnected}
      />
    </div>
  );
};
