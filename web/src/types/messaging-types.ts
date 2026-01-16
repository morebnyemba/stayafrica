export interface Message {
  id: string;
  conversation: string;
  sender: string;
  sender_name?: string;
  content: string;
  is_read: boolean;
  created_at: string;
  updated_at: string;
}

export interface Conversation {
  id: string;
  participants: string[];
  property?: string;
  property_name?: string;
  last_message?: Message;
  unread_count: number;
  created_at: string;
  updated_at: string;
}

export interface TypingStatus {
  user_id: string;
  user_name: string;
  is_typing: boolean;
}

export interface WebSocketMessage {
  type: 'message' | 'typing' | 'read' | 'error';
  data: Message | TypingStatus | { error: string };
}
