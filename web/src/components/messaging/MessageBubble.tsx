'use client';

import { Message } from '@/types/messaging-types';
import { formatDistanceToNow } from 'date-fns';
import { Check, CheckCheck } from 'lucide-react';

interface MessageBubbleProps {
  message: Message;
  isOwn: boolean;
  showTimestamp?: boolean;
}

export const MessageBubble = ({ message, isOwn, showTimestamp = true }: MessageBubbleProps) => {
  return (
    <div className={`flex ${isOwn ? 'justify-end' : 'justify-start'} mb-4`}>
      <div className={`max-w-[70%] ${isOwn ? 'order-2' : 'order-1'}`}>
        {!isOwn && message.sender_name && (
          <p className="text-xs text-primary-400 dark:text-sand-500 mb-1 px-3">
            {message.sender_name}
          </p>
        )}
        
        <div
          className={`rounded-2xl px-4 py-2 ${
            isOwn
              ? 'bg-secondary-600 text-white rounded-br-none'
              : 'bg-primary-100 dark:bg-primary-800 text-primary-900 dark:text-sand-50 rounded-bl-none'
          }`}
        >
          <p className="text-sm whitespace-pre-wrap break-words">
            {message.content}
          </p>
        </div>
        
        {showTimestamp && (
          <div className={`flex items-center gap-1 mt-1 px-3 ${
            isOwn ? 'justify-end' : 'justify-start'
          }`}>
            <p className="text-xs text-primary-300 dark:text-primary-500">
              {formatDistanceToNow(new Date(message.created_at), { addSuffix: true })}
            </p>
            
            {isOwn && (
              <span className={message.is_read ? 'text-blue-500' : 'text-primary-300 dark:text-primary-500'}>
                {message.is_read ? (
                  <CheckCheck className="h-3 w-3" aria-label="Read" />
                ) : (
                  <Check className="h-3 w-3" aria-label="Sent" />
                )}
              </span>
            )}
          </div>
        )}
      </div>
    </div>
  );
};
