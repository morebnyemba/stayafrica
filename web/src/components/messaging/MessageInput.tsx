'use client';

import { useState, useRef, useEffect } from 'react';
import { Send, Paperclip, Smile, Loader2 } from 'lucide-react';
import { useTypingIndicator } from './useTypingIndicator';

interface MessageInputProps {
  onSend: (content: string) => void;
  onTypingChange: (isTyping: boolean) => void;
  disabled?: boolean;
  placeholder?: string;
}

export const MessageInput = ({ 
  onSend, 
  onTypingChange, 
  disabled = false,
  placeholder = 'Type a message...'
}: MessageInputProps) => {
  const [content, setContent] = useState('');
  const [isSending, setIsSending] = useState(false);
  const textareaRef = useRef<HTMLTextAreaElement>(null);
  
  const { startTyping, stopTyping } = useTypingIndicator({
    onTypingChange,
    debounceMs: 1000,
  });

  useEffect(() => {
    // Auto-resize textarea
    if (textareaRef.current) {
      textareaRef.current.style.height = 'auto';
      textareaRef.current.style.height = `${Math.min(textareaRef.current.scrollHeight, 120)}px`;
    }
  }, [content]);

  const handleChange = (e: React.ChangeEvent<HTMLTextAreaElement>) => {
    setContent(e.target.value);
    if (e.target.value.trim()) {
      startTyping();
    } else {
      stopTyping();
    }
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!content.trim() || isSending || disabled) return;

    setIsSending(true);
    stopTyping();
    
    try {
      await onSend(content.trim());
      setContent('');
    } catch (error) {
      console.error('Error sending message:', error);
    } finally {
      setIsSending(false);
    }
  };

  const handleKeyDown = (e: React.KeyboardEvent<HTMLTextAreaElement>) => {
    if (e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault();
      handleSubmit(e);
    }
  };

  return (
    <form onSubmit={handleSubmit} className="border-t bg-white p-4">
      <div className="flex items-end gap-2">
        <button
          type="button"
          className="flex-shrink-0 p-2 text-gray-400 hover:text-gray-600 transition-colors"
          aria-label="Add attachment"
          disabled={disabled}
        >
          <Paperclip className="h-5 w-5" />
        </button>

        <div className="flex-1 relative">
          <textarea
            ref={textareaRef}
            value={content}
            onChange={handleChange}
            onKeyDown={handleKeyDown}
            placeholder={placeholder}
            disabled={disabled || isSending}
            className="w-full px-4 py-2 border border-gray-300 rounded-lg resize-none focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent disabled:bg-gray-50 disabled:cursor-not-allowed"
            rows={1}
            maxLength={2000}
            aria-label="Message input"
          />
        </div>

        <button
          type="button"
          className="flex-shrink-0 p-2 text-gray-400 hover:text-gray-600 transition-colors"
          aria-label="Add emoji"
          disabled={disabled}
        >
          <Smile className="h-5 w-5" />
        </button>

        <button
          type="submit"
          disabled={!content.trim() || isSending || disabled}
          className="flex-shrink-0 p-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
          aria-label="Send message"
        >
          {isSending ? (
            <Loader2 className="h-5 w-5 animate-spin" />
          ) : (
            <Send className="h-5 w-5" />
          )}
        </button>
      </div>

      {content.length > 1900 && (
        <p className="text-xs text-gray-500 mt-1 text-right">
          {2000 - content.length} characters remaining
        </p>
      )}
    </form>
  );
};
