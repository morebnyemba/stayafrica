'use client';

import { useEffect, useState } from 'react';

interface TypingIndicatorProps {
  userNames: string[];
  isVisible: boolean;
}

export const TypingIndicator = ({ userNames, isVisible }: TypingIndicatorProps) => {
  const [dots, setDots] = useState('');

  useEffect(() => {
    if (!isVisible) {
      setDots('');
      return;
    }

    const interval = setInterval(() => {
      setDots(prev => {
        if (prev.length >= 3) return '';
        return prev + '.';
      });
    }, 500);

    return () => clearInterval(interval);
  }, [isVisible]);

  if (!isVisible || userNames.length === 0) {
    return null;
  }

  const displayText = userNames.length === 1
    ? `${userNames[0]} is typing`
    : userNames.length === 2
    ? `${userNames[0]} and ${userNames[1]} are typing`
    : `${userNames.length} people are typing`;

  return (
    <div className="flex items-center gap-2 px-4 py-2 text-sm text-gray-500">
      <div className="flex gap-1">
        <span className="w-2 h-2 bg-gray-400 rounded-full animate-bounce" style={{ animationDelay: '0ms' }} />
        <span className="w-2 h-2 bg-gray-400 rounded-full animate-bounce" style={{ animationDelay: '150ms' }} />
        <span className="w-2 h-2 bg-gray-400 rounded-full animate-bounce" style={{ animationDelay: '300ms' }} />
      </div>
      <span aria-live="polite">
        {displayText}
        {dots}
      </span>
    </div>
  );
};
