'use client';

import React from 'react';
import { Calendar } from 'lucide-react';

interface FlexibilityToggleProps {
  isFlexible: boolean;
  onChange: (flexible: boolean) => void;
  className?: string;
}

export default function FlexibilityToggle({
  isFlexible,
  onChange,
  className = '',
}: FlexibilityToggleProps) {
  return (
    <button
      onClick={() => onChange(!isFlexible)}
      className={`
        flex items-center gap-2 px-4 py-2.5 rounded-lg border-2 font-medium transition
        ${isFlexible 
          ? 'border-primary-600 bg-primary-50 text-primary-700' 
          : 'border-gray-300 bg-white text-gray-700 hover:border-gray-400'
        }
        ${className}
      `}
    >
      <Calendar className="w-5 h-5" />
      <span>I'm flexible</span>
      {isFlexible && (
        <span className="ml-1 px-2 py-0.5 bg-primary-600 text-white text-xs rounded-full">
          ON
        </span>
      )}
    </button>
  );
}
