/**
 * Modal Component - Accessible overlay dialog
 */
'use client';

import React, { useEffect } from 'react';
import { cn } from '@/lib/utils';
import { X } from 'lucide-react';

interface ModalProps {
  isOpen: boolean;
  onClose: () => void;
  title?: string;
  children: React.ReactNode;
  actions?: React.ReactNode;
  size?: 'sm' | 'md' | 'lg' | 'xl';
  closeButton?: boolean;
}

const sizeClasses = {
  sm: 'max-w-sm',
  md: 'max-w-md',
  lg: 'max-w-lg',
  xl: 'max-w-xl',
};

export const Modal: React.FC<ModalProps> = ({
  isOpen,
  onClose,
  title,
  children,
  actions,
  size = 'md',
  closeButton = true,
}) => {
  useEffect(() => {
    const handleEsc = (e: KeyboardEvent) => {
      if (e.key === 'Escape') onClose();
    };
    if (isOpen) {
      document.addEventListener('keydown', handleEsc);
      document.body.style.overflow = 'hidden';
      return () => {
        document.removeEventListener('keydown', handleEsc);
        document.body.style.overflow = 'unset';
      };
    }
  }, [isOpen, onClose]);

  if (!isOpen) return null;

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center">
      {/* Backdrop */}
      <div
        className="absolute inset-0 bg-black/50 transition-opacity"
        onClick={onClose}
        role="presentation"
      />
      {/* Modal */}
      <div
        className={cn(
          'relative z-50 w-full rounded-2xl bg-white p-6 shadow-xl',
          sizeClasses[size],
          'mx-4'
        )}
        role="dialog"
        aria-modal="true"
        aria-labelledby="modal-title"
      >
        {/* Header */}
        <div className="mb-4 flex items-center justify-between">
          {title && (
            <h2 id="modal-title" className="text-2xl font-bold text-neutral-900">
              {title}
            </h2>
          )}
          {closeButton && (
            <button
              onClick={onClose}
              className="rounded-lg p-1 text-neutral-500 hover:bg-neutral-100 hover:text-neutral-900"
              aria-label="Close modal"
            >
              <X className="h-6 w-6" />
            </button>
          )}
        </div>

        {/* Body */}
        <div className="mb-6 max-h-[60vh] overflow-y-auto">{children}</div>

        {/* Footer */}
        {actions && <div className="flex gap-3 justify-end border-t border-neutral-200 pt-4">{actions}</div>}
      </div>
    </div>
  );
};
