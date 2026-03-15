'use client';

import { Modal } from '@/components/ui';

interface NoticeModalProps {
  isOpen: boolean;
  title: string;
  message: string;
  buttonText?: string;
  onClose: () => void;
}

export function NoticeModal({
  isOpen,
  title,
  message,
  buttonText = 'OK',
  onClose,
}: NoticeModalProps) {
  return (
    <Modal isOpen={isOpen} onClose={onClose} title={title} size="sm">
      <div className="space-y-4">
        <p className="text-primary-700 whitespace-pre-wrap">{message}</p>
        <div className="flex justify-end">
          <button
            type="button"
            onClick={onClose}
            className="px-4 py-2 bg-secondary-600 text-white rounded-lg hover:bg-secondary-700 transition-colors"
          >
            {buttonText}
          </button>
        </div>
      </div>
    </Modal>
  );
}