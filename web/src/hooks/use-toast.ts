import { useState, useCallback } from 'react';

export interface ToastProps {
  title?: string;
  description?: string;
  variant?: 'default' | 'destructive';
  duration?: number;
}

export function useToast() {
  const [toasts, setToasts] = useState<ToastProps[]>([]);

  const toast = useCallback(({ title, description, variant = 'default', duration = 3000 }: ToastProps) => {
    const newToast = { title, description, variant, duration };
    setToasts((prev) => [...prev, newToast]);

    // Simple auto-dismiss
    setTimeout(() => {
      setToasts((prev) => prev.filter(t => t !== newToast));
    }, duration);
  }, []);

  return { toast, toasts };
}
