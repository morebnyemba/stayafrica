'use client';

import { useState, useRef, useEffect } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import axios from 'axios';
import { Bell, CheckCheck, Trash2, X } from 'lucide-react';
import { NotificationItem } from './NotificationItem';
import { Notification } from '@/types/notification-types';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8000';

export const NotificationCenter = () => {
  const [isOpen, setIsOpen] = useState(false);
  const dropdownRef = useRef<HTMLDivElement>(null);
  const queryClient = useQueryClient();

  const { data: notifications, isLoading } = useQuery<Notification[]>({
    queryKey: ['notifications'],
    queryFn: async () => {
      const token = localStorage.getItem('access_token');
      const response = await axios.get(
        `${API_BASE_URL}/api/v1/notifications/`,
        {
          headers: {
            Authorization: `Bearer ${token}`,
          },
        }
      );
      return response.data.results || response.data;
    },
    refetchInterval: 30000,
  });

  const markAllAsReadMutation = useMutation({
    mutationFn: async () => {
      const token = localStorage.getItem('access_token');
      await axios.post(
        `${API_BASE_URL}/api/v1/notifications/mark_all_read/`,
        {},
        {
          headers: {
            Authorization: `Bearer ${token}`,
            'Content-Type': 'application/json',
          },
        }
      );
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['notifications'] });
    },
  });

  const clearAllMutation = useMutation({
    mutationFn: async () => {
      const token = localStorage.getItem('access_token');
      await axios.post(
        `${API_BASE_URL}/api/v1/notifications/clear_all/`,
        {},
        {
          headers: {
            Authorization: `Bearer ${token}`,
            'Content-Type': 'application/json',
          },
        }
      );
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['notifications'] });
    },
  });

  useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
      if (dropdownRef.current && !dropdownRef.current.contains(event.target as Node)) {
        setIsOpen(false);
      }
    };

    document.addEventListener('mousedown', handleClickOutside);
    return () => document.removeEventListener('mousedown', handleClickOutside);
  }, []);

  // Prevent body scroll on mobile when panel is open
  useEffect(() => {
    if (isOpen) {
      const isMobile = window.innerWidth < 640;
      if (isMobile) {
        document.body.style.overflow = 'hidden';
      }
    }
    return () => {
      document.body.style.overflow = '';
    };
  }, [isOpen]);

  const unreadCount = notifications?.filter(n => !n.is_read).length || 0;

  return (
    <div className="relative" ref={dropdownRef}>
      <button
        onClick={() => setIsOpen(!isOpen)}
        className="relative p-2 text-primary-500 hover:text-primary-900 hover:bg-primary-100 rounded-full transition-colors"
        aria-label="Notifications"
        aria-expanded={isOpen}
        aria-haspopup="true"
      >
        <Bell className="h-6 w-6" />
        {unreadCount > 0 && (
          <span className="absolute top-0 right-0 inline-flex items-center justify-center px-1.5 py-0.5 text-xs font-bold leading-none text-white transform translate-x-1/2 -translate-y-1/2 bg-red-600 rounded-full min-w-[1.25rem]">
            {unreadCount > 99 ? '99+' : unreadCount}
          </span>
        )}
      </button>

      {isOpen && (
        <>
          {/* Mobile overlay */}
          <div className="fixed inset-0 bg-black/30 z-40 sm:hidden" onClick={() => setIsOpen(false)} />
          
          <div
            className="fixed inset-x-0 bottom-0 top-auto sm:absolute sm:inset-auto sm:right-0 sm:top-full sm:mt-2 sm:w-96 bg-white sm:rounded-xl rounded-t-2xl shadow-elevated border border-sand-200/50 z-50 flex flex-col max-h-[85vh] sm:max-h-[600px]"
            role="dialog"
            aria-label="Notifications panel"
          >
            {/* Header */}
            <div className="flex items-center justify-between p-4 border-b border-sand-200/50 shrink-0">
              <div className="flex items-center gap-2">
                {/* Mobile drag handle */}
                <div className="w-8 h-1 rounded-full bg-sand-300 absolute top-2 left-1/2 -translate-x-1/2 sm:hidden" />
                <h3 className="text-lg font-semibold text-primary-900">Notifications</h3>
                {unreadCount > 0 && (
                  <span className="inline-flex items-center justify-center px-2 py-0.5 text-xs font-semibold bg-secondary-100 text-secondary-700 rounded-full">
                    {unreadCount}
                  </span>
                )}
              </div>
              
              <div className="flex items-center gap-1">
                {unreadCount > 0 && (
                  <button
                    onClick={() => markAllAsReadMutation.mutate()}
                    disabled={markAllAsReadMutation.isPending}
                    className="flex items-center gap-1 px-2.5 py-1.5 text-xs font-medium text-secondary-600 hover:bg-secondary-50 rounded-lg transition-colors disabled:opacity-50"
                    aria-label="Mark all as read"
                  >
                    <CheckCheck className="h-3.5 w-3.5" />
                    <span className="hidden sm:inline">Read all</span>
                  </button>
                )}
                
                {notifications && notifications.length > 0 && (
                  <button
                    onClick={() => clearAllMutation.mutate()}
                    disabled={clearAllMutation.isPending}
                    className="flex items-center gap-1 px-2.5 py-1.5 text-xs font-medium text-red-500 hover:bg-red-50 rounded-lg transition-colors disabled:opacity-50"
                    aria-label="Clear all notifications"
                  >
                    <Trash2 className="h-3.5 w-3.5" />
                    <span className="hidden sm:inline">Clear</span>
                  </button>
                )}
                
                <button
                  onClick={() => setIsOpen(false)}
                  className="p-1.5 text-primary-400 hover:bg-sand-100 rounded-lg transition-colors sm:hidden"
                  aria-label="Close notifications"
                >
                  <X className="h-4 w-4" />
                </button>
              </div>
            </div>

            {/* Content */}
            <div className="overflow-y-auto flex-1 overscroll-contain">
              {isLoading ? (
                <div className="space-y-1">
                  {[...Array(4)].map((_, i) => (
                    <div key={i} className="animate-pulse flex items-start gap-3 p-4">
                      <div className="w-9 h-9 rounded-full bg-sand-200 shrink-0" />
                      <div className="flex-1 space-y-2">
                        <div className="h-3.5 w-3/4 rounded bg-sand-200" />
                        <div className="h-3 w-1/2 rounded bg-sand-200" />
                      </div>
                    </div>
                  ))}
                </div>
              ) : notifications && notifications.length > 0 ? (
                notifications.map(notification => (
                  <NotificationItem key={notification.id} notification={notification} />
                ))
              ) : (
                <div className="flex flex-col items-center justify-center py-12 px-4 text-primary-400">
                  <div className="w-12 h-12 rounded-full bg-sand-100 flex items-center justify-center mb-3">
                    <Bell className="h-6 w-6 text-sand-400" />
                  </div>
                  <p className="text-sm font-medium text-primary-600">All caught up!</p>
                  <p className="text-xs text-primary-400 mt-1">No new notifications</p>
                </div>
              )}
            </div>
          </div>
        </>
      )}
    </div>
  );
};
