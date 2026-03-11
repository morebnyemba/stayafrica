'use client';

import { Notification } from '@/types/notification-types';
import { formatDistanceToNow } from 'date-fns';
import { 
  Bell, 
  Calendar, 
  MessageSquare, 
  Star, 
  DollarSign, 
  TrendingDown, 
  Heart,
  AlertCircle,
  Check,
  X
} from 'lucide-react';
import { useMutation, useQueryClient } from '@tanstack/react-query';
import axios from 'axios';
import Link from 'next/link';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8000';

interface NotificationItemProps {
  notification: Notification;
}

const getNotificationIcon = (type: Notification['notification_type']) => {
  const iconClass = 'h-5 w-5';
  
  switch (type) {
    case 'BOOKING_CONFIRMED':
    case 'BOOKING_CANCELLED':
      return <Calendar className={iconClass} />;
    case 'NEW_MESSAGE':
      return <MessageSquare className={iconClass} />;
    case 'REVIEW_RECEIVED':
      return <Star className={iconClass} />;
    case 'PAYOUT_COMPLETED':
      return <DollarSign className={iconClass} />;
    case 'PRICE_DROP':
      return <TrendingDown className={iconClass} />;
    case 'WISHLIST_AVAILABLE':
      return <Heart className={iconClass} />;
    case 'SYSTEM_ALERT':
      return <AlertCircle className={iconClass} />;
    default:
      return <Bell className={iconClass} />;
  }
};

export const NotificationItem = ({ notification }: NotificationItemProps) => {
  const queryClient = useQueryClient();

  const markAsReadMutation = useMutation({
    mutationFn: async (notificationId: string) => {
      const token = localStorage.getItem('access_token');
      const response = await axios.post(
        `${API_BASE_URL}/api/v1/notifications/${notificationId}/mark_read/`,
        {},
        {
          headers: {
            Authorization: `Bearer ${token}`,
            'Content-Type': 'application/json',
          },
        }
      );
      return response.data;
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['notifications'] });
    },
  });

  const deleteMutation = useMutation({
    mutationFn: async (notificationId: string) => {
      const token = localStorage.getItem('access_token');
      await axios.delete(
        `${API_BASE_URL}/api/v1/notifications/${notificationId}/`,
        {
          headers: {
            Authorization: `Bearer ${token}`,
          },
        }
      );
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['notifications'] });
    },
  });

  const handleMarkAsRead = (e: React.MouseEvent) => {
    e.preventDefault();
    e.stopPropagation();
    if (!notification.is_read) {
      markAsReadMutation.mutate(notification.id);
    }
  };

  const handleDelete = (e: React.MouseEvent) => {
    e.preventDefault();
    e.stopPropagation();
    deleteMutation.mutate(notification.id);
  };

  const content = (
    <div
      className={`flex items-start gap-3 p-3 sm:p-4 border-b border-sand-100 transition-colors ${
        !notification.is_read
          ? 'bg-secondary-50/50 hover:bg-secondary-50'
          : 'hover:bg-sand-50'
      }`}
    >
      <div className={`flex-shrink-0 p-2 rounded-full ${
        !notification.is_read
          ? 'bg-secondary-100 text-secondary-600'
          : 'bg-sand-100 text-primary-500'
      }`}>
        {getNotificationIcon(notification.notification_type)}
      </div>
      
      <div className="flex-1 min-w-0">
        <div className="flex items-start justify-between gap-1">
          <h4 className={`text-sm leading-snug text-primary-900 ${
            !notification.is_read ? 'font-semibold' : 'font-medium'
          }`}>
            {notification.title}
          </h4>
          
          <div className="flex items-center gap-0.5 flex-shrink-0">
            {!notification.is_read && (
              <button
                onClick={handleMarkAsRead}
                className="p-1 text-secondary-600 hover:bg-secondary-100 rounded transition-colors"
                aria-label="Mark as read"
                title="Mark as read"
              >
                <Check className="h-3.5 w-3.5" />
              </button>
            )}
            
            <button
              onClick={handleDelete}
              className="p-1 text-primary-300 hover:text-red-500 hover:bg-red-50 rounded transition-colors"
              aria-label="Delete notification"
              title="Delete"
            >
              <X className="h-3.5 w-3.5" />
            </button>
          </div>
        </div>
        
        <p className="text-xs text-primary-500 mt-0.5 line-clamp-2">
          {notification.message}
        </p>
        
        <p className="text-xs text-primary-300 mt-1">
          {formatDistanceToNow(new Date(notification.created_at), { addSuffix: true })}
        </p>
      </div>
      
      {!notification.is_read && (
        <div className="w-2 h-2 bg-secondary-500 rounded-full flex-shrink-0 mt-2" aria-label="Unread" />
      )}
    </div>
  );

  if (notification.link) {
    return (
      <Link href={notification.link} className="block">
        {content}
      </Link>
    );
  }

  return content;
};
