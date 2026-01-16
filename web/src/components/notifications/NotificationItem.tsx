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
      const response = await axios.patch(
        `${API_BASE_URL}/api/v1/notifications/${notificationId}/`,
        { is_read: true },
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
      className={`flex items-start gap-3 p-4 border-b hover:bg-gray-50 transition-colors ${
        !notification.is_read ? 'bg-blue-50' : ''
      }`}
    >
      <div className={`flex-shrink-0 p-2 rounded-full ${
        !notification.is_read ? 'bg-blue-100 text-blue-600' : 'bg-gray-100 text-gray-600'
      }`}>
        {getNotificationIcon(notification.notification_type)}
      </div>
      
      <div className="flex-1 min-w-0">
        <div className="flex items-start justify-between gap-2">
          <h4 className={`text-sm font-medium text-gray-900 ${
            !notification.is_read ? 'font-semibold' : ''
          }`}>
            {notification.title}
          </h4>
          
          <div className="flex items-center gap-1 flex-shrink-0">
            {!notification.is_read && (
              <button
                onClick={handleMarkAsRead}
                className="p-1 text-blue-600 hover:bg-blue-100 rounded"
                aria-label="Mark as read"
                title="Mark as read"
              >
                <Check className="h-4 w-4" />
              </button>
            )}
            
            <button
              onClick={handleDelete}
              className="p-1 text-gray-400 hover:bg-gray-200 rounded"
              aria-label="Delete notification"
              title="Delete"
            >
              <X className="h-4 w-4" />
            </button>
          </div>
        </div>
        
        <p className="text-sm text-gray-600 mt-1">
          {notification.message}
        </p>
        
        <p className="text-xs text-gray-400 mt-2">
          {formatDistanceToNow(new Date(notification.created_at), { addSuffix: true })}
        </p>
      </div>
      
      {!notification.is_read && (
        <div className="w-2 h-2 bg-blue-600 rounded-full flex-shrink-0 mt-2" aria-label="Unread" />
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
