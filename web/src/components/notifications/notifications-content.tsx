'use client';

import { useState, useEffect, useCallback } from 'react';
import { apiClient } from '@/services/api-client';
import { Bell, BellOff, Check, CheckCheck, Clock, AlertCircle, Home, Calendar, CreditCard, MessageSquare, Star, Trash2 } from 'lucide-react';

interface Notification {
    id: string;
    notification_type: string;
    title: string;
    message: string;
    is_read: boolean;
    created_at: string;
    metadata?: Record<string, any>;
}

const NOTIFICATION_ICONS: Record<string, any> = {
    booking: Calendar,
    payment: CreditCard,
    message: MessageSquare,
    review: Star,
    property: Home,
    system: Bell,
};

function getNotificationIcon(type: string) {
    const Icon = NOTIFICATION_ICONS[type] || Bell;
    return <Icon className="w-5 h-5" />;
}

function formatTimeAgo(dateStr: string): string {
    const date = new Date(dateStr);
    const now = new Date();
    const diffMs = now.getTime() - date.getTime();
    const diffMins = Math.floor(diffMs / 60000);
    const diffHours = Math.floor(diffMins / 60);
    const diffDays = Math.floor(diffHours / 24);

    if (diffMins < 1) return 'Just now';
    if (diffMins < 60) return `${diffMins}m ago`;
    if (diffHours < 24) return `${diffHours}h ago`;
    if (diffDays < 7) return `${diffDays}d ago`;
    return date.toLocaleDateString('en-US', { month: 'short', day: 'numeric' });
}

export function NotificationsContent() {
    const [notifications, setNotifications] = useState<Notification[]>([]);
    const [loading, setLoading] = useState(true);
    const [filter, setFilter] = useState<'all' | 'unread'>('all');
    const [unreadCount, setUnreadCount] = useState(0);

    const fetchNotifications = useCallback(async () => {
        try {
            setLoading(true);
            const response = await apiClient.get('/notifications/notifications/');
            const data = response.data?.results || response.data || [];
            setNotifications(data);
            setUnreadCount(data.filter((n: Notification) => !n.is_read).length);
        } catch (error) {
            console.error('Failed to fetch notifications:', error);
        } finally {
            setLoading(false);
        }
    }, []);

    useEffect(() => {
        fetchNotifications();
    }, [fetchNotifications]);

    const markAsRead = async (id: string) => {
        try {
            await apiClient.patch(`/notifications/notifications/${id}/`, { is_read: true });
            setNotifications(prev =>
                prev.map(n => (n.id === id ? { ...n, is_read: true } : n))
            );
            setUnreadCount(prev => Math.max(0, prev - 1));
        } catch (error) {
            console.error('Failed to mark notification as read:', error);
        }
    };

    const markAllAsRead = async () => {
        try {
            await apiClient.post('/notifications/notifications/mark_all_read/');
            setNotifications(prev => prev.map(n => ({ ...n, is_read: true })));
            setUnreadCount(0);
        } catch (error) {
            console.error('Failed to mark all notifications as read:', error);
        }
    };

    const filteredNotifications = filter === 'unread'
        ? notifications.filter(n => !n.is_read)
        : notifications;

    return (
        <div className="max-w-3xl mx-auto px-4 py-8">
            {/* Header */}
            <div className="flex items-center justify-between mb-6">
                <div>
                    <h1 className="text-2xl font-bold text-primary-900 dark:text-sand-100">Notifications</h1>
                    {unreadCount > 0 && (
                        <p className="text-sm text-primary-500 dark:text-sand-400 mt-1">
                            {unreadCount} unread notification{unreadCount !== 1 ? 's' : ''}
                        </p>
                    )}
                </div>
                {unreadCount > 0 && (
                    <button
                        onClick={markAllAsRead}
                        className="flex items-center gap-2 px-4 py-2 text-sm font-medium text-accent-600 hover:text-accent-700 hover:bg-accent-50 rounded-lg transition-colors dark:text-accent-400 dark:hover:bg-accent-900/30"
                    >
                        <CheckCheck className="w-4 h-4" />
                        Mark all read
                    </button>
                )}
            </div>

            {/* Filter Tabs */}
            <div className="flex gap-1 mb-6 bg-sand-200/50 dark:bg-primary-800/50 rounded-lg p-1">
                <button
                    onClick={() => setFilter('all')}
                    className={`flex-1 py-2 px-4 text-sm font-medium rounded-md transition-colors ${filter === 'all'
                            ? 'bg-white dark:bg-primary-700 text-primary-900 dark:text-sand-100 shadow-sm'
                            : 'text-primary-500 dark:text-sand-400 hover:text-primary-700'
                        }`}
                >
                    All
                </button>
                <button
                    onClick={() => setFilter('unread')}
                    className={`flex-1 py-2 px-4 text-sm font-medium rounded-md transition-colors ${filter === 'unread'
                            ? 'bg-white dark:bg-primary-700 text-primary-900 dark:text-sand-100 shadow-sm'
                            : 'text-primary-500 dark:text-sand-400 hover:text-primary-700'
                        }`}
                >
                    Unread {unreadCount > 0 && `(${unreadCount})`}
                </button>
            </div>

            {/* Notifications List */}
            {loading ? (
                <div className="space-y-4">
                    {[...Array(5)].map((_, i) => (
                        <div key={i} className="animate-pulse flex gap-4 p-4 rounded-xl bg-sand-100 dark:bg-primary-800/40">
                            <div className="w-10 h-10 rounded-full bg-sand-200 dark:bg-primary-700" />
                            <div className="flex-1 space-y-2">
                                <div className="h-4 w-2/3 rounded bg-sand-200 dark:bg-primary-700" />
                                <div className="h-3 w-1/2 rounded bg-sand-200 dark:bg-primary-700" />
                            </div>
                        </div>
                    ))}
                </div>
            ) : filteredNotifications.length === 0 ? (
                <div className="text-center py-16">
                    <BellOff className="w-12 h-12 mx-auto text-sand-300 dark:text-primary-600 mb-4" />
                    <h3 className="text-lg font-medium text-primary-700 dark:text-sand-300">
                        {filter === 'unread' ? 'No unread notifications' : 'No notifications yet'}
                    </h3>
                    <p className="text-sm text-primary-500 dark:text-sand-400 mt-1">
                        {filter === 'unread' ? "You're all caught up!" : "We'll notify you about bookings, messages, and more."}
                    </p>
                </div>
            ) : (
                <div className="space-y-2">
                    {filteredNotifications.map(notification => (
                        <button
                            key={notification.id}
                            onClick={() => !notification.is_read && markAsRead(notification.id)}
                            className={`w-full text-left flex gap-4 p-4 rounded-xl transition-all duration-200 ${notification.is_read
                                    ? 'bg-white dark:bg-primary-800/30 hover:bg-sand-50 dark:hover:bg-primary-800/50'
                                    : 'bg-accent-50/50 dark:bg-accent-900/20 hover:bg-accent-50 dark:hover:bg-accent-900/30 border-l-4 border-accent-500'
                                }`}
                        >
                            {/* Icon */}
                            <div className={`flex-shrink-0 w-10 h-10 rounded-full flex items-center justify-center ${notification.is_read
                                    ? 'bg-sand-200 dark:bg-primary-700 text-primary-500 dark:text-sand-400'
                                    : 'bg-accent-100 dark:bg-accent-900/40 text-accent-600 dark:text-accent-400'
                                }`}>
                                {getNotificationIcon(notification.notification_type)}
                            </div>

                            {/* Content */}
                            <div className="flex-1 min-w-0">
                                <p className={`text-sm ${notification.is_read
                                        ? 'text-primary-700 dark:text-sand-300'
                                        : 'text-primary-900 dark:text-sand-100 font-semibold'
                                    }`}>
                                    {notification.title}
                                </p>
                                <p className="text-sm text-primary-500 dark:text-sand-400 mt-0.5 line-clamp-2">
                                    {notification.message}
                                </p>
                                <p className="text-xs text-primary-400 dark:text-sand-500 mt-1.5 flex items-center gap-1">
                                    <Clock className="w-3 h-3" />
                                    {formatTimeAgo(notification.created_at)}
                                </p>
                            </div>

                            {/* Read indicator */}
                            {!notification.is_read && (
                                <div className="flex-shrink-0 w-2.5 h-2.5 rounded-full bg-accent-500 mt-2" />
                            )}
                        </button>
                    ))}
                </div>
            )}
        </div>
    );
}
