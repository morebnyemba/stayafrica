import { View, Text, TouchableOpacity, FlatList, RefreshControl } from 'react-native';
import { useState, useCallback } from 'react';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useSafeAreaInsets } from 'react-native-safe-area-context';
import {
  useNotifications,
  useUnreadNotificationCount,
  useMarkNotificationRead,
  useMarkAllNotificationsRead,
} from '@/hooks/api-hooks';
import type { AppNotification, NotificationType } from '@/types';

const getNotificationIcon = (type: NotificationType): string => {
  switch (type) {
    case 'booking_confirmed':
      return 'checkmark-circle';
    case 'booking_cancelled':
      return 'close-circle';
    case 'booking_reminder':
      return 'calendar';
    case 'new_message':
      return 'chatbubble';
    case 'payment_received':
      return 'wallet';
    case 'payment_required':
      return 'card';
    case 'review_received':
      return 'star';
    case 'review_reminder':
      return 'star-half';
    case 'price_drop':
      return 'trending-down';
    case 'system':
      return 'information-circle';
    default:
      return 'notifications';
  }
};

const getNotificationColor = (type: NotificationType): string => {
  switch (type) {
    case 'booking_confirmed':
      return '#10B981';
    case 'booking_cancelled':
      return '#EF4444';
    case 'booking_reminder':
      return '#3B82F6';
    case 'new_message':
      return '#8B5CF6';
    case 'payment_received':
      return '#10B981';
    case 'payment_required':
      return '#F59E0B';
    case 'review_received':
      return '#F59E0B';
    case 'review_reminder':
      return '#F97316';
    case 'price_drop':
      return '#06B6D4';
    case 'system':
      return '#6B7280';
    default:
      return '#3A5C50';
  }
};

function formatTimeAgo(dateStr: string): string {
  const now = new Date();
  const date = new Date(dateStr);
  const diffMs = now.getTime() - date.getTime();
  const diffMins = Math.floor(diffMs / 60000);
  const diffHours = Math.floor(diffMs / 3600000);
  const diffDays = Math.floor(diffMs / 86400000);

  if (diffMins < 1) return 'Just now';
  if (diffMins < 60) return `${diffMins}m ago`;
  if (diffHours < 24) return `${diffHours}h ago`;
  if (diffDays < 7) return `${diffDays}d ago`;
  return date.toLocaleDateString();
}

export default function NotificationsScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const [filter, setFilter] = useState<'all' | 'unread'>('all');

  const { data: notificationsData, isLoading, refetch, isRefetching } = useNotifications();
  const { data: unreadData } = useUnreadNotificationCount();
  const markReadMutation = useMarkNotificationRead();
  const markAllReadMutation = useMarkAllNotificationsRead();

  const notifications = notificationsData?.results || [];
  const unreadCount = unreadData?.unread_count || 0;

  const filteredNotifications =
    filter === 'unread'
      ? notifications.filter((n) => !n.is_read)
      : notifications;

  const handleNotificationPress = useCallback(
    (notification: AppNotification) => {
      // Mark as read
      if (!notification.is_read) {
        markReadMutation.mutate(notification.id);
      }

      // Navigate via deep link if available
      if (notification.deep_link) {
        const link = notification.deep_link;
        if (link.includes('bookings/')) {
          const bookingId = link.split('bookings/').pop();
          if (bookingId) router.push(`/booking/${bookingId}` as any);
        } else if (link.includes('messages/')) {
          router.push(`/(tabs)/inbox` as any);
        } else if (link.includes('properties/')) {
          const propId = link.split('properties/').pop();
          if (propId) router.push(`/property/${propId}` as any);
        } else if (link.includes('reviews/')) {
          router.push(`/reviews` as any);
        }
      }
    },
    [markReadMutation, router]
  );

  const handleMarkAllRead = useCallback(() => {
    markAllReadMutation.mutate();
  }, [markAllReadMutation]);

  const onRefresh = useCallback(() => {
    refetch();
  }, [refetch]);

  const renderNotification = ({ item }: { item: AppNotification }) => {
    const color = getNotificationColor(item.notification_type);
    const icon = getNotificationIcon(item.notification_type);

    return (
      <TouchableOpacity
        onPress={() => handleNotificationPress(item)}
        className="mb-3"
        activeOpacity={0.7}
      >
        <View
          className={`bg-white rounded-2xl p-4 ${!item.is_read ? 'border-2 border-gold' : ''}`}
          style={{
            shadowColor: '#122F26',
            shadowOffset: { width: 0, height: 2 },
            shadowOpacity: !item.is_read ? 0.08 : 0.04,
            shadowRadius: 4,
            elevation: !item.is_read ? 3 : 2,
          }}
        >
          <View className="flex-row items-start">
            {/* Icon */}
            <View
              className="w-12 h-12 rounded-full items-center justify-center mr-3"
              style={{ backgroundColor: `${color}20` }}
            >
              <Ionicons name={icon as any} size={24} color={color} />
            </View>

            {/* Content */}
            <View className="flex-1">
              <View className="flex-row items-center justify-between mb-1">
                <Text
                  className={`text-base ${!item.is_read ? 'font-bold' : 'font-semibold'} text-forest flex-1`}
                  numberOfLines={1}
                >
                  {item.title}
                </Text>
                {!item.is_read && (
                  <View className="w-2.5 h-2.5 rounded-full bg-gold ml-2" />
                )}
              </View>
              <Text className="text-sm text-moss mb-2" numberOfLines={2}>
                {item.body}
              </Text>
              <Text className="text-xs text-moss/70">
                {formatTimeAgo(item.created_at)}
              </Text>
            </View>
          </View>
        </View>
      </TouchableOpacity>
    );
  };

  return (
    <View className="flex-1 bg-sand-100">
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f']}
        className="pb-4"
        style={{ paddingTop: insets.top + 12 }}
      >
        <View className="px-4">
          <View className="flex-row items-center justify-between mb-4">
            <View className="flex-row items-center">
              <TouchableOpacity
                onPress={() => router.back()}
                className="w-10 h-10 rounded-xl items-center justify-center mr-3"
                style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
              >
                <Ionicons name="arrow-back" size={24} color="#fff" />
              </TouchableOpacity>
              <View>
                <Text className="text-2xl font-black text-white tracking-tight">
                  Notifications
                </Text>
                {unreadCount > 0 && (
                  <Text className="text-sand-200 text-sm mt-0.5">
                    {unreadCount} unread
                  </Text>
                )}
              </View>
            </View>

            {unreadCount > 0 && (
              <TouchableOpacity
                onPress={handleMarkAllRead}
                disabled={markAllReadMutation.isPending}
                className="px-3 py-1.5 rounded-lg"
                style={{ backgroundColor: 'rgba(217, 177, 104, 0.2)' }}
              >
                <Text className="text-gold text-xs font-semibold">
                  {markAllReadMutation.isPending ? 'Marking...' : 'Mark all read'}
                </Text>
              </TouchableOpacity>
            )}
          </View>

          {/* Filter Tabs */}
          <View className="flex-row gap-2">
            <TouchableOpacity
              onPress={() => setFilter('all')}
              className={`flex-1 py-2 rounded-xl ${
                filter === 'all' ? 'bg-gold' : 'bg-white/10'
              }`}
            >
              <Text
                className={`text-center font-semibold ${
                  filter === 'all' ? 'text-forest' : 'text-white'
                }`}
              >
                All
              </Text>
            </TouchableOpacity>
            <TouchableOpacity
              onPress={() => setFilter('unread')}
              className={`flex-1 py-2 rounded-xl ${
                filter === 'unread' ? 'bg-gold' : 'bg-white/10'
              }`}
            >
              <Text
                className={`text-center font-semibold ${
                  filter === 'unread' ? 'text-forest' : 'text-white'
                }`}
              >
                Unread {unreadCount > 0 && `(${unreadCount})`}
              </Text>
            </TouchableOpacity>
          </View>
        </View>
      </LinearGradient>

      {/* Notifications List */}
      {isLoading ? (
        <View className="flex-1 items-center justify-center">
          <View className="bg-white rounded-3xl p-8 items-center"
            style={{
              shadowColor: '#000',
              shadowOffset: { width: 0, height: 8 },
              shadowOpacity: 0.1,
              shadowRadius: 16,
              elevation: 8,
            }}
          >
            <View className="bg-sand-200 rounded-full p-6 mb-4">
              <Ionicons name="notifications-outline" size={48} color="#3A5C50" />
            </View>
            <Text className="text-lg font-semibold text-forest">Loading notifications...</Text>
          </View>
        </View>
      ) : filteredNotifications.length === 0 ? (
        <View className="flex-1 items-center justify-center px-6">
          <View
            className="bg-white rounded-3xl p-8 items-center"
            style={{
              shadowColor: '#000',
              shadowOffset: { width: 0, height: 8 },
              shadowOpacity: 0.1,
              shadowRadius: 16,
              elevation: 8,
            }}
          >
            <View className="bg-sand-200 rounded-full p-8 mb-6">
              <Ionicons name="notifications-off-outline" size={72} color="#D9B168" />
            </View>
            <Text className="text-2xl font-bold text-forest mb-3">
              {filter === 'unread' ? 'All caught up!' : 'No notifications'}
            </Text>
            <Text className="text-moss text-center px-4 leading-6">
              {filter === 'unread'
                ? "You've read all your notifications"
                : "You'll see notifications here when you have them"}
            </Text>
          </View>
        </View>
      ) : (
        <FlatList
          data={filteredNotifications}
          renderItem={renderNotification}
          keyExtractor={(item) => item.id}
          contentContainerStyle={{
            padding: 16,
            paddingBottom: insets.bottom + 16,
          }}
          showsVerticalScrollIndicator={false}
          refreshControl={
            <RefreshControl
              refreshing={isRefetching}
              onRefresh={onRefresh}
              tintColor="#3A5C50"
              colors={['#3A5C50']}
            />
          }
        />
      )}
    </View>
  );
}
