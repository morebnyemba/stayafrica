import { View, Text, ScrollView, TouchableOpacity, Platform, FlatList } from 'react-native';
import { useState } from 'react';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useSafeAreaInsets } from 'react-native-safe-area-context';

interface Notification {
  id: string;
  type: 'booking' | 'message' | 'review' | 'payment' | 'system';
  title: string;
  message: string;
  timestamp: string;
  read: boolean;
  actionUrl?: string;
}

// Mock data - replace with actual API call
const MOCK_NOTIFICATIONS: Notification[] = [
  {
    id: '1',
    type: 'booking',
    title: 'New Booking Request',
    message: 'John Doe wants to book your Safari Lodge for 3 nights',
    timestamp: '2 hours ago',
    read: false,
  },
  {
    id: '2',
    type: 'message',
    title: 'New Message',
    message: 'Sarah replied to your message about check-in time',
    timestamp: '5 hours ago',
    read: false,
  },
  {
    id: '3',
    type: 'review',
    title: 'New Review',
    message: 'Michael left you a 5-star review for Beach Villa',
    timestamp: '1 day ago',
    read: true,
  },
  {
    id: '4',
    type: 'payment',
    title: 'Payment Received',
    message: 'Payment of $450 has been processed for your booking',
    timestamp: '2 days ago',
    read: true,
  },
  {
    id: '5',
    type: 'system',
    title: 'Verification Complete',
    message: 'Your identity verification has been approved!',
    timestamp: '3 days ago',
    read: true,
  },
];

export default function NotificationsScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const [notifications, setNotifications] = useState<Notification[]>(MOCK_NOTIFICATIONS);
  const [filter, setFilter] = useState<'all' | 'unread'>('all');

  const unreadCount = notifications.filter((n) => !n.read).length;

  const getNotificationIcon = (type: Notification['type']) => {
    switch (type) {
      case 'booking':
        return 'calendar';
      case 'message':
        return 'chatbubble';
      case 'review':
        return 'star';
      case 'payment':
        return 'wallet';
      case 'system':
        return 'information-circle';
      default:
        return 'notifications';
    }
  };

  const getNotificationColor = (type: Notification['type']) => {
    switch (type) {
      case 'booking':
        return '#3B82F6';
      case 'message':
        return '#10B981';
      case 'review':
        return '#F59E0B';
      case 'payment':
        return '#8B5CF6';
      case 'system':
        return '#6B7280';
      default:
        return '#3A5C50';
    }
  };

  const markAsRead = (id: string) => {
    setNotifications((prev) =>
      prev.map((n) => (n.id === id ? { ...n, read: true } : n))
    );
  };

  const markAllAsRead = () => {
    setNotifications((prev) => prev.map((n) => ({ ...n, read: true })));
  };

  const deleteNotification = (id: string) => {
    setNotifications((prev) => prev.filter((n) => n.id !== id));
  };

  const filteredNotifications =
    filter === 'unread' ? notifications.filter((n) => !n.read) : notifications;

  const renderNotification = ({ item }: { item: Notification }) => {
    const color = getNotificationColor(item.type);
    const icon = getNotificationIcon(item.type);

    return (
      <TouchableOpacity
        onPress={() => markAsRead(item.id)}
        className="mb-3"
      >
        <View
          className={`bg-white rounded-2xl p-4 ${!item.read ? 'border-2 border-gold' : ''}`}
          style={{
            shadowColor: '#122F26',
            shadowOffset: { width: 0, height: 2 },
            shadowOpacity: !item.read ? 0.08 : 0.04,
            shadowRadius: 4,
            elevation: !item.read ? 3 : 2,
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
                <Text className="text-base font-bold text-forest flex-1">
                  {item.title}
                </Text>
                {!item.read && (
                  <View className="w-2 h-2 rounded-full bg-gold ml-2" />
                )}
              </View>
              <Text className="text-sm text-moss mb-2">{item.message}</Text>
              <View className="flex-row items-center justify-between">
                <Text className="text-xs text-moss/70">{item.timestamp}</Text>
                <TouchableOpacity
                  onPress={() => deleteNotification(item.id)}
                  className="px-3 py-1"
                >
                  <Text className="text-xs text-red-500 font-semibold">Delete</Text>
                </TouchableOpacity>
              </View>
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
                onPress={markAllAsRead}
                className="px-3 py-1.5 rounded-lg"
                style={{ backgroundColor: 'rgba(217, 177, 104, 0.2)' }}
              >
                <Text className="text-gold text-xs font-semibold">Mark all read</Text>
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
      {filteredNotifications.length === 0 ? (
        <View className="flex-1 items-center justify-center px-6">
          <View className="bg-white rounded-3xl p-8 items-center"
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
        />
      )}
    </View>
  );
}

NotificationsScreen.displayName = 'NotificationsScreen';
