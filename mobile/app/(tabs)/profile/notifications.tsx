import { View, Text, ScrollView, TouchableOpacity, Switch, ActivityIndicator } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { useSafeAreaInsets } from 'react-native-safe-area-context';
import { useNotificationPreferences, useUpdateNotificationPreferences } from '@/hooks/api-hooks';
import { useState } from 'react';
import { AppDialog, AppDialogAction } from '@/components/common/AppDialog';

export default function NotificationsScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const { data: prefs, isLoading } = useNotificationPreferences();
  const { mutate: updatePrefs, isPending: saving } = useUpdateNotificationPreferences();
  const [dialog, setDialog] = useState<{ visible: boolean; title: string; message: string; primaryAction: AppDialogAction }>({
    visible: false,
    title: '',
    message: '',
    primaryAction: { label: 'OK' },
  });

  const openDialog = (title: string, message: string) => {
    setDialog({ visible: true, title, message, primaryAction: { label: 'OK' } });
  };

  const handleToggle = (key: string, value: boolean) => {
    updatePrefs({ [key]: value } as any, {
      onError: () => openDialog('Error', 'Failed to update notification settings.'),
    });
  };

  if (isLoading) {
    return (
      <View className="flex-1 bg-sand-100 items-center justify-center">
        <ActivityIndicator size="large" color="#D9B168" />
      </View>
    );
  }

  const settings = [
    { key: 'email_booking_updates', label: 'Booking Updates', description: 'Email notifications for booking confirmations and changes', icon: 'calendar' },
    { key: 'email_messages', label: 'Messages', description: 'Email when you receive a new message', icon: 'chatbubble' },
    { key: 'email_promotions', label: 'Promotions & Deals', description: 'Special offers and promotional emails', icon: 'pricetag' },
    { key: 'email_reviews', label: 'Reviews', description: 'Email when a review is left on your property', icon: 'star' },
    { key: 'sms_booking_updates', label: 'SMS Booking Alerts', description: 'Text messages for urgent booking updates', icon: 'phone-portrait' },
    { key: 'sms_security', label: 'SMS Security Alerts', description: 'Text messages for login attempts and security events', icon: 'shield-checkmark' },
    { key: 'push_messages', label: 'Push - Messages', description: 'Push notifications for new messages', icon: 'notifications' },
    { key: 'push_bookings', label: 'Push - Bookings', description: 'Push notifications for booking activity', icon: 'notifications-circle' },
  ];

  return (
    <View className="flex-1 bg-sand-100">
      {/* Header */}
      <View
        className="bg-white px-4 py-3 border-b border-sand-200 flex-row items-center"
        style={{ paddingTop: insets.top + 8 }}
      >
        <TouchableOpacity onPress={() => router.back()}>
          <Ionicons name="arrow-back" size={24} color="#122F26" />
        </TouchableOpacity>
        <Text className="text-lg font-semibold text-forest ml-3">Notification Settings</Text>
      </View>

      <ScrollView className="flex-1 p-4" showsVerticalScrollIndicator={false} contentContainerStyle={{ paddingBottom: 32 }}>
        <Text className="text-sm text-moss mb-4">
          Choose which notifications you'd like to receive. Changes are saved automatically.
        </Text>

        {settings.map(setting => (
          <View
            key={setting.key}
            className="bg-white rounded-2xl p-4 mb-3 flex-row items-center"
            style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 2 },
              shadowOpacity: 0.05,
              shadowRadius: 4,
              elevation: 2,
            }}
          >
            <View className="bg-gold/20 rounded-full p-2 mr-3">
              <Ionicons name={setting.icon as any} size={20} color="#D9B168" />
            </View>
            <View className="flex-1 mr-3">
              <Text className="font-semibold text-forest">{setting.label}</Text>
              <Text className="text-xs text-moss mt-1">{setting.description}</Text>
            </View>
            <Switch
              value={!!(prefs as any)?.[setting.key]}
              onValueChange={v => handleToggle(setting.key, v)}
              trackColor={{ false: '#D1D5DB', true: '#D9B168' }}
              thumbColor="#fff"
            />
          </View>
        ))}
      </ScrollView>
      <AppDialog
        visible={dialog.visible}
        title={dialog.title}
        message={dialog.message}
        primaryAction={dialog.primaryAction}
        onRequestClose={() => setDialog((prev) => ({ ...prev, visible: false }))}
      />
    </View>
  );
}
