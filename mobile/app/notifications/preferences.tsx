import { View, Text, TouchableOpacity, ScrollView, Switch, ActivityIndicator } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useSafeAreaInsets } from 'react-native-safe-area-context';
import { useNotificationPreferences, useUpdateNotificationPreferences } from '@/hooks/api-hooks';
import type { NotificationPreference } from '@/types';

interface PrefToggleProps {
  label: string;
  description: string;
  icon: string;
  value: boolean;
  onToggle: () => void;
  disabled?: boolean;
}

function PrefToggle({ label, description, icon, value, onToggle, disabled }: PrefToggleProps) {
  return (
    <View className="flex-row items-center justify-between bg-white rounded-2xl p-4 mb-3"
      style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 1 },
        shadowOpacity: 0.04,
        shadowRadius: 3,
        elevation: 1,
      }}
    >
      <View className="flex-row items-center flex-1 mr-3">
        <View className="w-10 h-10 rounded-full items-center justify-center mr-3"
          style={{ backgroundColor: value ? '#3A5C5020' : '#6B728020' }}
        >
          <Ionicons name={icon as any} size={20} color={value ? '#3A5C50' : '#6B7280'} />
        </View>
        <View className="flex-1">
          <Text className="text-base font-semibold text-forest">{label}</Text>
          <Text className="text-xs text-moss mt-0.5">{description}</Text>
        </View>
      </View>
      <Switch
        value={value}
        onValueChange={onToggle}
        disabled={disabled}
        trackColor={{ false: '#E5E7EB', true: '#3A5C50' }}
        thumbColor={value ? '#D9B168' : '#f4f3f4'}
        ios_backgroundColor="#E5E7EB"
      />
    </View>
  );
}

export default function NotificationPreferencesScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const { data: preferences, isLoading } = useNotificationPreferences();
  const updateMutation = useUpdateNotificationPreferences();

  const handleToggle = (key: keyof NotificationPreference) => {
    if (!preferences) return;
    updateMutation.mutate({
      [key]: !preferences[key],
    });
  };

  return (
    <View className="flex-1 bg-sand-100">
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f']}
        className="pb-4"
        style={{ paddingTop: insets.top + 12 }}
      >
        <View className="px-4 flex-row items-center">
          <TouchableOpacity
            onPress={() => router.back()}
            className="w-10 h-10 rounded-xl items-center justify-center mr-3"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </TouchableOpacity>
          <Text className="text-2xl font-black text-white tracking-tight">
            Notification Settings
          </Text>
        </View>
      </LinearGradient>

      {isLoading ? (
        <View className="flex-1 items-center justify-center">
          <ActivityIndicator size="large" color="#3A5C50" />
          <Text className="text-moss mt-4">Loading preferences...</Text>
        </View>
      ) : (
        <ScrollView
          className="flex-1"
          contentContainerStyle={{ padding: 16, paddingBottom: insets.bottom + 24 }}
          showsVerticalScrollIndicator={false}
        >
          {/* Bookings Section */}
          <Text className="text-sm font-bold text-moss uppercase tracking-wide mb-3 ml-1">
            Bookings
          </Text>
          <PrefToggle
            label="Booking Confirmed"
            description="When a booking is confirmed"
            icon="checkmark-circle"
            value={preferences?.booking_confirmed ?? true}
            onToggle={() => handleToggle('booking_confirmed')}
            disabled={updateMutation.isPending}
          />
          <PrefToggle
            label="Booking Cancelled"
            description="When a booking is cancelled"
            icon="close-circle"
            value={preferences?.booking_cancelled ?? true}
            onToggle={() => handleToggle('booking_cancelled')}
            disabled={updateMutation.isPending}
          />
          <PrefToggle
            label="Booking Reminders"
            description="Check-in and check-out reminders"
            icon="calendar"
            value={preferences?.booking_reminder ?? true}
            onToggle={() => handleToggle('booking_reminder')}
            disabled={updateMutation.isPending}
          />

          {/* Communication Section */}
          <Text className="text-sm font-bold text-moss uppercase tracking-wide mb-3 ml-1 mt-6">
            Communication
          </Text>
          <PrefToggle
            label="New Messages"
            description="When you receive a new message"
            icon="chatbubble"
            value={preferences?.new_message ?? true}
            onToggle={() => handleToggle('new_message')}
            disabled={updateMutation.isPending}
          />

          {/* Payments Section */}
          <Text className="text-sm font-bold text-moss uppercase tracking-wide mb-3 ml-1 mt-6">
            Payments
          </Text>
          <PrefToggle
            label="Payment Received"
            description="When a payment is received"
            icon="wallet"
            value={preferences?.payment_received ?? true}
            onToggle={() => handleToggle('payment_received')}
            disabled={updateMutation.isPending}
          />
          <PrefToggle
            label="Payment Required"
            description="When a payment is due"
            icon="card"
            value={preferences?.payment_required ?? true}
            onToggle={() => handleToggle('payment_required')}
            disabled={updateMutation.isPending}
          />

          {/* Reviews Section */}
          <Text className="text-sm font-bold text-moss uppercase tracking-wide mb-3 ml-1 mt-6">
            Reviews
          </Text>
          <PrefToggle
            label="Review Received"
            description="When someone reviews your property"
            icon="star"
            value={preferences?.review_received ?? true}
            onToggle={() => handleToggle('review_received')}
            disabled={updateMutation.isPending}
          />
          <PrefToggle
            label="Review Reminders"
            description="Reminders to leave reviews after stays"
            icon="star-half"
            value={preferences?.review_reminder ?? true}
            onToggle={() => handleToggle('review_reminder')}
            disabled={updateMutation.isPending}
          />

          {/* Deals Section */}
          <Text className="text-sm font-bold text-moss uppercase tracking-wide mb-3 ml-1 mt-6">
            Deals & Updates
          </Text>
          <PrefToggle
            label="Price Drops"
            description="When wishlisted properties drop in price"
            icon="trending-down"
            value={preferences?.price_drop ?? true}
            onToggle={() => handleToggle('price_drop')}
            disabled={updateMutation.isPending}
          />
        </ScrollView>
      )}
    </View>
  );
}
