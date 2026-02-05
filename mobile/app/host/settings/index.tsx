import { View, Text, ScrollView, TouchableOpacity, Platform, Switch } from 'react-native';
import { useState } from 'react';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';

export default function HostSettingsScreen() {
  const router = useRouter();
  const { user, isAuthenticated } = useAuth();
  const [instantBooking, setInstantBooking] = useState(false);
  const [emailNotifications, setEmailNotifications] = useState(true);
  const [smsNotifications, setSmsNotifications] = useState(false);

  if (!isAuthenticated) {
    return (
      <View className="flex-1 bg-sand-100">
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
        >
          <TouchableOpacity 
            onPress={() => router.back()} 
            className="mb-4 w-10 h-10 rounded-xl items-center justify-center"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </TouchableOpacity>
          <Text className="text-3xl font-black text-white tracking-tight">
            Host Settings
          </Text>
        </LinearGradient>
        <View className="flex-1 items-center justify-center px-6">
          <Text className="text-forest text-lg">Please sign in to access settings</Text>
        </View>
      </View>
    );
  }

  const SettingToggle = ({ icon, title, description, value, onToggle }: any) => (
    <View
      className="p-4 bg-white rounded-2xl mb-3 flex-row items-center"
      style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 2 },
        shadowOpacity: 0.05,
        shadowRadius: 4,
        elevation: 2,
      }}
    >
      <LinearGradient
        colors={['#3A5C5020', '#3A5C5010']}
        className="w-12 h-12 rounded-full items-center justify-center"
      >
        <Ionicons name={icon} size={24} color="#3A5C50" />
      </LinearGradient>
      <View className="flex-1 ml-4">
        <Text className="text-base font-semibold text-forest">{title}</Text>
        <Text className="text-sm text-moss mt-1">{description}</Text>
      </View>
      <Switch
        value={value}
        onValueChange={onToggle}
        trackColor={{ false: '#e5dfd0', true: '#10B981' }}
        thumbColor={value ? '#fff' : '#fff'}
      />
    </View>
  );

  const SettingLink = ({ icon, title, description, onPress, color = '#3A5C50' }: any) => (
    <TouchableOpacity
      className="mb-3"
      onPress={onPress}
    >
      <View
        className="p-4 bg-white rounded-2xl flex-row items-center"
        style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 2 },
          shadowOpacity: 0.05,
          shadowRadius: 4,
          elevation: 2,
        }}
      >
        <LinearGradient
          colors={[`${color}20`, `${color}10`]}
          className="w-12 h-12 rounded-full items-center justify-center"
        >
          <Ionicons name={icon} size={24} color={color} />
        </LinearGradient>
        <View className="flex-1 ml-4">
          <Text className="text-base font-semibold text-forest">{title}</Text>
          <Text className="text-sm text-moss mt-1">{description}</Text>
        </View>
        <Ionicons name="chevron-forward" size={20} color="#3A5C50" />
      </View>
    </TouchableOpacity>
  );

  return (
    <ScrollView className="flex-1 bg-sand-100" showsVerticalScrollIndicator={false}>
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="px-4 pb-8"
        style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
      >
        <TouchableOpacity onPress={() => router.back()} className="mb-4">
          <View className="w-10 h-10 rounded-xl items-center justify-center" style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}>
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </View>
        </TouchableOpacity>
        <Text className="text-3xl font-black text-white tracking-tight mb-2">
          Host Settings
        </Text>
        <View className="flex-row items-center">
          <Ionicons name="settings" size={16} color="#D9B168" />
          <Text className="text-sand-100 ml-2">
            Manage your hosting preferences
          </Text>
        </View>
      </LinearGradient>

      <View className="px-4 -mt-4">
        {/* Booking Settings */}
        <Text className="text-lg font-bold text-forest mb-3 mt-4">Booking Settings</Text>

        <SettingToggle
          icon="flash-outline"
          title="Instant Booking"
          description="Allow guests to book without approval"
          value={instantBooking}
          onToggle={setInstantBooking}
        />

        <SettingLink
          icon="calendar-outline"
          title="Availability Calendar"
          description="Set your property availability"
          onPress={() => {}}
          color="#6366F1"
        />

        <SettingLink
          icon="cash-outline"
          title="Pricing Rules"
          description="Configure dynamic pricing"
          onPress={() => router.push('/host/pricing')}
          color="#10B981"
        />

        {/* Notification Settings */}
        <Text className="text-lg font-bold text-forest mb-3 mt-6">Notifications</Text>

        <SettingToggle
          icon="mail-outline"
          title="Email Notifications"
          description="Receive booking updates via email"
          value={emailNotifications}
          onToggle={setEmailNotifications}
        />

        <SettingToggle
          icon="chatbubble-outline"
          title="SMS Notifications"
          description="Receive booking updates via SMS"
          value={smsNotifications}
          onToggle={setSmsNotifications}
        />

        {/* Account Settings */}
        <Text className="text-lg font-bold text-forest mb-3 mt-6">Account</Text>

        <SettingLink
          icon="person-outline"
          title="Profile Settings"
          description="Update your host profile"
          onPress={() => router.push('/(tabs)/profile/edit')}
          color="#3A5C50"
        />

        <SettingLink
          icon="shield-checkmark-outline"
          title="Verification Status"
          description="Verify your identity"
          onPress={() => router.push('/host/verification')}
          color="#F59E0B"
        />

        <SettingLink
          icon="card-outline"
          title="Payout Methods"
          description="Manage your payout preferences"
          onPress={() => router.push('/(tabs)/wallet')}
          color="#8B5CF6"
        />

        <SettingLink
          icon="receipt-outline"
          title="Tax Information"
          description="View and update tax details"
          onPress={() => router.push('/host/tax-reports')}
          color="#14B8A6"
        />

        {/* Help Section */}
        <Text className="text-lg font-bold text-forest mb-3 mt-6">Help & Support</Text>

        <SettingLink
          icon="help-circle-outline"
          title="Help Center"
          description="Get help with hosting"
          onPress={() => {}}
          color="#6B7280"
        />

        <SettingLink
          icon="document-text-outline"
          title="Hosting Guidelines"
          description="View hosting policies"
          onPress={() => {}}
          color="#6B7280"
        />

        <View className="h-8" />
      </View>
    </ScrollView>
  );
}
