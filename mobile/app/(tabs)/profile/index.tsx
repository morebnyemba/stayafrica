import { View, Text, TouchableOpacity, ScrollView, Alert } from 'react-native';
import { useState } from 'react';
import { useAuth } from '@/context/auth-context';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { Avatar } from '@/components/common/Avatar';
import { Sidebar } from '@/components/common/Sidebar';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { useBookings, useWishlist } from '@/hooks/api-hooks';
import Animated, { FadeInDown } from 'react-native-reanimated';

export default function ProfileScreen() {
  const { user, logout, isAuthenticated, switchProfile } = useAuth();
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const [sidebarVisible, setSidebarVisible] = useState(false);
  const [switchLoading, setSwitchLoading] = useState(false);

  const { data: bookingsData } = useBookings();
  const { data: wishlistData } = useWishlist();
  const totalBookings = bookingsData?.results?.length ?? 0;
  const savedCount = wishlistData?.results?.length ?? 0;

  const isHost = user?.role === 'host' || user?.role === 'admin';
  const activeProfile = user?.active_profile ?? 'guest';

  const handleSwitchProfile = async (target: 'guest' | 'host') => {
    try {
      setSwitchLoading(true);
      await switchProfile(target);
      if (target === 'host') {
        router.push('/(tabs)/host');
      } else {
        router.push('/(tabs)/dashboard');
      }
    } catch (error: any) {
      Alert.alert('Error', error.message || 'Failed to switch profile');
    } finally {
      setSwitchLoading(false);
    }
  };

  if (!isAuthenticated) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100">
        <Sidebar isVisible={sidebarVisible} onClose={() => setSidebarVisible(false)} />
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="pb-6"
          style={{ paddingTop: insets.top + 12 }}
        >
          <View className="flex-row items-center justify-between px-4 mb-4">
            <TouchableOpacity
              onPress={() => setSidebarVisible(true)}
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="menu" size={24} color="#fff" />
            </TouchableOpacity>
          </View>
          <View className="px-4">
            <Text className="text-3xl font-black text-white tracking-tight">Profile</Text>
            <Text className="text-sand-200 text-sm mt-1">Manage your account</Text>
          </View>
        </LinearGradient>

        <View className="flex-1 items-center justify-center px-6">
          <View
            className="bg-white rounded-3xl p-8 items-center"
            style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}
          >
            <View className="bg-sand-200 rounded-full p-8 mb-6">
              <Ionicons name="person-outline" size={72} color="#D9B168" />
            </View>
            <Text className="text-2xl font-bold text-forest mb-3">Welcome to StayAfrica</Text>
            <Text className="text-moss text-center mb-8 px-4 leading-6">
              Sign in to access your profile and personalize your experience
            </Text>
            <View className="flex-row gap-3">
              <TouchableOpacity className="flex-1" onPress={() => router.push('/(auth)/login')}>
                <LinearGradient
                  colors={['#D9B168', '#bea04f']}
                  className="px-6 py-4 rounded-2xl items-center"
                  style={{ shadowColor: '#D9B168', shadowOffset: { width: 0, height: 4 }, shadowOpacity: 0.3, shadowRadius: 8, elevation: 5 }}
                >
                  <Text className="text-forest font-bold text-base">Log In</Text>
                </LinearGradient>
              </TouchableOpacity>
              <TouchableOpacity className="flex-1" onPress={() => router.push('/(auth)/register')}>
                <LinearGradient
                  colors={['#122F26', '#1d392f']}
                  className="px-6 py-4 rounded-2xl items-center"
                  style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 4 }, shadowOpacity: 0.3, shadowRadius: 8, elevation: 5 }}
                >
                  <Text className="text-gold font-bold text-base">Sign Up</Text>
                </LinearGradient>
              </TouchableOpacity>
            </View>
          </View>
        </View>
      </SafeAreaView>
    );
  }

  const handleLogout = () => {
    Alert.alert('Logout', 'Are you sure you want to logout?', [
      { text: 'Cancel', style: 'cancel' },
      {
        text: 'Logout',
        style: 'destructive',
        onPress: () => {
          logout();
          router.replace('/(auth)/login');
        },
      },
    ]);
  };

  // Compact settings row
  const SettingsRow = ({ icon, label, onPress, color = '#3A5C50', showBorder = true }: {
    icon: keyof typeof Ionicons.glyphMap;
    label: string;
    onPress: () => void;
    color?: string;
    showBorder?: boolean;
  }) => (
    <TouchableOpacity
      className={`flex-row items-center py-3.5 px-4 ${showBorder ? 'border-b border-sand-100' : ''}`}
      onPress={onPress}
      activeOpacity={0.6}
      accessibilityRole="button"
      accessibilityLabel={label}
    >
      <View
        className="w-9 h-9 rounded-xl items-center justify-center mr-3"
        style={{ backgroundColor: `${color}12` }}
      >
        <Ionicons name={icon} size={18} color={color} />
      </View>
      <Text className="flex-1 text-forest font-medium text-sm">{label}</Text>
      <Ionicons name="chevron-forward" size={18} color="#94a3b8" />
    </TouchableOpacity>
  );

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      <Sidebar isVisible={sidebarVisible} onClose={() => setSidebarVisible(false)} />

      <ScrollView className="flex-1" showsVerticalScrollIndicator={false}>
        {/* Header with Profile */}
        <LinearGradient
          colors={['#122F26', '#1d392f', '#2d4a40']}
          start={{ x: 0, y: 0 }}
          end={{ x: 1, y: 1 }}
          className="pb-8"
          style={{ paddingTop: insets.top + 12 }}
        >
          <View className="flex-row items-center justify-between px-4 mb-6">
            <TouchableOpacity
              onPress={() => setSidebarVisible(true)}
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
              accessibilityRole="button"
              accessibilityLabel="Open menu"
            >
              <Ionicons name="menu" size={24} color="#fff" />
            </TouchableOpacity>
            <TouchableOpacity
              onPress={() => router.push('/(tabs)/profile/edit')}
              className="flex-row items-center px-3 py-1.5 rounded-full"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.12)' }}
              accessibilityRole="button"
              accessibilityLabel="Edit profile"
            >
              <Ionicons name="create-outline" size={16} color="#D9B168" />
              <Text className="text-gold text-xs font-semibold ml-1">Edit</Text>
            </TouchableOpacity>
          </View>

          <View className="items-center">
            <Avatar uri={null} firstName={user?.first_name} lastName={user?.last_name} size="large" />
            <Text className="text-2xl font-black text-white text-center mt-3">
              {user?.first_name} {user?.last_name}
            </Text>
            <Text className="text-sand-200 text-sm text-center mt-1">{user?.email}</Text>
            <View className="flex-row items-center mt-2">
              {user?.is_verified ? (
                <View className="flex-row items-center bg-green-500/20 px-3 py-1 rounded-full">
                  <Ionicons name="shield-checkmark" size={13} color="#10B981" />
                  <Text className="text-green-400 text-xs font-semibold ml-1">Verified</Text>
                </View>
              ) : (
                <TouchableOpacity
                  className="flex-row items-center bg-yellow-500/20 px-3 py-1 rounded-full"
                  onPress={() => router.push('/profile/verification')}
                >
                  <Ionicons name="alert-circle" size={13} color="#F59E0B" />
                  <Text className="text-yellow-400 text-xs font-semibold ml-1">Verify now</Text>
                </TouchableOpacity>
              )}
            </View>
          </View>
        </LinearGradient>

        {/* Quick Stats - uses real data */}
        <Animated.View entering={FadeInDown.delay(100).duration(400)} className="flex-row px-4 -mt-4" style={{ gap: 8 }}>
          {[
            { label: 'Bookings', value: totalBookings, icon: 'calendar' as const, color: '#3B82F6' },
            { label: 'Saved', value: savedCount, icon: 'heart' as const, color: '#EF4444' },
            { label: 'Reviews', value: 0, icon: 'star' as const, color: '#8B5CF6' },
          ].map((stat) => (
            <View
              key={stat.label}
              className="flex-1 bg-white rounded-2xl p-3 items-center"
              style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 3 }, shadowOpacity: 0.06, shadowRadius: 6, elevation: 3 }}
            >
              <View className="flex-row items-center">
                <Ionicons name={stat.icon} size={16} color={stat.color} />
                <Text className="text-xl font-bold text-forest ml-2">{stat.value}</Text>
              </View>
              <Text className="text-xs text-moss mt-0.5">{stat.label}</Text>
            </View>
          ))}
        </Animated.View>

        {/* Host/Guest switcher */}
        {isHost && (
          <Animated.View entering={FadeInDown.delay(200).duration(400)} className="mx-4 mt-4">
            <LinearGradient
              colors={activeProfile === 'host' ? ['#122F26', '#1d392f'] : ['#D9B168', '#c5a050']}
              className="p-4 rounded-2xl"
              start={{ x: 0, y: 0 }}
              end={{ x: 1, y: 0 }}
            >
              <View className="flex-row items-center justify-between">
                <View className="flex-1">
                  <Text className="text-white font-bold text-sm">
                    {activeProfile === 'host' ? '🏠 Hosting Mode' : '✈️ Traveling Mode'}
                  </Text>
                  <Text className="text-white/70 text-xs mt-0.5">
                    {activeProfile === 'host' ? 'Switch to traveler view' : 'Switch to host dashboard'}
                  </Text>
                </View>
                <TouchableOpacity
                  disabled={switchLoading}
                  onPress={() => handleSwitchProfile(activeProfile === 'host' ? 'guest' : 'host')}
                  className="bg-white/20 rounded-xl px-4 py-2"
                >
                  <Text className="text-white font-semibold text-xs">
                    {switchLoading ? 'Switching...' : 'Switch'}
                  </Text>
                </TouchableOpacity>
              </View>
            </LinearGradient>
          </Animated.View>
        )}

        {/* Account Info */}
        <Animated.View entering={FadeInDown.delay(250).duration(400)} className="mx-4 mt-5">
          <Text className="text-sm font-bold text-moss uppercase tracking-wider mb-2 px-1">Account</Text>
          <View
            className="bg-white rounded-2xl overflow-hidden"
            style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.04, shadowRadius: 4, elevation: 2 }}
          >
            <View className="flex-row items-center py-3 px-4 border-b border-sand-100">
              <Ionicons name="person-outline" size={16} color="#3A5C50" />
              <Text className="text-moss text-xs ml-2 w-16">Name</Text>
              <Text className="flex-1 text-forest font-medium text-sm text-right">
                {user?.first_name} {user?.last_name}
              </Text>
            </View>
            <View className="flex-row items-center py-3 px-4 border-b border-sand-100">
              <Ionicons name="call-outline" size={16} color="#3A5C50" />
              <Text className="text-moss text-xs ml-2 w-16">Phone</Text>
              <Text className="flex-1 text-forest font-medium text-sm text-right">
                {user?.phone_number || 'Not set'}
              </Text>
            </View>
            <View className="flex-row items-center py-3 px-4">
              <Ionicons name="location-outline" size={16} color="#3A5C50" />
              <Text className="text-moss text-xs ml-2 w-16">Country</Text>
              <Text className="flex-1 text-forest font-medium text-sm text-right">
                {user?.country_of_residence || 'Not set'}
              </Text>
            </View>
          </View>
        </Animated.View>

        {/* Quick Actions grid */}
        <Animated.View entering={FadeInDown.delay(300).duration(400)} className="mx-4 mt-5">
          <Text className="text-sm font-bold text-moss uppercase tracking-wider mb-2 px-1">Quick Access</Text>
          <View className="flex-row flex-wrap" style={{ gap: 8 }}>
            {[
              { icon: 'calendar' as const, label: 'Bookings', color: '#3B82F6', route: '/(tabs)/bookings' },
              { icon: 'heart' as const, label: 'Wishlist', color: '#EF4444', route: '/(tabs)/wishlist' },
              { icon: 'chatbubbles' as const, label: 'Messages', color: '#8B5CF6', route: '/(tabs)/messages' },
              { icon: 'wallet' as const, label: 'Wallet', color: '#10B981', route: '/(tabs)/wallet' },
              { icon: 'star' as const, label: 'Reviews', color: '#F59E0B', route: '/reviews/my-reviews' },
              { icon: isHost ? 'business' as const : 'add-circle-outline' as const, label: isHost ? 'Host Mode' : 'Become Host', color: '#F97316', route: '/(tabs)/host' },
            ].map((item) => (
              <TouchableOpacity
                key={item.label}
                style={{ width: '31%' }}
                onPress={() => {
                  if (item.label === 'Host Mode' && activeProfile !== 'host') {
                    handleSwitchProfile('host');
                  } else {
                    router.push(item.route as any);
                  }
                }}
                activeOpacity={0.7}
                accessibilityRole="button"
                accessibilityLabel={item.label}
              >
                <View
                  className="bg-white rounded-2xl p-3.5 items-center"
                  style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.04, shadowRadius: 4, elevation: 2 }}
                >
                  <View
                    className="w-10 h-10 rounded-xl items-center justify-center mb-1.5"
                    style={{ backgroundColor: `${item.color}12` }}
                  >
                    <Ionicons name={item.icon} size={22} color={item.color} />
                  </View>
                  <Text className="text-forest font-semibold text-xs text-center">{item.label}</Text>
                </View>
              </TouchableOpacity>
            ))}
          </View>
        </Animated.View>

        {/* Settings */}
        <Animated.View entering={FadeInDown.delay(350).duration(400)} className="mx-4 mt-5">
          <Text className="text-sm font-bold text-moss uppercase tracking-wider mb-2 px-1">Settings</Text>
          <View
            className="bg-white rounded-2xl overflow-hidden"
            style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.04, shadowRadius: 4, elevation: 2 }}
          >
            <SettingsRow icon="create-outline" label="Edit Profile" onPress={() => router.push('/(tabs)/profile/edit')} color="#D9B168" />
            <SettingsRow icon="lock-closed-outline" label="Change Password" onPress={() => router.push('/(tabs)/profile/change-password')} color="#3B82F6" />
            <SettingsRow icon="shield-checkmark-outline" label="Security & 2FA" onPress={() => router.push('/(tabs)/profile/security')} color="#10B981" />
            <SettingsRow icon="notifications-outline" label="Notifications" onPress={() => router.push('/(tabs)/profile/notifications')} color="#8B5CF6" />
            <SettingsRow icon="heart-outline" label="Travel Preferences" onPress={() => router.push('/(tabs)/profile/preferences')} color="#EC4899" />
            <SettingsRow icon="card-outline" label="Payment Methods" onPress={() => router.push('/(tabs)/wallet')} color="#F59E0B" showBorder={false} />
          </View>
        </Animated.View>

        {/* Logout */}
        <Animated.View entering={FadeInDown.delay(400).duration(400)} className="px-4 mt-5 mb-8">
          <TouchableOpacity
            className="rounded-2xl overflow-hidden"
            onPress={handleLogout}
            activeOpacity={0.85}
            accessibilityRole="button"
            accessibilityLabel="Logout"
          >
            <View className="bg-red-50 py-4 flex-row items-center justify-center rounded-2xl border border-red-100">
              <Ionicons name="log-out-outline" size={20} color="#EF4444" />
              <Text className="text-red-500 font-bold ml-2 text-sm">Logout</Text>
            </View>
          </TouchableOpacity>
        </Animated.View>
      </ScrollView>
    </SafeAreaView>
  );
}
