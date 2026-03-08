import { View, Text, ScrollView, TouchableOpacity, RefreshControl, Dimensions } from 'react-native';
import { useState, useMemo } from 'react';
import { Redirect, useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { AnimatedCompassIcon } from '@/components/common/AnimatedCompassIcon';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { Sidebar } from '@/components/common/Sidebar';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { useBookings, useWishlist, useUnreadCount } from '@/hooks/api-hooks';
import Animated, { FadeInDown, FadeInRight } from 'react-native-reanimated';

const { width: SCREEN_WIDTH } = Dimensions.get('window');

function getGreeting(): string {
  const h = new Date().getHours();
  if (h < 12) return 'Good morning';
  if (h < 17) return 'Good afternoon';
  return 'Good evening';
}

function formatDateShort(dateStr: string): string {
  return new Date(dateStr).toLocaleDateString('en-US', { month: 'short', day: 'numeric' });
}

function nightsBetween(checkIn: string, checkOut: string): number {
  const ms = new Date(checkOut).getTime() - new Date(checkIn).getTime();
  return Math.max(1, Math.round(ms / 86400000));
}

interface StatCardProps {
  icon: keyof typeof Ionicons.glyphMap;
  label: string;
  value: string | number;
  color: string;
  onPress?: () => void;
  index: number;
}

interface QuickActionProps {
  icon: keyof typeof Ionicons.glyphMap;
  title: string;
  onPress: () => void;
  color: string;
  index: number;
}

export default function GuestDashboardScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const { user, isAuthenticated, isLoading, refreshUser } = useAuth();
  const [sidebarVisible, setSidebarVisible] = useState(false);
  const [refreshing, setRefreshing] = useState(false);

  const { data: bookingsData, refetch: refetchBookings } = useBookings();
  const { data: wishlistData, refetch: refetchWishlist } = useWishlist();
  const { data: unreadData, refetch: refetchUnread } = useUnreadCount();

  const bookings = bookingsData?.results ?? [];
  const totalBookings = bookings.length;
  const upcomingTrips = useMemo(
    () =>
      bookings
        .filter((b: any) => b.status === 'confirmed' && new Date(b.check_in) >= new Date())
        .sort((a: any, b: any) => new Date(a.check_in).getTime() - new Date(b.check_in).getTime())
        .slice(0, 3),
    [bookings],
  );
  const savedProperties = wishlistData?.results?.length ?? 0;
  const unreadMessages = unreadData?.unread_count ?? 0;

  // Build recent activity from bookings data
  const recentActivity = useMemo(() => {
    return bookings
      .slice(0, 5)
      .map((b: any) => ({
        id: b.id,
        type: 'booking' as const,
        title: b.property?.title || 'Booking',
        subtitle: `${formatDateShort(b.check_in)} → ${formatDateShort(b.check_out)} · ${b.status}`,
        status: b.status,
      }));
  }, [bookings]);

  const onRefresh = async () => {
    setRefreshing(true);
    try {
      await Promise.all([refreshUser(), refetchBookings(), refetchWishlist(), refetchUnread()]);
    } catch (error) {
      console.error('Error refreshing dashboard:', error);
    } finally {
      setRefreshing(false);
    }
  };

  if (isAuthenticated && user?.active_profile === 'host') {
    return <Redirect href="/(tabs)/host" />;
  }

  if (isLoading) {
    return null;
  }

  if (!isAuthenticated) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100">
        <Sidebar isVisible={sidebarVisible} onClose={() => setSidebarVisible(false)} />
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: insets.top + 12 }}
        >
          <View className="flex-row items-center justify-between mb-4">
            <TouchableOpacity
              onPress={() => setSidebarVisible(true)}
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="menu" size={24} color="#fff" />
            </TouchableOpacity>
          </View>
          <Text className="text-3xl font-black text-white tracking-tight">Dashboard</Text>
          <Text className="text-sand-200 text-sm mt-1">Your travel hub</Text>
        </LinearGradient>

        <View className="flex-1 items-center justify-center px-6">
          <View
            className="bg-white rounded-3xl p-8 items-center"
            style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}
          >
            <View className="bg-sand-200 rounded-full p-8 mb-6">
              <AnimatedCompassIcon size={72} color="#D9B168" />
            </View>
            <Text className="text-2xl font-bold text-forest mb-3">Welcome to StayAfrica</Text>
            <Text className="text-moss text-center mb-8 px-4 leading-6">
              Sign in to access your personalized travel dashboard
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

  // --- Stat Card ---
  const StatCard = ({ icon, label, value, color, onPress, index }: StatCardProps) => (
    <Animated.View entering={FadeInDown.delay(index * 80).duration(400)} className="flex-1 m-1.5">
      <TouchableOpacity
        className="bg-white rounded-2xl p-4"
        onPress={onPress}
        activeOpacity={0.7}
        accessibilityRole="button"
        accessibilityLabel={`${label}: ${value}`}
        style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 3 }, shadowOpacity: 0.06, shadowRadius: 6, elevation: 3 }}
      >
        <View className="flex-row items-center justify-between mb-2">
          <View
            className="w-10 h-10 rounded-xl items-center justify-center"
            style={{ backgroundColor: `${color}15` }}
          >
            <Ionicons name={icon} size={20} color={color} />
          </View>
          {typeof value === 'number' && value > 0 && (
            <View className="w-2 h-2 rounded-full" style={{ backgroundColor: color }} />
          )}
        </View>
        <Text className="text-2xl font-black text-forest">{value}</Text>
        <Text className="text-xs text-moss mt-0.5">{label}</Text>
      </TouchableOpacity>
    </Animated.View>
  );

  // --- Quick Action (compact grid) ---
  const QuickAction = ({ icon, title, onPress, color, index }: QuickActionProps) => (
    <Animated.View entering={FadeInDown.delay(300 + index * 60).duration(350)} style={{ width: (SCREEN_WIDTH - 48) / 3 }}>
      <TouchableOpacity
        className="bg-white rounded-2xl p-3 items-center"
        onPress={onPress}
        activeOpacity={0.7}
        accessibilityRole="button"
        accessibilityLabel={title}
        style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.04, shadowRadius: 4, elevation: 2 }}
      >
        <View
          className="w-11 h-11 rounded-xl items-center justify-center mb-2"
          style={{ backgroundColor: `${color}12` }}
        >
          {icon === 'compass' ? (
            <AnimatedCompassIcon size={22} color={color} filled />
          ) : (
            <Ionicons name={icon} size={22} color={color} />
          )}
        </View>
        <Text className="text-xs font-semibold text-forest text-center" numberOfLines={1}>
          {title}
        </Text>
      </TouchableOpacity>
    </Animated.View>
  );

  // --- Upcoming Trip Card ---
  const TripCard = ({ booking, index }: { booking: any; index: number }) => {
    const nights = nightsBetween(booking.check_in, booking.check_out);
    return (
      <Animated.View entering={FadeInRight.delay(index * 100).duration(400)}>
        <TouchableOpacity
          className="bg-white rounded-2xl overflow-hidden mr-3"
          style={{ width: SCREEN_WIDTH * 0.78, shadowColor: '#122F26', shadowOffset: { width: 0, height: 4 }, shadowOpacity: 0.08, shadowRadius: 8, elevation: 4 }}
          onPress={() => router.push(`/(tabs)/bookings/${booking.id}`)}
          activeOpacity={0.8}
          accessibilityRole="button"
          accessibilityLabel={`Upcoming trip to ${booking.property?.title || 'property'}`}
        >
          <LinearGradient
            colors={['#122F26', '#1d3a30']}
            start={{ x: 0, y: 0 }}
            end={{ x: 1, y: 1 }}
            className="px-4 py-3"
          >
            <View className="flex-row items-center justify-between">
              <View className="flex-1 mr-3">
                <Text className="text-white font-bold text-base" numberOfLines={1}>
                  {booking.property?.title || 'Property'}
                </Text>
                <View className="flex-row items-center mt-1">
                  <Ionicons name="location" size={12} color="#D9B168" />
                  <Text className="text-sand-200 text-xs ml-1" numberOfLines={1}>
                    {booking.property?.city || 'Location'}
                  </Text>
                </View>
              </View>
              <View className="bg-green-500/20 px-2.5 py-1 rounded-full">
                <Text className="text-green-400 text-xs font-semibold">Confirmed</Text>
              </View>
            </View>
          </LinearGradient>

          <View className="px-4 py-3">
            <View className="flex-row items-center justify-between">
              <View>
                <Text className="text-moss text-xs">Check-in</Text>
                <Text className="text-forest font-bold text-sm">{formatDateShort(booking.check_in)}</Text>
              </View>
              <View className="items-center px-3">
                <View className="flex-row items-center">
                  <View className="w-6 h-px bg-sand-200" />
                  <Text className="text-moss text-xs mx-1.5">{nights}n</Text>
                  <View className="w-6 h-px bg-sand-200" />
                </View>
              </View>
              <View className="items-end">
                <Text className="text-moss text-xs">Check-out</Text>
                <Text className="text-forest font-bold text-sm">{formatDateShort(booking.check_out)}</Text>
              </View>
            </View>
          </View>
        </TouchableOpacity>
      </Animated.View>
    );
  };

  // Status color helpers for activity
  const getActivityColor = (status: string) => {
    switch (status) {
      case 'confirmed': return '#10B981';
      case 'pending': return '#F59E0B';
      case 'cancelled': return '#EF4444';
      case 'checked_in': return '#3B82F6';
      default: return '#6B7280';
    }
  };

  const getActivityIcon = (status: string): keyof typeof Ionicons.glyphMap => {
    switch (status) {
      case 'confirmed': return 'checkmark-circle';
      case 'pending': return 'time';
      case 'cancelled': return 'close-circle';
      case 'checked_in': return 'enter';
      default: return 'calendar';
    }
  };

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      <ScrollView
        className="flex-1"
        showsVerticalScrollIndicator={false}
        refreshControl={
          <RefreshControl refreshing={refreshing} onRefresh={onRefresh} tintColor="#D9B168" colors={['#D9B168']} />
        }
      >
        <Sidebar isVisible={sidebarVisible} onClose={() => setSidebarVisible(false)} />

        {/* Header */}
        <LinearGradient
          colors={['#122F26', '#1d392f', '#2d4a40']}
          start={{ x: 0, y: 0 }}
          end={{ x: 1, y: 1 }}
          className="px-4 pb-6"
          style={{ paddingTop: insets.top + 12 }}
        >
          <View className="flex-row items-center justify-between mb-4">
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
              onPress={() => router.push('/notifications')}
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
              accessibilityRole="button"
              accessibilityLabel={`Notifications${unreadMessages > 0 ? `, ${unreadMessages} unread` : ''}`}
            >
              <Ionicons name="notifications-outline" size={22} color="#fff" />
              {unreadMessages > 0 && (
                <View className="absolute -top-0.5 -right-0.5 bg-gold w-4 h-4 rounded-full items-center justify-center">
                  <Text className="text-forest text-[9px] font-bold">{unreadMessages > 9 ? '9+' : unreadMessages}</Text>
                </View>
              )}
            </TouchableOpacity>
          </View>

          <Text className="text-2xl font-black text-white tracking-tight mb-0.5">
            {getGreeting()}, {user?.first_name} 👋
          </Text>
          <Text className="text-sand-200 text-sm">Here's your travel overview</Text>
        </LinearGradient>

        {/* Verification Banner */}
        {!user?.is_verified && (
          <Animated.View entering={FadeInDown.delay(100).duration(400)} className="mx-4 mt-4">
            <TouchableOpacity onPress={() => router.push('/profile/verification')} activeOpacity={0.85}>
              <LinearGradient
                colors={['#F59E0B', '#D97706']}
                className="p-3.5 rounded-2xl flex-row items-center"
                start={{ x: 0, y: 0 }}
                end={{ x: 1, y: 0 }}
              >
                <View className="bg-white/20 rounded-full p-2 mr-3">
                  <Ionicons name="alert-circle" size={20} color="#fff" />
                </View>
                <View className="flex-1">
                  <Text className="text-white font-bold text-sm">Verify Your Identity</Text>
                  <Text className="text-white/80 text-xs mt-0.5">Unlock faster bookings & deals</Text>
                </View>
                <Ionicons name="chevron-forward" size={20} color="#fff" />
              </LinearGradient>
            </TouchableOpacity>
          </Animated.View>
        )}

        {/* Stats Grid */}
        <View className="px-2.5 mt-4">
          <View className="flex-row">
            <StatCard icon="airplane" label="Upcoming" value={upcomingTrips.length} color="#3B82F6" onPress={() => router.push('/(tabs)/bookings')} index={0} />
            <StatCard icon="heart" label="Saved" value={savedProperties} color="#EF4444" onPress={() => router.push('/(tabs)/wishlist')} index={1} />
          </View>
          <View className="flex-row">
            <StatCard icon="checkmark-circle" label="Bookings" value={totalBookings} color="#10B981" onPress={() => router.push('/(tabs)/bookings')} index={2} />
            <StatCard icon="chatbubbles" label="Messages" value={unreadMessages} color="#8B5CF6" onPress={() => router.push('/(tabs)/messages')} index={3} />
          </View>
        </View>

        {/* Upcoming Trips */}
        {upcomingTrips.length > 0 && (
          <Animated.View entering={FadeInDown.delay(250).duration(400)} className="mt-6">
            <View className="flex-row items-center justify-between px-4 mb-3">
              <Text className="text-lg font-bold text-forest">Upcoming Trips</Text>
              <TouchableOpacity onPress={() => router.push('/(tabs)/bookings')}>
                <Text className="text-gold font-semibold text-sm">See all</Text>
              </TouchableOpacity>
            </View>
            <ScrollView horizontal showsHorizontalScrollIndicator={false} contentContainerStyle={{ paddingHorizontal: 16 }}>
              {upcomingTrips.map((booking: any, i: number) => (
                <TripCard key={booking.id} booking={booking} index={i} />
              ))}
            </ScrollView>
          </Animated.View>
        )}

        {/* Quick Actions — compact 2×3 grid */}
        <Animated.View entering={FadeInDown.delay(350).duration(400)} className="mt-6 px-4">
          <Text className="text-lg font-bold text-forest mb-3">Quick Actions</Text>
          <View className="flex-row flex-wrap justify-between" style={{ gap: 8 }}>
            <QuickAction icon="compass" title="Explore" onPress={() => router.push('/(tabs)/explore')} color="#D9B168" index={0} />
            <QuickAction icon="calendar" title="Bookings" onPress={() => router.push('/(tabs)/bookings')} color="#3B82F6" index={1} />
            <QuickAction icon="person" title="Profile" onPress={() => router.push('/(tabs)/profile')} color="#10B981" index={2} />
            <QuickAction icon="wallet" title="Wallet" onPress={() => router.push('/(tabs)/wallet')} color="#8B5CF6" index={3} />
            <QuickAction icon="star" title="Reviews" onPress={() => router.push('/reviews/my-reviews')} color="#EC4899" index={4} />
            <QuickAction icon="shield-checkmark" title="Verify" onPress={() => router.push('/profile/verification')} color="#6366F1" index={5} />
          </View>
        </Animated.View>

        {/* Recent Activity */}
        <Animated.View entering={FadeInDown.delay(450).duration(400)} className="px-4 mt-6 mb-8">
          <View className="flex-row items-center justify-between mb-3">
            <Text className="text-lg font-bold text-forest">Recent Activity</Text>
            {recentActivity.length > 0 && (
              <TouchableOpacity onPress={() => router.push('/(tabs)/bookings')}>
                <Text className="text-gold font-semibold text-sm">View All</Text>
              </TouchableOpacity>
            )}
          </View>

          <View
            className="bg-white rounded-2xl overflow-hidden"
            style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 3 }, shadowOpacity: 0.06, shadowRadius: 6, elevation: 3 }}
          >
            {recentActivity.length === 0 ? (
              <View className="items-center py-8 px-6">
                <View className="bg-sand-100 rounded-full p-3.5 mb-3">
                  <Ionicons name="time-outline" size={28} color="#94a3b8" />
                </View>
                <Text className="text-forest font-semibold text-sm">No Recent Activity</Text>
                <Text className="text-moss text-xs text-center mt-1">
                  Your bookings and activity will appear here
                </Text>
              </View>
            ) : (
              recentActivity.map((item: any, idx: number) => (
                <TouchableOpacity
                  key={item.id}
                  className={`flex-row items-center px-4 py-3.5 ${idx < recentActivity.length - 1 ? 'border-b border-sand-100' : ''}`}
                  onPress={() => router.push(`/(tabs)/bookings/${item.id}`)}
                  activeOpacity={0.7}
                >
                  <View
                    className="w-9 h-9 rounded-xl items-center justify-center mr-3"
                    style={{ backgroundColor: `${getActivityColor(item.status)}12` }}
                  >
                    <Ionicons name={getActivityIcon(item.status)} size={18} color={getActivityColor(item.status)} />
                  </View>
                  <View className="flex-1">
                    <Text className="text-sm font-semibold text-forest" numberOfLines={1}>{item.title}</Text>
                    <Text className="text-xs text-moss mt-0.5">{item.subtitle}</Text>
                  </View>
                  <Ionicons name="chevron-forward" size={16} color="#94a3b8" />
                </TouchableOpacity>
              ))
            )}
          </View>
        </Animated.View>
      </ScrollView>
    </SafeAreaView>
  );
}
