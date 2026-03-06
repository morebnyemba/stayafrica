import { View, Text, FlatList, TouchableOpacity, RefreshControl } from 'react-native';
import { useRouter } from 'expo-router';
import { useState, useCallback } from 'react';
import { useBookings, useExperienceBookings } from '@/hooks/api-hooks';
import { useAuth } from '@/context/auth-context';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { BookingCardSkeleton } from '@/components/common/Skeletons';
import { Booking, ExperienceBooking } from '@/types';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { Sidebar } from '@/components/common/Sidebar';
import Animated, { FadeInDown } from 'react-native-reanimated';

type BookingTab = 'properties' | 'experiences';

function formatDateShort(dateStr: string): string {
  return new Date(dateStr).toLocaleDateString('en-US', { month: 'short', day: 'numeric' });
}

function nightsBetween(checkIn: string, checkOut: string): number {
  const ms = new Date(checkOut).getTime() - new Date(checkIn).getTime();
  return Math.max(1, Math.round(ms / 86400000));
}

const STATUS_CONFIG: Record<string, { bg: string; text: string; icon: keyof typeof Ionicons.glyphMap }> = {
  confirmed: { bg: '#10B98118', text: '#10B981', icon: 'checkmark-circle' },
  pending: { bg: '#F59E0B18', text: '#F59E0B', icon: 'time' },
  cancelled: { bg: '#EF444418', text: '#EF4444', icon: 'close-circle' },
  checked_in: { bg: '#3B82F618', text: '#3B82F6', icon: 'enter' },
  checked_out: { bg: '#8B5CF618', text: '#8B5CF6', icon: 'checkbox' },
  completed: { bg: '#8B5CF618', text: '#8B5CF6', icon: 'checkbox' },
};

const getStatus = (s: string) => STATUS_CONFIG[s] || { bg: '#6B728018', text: '#6B7280', icon: 'information-circle' as const };

export default function BookingsScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const { isAuthenticated } = useAuth();
  const { data: bookingsData, isLoading: propLoading, refetch: refetchBookings } = useBookings();
  const { data: expBookingsRaw, isLoading: expLoading, refetch: refetchExp } = useExperienceBookings();
  const bookings = bookingsData?.results || [];
  const expBookings: ExperienceBooking[] = Array.isArray(expBookingsRaw)
    ? expBookingsRaw
    : (expBookingsRaw as any)?.results ?? [];
  const [sidebarVisible, setSidebarVisible] = useState(false);
  const [activeTab, setActiveTab] = useState<BookingTab>('properties');
  const [refreshing, setRefreshing] = useState(false);

  const onRefresh = useCallback(async () => {
    setRefreshing(true);
    try {
      await Promise.all([refetchBookings(), refetchExp()]);
    } catch (e) { /* ignore */ }
    setRefreshing(false);
  }, [refetchBookings, refetchExp]);

  if (!isAuthenticated) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100">
        <Sidebar isVisible={sidebarVisible} onClose={() => setSidebarVisible(false)} />
        <LinearGradient colors={['#122F26', '#1d392f']} className="px-4 pb-6" style={{ paddingTop: insets.top + 12 }}>
          <View className="flex-row items-center justify-between mb-4">
            <TouchableOpacity
              onPress={() => setSidebarVisible(true)}
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="menu" size={24} color="#fff" />
            </TouchableOpacity>
          </View>
          <Text className="text-3xl font-black text-white tracking-tight">My Bookings</Text>
          <Text className="text-sand-200 text-sm mt-1">Manage your reservations</Text>
        </LinearGradient>
        <View className="flex-1 items-center justify-center px-6">
          <View className="bg-white rounded-3xl p-8 items-center" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}>
            <View className="bg-sand-200 rounded-full p-8 mb-6">
              <Ionicons name="calendar-outline" size={72} color="#D9B168" />
            </View>
            <Text className="text-2xl font-bold text-forest mb-3">Sign In Required</Text>
            <Text className="text-moss text-center mb-8 px-4 leading-6">Please sign in to view and manage your bookings</Text>
            <TouchableOpacity onPress={() => router.replace('/(auth)/login')}>
              <LinearGradient colors={['#D9B168', '#bea04f']} className="px-8 py-4 rounded-2xl" style={{ shadowColor: '#D9B168', shadowOffset: { width: 0, height: 4 }, shadowOpacity: 0.3, shadowRadius: 8, elevation: 5 }}>
                <Text className="text-forest font-bold text-base">Sign In Now</Text>
              </LinearGradient>
            </TouchableOpacity>
          </View>
        </View>
      </SafeAreaView>
    );
  }

  const BookingCard = ({ booking, index }: { booking: Booking; index: number }) => {
    const sc = getStatus(booking.status);
    const nights = nightsBetween(booking.check_in, booking.check_out);

    return (
      <Animated.View entering={FadeInDown.delay(index * 70).duration(350)}>
        <TouchableOpacity
          className="mx-4 mb-3 rounded-2xl overflow-hidden bg-white"
          onPress={() => router.push(`/(tabs)/bookings/${booking.id}`)}
          activeOpacity={0.8}
          accessibilityRole="button"
          accessibilityLabel={`Booking at ${booking.property?.title || 'property'}, ${booking.status}`}
          style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 4 }, shadowOpacity: 0.08, shadowRadius: 8, elevation: 4 }}
        >
          {/* Header */}
          <LinearGradient colors={['#122F26', '#1d3a30']} start={{ x: 0, y: 0 }} end={{ x: 1, y: 1 }} className="px-4 py-3">
            <View className="flex-row justify-between items-center">
              <View className="flex-1 mr-3">
                <Text className="text-white font-bold text-base" numberOfLines={1}>
                  {booking.property?.title || 'Property'}
                </Text>
                <View className="flex-row items-center mt-0.5">
                  <Ionicons name="location" size={12} color="#D9B168" />
                  <Text className="text-sand-200 text-xs ml-1" numberOfLines={1}>
                    {booking.property?.city || 'Location'}
                  </Text>
                </View>
              </View>
              <View className="px-2.5 py-1 rounded-full flex-row items-center" style={{ backgroundColor: sc.bg }}>
                <Ionicons name={sc.icon} size={12} color={sc.text} />
                <Text className="text-xs font-semibold capitalize ml-1" style={{ color: sc.text }}>
                  {booking.status.replace('_', ' ')}
                </Text>
              </View>
            </View>
          </LinearGradient>

          {/* Content */}
          <View className="px-4 py-3">
            {/* Dates row */}
            <View className="flex-row items-center justify-between bg-sand-100 rounded-xl px-3 py-2.5 mb-3">
              <View>
                <Text className="text-moss text-[10px] font-medium">CHECK-IN</Text>
                <Text className="text-forest font-bold text-sm">{formatDateShort(booking.check_in)}</Text>
              </View>
              <View className="items-center">
                <View className="flex-row items-center">
                  <View className="w-4 h-px bg-sand-300" />
                  <View className="bg-gold/20 px-2 py-0.5 rounded-full mx-1">
                    <Text className="text-gold text-[10px] font-bold">{nights}N</Text>
                  </View>
                  <View className="w-4 h-px bg-sand-300" />
                </View>
              </View>
              <View className="items-end">
                <Text className="text-moss text-[10px] font-medium">CHECK-OUT</Text>
                <Text className="text-forest font-bold text-sm">{formatDateShort(booking.check_out)}</Text>
              </View>
            </View>

            {/* Price + actions */}
            <View className="flex-row items-center justify-between">
              <View>
                <Text className="text-moss text-[10px] font-medium">TOTAL</Text>
                <Text className="text-xl font-black text-gold">${booking.grand_total}</Text>
              </View>

              <View className="flex-row items-center" style={{ gap: 8 }}>
                {booking.status === 'confirmed' && (booking as any).payment_status !== 'success' && (
                  <TouchableOpacity
                    onPress={(e) => {
                      e.stopPropagation();
                      router.push({
                        pathname: '/booking/payment',
                        params: {
                          bookingId: booking.id,
                          total: booking.grand_total,
                          propertyName: booking.property?.title || 'Property',
                          checkIn: booking.check_in,
                          checkOut: booking.check_out,
                          guests: booking.number_of_guests,
                        },
                      });
                    }}
                    activeOpacity={0.85}
                  >
                    <LinearGradient colors={['#D9B168', '#c5a050']} className="px-4 py-2 rounded-xl flex-row items-center">
                      <Ionicons name="card" size={14} color="#122F26" />
                      <Text className="text-forest font-bold text-xs ml-1">Pay</Text>
                    </LinearGradient>
                  </TouchableOpacity>
                )}
                <TouchableOpacity
                  onPress={() => router.push(`/(tabs)/bookings/${booking.id}`)}
                  className="bg-forest px-4 py-2 rounded-xl"
                  activeOpacity={0.8}
                >
                  <Text className="text-gold font-bold text-xs">Details</Text>
                </TouchableOpacity>
              </View>
            </View>
          </View>
        </TouchableOpacity>
      </Animated.View>
    );
  };

  const ExperienceBookingCard = ({ booking, index }: { booking: ExperienceBooking; index: number }) => {
    const sc = getStatus(booking.status);
    return (
      <Animated.View entering={FadeInDown.delay(index * 70).duration(350)}>
        <View
          className="mx-4 mb-3 rounded-2xl overflow-hidden bg-white"
          style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 4 }, shadowOpacity: 0.08, shadowRadius: 8, elevation: 4 }}
        >
          <LinearGradient colors={['#3A5C50', '#4a6f62']} start={{ x: 0, y: 0 }} end={{ x: 1, y: 1 }} className="px-4 py-3">
            <View className="flex-row justify-between items-center">
              <View className="flex-1 mr-3">
                <View className="flex-row items-center mb-0.5">
                  <Ionicons name="compass" size={12} color="#D9B168" />
                  <Text className="text-sand-200 text-[10px] ml-1 font-semibold tracking-wider">EXPERIENCE</Text>
                </View>
                <Text className="text-white font-bold text-base" numberOfLines={1}>
                  {(booking as any).experience_title || (booking as any).experience?.title || 'Experience'}
                </Text>
              </View>
              <View className="px-2.5 py-1 rounded-full flex-row items-center" style={{ backgroundColor: sc.bg }}>
                <Ionicons name={sc.icon} size={12} color={sc.text} />
                <Text className="text-xs font-semibold capitalize ml-1" style={{ color: sc.text }}>{booking.status}</Text>
              </View>
            </View>
          </LinearGradient>

          <View className="px-4 py-3">
            <View className="flex-row items-center justify-between bg-sand-100 rounded-xl px-3 py-2.5 mb-3">
              <View>
                <Text className="text-moss text-[10px] font-medium">DATE</Text>
                <Text className="text-forest font-bold text-sm">
                  {new Date(booking.booking_date).toLocaleDateString('en-US', { month: 'short', day: 'numeric', year: 'numeric' })}
                </Text>
              </View>
              <View className="items-end">
                <Text className="text-moss text-[10px] font-medium">PARTICIPANTS</Text>
                <View className="flex-row items-center">
                  <Ionicons name="people" size={14} color="#3A5C50" />
                  <Text className="text-forest font-bold text-sm ml-1">{booking.num_participants}</Text>
                </View>
              </View>
            </View>

            <View className="flex-row items-center justify-between">
              <View>
                <Text className="text-moss text-[10px] font-medium">TOTAL</Text>
                <Text className="text-xl font-black text-gold">${booking.total_amount}</Text>
              </View>
              {booking.status === 'pending' && (
                <TouchableOpacity onPress={() => router.push('/messages')} className="bg-forest px-4 py-2 rounded-xl">
                  <Text className="text-gold font-bold text-xs">Contact Host</Text>
                </TouchableOpacity>
              )}
            </View>
          </View>
        </View>
      </Animated.View>
    );
  };

  const EmptyBookings = ({ type }: { type: 'stays' | 'experiences' }) => (
    <View className="flex-1 justify-center items-center py-20 px-6">
      <View className="bg-white rounded-3xl p-8 items-center" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}>
        <View className="bg-sand-200 rounded-full p-8 mb-6">
          <Ionicons name={type === 'stays' ? 'calendar-outline' : 'compass-outline'} size={72} color="#3A5C50" />
        </View>
        <Text className="text-2xl font-bold text-forest mb-3">
          No {type === 'stays' ? 'Stays' : 'Experiences'} Yet
        </Text>
        <Text className="text-moss text-center mb-8 px-4 leading-6">
          {type === 'stays'
            ? 'Start exploring amazing properties and make your first booking'
            : 'Discover exciting experiences and activities to book'}
        </Text>
        <TouchableOpacity onPress={() => router.push('/(tabs)/explore')}>
          <LinearGradient
            colors={['#122F26', '#1d392f']}
            className="px-8 py-4 rounded-2xl"
            style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 4 }, shadowOpacity: 0.3, shadowRadius: 8, elevation: 5 }}
          >
            <Text className="text-gold font-bold text-base">
              Explore {type === 'stays' ? 'Properties' : 'Experiences'}
            </Text>
          </LinearGradient>
        </TouchableOpacity>
      </View>
    </View>
  );

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      <Sidebar isVisible={sidebarVisible} onClose={() => setSidebarVisible(false)} />

      <View className="flex-1 bg-sand-100">
        {/* Header */}
        <LinearGradient
          colors={['#122F26', '#1d392f', '#2d4a40']}
          start={{ x: 0, y: 0 }}
          end={{ x: 1, y: 1 }}
          className="px-4 pb-5"
          style={{ paddingTop: insets.top + 12 }}
        >
          <View className="flex-row items-center justify-between mb-3">
            <TouchableOpacity
              onPress={() => setSidebarVisible(true)}
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
              accessibilityRole="button"
              accessibilityLabel="Open menu"
            >
              <Ionicons name="menu" size={24} color="#fff" />
            </TouchableOpacity>
          </View>

          <Text className="text-2xl font-black text-white tracking-tight mb-1">My Bookings</Text>
          <View className="flex-row items-center">
            <Ionicons name="calendar" size={14} color="#D9B168" />
            <Text className="text-sand-200 text-sm ml-1.5">
              {activeTab === 'properties'
                ? `${bookings.length} ${bookings.length === 1 ? 'stay' : 'stays'}`
                : `${expBookings.length} ${expBookings.length === 1 ? 'experience' : 'experiences'}`}
            </Text>
          </View>

          {/* Tab Toggle */}
          <View className="flex-row mt-4 bg-white/12 rounded-xl p-1">
            {(['properties', 'experiences'] as const).map((tab) => (
              <TouchableOpacity
                key={tab}
                onPress={() => setActiveTab(tab)}
                className={`flex-1 py-2.5 rounded-lg items-center flex-row justify-center ${activeTab === tab ? 'bg-gold' : ''}`}
                accessibilityRole="tab"
                accessibilityState={{ selected: activeTab === tab }}
              >
                <Ionicons
                  name={tab === 'properties' ? 'home-outline' : 'compass-outline'}
                  size={15}
                  color={activeTab === tab ? '#122F26' : '#fff'}
                />
                <Text className={`ml-1.5 font-bold text-sm ${activeTab === tab ? 'text-forest' : 'text-white'}`}>
                  {tab === 'properties' ? 'Stays' : 'Experiences'}
                </Text>
              </TouchableOpacity>
            ))}
          </View>
        </LinearGradient>

        {/* Property Bookings */}
        {activeTab === 'properties' && (
          propLoading ? (
            <View className="pt-3">
              {[1, 2, 3].map((i) => <BookingCardSkeleton key={i} />)}
            </View>
          ) : (
            <FlatList
              data={bookings}
              renderItem={({ item, index }) => <BookingCard booking={item} index={index} />}
              keyExtractor={(item) => item.id}
              contentContainerStyle={{ paddingTop: 12, paddingBottom: 40 }}
              ListEmptyComponent={<EmptyBookings type="stays" />}
              refreshControl={
                <RefreshControl refreshing={refreshing} onRefresh={onRefresh} tintColor="#D9B168" colors={['#D9B168']} />
              }
            />
          )
        )}

        {/* Experience Bookings */}
        {activeTab === 'experiences' && (
          expLoading ? (
            <View className="pt-3">
              {[1, 2, 3].map((i) => <BookingCardSkeleton key={i} />)}
            </View>
          ) : (
            <FlatList
              data={expBookings}
              renderItem={({ item, index }) => <ExperienceBookingCard booking={item} index={index} />}
              keyExtractor={(item) => String(item.id)}
              contentContainerStyle={{ paddingTop: 12, paddingBottom: 40 }}
              ListEmptyComponent={<EmptyBookings type="experiences" />}
              refreshControl={
                <RefreshControl refreshing={refreshing} onRefresh={onRefresh} tintColor="#D9B168" colors={['#D9B168']} />
              }
            />
          )
        )}
      </View>
    </SafeAreaView>
  );
}
