import { View, Text, FlatList, TouchableOpacity, ActivityIndicator } from 'react-native';
import { useRouter } from 'expo-router';
import { useState } from 'react';
import { useBookings, useExperienceBookings } from '@/hooks/api-hooks';
import { useAuth } from '@/context/auth-context';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { BookingCardSkeleton } from '@/components/common/Skeletons';
import { Booking, ExperienceBooking } from '@/types';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { Sidebar } from '@/components/common/Sidebar';

type BookingTab = 'properties' | 'experiences';

export default function BookingsScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const { isAuthenticated } = useAuth();
  const { data: bookingsData, isLoading: propLoading } = useBookings();
  const { data: expBookingsRaw, isLoading: expLoading } = useExperienceBookings();
  const bookings = bookingsData?.results || [];
  const expBookings: ExperienceBooking[] = Array.isArray(expBookingsRaw)
    ? expBookingsRaw
    : (expBookingsRaw as any)?.results ?? [];
  const [sidebarVisible, setSidebarVisible] = useState(false);
  const [activeTab, setActiveTab] = useState<BookingTab>('properties');

  if (!isAuthenticated) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100">
        {/* Sidebar */}
        <Sidebar
          isVisible={sidebarVisible}
          onClose={() => setSidebarVisible(false)}
        />
        
        {/* Header */}
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: insets.top + 12 }}
        >
          {/* Top Navigation Bar with Menu */}
          <View className="flex-row items-center justify-between mb-4">
            {/* Hamburger Menu */}
            <TouchableOpacity
              onPress={() => setSidebarVisible(true)}
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="menu" size={24} color="#fff" />
            </TouchableOpacity>
          </View>
          
          <Text className="text-3xl font-black text-white tracking-tight">
            My Bookings
          </Text>
          <Text className="text-sand-200 text-sm mt-1">
            Manage your reservations
          </Text>
        </LinearGradient>

        <View className="flex-1 items-center justify-center px-6">
          <View className="bg-white rounded-3xl p-8 items-center" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}>
            <View className="bg-sand-200 rounded-full p-8 mb-6">
              <Ionicons name="calendar-outline" size={72} color="#D9B168" />
            </View>
            <Text className="text-2xl font-bold text-forest mb-3">Sign In Required</Text>
            <Text className="text-moss text-center mb-8 px-4 leading-6">
              Please sign in to view and manage your bookings
            </Text>
            <TouchableOpacity
              onPress={() => router.replace('/(auth)/login')}
            >
              <LinearGradient
                colors={['#D9B168', '#bea04f']}
                className="px-8 py-4 rounded-2xl"
                style={{
                  shadowColor: '#D9B168',
                  shadowOffset: { width: 0, height: 4 },
                  shadowOpacity: 0.3,
                  shadowRadius: 8,
                  elevation: 5,
                }}
              >
                <Text className="text-forest font-bold text-base">Sign In Now</Text>
              </LinearGradient>
            </TouchableOpacity>
          </View>
        </View>
      </SafeAreaView>
    );
  }

  // Status color mapping
  const STATUS_COLORS = {
    confirmed: { bg: 'bg-green-100', text: 'text-green-800', icon: 'checkmark-circle', bgColor: '#10B98120', textColor: '#10B981' },
    pending: { bg: 'bg-yellow-100', text: 'text-yellow-800', icon: 'time', bgColor: '#F59E0B20', textColor: '#F59E0B' },
    cancelled: { bg: 'bg-red-100', text: 'text-red-800', icon: 'close-circle', bgColor: '#EF444420', textColor: '#EF4444' },
    checked_in: { bg: 'bg-blue-100', text: 'text-blue-800', icon: 'enter', bgColor: '#3B82F620', textColor: '#3B82F6' },
    checked_out: { bg: 'bg-purple-100', text: 'text-purple-800', icon: 'checkbox', bgColor: '#8B5CF620', textColor: '#8B5CF6' },
    default: { bg: 'bg-gray-100', text: 'text-gray-800', icon: 'information-circle', bgColor: '#6B728020', textColor: '#6B7280' },
  };

  const getStatusColor = (status: string) => {
    return STATUS_COLORS[status as keyof typeof STATUS_COLORS] || STATUS_COLORS.default;
  };

  const BookingCard = ({ booking }: { booking: Booking & { property?: { title: string; city: string } } }) => {
    const statusStyle = getStatusColor(booking.status);
    
    return (
      <TouchableOpacity
        className="mx-4 mb-4 rounded-3xl overflow-hidden bg-white"
        onPress={() => router.push(`/(tabs)/bookings/${booking.id}`)}
        style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 6 },
          shadowOpacity: 0.12,
          shadowRadius: 12,
          elevation: 6,
        }}
      >
        {/* Header with Gradient */}
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          start={{ x: 0, y: 0 }}
          end={{ x: 1, y: 1 }}
          className="p-4 bg-white rounded-2xl"
        >
          <View className="flex-row justify-between items-start">
            <View className="flex-1">
              <Text className="text-xl font-bold text-white mb-1">
                {booking.property?.title || 'Property'}
              </Text>
              <View className="flex-row items-center">
                <Ionicons name="location" size={14} color="#D9B168" />
                <Text className="text-sand-200 text-sm ml-1">
                  {booking.property?.city || 'Unknown location'}
                </Text>
              </View>
            </View>
            <View
              className="px-3 py-1.5 rounded-full"
              style={{ backgroundColor: statusStyle.bgColor }}
            >
              <View className="flex-row items-center">
                <Ionicons name={statusStyle.icon as any} size={14} color={statusStyle.textColor} />
                <Text className={`text-xs font-semibold capitalize ml-1 ${statusStyle.text}`}>
                  {booking.status}
                </Text>
              </View>
            </View>
          </View>
        </LinearGradient>

        {/* Content */}
        <View className="p-4 bg-white rounded-2xl">
          {/* Dates Timeline */}
          <View className="bg-sand-100 rounded-2xl p-4 bg-white rounded-2xl mb-4">
            <View className="flex-row items-center justify-between">
              <View className="flex-1">
                <Text className="text-moss text-xs font-medium mb-1">Check-in</Text>
                <View className="flex-row items-center">
                  <Ionicons name="calendar" size={16} color="#3A5C50" />
                  <Text className="text-forest font-bold ml-2">
                    {new Date(booking.check_in).toLocaleDateString('en-US', { month: 'short', day: 'numeric' })}
                  </Text>
                </View>
              </View>
              
              <View className="px-4">
                <Ionicons name="arrow-forward" size={20} color="#D9B168" />
              </View>
              
              <View className="flex-1 items-end">
                <Text className="text-moss text-xs font-medium mb-1">Check-out</Text>
                <View className="flex-row items-center">
                  <Ionicons name="calendar" size={16} color="#3A5C50" />
                  <Text className="text-forest font-bold ml-2">
                    {new Date(booking.check_out).toLocaleDateString('en-US', { month: 'short', day: 'numeric' })}
                  </Text>
                </View>
              </View>
            </View>
          </View>

          {/* Price and Actions */}
          <View className="space-y-3">
            <View className="flex-row items-center justify-between">
              <View>
                <Text className="text-moss text-xs font-medium mb-1">Total Amount</Text>
                <Text className="text-2xl font-black text-gold">${booking.grand_total}</Text>
              </View>
              <TouchableOpacity 
                onPress={() => router.push(`/(tabs)/bookings/${booking.id}`)}
                className="bg-forest px-5 py-3 rounded-xl"
              >
                <Text className="text-gold font-bold text-sm">View Details</Text>
              </TouchableOpacity>
            </View>

            {/* Quick Actions */}
            {booking.status === 'confirmed' && (
              <View className="flex-row gap-2 pt-2">
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
                      }
                    });
                  }}
                  className="flex-1"
                >
                  <LinearGradient
                    colors={['#D9B168', '#bea04f']}
                    className="py-2.5 rounded-xl items-center"
                  >
                    <View className="flex-row items-center">
                      <Ionicons name="card" size={16} color="#122F26" />
                      <Text className="text-forest font-bold text-xs ml-1">Pay Now</Text>
                    </View>
                  </LinearGradient>
                </TouchableOpacity>
                <TouchableOpacity
                  onPress={(e) => {
                    e.stopPropagation();
                    router.push('/messages');
                  }}
                  className="flex-1 bg-white border border-forest py-2.5 rounded-xl items-center"
                >
                  <View className="flex-row items-center">
                    <Ionicons name="chatbubble-outline" size={16} color="#122F26" />
                    <Text className="text-forest font-bold text-xs ml-1">Chat</Text>
                  </View>
                </TouchableOpacity>
              </View>
            )}

            {booking.status === 'pending' && (
              <View className="flex-row gap-2 pt-2">
                <TouchableOpacity
                  onPress={(e) => {
                    e.stopPropagation();
                    router.push('/messages');
                  }}
                  className="flex-1 bg-white border border-moss py-2.5 rounded-xl items-center"
                >
                  <View className="flex-row items-center">
                    <Ionicons name="chatbubble-outline" size={16} color="#3A5C50" />
                    <Text className="text-moss font-semibold text-xs ml-1">Contact Host</Text>
                  </View>
                </TouchableOpacity>
              </View>
            )}
          </View>
        </View>
      </TouchableOpacity>
    );
  };

  const EXP_STATUS_COLORS: Record<string, { bgColor: string; textColor: string; icon: string }> = {
    confirmed: { bgColor: '#10B98120', textColor: '#10B981', icon: 'checkmark-circle' },
    pending: { bgColor: '#F59E0B20', textColor: '#F59E0B', icon: 'time' },
    cancelled: { bgColor: '#EF444420', textColor: '#EF4444', icon: 'close-circle' },
    completed: { bgColor: '#8B5CF620', textColor: '#8B5CF6', icon: 'checkbox' },
  };

  const ExperienceBookingCard = ({ booking }: { booking: ExperienceBooking }) => {
    const sc = EXP_STATUS_COLORS[booking.status] || EXP_STATUS_COLORS.pending;
    return (
      <View
        className="mx-4 mb-4 rounded-3xl overflow-hidden bg-white"
        style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 6 }, shadowOpacity: 0.12, shadowRadius: 12, elevation: 6 }}
      >
        {/* Header */}
        <LinearGradient
          colors={['#3A5C50', '#4a6f62']}
          start={{ x: 0, y: 0 }}
          end={{ x: 1, y: 1 }}
          className="p-4"
        >
          <View className="flex-row justify-between items-start">
            <View className="flex-1">
              <View className="flex-row items-center mb-1">
                <Ionicons name="compass" size={14} color="#D9B168" />
                <Text className="text-sand-200 text-xs ml-1 font-medium">EXPERIENCE</Text>
              </View>
              <Text className="text-xl font-bold text-white">
                {(booking as any).experience_title || (booking as any).experience?.title || 'Experience'}
              </Text>
            </View>
            <View className="px-3 py-1.5 rounded-full" style={{ backgroundColor: sc.bgColor }}>
              <View className="flex-row items-center">
                <Ionicons name={sc.icon as any} size={14} color={sc.textColor} />
                <Text className="text-xs font-semibold capitalize ml-1" style={{ color: sc.textColor }}>
                  {booking.status}
                </Text>
              </View>
            </View>
          </View>
        </LinearGradient>

        {/* Content */}
        <View className="p-4">
          <View className="bg-sand-100 rounded-2xl p-4 mb-4">
            <View className="flex-row items-center justify-between">
              <View className="flex-1">
                <Text className="text-moss text-xs font-medium mb-1">Date</Text>
                <View className="flex-row items-center">
                  <Ionicons name="calendar" size={16} color="#3A5C50" />
                  <Text className="text-forest font-bold ml-2">
                    {new Date(booking.booking_date).toLocaleDateString('en-US', { month: 'short', day: 'numeric', year: 'numeric' })}
                  </Text>
                </View>
              </View>
              <View className="flex-1 items-end">
                <Text className="text-moss text-xs font-medium mb-1">Participants</Text>
                <View className="flex-row items-center">
                  <Ionicons name="people" size={16} color="#3A5C50" />
                  <Text className="text-forest font-bold ml-2">{booking.num_participants}</Text>
                </View>
              </View>
            </View>
          </View>

          <View className="flex-row items-center justify-between">
            <View>
              <Text className="text-moss text-xs font-medium mb-1">Total Amount</Text>
              <Text className="text-2xl font-black text-gold">${booking.total_amount}</Text>
            </View>
            {booking.status === 'pending' && (
              <TouchableOpacity onPress={() => router.push('/messages')} className="bg-forest px-5 py-3 rounded-xl">
                <Text className="text-gold font-bold text-sm">Contact Host</Text>
              </TouchableOpacity>
            )}
          </View>
        </View>
      </View>
    );
  };

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      {/* Sidebar */}
      <Sidebar
        isVisible={sidebarVisible}
        onClose={() => setSidebarVisible(false)}
      />
      
      <View className="flex-1 bg-sand-100">
        {/* Modern Header */}
        <LinearGradient
          colors={['#122F26', '#1d392f', '#2d4a40']}
          start={{ x: 0, y: 0 }}
          end={{ x: 1, y: 1 }}
          className="px-4 pb-6"
          style={{ paddingTop: insets.top + 12 }}
        >
          {/* Top Navigation Bar with Menu */}
          <View className="flex-row items-center justify-between mb-4">
            {/* Hamburger Menu */}
            <TouchableOpacity
              onPress={() => setSidebarVisible(true)}
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="menu" size={24} color="#fff" />
            </TouchableOpacity>
          </View>
          
          <Text className="text-3xl font-black text-white tracking-tight mb-2">
            My Bookings
          </Text>
          <View className="flex-row items-center">
            <Ionicons name="calendar" size={16} color="#D9B168" />
            <Text className="text-sand-100 ml-2">
              {activeTab === 'properties'
                ? `${bookings.length} ${bookings.length === 1 ? 'stay' : 'stays'}`
                : `${expBookings.length} ${expBookings.length === 1 ? 'experience' : 'experiences'}`}
            </Text>
          </View>

          {/* Tab Toggle */}
          <View className="flex-row mt-4 bg-white/15 rounded-xl p-1">
            <TouchableOpacity
              onPress={() => setActiveTab('properties')}
              className={`flex-1 py-2.5 rounded-lg items-center flex-row justify-center ${activeTab === 'properties' ? 'bg-gold' : ''}`}
            >
              <Ionicons name="home-outline" size={16} color={activeTab === 'properties' ? '#122F26' : '#fff'} />
              <Text className={`ml-1.5 font-bold text-sm ${activeTab === 'properties' ? 'text-forest' : 'text-white'}`}>
                Stays
              </Text>
            </TouchableOpacity>
            <TouchableOpacity
              onPress={() => setActiveTab('experiences')}
              className={`flex-1 py-2.5 rounded-lg items-center flex-row justify-center ${activeTab === 'experiences' ? 'bg-gold' : ''}`}
            >
              <Ionicons name="compass-outline" size={16} color={activeTab === 'experiences' ? '#122F26' : '#fff'} />
              <Text className={`ml-1.5 font-bold text-sm ${activeTab === 'experiences' ? 'text-forest' : 'text-white'}`}>
                Experiences
              </Text>
            </TouchableOpacity>
          </View>
        </LinearGradient>

      {/* Property Bookings List */}
      {activeTab === 'properties' && (
        propLoading ? (
          <View className="pt-4">
            {[1, 2, 3, 4].map((i) => (
              <BookingCardSkeleton key={i} />
            ))}
          </View>
        ) : (
          <FlatList
            data={bookings}
            renderItem={({ item }) => <BookingCard booking={item} />}
            keyExtractor={(item) => item.id}
            contentContainerStyle={{ paddingVertical: 12, paddingBottom: 40 }}
            ListEmptyComponent={
              <View className="flex-1 justify-center items-center py-20 px-6">
                <View className="bg-white rounded-3xl p-8 items-center" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}>
                  <View className="bg-sand-200 rounded-full p-8 mb-6">
                    <Ionicons name="calendar-outline" size={72} color="#3A5C50" />
                  </View>
                  <Text className="text-2xl font-bold text-forest mb-3">No Stays Yet</Text>
                  <Text className="text-moss text-center mb-8 px-4 leading-6">
                    Start exploring amazing properties and make your first booking
                  </Text>
                  <TouchableOpacity onPress={() => router.push('/(tabs)/explore')}>
                    <LinearGradient
                      colors={['#122F26', '#1d392f']}
                      className="px-8 py-4 rounded-2xl"
                      style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 4 }, shadowOpacity: 0.3, shadowRadius: 8, elevation: 5 }}
                    >
                      <Text className="text-gold font-bold text-base">Explore Properties</Text>
                    </LinearGradient>
                  </TouchableOpacity>
                </View>
              </View>
            }
          />
        )
      )}

      {/* Experience Bookings List */}
      {activeTab === 'experiences' && (
        expLoading ? (
          <View className="pt-4">
            {[1, 2, 3, 4].map((i) => (
              <BookingCardSkeleton key={i} />
            ))}
          </View>
        ) : (
          <FlatList
            data={expBookings}
            renderItem={({ item }) => <ExperienceBookingCard booking={item} />}
            keyExtractor={(item) => String(item.id)}
            contentContainerStyle={{ paddingVertical: 12, paddingBottom: 40 }}
            ListEmptyComponent={
              <View className="flex-1 justify-center items-center py-20 px-6">
                <View className="bg-white rounded-3xl p-8 items-center" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}>
                  <View className="bg-sand-200 rounded-full p-8 mb-6">
                    <Ionicons name="compass-outline" size={72} color="#3A5C50" />
                  </View>
                  <Text className="text-2xl font-bold text-forest mb-3">No Experiences Yet</Text>
                  <Text className="text-moss text-center mb-8 px-4 leading-6">
                    Discover exciting experiences and activities to book
                  </Text>
                  <TouchableOpacity onPress={() => router.push('/(tabs)/explore')}>
                    <LinearGradient
                      colors={['#122F26', '#1d392f']}
                      className="px-8 py-4 rounded-2xl"
                      style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 4 }, shadowOpacity: 0.3, shadowRadius: 8, elevation: 5 }}
                    >
                      <Text className="text-gold font-bold text-base">Explore Experiences</Text>
                    </LinearGradient>
                  </TouchableOpacity>
                </View>
              </View>
            }
          />
        )
      )}
      </View>
    </SafeAreaView>
  );
}
