import { View, Text, FlatList, TouchableOpacity, ActivityIndicator } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { useHostBookings } from '@/hooks/api-hooks';
import { Skeleton } from '@/components/common/Skeletons';
import { format } from 'date-fns';
import type { Booking } from '@/types';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';

export default function HostBookingsScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const insets = useSafeAreaInsets();
  const { data: bookingsData, isLoading } = useHostBookings();
  const bookings = bookingsData?.results || [];

  if (!isAuthenticated) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100">
        {/* Header */}
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: insets.top + 12 }}
        >
          <Text className="text-3xl font-black text-white tracking-tight">
            Property Bookings
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
              Please sign in to view your property bookings
            </Text>
            <TouchableOpacity
              onPress={() => router.push('/(auth)/login')}
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

  const getStatusStyle = (status: string) => {
    const styles: Record<string, { bg: string; text: string; icon: keyof typeof Ionicons.glyphMap }> = {
      pending: { bg: 'bg-yellow-100', text: 'text-yellow-800', icon: 'time-outline' },
      confirmed: { bg: 'bg-green-100', text: 'text-green-800', icon: 'checkmark-circle' },
      checked_in: { bg: 'bg-blue-100', text: 'text-blue-800', icon: 'enter' },
      checked_out: { bg: 'bg-gray-100', text: 'text-gray-800', icon: 'exit' },
      cancelled: { bg: 'bg-red-100', text: 'text-red-800', icon: 'close-circle' },
    };
    return styles[status] || { bg: 'bg-gray-100', text: 'text-gray-800', icon: 'help-circle' as keyof typeof Ionicons.glyphMap };
  };

  const BookingItem = ({ booking }: { booking: Booking }) => {
    const statusStyle = getStatusStyle(booking.status);
    
    return (
      <TouchableOpacity
        className="mb-3 mx-4"
        onPress={() => router.push(`/host/bookings/${booking.id}`)}
      >
        <View
          className="p-4 bg-white rounded-2xl"
          style={{
            shadowColor: '#122F26',
            shadowOffset: { width: 0, height: 4 },
            shadowOpacity: 0.08,
            shadowRadius: 8,
            elevation: 4,
          }}
        >
          {/* Property Name */}
          <Text className="text-base font-bold text-forest mb-2">
            {booking.property?.title || 'Property'}
          </Text>
          
          {/* Guest Info */}
          <View className="flex-row items-center mb-3">
            <LinearGradient
              colors={['#D9B168', '#bea04f']}
              className="w-10 h-10 rounded-full items-center justify-center mr-3"
            >
              <Ionicons name="person" size={20} color="#122F26" />
            </LinearGradient>
            <View className="flex-1">
              <Text className="text-sm font-semibold text-forest">
                Guest
              </Text>
              <Text className="text-xs text-moss">{booking.guest_email || 'No email'}</Text>
            </View>
          </View>

          {/* Dates */}
          <View className="flex-row items-center mb-2">
            <Ionicons name="calendar" size={16} color="#3A5C50" />
            <Text className="text-sm text-moss ml-2">
              {format(new Date(booking.check_in), 'MMM dd')} - {format(new Date(booking.check_out), 'MMM dd, yyyy')}
            </Text>
          </View>

          {/* Guests */}
          <View className="flex-row items-center mb-3">
            <Ionicons name="people" size={16} color="#3A5C50" />
            <Text className="text-sm text-moss ml-2">
              {booking.number_of_guests} {booking.number_of_guests === 1 ? 'guest' : 'guests'}
            </Text>
          </View>

          {/* Status and Price */}
          <View className="flex-row items-center justify-between">
            <View className={`px-3 py-1 rounded-full flex-row items-center ${statusStyle.bg}`}>
              <Ionicons name={statusStyle.icon} size={14} color={statusStyle.text.includes('yellow') ? '#92400E' : statusStyle.text.includes('green') ? '#166534' : statusStyle.text.includes('blue') ? '#1E40AF' : statusStyle.text.includes('red') ? '#991B1B' : '#374151'} />
              <Text className={`text-xs font-semibold ml-1 ${statusStyle.text}`}>
                {booking.status?.replace('_', ' ').toUpperCase()}
              </Text>
            </View>
            <Text className="text-lg font-bold text-gold">
              ${booking.grand_total}
            </Text>
          </View>
        </View>
      </TouchableOpacity>
    );
  };

  const BookingSkeleton = () => (
    <View className="bg-white rounded-2xl p-4 bg-white rounded-2xl mb-3 mx-4">
      <Skeleton height={18} width="70%" className="mb-2" />
      <View className="flex-row items-center mb-3">
        <Skeleton height={40} width={40} borderRadius={20} className="mr-3" />
        <View className="flex-1">
          <Skeleton height={14} width="60%" className="mb-2" />
          <Skeleton height={12} width="80%" />
        </View>
      </View>
      <Skeleton height={16} width="50%" className="mb-2" />
      <Skeleton height={16} width="40%" className="mb-3" />
      <View className="flex-row justify-between">
        <Skeleton height={24} width={80} borderRadius={12} />
        <Skeleton height={24} width={60} />
      </View>
    </View>
  );

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      {/* Modern Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="px-4 pb-6"
        style={{ paddingTop: insets.top + 12 }}
      >
        <TouchableOpacity onPress={() => router.back()} className="mb-4">
          <View className="w-10 h-10 rounded-xl items-center justify-center" style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}>
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </View>
        </TouchableOpacity>
        <Text className="text-3xl font-black text-white tracking-tight mb-2">
          Property Bookings
        </Text>
        <View className="flex-row items-center">
          <Ionicons name="calendar" size={16} color="#D9B168" />
          <Text className="text-sand-100 ml-2">
            {bookings.length} {bookings.length === 1 ? 'booking' : 'bookings'}
          </Text>
        </View>
      </LinearGradient>

      {/* Bookings List */}
      {isLoading ? (
        <View className="pt-4">
          {[1, 2, 3, 4].map((i) => (
            <BookingSkeleton key={i} />
          ))}
        </View>
      ) : bookings.length === 0 ? (
        <View className="flex-1 justify-center items-center px-6">
          <View className="bg-white rounded-3xl p-8 items-center" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}>
            <View className="bg-sand-200 rounded-full p-8 mb-6">
              <Ionicons name="calendar-outline" size={72} color="#3A5C50" />
            </View>
            <Text className="text-2xl font-bold text-forest mb-3">No Bookings Yet</Text>
            <Text className="text-moss text-center px-4 leading-6">
              Bookings for your properties will appear here
            </Text>
          </View>
        </View>
      ) : (
        <FlatList
          data={bookings}
          renderItem={({ item }) => <BookingItem booking={item} />}
          keyExtractor={(item) => item.id}
          contentContainerStyle={{ paddingVertical: 12, paddingBottom: 40 }}
        />
      )}
    </SafeAreaView>
  );
}
