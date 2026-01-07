import { View, Text, FlatList, TouchableOpacity, ActivityIndicator } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { useAuth } from '@/context/auth-context';
import { useHostBookings } from '@/hooks/api-hooks';
import { format } from 'date-fns';

export default function HostBookingsScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const { data: bookingsData, isLoading } = useHostBookings();
  const bookings = bookingsData?.results || [];

  if (!isAuthenticated) {
    return (
      <View className="flex-1 items-center justify-center bg-white px-6">
        <Ionicons name="calendar-outline" size={64} color="#ddd" />
        <Text className="text-xl font-bold text-gray-800 mt-4">Sign In Required</Text>
        <Text className="text-gray-600 text-center mt-2 mb-6">
          Please sign in to view your bookings
        </Text>
        <TouchableOpacity
          className="bg-primary-600 px-8 py-3 rounded-lg"
          onPress={() => router.push('/(auth)/login')}
        >
          <Text className="text-white font-semibold">Sign In</Text>
        </TouchableOpacity>
      </View>
    );
  }

  const getStatusColor = (status: string) => {
    const colors: Record<string, { bg: string; text: string }> = {
      pending: { bg: 'bg-yellow-100', text: 'text-yellow-800' },
      confirmed: { bg: 'bg-green-100', text: 'text-green-800' },
      checked_in: { bg: 'bg-blue-100', text: 'text-blue-800' },
      checked_out: { bg: 'bg-gray-100', text: 'text-gray-800' },
      cancelled: { bg: 'bg-red-100', text: 'text-red-800' },
    };
    return colors[status] || { bg: 'bg-gray-100', text: 'text-gray-800' };
  };

  const getStatusIcon = (status: string) => {
    const icons: Record<string, string> = {
      pending: 'time-outline',
      confirmed: 'checkmark-circle',
      checked_in: 'enter',
      checked_out: 'exit',
      cancelled: 'close-circle',
    };
    return icons[status] || 'help-circle';
  };

  const BookingItem = ({ booking }: any) => {
    const statusColors = getStatusColor(booking.status);
    const statusIcon = getStatusIcon(booking.status);
    
    return (
      <TouchableOpacity
        className="bg-white rounded-xl p-4 mb-3 shadow-sm border border-gray-100"
        onPress={() => router.push(`/host/bookings/${booking.id}`)}
      >
        {/* Property Name */}
        <Text className="text-base font-bold text-gray-900 mb-2">
          {booking.property?.title || 'Property'}
        </Text>
        
        {/* Guest Info */}
        <View className="flex-row items-center mb-3">
          <View className="w-10 h-10 rounded-full bg-primary-100 items-center justify-center mr-3">
            <Ionicons name="person" size={20} color="#3A5C50" />
          </View>
          <View className="flex-1">
            <Text className="text-sm font-semibold text-gray-900">
              {booking.guest?.first_name} {booking.guest?.last_name}
            </Text>
            <Text className="text-xs text-gray-600">{booking.guest?.email}</Text>
          </View>
        </View>

        {/* Dates */}
        <View className="flex-row items-center mb-2">
          <Ionicons name="calendar" size={16} color="#666" />
          <Text className="text-sm text-gray-600 ml-2">
            {format(new Date(booking.check_in_date), 'MMM dd')} - {format(new Date(booking.check_out_date), 'MMM dd, yyyy')}
          </Text>
        </View>

        {/* Guests */}
        <View className="flex-row items-center mb-3">
          <Ionicons name="people" size={16} color="#666" />
          <Text className="text-sm text-gray-600 ml-2">
            {booking.number_of_guests} {booking.number_of_guests === 1 ? 'guest' : 'guests'}
          </Text>
        </View>

        {/* Status and Price */}
        <View className="flex-row items-center justify-between">
          <View className={`px-3 py-1 rounded-full flex-row items-center ${statusColors.bg}`}>
            <Ionicons name={statusIcon as any} size={14} color={statusColors.text.replace('text-', '#')} />
            <Text className={`text-xs font-semibold ml-1 ${statusColors.text}`}>
              {booking.status?.replace('_', ' ').toUpperCase()}
            </Text>
          </View>
          <Text className="text-lg font-bold text-primary-600">
            ${booking.total_price}
          </Text>
        </View>
      </TouchableOpacity>
    );
  };

  return (
    <View className="flex-1 bg-gray-50">
      {/* Header */}
      <View className="bg-white px-4 pt-12 pb-4 border-b border-gray-100 shadow-sm">
        <TouchableOpacity onPress={() => router.back()} className="mb-2">
          <Ionicons name="arrow-back" size={24} color="#333" />
        </TouchableOpacity>
        <Text className="text-2xl font-extrabold text-primary-900">Property Bookings</Text>
        <Text className="text-sm text-gray-500 mt-1">
          {bookings.length} {bookings.length === 1 ? 'booking' : 'bookings'}
        </Text>
      </View>

      {/* Bookings List */}
      {isLoading ? (
        <View className="flex-1 justify-center items-center">
          <ActivityIndicator size="large" color="#3A5C50" />
          <Text className="mt-2 text-gray-600">Loading bookings...</Text>
        </View>
      ) : bookings.length === 0 ? (
        <View className="flex-1 justify-center items-center px-6">
          <Ionicons name="calendar-outline" size={64} color="#ddd" />
          <Text className="text-xl font-bold text-gray-800 mt-4">No Bookings Yet</Text>
          <Text className="text-gray-600 text-center mt-2">
            Bookings for your properties will appear here
          </Text>
        </View>
      ) : (
        <FlatList
          data={bookings}
          renderItem={({ item }) => <BookingItem booking={item} />}
          keyExtractor={(item) => item.id}
          contentContainerStyle={{ padding: 16 }}
        />
      )}
    </View>
  );
}
