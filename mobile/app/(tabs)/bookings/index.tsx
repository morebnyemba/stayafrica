import { View, Text, FlatList, TouchableOpacity, ActivityIndicator } from 'react-native';
import { useRouter } from 'expo-router';
import { useBookings } from '@/hooks/api-hooks';
import { useAuth } from '@/context/auth-context';
import { Ionicons } from '@expo/vector-icons';

interface Booking {
  id: string;
  property_id: string;
  check_in: string;
  check_out: string;
  grand_total: number;
  status: 'pending' | 'confirmed' | 'cancelled' | 'completed';
  property?: {
    title: string;
    city: string;
  };
}

export default function BookingsScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const { data: bookingsData, isLoading } = useBookings();
  const bookings = bookingsData?.results || [];

  if (!isAuthenticated) {
    return (
      <View className="flex-1 items-center justify-center bg-white">
        <Text className="text-gray-600 mb-4">Please login to view bookings</Text>
        <TouchableOpacity
          className="bg-secondary-600 px-6 py-2 rounded-lg"
          onPress={() => router.replace('/(auth)/login')}
        >
          <Text className="text-white font-semibold">Go to Login</Text>
        </TouchableOpacity>
      </View>
    );
  }

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'confirmed':
        return 'bg-green-100 text-green-800';
      case 'pending':
        return 'bg-yellow-100 text-yellow-800';
      case 'cancelled':
        return 'bg-red-100 text-red-800';
      case 'completed':
        return 'bg-blue-100 text-blue-800';
      default:
        return 'bg-gray-100 text-gray-800';
    }
  };

  const BookingCard = ({ booking }: { booking: Booking }) => (
    <TouchableOpacity
      className="mx-4 mb-4 bg-white border border-gray-200 rounded-xl overflow-hidden"
      onPress={() => router.push(`/(tabs)/bookings/${booking.id}`)}
    >
      <View className="p-4">
        {/* Header */}
        <View className="flex-row justify-between items-start mb-3">
          <View className="flex-1">
            <Text className="text-lg font-bold text-primary-900">
              {booking.property?.title || 'Property'}
            </Text>
            <View className="flex-row items-center mt-1">
              <Ionicons name="location" size={14} color="#666" />
              <Text className="text-gray-600 text-sm ml-1">
                {booking.property?.city || 'Unknown location'}
              </Text>
            </View>
          </View>
          <View className={`px-3 py-1 rounded-full ${getStatusColor(booking.status)}`}>
            <Text className="text-xs font-semibold capitalize">{booking.status}</Text>
          </View>
        </View>

        {/* Dates */}
        <View className="flex-row items-center py-2 mb-3 border-t border-b border-gray-100">
          <Ionicons name="calendar" size={16} color="#666" />
          <Text className="text-gray-700 ml-2 text-sm">
            {new Date(booking.check_in).toLocaleDateString()} → {new Date(booking.check_out).toLocaleDateString()}
          </Text>
        </View>

        {/* Price */}
        <View className="flex-row items-center justify-between">
          <View className="flex-row items-baseline">
            <Ionicons name="cash" size={16} color="#D4A574" />
            <Text className="text-lg font-bold text-secondary-600 ml-1">{booking.grand_total}</Text>
          </View>
          <Text className="text-gray-600 text-sm">Total price</Text>
        </View>
      </View>
    </TouchableOpacity>
  );

  return (
    <View className="flex-1 bg-gray-50">
      {/* Header */}
      <View className="bg-white px-4 py-4 border-b border-gray-200">
        <Text className="text-3xl font-bold text-primary-900">My Bookings</Text>
        <Text className="text-gray-600 text-sm mt-1">{bookings.length} active bookings</Text>
      </View>

      {/* Bookings List */}
      {isLoading ? (
        <View className="flex-1 justify-center items-center">
          <ActivityIndicator size="large" color="#D4A574" />
        </View>
      ) : (
        <FlatList
          data={bookings}
          renderItem={({ item }) => <BookingCard booking={item} />}
          keyExtractor={(item) => item.id}
          contentContainerStyle={{ paddingVertical: 12 }}
          ListEmptyComponent={
            <View className="flex-1 justify-center items-center py-12">
              <Text className="text-gray-600 text-base mb-2">No bookings yet</Text>
              <TouchableOpacity
                className="bg-secondary-600 px-6 py-2 rounded-lg mt-4"
                onPress={() => router.push('/(tabs)/explore')}
              >
                <Text className="text-white font-semibold">Start Exploring</Text>
              </TouchableOpacity>
            </View>
          }
        />
      )}
    </View>
  );
}
