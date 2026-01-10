import { View, Text, FlatList, TouchableOpacity, ActivityIndicator } from 'react-native';
import { useRouter } from 'expo-router';
import { useBookings } from '@/hooks/api-hooks';
import { useAuth } from '@/context/auth-context';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { Booking } from '@/types';

export default function BookingsScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const { data: bookingsData, isLoading } = useBookings();
  const bookings = bookingsData?.results || [];

  if (!isAuthenticated) {
    return (
      <View className="flex-1 bg-sand-100">
        {/* Header */}
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pt-12 pb-6"
        >
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
      </View>
    );
  }

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'confirmed':
        return { bg: 'bg-green-100', text: 'text-green-800', icon: 'checkmark-circle' };
      case 'pending':
        return { bg: 'bg-yellow-100', text: 'text-yellow-800', icon: 'time' };
      case 'cancelled':
        return { bg: 'bg-red-100', text: 'text-red-800', icon: 'close-circle' };
      case 'checked_in':
        return { bg: 'bg-blue-100', text: 'text-blue-800', icon: 'enter' };
      case 'checked_out':
        return { bg: 'bg-purple-100', text: 'text-purple-800', icon: 'checkbox' };
      default:
        return { bg: 'bg-gray-100', text: 'text-gray-800', icon: 'information-circle' };
    }
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
          className="p-4"
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
            <View className={`px-3 py-1.5 rounded-full ${statusStyle.bg} flex-row items-center`}>
              <Ionicons name={statusStyle.icon as any} size={14} color={statusStyle.text.replace('text-', '#')} />
              <Text className={`text-xs font-semibold capitalize ml-1 ${statusStyle.text}`}>
                {booking.status}
              </Text>
            </View>
          </View>
        </LinearGradient>

        {/* Content */}
        <View className="p-4">
          {/* Dates Timeline */}
          <View className="bg-sand-100 rounded-2xl p-4 mb-4">
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

          {/* Price and Action */}
          <View className="flex-row items-center justify-between">
            <View>
              <Text className="text-moss text-xs font-medium mb-1">Total Amount</Text>
              <Text className="text-2xl font-black text-gold">${booking.grand_total}</Text>
            </View>
            <View className="bg-forest px-5 py-3 rounded-xl">
              <Text className="text-gold font-bold text-sm">View Details</Text>
            </View>
          </View>
        </View>
      </TouchableOpacity>
    );
  };

  return (
    <View className="flex-1 bg-sand-100">
      {/* Modern Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="px-4 pt-12 pb-6"
      >
        <Text className="text-3xl font-black text-white tracking-tight mb-2">
          My Bookings
        </Text>
        <View className="flex-row items-center">
          <Ionicons name="calendar" size={16} color="#D9B168" />
          <Text className="text-sand-100 ml-2">
            {bookings.length} active {bookings.length === 1 ? 'booking' : 'bookings'}
          </Text>
        </View>
      </LinearGradient>

      {/* Bookings List */}
      {isLoading ? (
        <View className="flex-1 justify-center items-center">
          <ActivityIndicator size="large" color="#D9B168" />
          <Text className="mt-4 text-primary-700 font-medium">Loading bookings...</Text>
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
                <Text className="text-2xl font-bold text-forest mb-3">No Bookings Yet</Text>
                <Text className="text-moss text-center mb-8 px-4 leading-6">
                  Start exploring amazing properties and make your first booking
                </Text>
                <TouchableOpacity
                  onPress={() => router.push('/(tabs)/explore')}
                >
                  <LinearGradient
                    colors={['#122F26', '#1d392f']}
                    className="px-8 py-4 rounded-2xl"
                    style={{
                      shadowColor: '#122F26',
                      shadowOffset: { width: 0, height: 4 },
                      shadowOpacity: 0.3,
                      shadowRadius: 8,
                      elevation: 5,
                    }}
                  >
                    <Text className="text-gold font-bold text-base">Start Exploring</Text>
                  </LinearGradient>
                </TouchableOpacity>
              </View>
            </View>
          }
        />
      )}
    </View>
  );
}
