import { View, Text, FlatList, TouchableOpacity } from 'react-native';
import { useBookings } from '@/hooks/api-hooks';
import { useAuth } from '@/context/auth-context';

export default function BookingsScreen() {
  const { isAuthenticated } = useAuth();
  const { data: bookings, isLoading } = useBookings();

  if (!isAuthenticated) {
    return (
      <View className="flex-1 items-center justify-center bg-white">
        <Text className="text-gray-600">Please login to view bookings</Text>
      </View>
    );
  }

  return (
    <View className="flex-1 bg-white">
      <View className="px-4 py-4 border-b border-gray-200">
        <Text className="text-2xl font-bold">My Bookings</Text>
      </View>

      <FlatList
        data={bookings?.results}
        keyExtractor={(item) => item.id}
        renderItem={({ item }) => (
          <TouchableOpacity className="px-4 py-4 border-b border-gray-200">
            <View className="flex-row justify-between mb-2">
              <Text className="font-semibold text-lg">{item.property_id}</Text>
              <Text className="text-primary-600 font-bold">${item.grand_total}</Text>
            </View>
            <View className="flex-row justify-between text-sm text-gray-600">
              <Text>{new Date(item.check_in).toLocaleDateString()}</Text>
              <Text>
                Status: <Text className="font-semibold">{item.status}</Text>
              </Text>
            </View>
          </TouchableOpacity>
        )}
        ListEmptyComponent={
          !isLoading && (
            <View className="items-center justify-center py-8">
              <Text className="text-gray-600">No bookings yet</Text>
            </View>
          )
        }
      />
    </View>
  );
}
