import { View, Text, TouchableOpacity, Image } from 'react-native';
import { Ionicons } from '@expo/vector-icons';
import { Booking } from '@/types';

interface BookingItemProps {
  booking: Booking;
  onPress: (id: string) => void;
}

export function BookingItem({ booking, onPress }: BookingItemProps) {
  const checkIn = new Date(booking.check_in);
  const checkOut = new Date(booking.check_out);

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'confirmed':
        return '#10b981';
      case 'checked_in':
        return '#3b82f6';
      case 'checked_out':
        return '#6b7280';
      case 'cancelled':
        return '#ef4444';
      default:
        return '#f59e0b';
    }
  };

  return (
    <TouchableOpacity
      className="mb-4 p-4 bg-white rounded-lg border border-gray-200"
      onPress={() => onPress(booking.id)}
    >
      <View className="flex-row justify-between mb-3">
        <Text className="font-bold text-lg flex-1">{booking.property_id}</Text>
        <View
          style={{ backgroundColor: getStatusColor(booking.status) + '20' }}
          className="px-3 py-1 rounded-full"
        >
          <Text style={{ color: getStatusColor(booking.status) }} className="text-xs font-bold">
            {booking.status.toUpperCase()}
          </Text>
        </View>
      </View>

      <View className="mb-3">
        <Text className="text-gray-600 text-sm mb-1">
          <Ionicons name="calendar" size={14} /> {checkIn.toLocaleDateString()} -{' '}
          {checkOut.toLocaleDateString()}
        </Text>
        <Text className="text-gray-600 text-sm">
          <Ionicons name="people" size={14} /> {booking.number_of_guests} guests ·{' '}
          {booking.number_of_nights} nights
        </Text>
      </View>

      <View className="border-t border-gray-100 pt-3">
        <Text className="text-primary-600 font-bold text-lg">${booking.grand_total}</Text>
      </View>
    </TouchableOpacity>
  );
}
