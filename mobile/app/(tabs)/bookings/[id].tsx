import { View, Text, ScrollView, TouchableOpacity, Platform, ActivityIndicator } from 'react-native';
import { useLocalSearchParams, useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { useBookingById } from '@/hooks/api-hooks';

export default function BookingDetailScreen() {
  const router = useRouter();
  const { id } = useLocalSearchParams<{ id: string }>();
  const { isAuthenticated } = useAuth();

  // Fetch booking details from API
  const { data: booking, isLoading: loading } = useBookingById(id);

  if (!isAuthenticated) {
    return (
      <View className="flex-1 bg-sand-100">
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
        >
          <View className="flex-row items-center">
            <TouchableOpacity
              onPress={() => router.back()}
              className="w-10 h-10 rounded-xl items-center justify-center mr-3"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="arrow-back" size={24} color="#fff" />
            </TouchableOpacity>
            <Text className="text-xl font-bold text-white">Booking Details</Text>
          </View>
        </LinearGradient>

        <View className="flex-1 items-center justify-center px-6">
          <Text className="text-moss text-center">Please sign in to view booking details</Text>
          <TouchableOpacity 
            onPress={() => router.push('/(auth)/login')}
            className="mt-4"
          >
            <LinearGradient
              colors={['#D9B168', '#bea04f']}
              className="px-8 py-4 rounded-2xl"
            >
              <Text className="text-forest font-bold">Sign In</Text>
            </LinearGradient>
          </TouchableOpacity>
        </View>
      </View>
    );
  }

  if (loading) {
    return (
      <View className="flex-1 bg-sand-100 items-center justify-center">
        <ActivityIndicator size="large" color="#D9B168" />
      </View>
    );
  }

  return (
    <ScrollView 
      className="flex-1 bg-sand-100" 
      showsVerticalScrollIndicator={false}
      contentContainerStyle={{ paddingBottom: 40 }}
    >
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f']}
        className="px-4 pb-6"
        style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
      >
        <View className="flex-row items-center">
          <TouchableOpacity
            onPress={() => router.back()}
            className="w-10 h-10 rounded-xl items-center justify-center mr-3"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </TouchableOpacity>
          <Text className="text-xl font-bold text-white">Booking #{id}</Text>
        </View>
      </LinearGradient>

      <View className="px-4 py-6 pb-8">
        {booking ? (
          <>
            {/* Status Badge */}
            <View className="mb-4">
              <View className="bg-white rounded-2xl p-4" style={{
                shadowColor: '#122F26',
                shadowOffset: { width: 0, height: 2 },
                shadowOpacity: 0.05,
                shadowRadius: 4,
                elevation: 2,
              }}>
                <View className="flex-row items-center justify-between">
                  <Text className="text-moss text-sm font-medium">Status</Text>
                  <View className="bg-green-100 px-3 py-1.5 rounded-full">
                    <Text className="text-green-800 text-xs font-semibold capitalize">{booking.status}</Text>
                  </View>
                </View>
              </View>
            </View>

            {/* Booking Details */}
            <View className="bg-white rounded-2xl p-4 mb-4" style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 2 },
              shadowOpacity: 0.05,
              shadowRadius: 4,
              elevation: 2,
            }}>
              <Text className="text-lg font-bold text-forest mb-4">Booking Details</Text>
              
              {/* Dates */}
              <View className="mb-4">
                <View className="flex-row items-center justify-between mb-3">
                  <View className="flex-1">
                    <Text className="text-moss text-xs font-medium mb-1">Check-in</Text>
                    <View className="flex-row items-center">
                      <Ionicons name="calendar" size={16} color="#3A5C50" />
                      <Text className="text-forest font-bold ml-2">
                        {new Date(booking.check_in).toLocaleDateString('en-US', { 
                          weekday: 'short',
                          month: 'short', 
                          day: 'numeric',
                          year: 'numeric'
                        })}
                      </Text>
                    </View>
                  </View>
                  <Ionicons name="arrow-forward" size={20} color="#D9B168" />
                  <View className="flex-1 items-end">
                    <Text className="text-moss text-xs font-medium mb-1">Check-out</Text>
                    <View className="flex-row items-center">
                      <Ionicons name="calendar" size={16} color="#3A5C50" />
                      <Text className="text-forest font-bold ml-2">
                        {new Date(booking.check_out).toLocaleDateString('en-US', { 
                          weekday: 'short',
                          month: 'short', 
                          day: 'numeric',
                          year: 'numeric'
                        })}
                      </Text>
                    </View>
                  </View>
                </View>
              </View>

              {/* Guests & Nights */}
              <View className="border-t border-sand-200 pt-4">
                <View className="flex-row justify-between mb-3">
                  <View className="flex-row items-center">
                    <Ionicons name="people" size={20} color="#3A5C50" />
                    <Text className="text-moss ml-2">Guests</Text>
                  </View>
                  <Text className="text-forest font-bold">{booking.number_of_guests}</Text>
                </View>
                <View className="flex-row justify-between">
                  <View className="flex-row items-center">
                    <Ionicons name="moon" size={20} color="#3A5C50" />
                    <Text className="text-moss ml-2">Nights</Text>
                  </View>
                  <Text className="text-forest font-bold">{booking.nights || booking.number_of_nights}</Text>
                </View>
              </View>
            </View>

            {/* Price Breakdown */}
            <View className="bg-white rounded-2xl p-4 mb-4" style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 2 },
              shadowOpacity: 0.05,
              shadowRadius: 4,
              elevation: 2,
            }}>
              <Text className="text-lg font-bold text-forest mb-4">Price Details</Text>
              
              <View className="space-y-3">
                <View className="flex-row justify-between mb-2">
                  <Text className="text-moss">Accommodation ({booking.nights || booking.number_of_nights} nights)</Text>
                  <Text className="text-forest font-semibold">${booking.nightly_total || booking.total_before_tax}</Text>
                </View>
                <View className="flex-row justify-between mb-2">
                  <Text className="text-moss">Service fee</Text>
                  <Text className="text-forest font-semibold">${booking.service_fee || 0}</Text>
                </View>
                {booking.cleaning_fee && parseFloat(booking.cleaning_fee) > 0 && (
                  <View className="flex-row justify-between mb-2">
                    <Text className="text-moss">Cleaning fee</Text>
                    <Text className="text-forest font-semibold">${booking.cleaning_fee}</Text>
                  </View>
                )}
                <View className="border-t border-sand-200 pt-3 mt-2">
                  <View className="flex-row justify-between">
                    <Text className="text-forest font-bold text-lg">Total</Text>
                    <Text className="text-gold font-black text-2xl">${booking.grand_total}</Text>
                  </View>
                </View>
              </View>
            </View>

            {/* Special Requests */}
            {booking.special_requests && (
              <View className="bg-white rounded-2xl p-4 mb-4" style={{
                shadowColor: '#122F26',
                shadowOffset: { width: 0, height: 2 },
                shadowOpacity: 0.05,
                shadowRadius: 4,
                elevation: 2,
              }}>
                <Text className="text-lg font-bold text-forest mb-2">Special Requests</Text>
                <Text className="text-moss">{booking.special_requests}</Text>
              </View>
            )}

            {/* Actions */}
            {booking.status === 'confirmed' && (
              <View className="flex-row gap-3">
                <TouchableOpacity className="flex-1">
                  <LinearGradient
                    colors={['#D9B168', '#bea04f']}
                    className="py-4 rounded-2xl items-center"
                  >
                    <Text className="text-forest font-bold">Contact Host</Text>
                  </LinearGradient>
                </TouchableOpacity>
              </View>
            )}
          </>
        ) : (
          <View className="items-center py-12">
            <View className="bg-sand-200 rounded-full p-6 mb-4">
              <Ionicons name="calendar-outline" size={48} color="#94a3b8" />
            </View>
            <Text className="text-xl font-bold text-forest mb-2">Booking Not Found</Text>
            <Text className="text-moss text-center">This booking doesn't exist or has been removed</Text>
            <TouchableOpacity 
              onPress={() => router.back()}
              className="mt-6"
            >
              <LinearGradient
                colors={['#D9B168', '#bea04f']}
                className="px-8 py-4 rounded-2xl"
              >
                <Text className="text-forest font-bold">Go Back</Text>
              </LinearGradient>
            </TouchableOpacity>
          </View>
        )}
      </View>
    </ScrollView>
  );
}
