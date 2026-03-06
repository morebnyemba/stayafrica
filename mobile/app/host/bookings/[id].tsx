import { View, Text, ScrollView, TouchableOpacity, ActivityIndicator, Alert } from 'react-native';
import { useLocalSearchParams, useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { useBookingById, useConfirmBooking, useCancelBooking } from '@/hooks/api-hooks';
import { apiClient } from '@/services/api-client';
import { useQueryClient } from '@tanstack/react-query';

export default function HostBookingDetailScreen() {
  const router = useRouter();
  const { id } = useLocalSearchParams<{ id: string }>();
  const { isAuthenticated } = useAuth();
  const insets = useSafeAreaInsets();

  // Fetch booking details from API
  const { data: booking, isLoading: loading } = useBookingById(id);
  const confirmBooking = useConfirmBooking();
  const cancelBooking = useCancelBooking();
  const queryClient = useQueryClient();

  const handleApprove = () => {
    Alert.alert(
      'Confirm Booking',
      'Are you sure you want to approve this booking?',
      [
        { text: 'Cancel', style: 'cancel' },
        {
          text: 'Approve',
          onPress: async () => {
            try {
              await confirmBooking.mutateAsync(id);
              queryClient.invalidateQueries({ queryKey: ['booking', id] });
              Alert.alert('Success', 'Booking has been approved!');
            } catch (error: any) {
              Alert.alert('Error', error.response?.data?.error || 'Failed to approve booking');
            }
          },
        },
      ]
    );
  };

  const handleDecline = () => {
    Alert.alert(
      'Decline Booking',
      'Are you sure you want to decline this booking?',
      [
        { text: 'Cancel', style: 'cancel' },
        {
          text: 'Decline',
          style: 'destructive',
          onPress: async () => {
            try {
              await cancelBooking.mutateAsync(id);
              queryClient.invalidateQueries({ queryKey: ['booking', id] });
              Alert.alert('Declined', 'Booking has been declined.');
              router.back();
            } catch (error: any) {
              Alert.alert('Error', error.response?.data?.error || 'Failed to decline booking');
            }
          },
        },
      ]
    );
  };

  if (!isAuthenticated) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100">
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: insets.top + 12 }}
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
      </SafeAreaView>
    );
  }

  if (loading) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100 items-center justify-center">
        <ActivityIndicator size="large" color="#D9B168" />
      </SafeAreaView>
    );
  }

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      <ScrollView className="flex-1 bg-sand-100" showsVerticalScrollIndicator={false}>
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f']}
        className="px-4 pb-6"
        style={{ paddingTop: insets.top + 12 }}
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

      <View className="px-4 py-6">
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
                  <Text className="text-moss text-sm font-medium">Booking Status</Text>
                  <View className="bg-green-100 px-3 py-1.5 rounded-full">
                    <Text className="text-green-800 text-xs font-semibold capitalize">{booking.status}</Text>
                  </View>
                </View>
              </View>
            </View>

            {/* Guest Info */}
            <View className="bg-white rounded-2xl p-4 mb-4" style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 2 },
              shadowOpacity: 0.05,
              shadowRadius: 4,
              elevation: 2,
            }}>
              <Text className="text-lg font-bold text-forest mb-4">Guest Information</Text>
              <View className="flex-row items-center mb-3">
                <View className="bg-gold/20 rounded-full p-3">
                  <Ionicons name="person" size={24} color="#D9B168" />
                </View>
                <View className="ml-3 flex-1">
                  <Text className="text-forest font-bold">
                    {booking.guest_first_name && booking.guest_last_name
                      ? `${booking.guest_first_name} ${booking.guest_last_name}`
                      : 'Guest'}
                  </Text>
                  <Text className="text-moss text-sm">{booking.guest_email}</Text>
                  <View className="flex-row items-center mt-1">
                    <Ionicons name="people" size={14} color="#3A5C50" />
                    <Text className="text-moss text-sm ml-1">{booking.number_of_guests} {booking.number_of_guests === 1 ? 'guest' : 'guests'}</Text>
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
                        {booking.check_in ? new Date(booking.check_in).toLocaleDateString('en-US', { 
                          weekday: 'short',
                          month: 'short', 
                          day: 'numeric',
                          year: 'numeric'
                        }) : 'TBD'}
                      </Text>
                    </View>
                  </View>
                  <Ionicons name="arrow-forward" size={20} color="#D9B168" />
                  <View className="flex-1 items-end">
                    <Text className="text-moss text-xs font-medium mb-1">Check-out</Text>
                    <View className="flex-row items-center">
                      <Ionicons name="calendar" size={16} color="#3A5C50" />
                      <Text className="text-forest font-bold ml-2">
                        {booking.check_out ? new Date(booking.check_out).toLocaleDateString('en-US', { 
                          weekday: 'short',
                          month: 'short', 
                          day: 'numeric',
                          year: 'numeric'
                        }) : 'TBD'}
                      </Text>
                    </View>
                  </View>
                </View>
              </View>

              {/* Duration */}
              <View className="border-t border-sand-200 pt-4">
                <View className="flex-row justify-between">
                  <View className="flex-row items-center">
                    <Ionicons name="moon" size={20} color="#3A5C50" />
                    <Text className="text-moss ml-2">Total Nights</Text>
                  </View>
                  <Text className="text-forest font-bold">{booking.nights || booking.number_of_nights}</Text>
                </View>
              </View>
            </View>

            {/* Earnings Breakdown */}
            <View className="bg-white rounded-2xl p-4 mb-4" style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 2 },
              shadowOpacity: 0.05,
              shadowRadius: 4,
              elevation: 2,
            }}>
              <Text className="text-lg font-bold text-forest mb-4">Earnings</Text>
              
              <View className="space-y-3">
                <View className="flex-row justify-between mb-2">
                  <Text className="text-moss">Nightly total × {booking.nights || booking.number_of_nights} nights</Text>
                  <Text className="text-forest font-semibold">${booking.nightly_total || booking.total_before_tax}</Text>
                </View>
                <View className="flex-row justify-between mb-2">
                  <Text className="text-moss">Service fee (StayAfrica)</Text>
                  <Text className="text-moss font-semibold">-${booking.service_fee || 0}</Text>
                </View>
                {booking.commission_fee && parseFloat(booking.commission_fee) > 0 && (
                  <View className="flex-row justify-between mb-2">
                    <Text className="text-moss">Commission fee</Text>
                    <Text className="text-moss font-semibold">-${booking.commission_fee}</Text>
                  </View>
                )}
                <View className="border-t border-sand-200 pt-3 mt-2">
                  <View className="flex-row justify-between">
                    <Text className="text-forest font-bold text-lg">Total Earnings</Text>
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
                <View className="bg-sand-100 rounded-xl p-3">
                  <Text className="text-moss">{booking.special_requests}</Text>
                </View>
              </View>
            )}

            {/* Actions for pending bookings */}
            {booking.status === 'pending' && (
              <View className="flex-row gap-3 mb-4">
                <TouchableOpacity 
                  className="flex-1 bg-green-500 py-4 rounded-2xl items-center"
                  onPress={handleApprove}
                  disabled={confirmBooking.isPending || cancelBooking.isPending}
                >
                  <Text className="text-white font-bold">
                    {confirmBooking.isPending ? 'Approving...' : 'Approve'}
                  </Text>
                </TouchableOpacity>
                <TouchableOpacity 
                  className="flex-1 bg-red-500 py-4 rounded-2xl items-center"
                  onPress={handleDecline}
                  disabled={confirmBooking.isPending || cancelBooking.isPending}
                >
                  <Text className="text-white font-bold">
                    {cancelBooking.isPending ? 'Declining...' : 'Decline'}
                  </Text>
                </TouchableOpacity>
              </View>
            )}

            {/* Check In action for confirmed bookings (on/after check-in date) */}
            {booking.status === 'confirmed' && new Date(booking.check_in) <= new Date(new Date().toDateString()) && (
              <TouchableOpacity
                className="mb-4"
                onPress={() => {
                  Alert.alert(
                    'Check In Guest',
                    'Mark this guest as checked in?',
                    [
                      { text: 'Cancel', style: 'cancel' },
                      {
                        text: 'Check In',
                        onPress: async () => {
                          try {
                            await apiClient.checkInBooking(id as string);
                            queryClient.invalidateQueries({ queryKey: ['booking', id] });
                            Alert.alert('Checked In', 'Guest has been marked as checked in.');
                          } catch (error: any) {
                            Alert.alert('Error', error?.response?.data?.error || 'Failed to check in guest');
                          }
                        },
                      },
                    ]
                  );
                }}
              >
                <LinearGradient
                  colors={['#3B82F6', '#2563EB']}
                  className="py-4 rounded-2xl items-center"
                  style={{ shadowColor: '#3B82F6', shadowOffset: { width: 0, height: 4 }, shadowOpacity: 0.3, shadowRadius: 8, elevation: 5 }}
                >
                  <View className="flex-row items-center">
                    <Ionicons name="enter" size={20} color="#fff" />
                    <Text className="text-white font-bold ml-2">Check In Guest</Text>
                  </View>
                </LinearGradient>
              </TouchableOpacity>
            )}

            {/* Checked-in status banner */}
            {booking.status === 'checked_in' && (
              <View className="bg-blue-50 border border-blue-200 rounded-2xl p-4 mb-4 flex-row items-center">
                <View className="bg-blue-100 rounded-full p-2 mr-3">
                  <Ionicons name="checkmark-circle" size={22} color="#3B82F6" />
                </View>
                <View className="flex-1">
                  <Text className="text-blue-800 font-bold">Guest is Checked In</Text>
                  <Text className="text-blue-600 text-xs mt-0.5">
                    {booking.checked_in_at
                      ? `Since ${new Date(booking.checked_in_at).toLocaleString()}`
                      : 'Currently staying'}
                  </Text>
                </View>
              </View>
            )}

            {/* Check Out action for checked-in bookings */}
            {booking.status === 'checked_in' && (
              <TouchableOpacity
                className="mb-4"
                onPress={() => {
                  Alert.alert(
                    'Check Out Guest',
                    'Mark this guest as checked out?',
                    [
                      { text: 'Cancel', style: 'cancel' },
                      {
                        text: 'Check Out',
                        onPress: async () => {
                          try {
                            await apiClient.checkOutBooking(id as string);
                            queryClient.invalidateQueries({ queryKey: ['booking', id] });
                            Alert.alert('Checked Out', 'Guest has been marked as checked out.');
                          } catch (error: any) {
                            Alert.alert('Error', error?.response?.data?.error || 'Failed to check out guest');
                          }
                        },
                      },
                    ]
                  );
                }}
              >
                <View className="bg-purple-100 border border-purple-200 py-4 rounded-2xl items-center">
                  <View className="flex-row items-center">
                    <Ionicons name="exit" size={20} color="#7C3AED" />
                    <Text className="text-purple-700 font-bold ml-2">Check Out Guest</Text>
                  </View>
                </View>
              </TouchableOpacity>
            )}

            {/* Complete action for checked-out bookings */}
            {booking.status === 'checked_out' && (
              <TouchableOpacity
                className="mb-4"
                onPress={() => {
                  Alert.alert(
                    'Complete Booking',
                    'Mark this booking as completed?',
                    [
                      { text: 'Cancel', style: 'cancel' },
                      {
                        text: 'Complete',
                        onPress: async () => {
                          try {
                            await apiClient.completeBooking(id as string);
                            queryClient.invalidateQueries({ queryKey: ['booking', id] });
                            Alert.alert('Completed', 'Booking has been marked as completed.');
                          } catch (error: any) {
                            Alert.alert('Error', error?.response?.data?.error || 'Failed to complete booking');
                          }
                        },
                      },
                    ]
                  );
                }}
              >
                <LinearGradient
                  colors={['#D9B168', '#bea04f']}
                  className="py-4 rounded-2xl items-center"
                >
                  <View className="flex-row items-center">
                    <Ionicons name="checkmark-done" size={20} color="#122F26" />
                    <Text className="text-forest font-bold ml-2">Mark as Completed</Text>
                  </View>
                </LinearGradient>
              </TouchableOpacity>
            )}

            {/* Contact guest button */}
            {['confirmed', 'checked_in'].includes(booking.status) && (
              <TouchableOpacity className="mb-4">
                <LinearGradient
                  colors={['#D9B168', '#bea04f']}
                  className="py-4 rounded-2xl items-center"
                >
                  <Text className="text-forest font-bold">Contact Guest</Text>
                </LinearGradient>
              </TouchableOpacity>
            )}

            {/* Cancel for confirmed */}
            {booking.status === 'confirmed' && (
              <TouchableOpacity onPress={handleDecline}>
                <View className="bg-red-50 border border-red-200 py-3 rounded-2xl items-center">
                  <Text className="text-red-600 font-semibold">Cancel Booking</Text>
                </View>
              </TouchableOpacity>
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
    </SafeAreaView>
  );
}
