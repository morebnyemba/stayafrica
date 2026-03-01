import { useState, useEffect, useCallback } from 'react';
import { View, Text, ScrollView, TouchableOpacity, Platform, ActivityIndicator, Alert, Linking } from 'react-native';
import { useLocalSearchParams, useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useSafeAreaInsets } from 'react-native-safe-area-context';
import { useAuth } from '@/context/auth-context';
import { useBookingById } from '@/hooks/api-hooks';
import { apiClient } from '@/services/api-client';
import { logApiError } from '@/utils/logger';

export default function BookingDetailScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const { id } = useLocalSearchParams<{ id: string }>();
  const { isAuthenticated } = useAuth();

  // Fetch booking details from API
  const { data: booking, isLoading: loading } = useBookingById(id);

  // Fetch payment data separately with polling
  const [payment, setPayment] = useState<any>(null);
  const [paymentLoading, setPaymentLoading] = useState(true);

  const fetchPayment = useCallback(async () => {
    if (!id || !isAuthenticated) return;
    try {
      const response = await apiClient.getPaymentHistory({ booking: id });
      const payments = response?.results || [];
      setPayment(payments.length > 0 ? payments[0] : null);
    } catch {
      setPayment(null);
    } finally {
      setPaymentLoading(false);
    }
  }, [id, isAuthenticated]);

  useEffect(() => {
    fetchPayment();
    const interval = setInterval(fetchPayment, 15000);
    return () => clearInterval(interval);
  }, [fetchPayment]);

  // Payment state helpers
  const paymentStatus = payment?.status?.toLowerCase();
  const paymentSuccess = paymentStatus === 'success';
  const paymentProcessing = paymentStatus === 'initiated';
  const paymentFailed = paymentStatus === 'failed';
  const canPay =
    booking &&
    ['pending', 'confirmed'].includes(booking.status?.toLowerCase()) &&
    (!payment || ['failed', 'pending'].includes(paymentStatus));

  const currency = booking?.currency || 'USD';

  if (!isAuthenticated) {
    return (
      <View className="flex-1 bg-sand-100">
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: insets.top + 8 }}
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

  const STATUS_COLORS: Record<string, { bg: string; text: string; label: string }> = {
    confirmed: { bg: 'bg-green-100', text: 'text-green-800', label: 'Confirmed' },
    pending: { bg: 'bg-yellow-100', text: 'text-yellow-800', label: 'Pending' },
    cancelled: { bg: 'bg-red-100', text: 'text-red-800', label: 'Cancelled' },
    checked_in: { bg: 'bg-blue-100', text: 'text-blue-800', label: 'Checked In' },
    checked_out: { bg: 'bg-purple-100', text: 'text-purple-800', label: 'Checked Out' },
    completed: { bg: 'bg-blue-100', text: 'text-blue-800', label: 'Completed' },
  };
  const defaultStatusColor = { bg: 'bg-gray-100', text: 'text-gray-800', label: 'Unknown' };

  const cardStyle = {
    shadowColor: '#122F26',
    shadowOffset: { width: 0, height: 2 },
    shadowOpacity: 0.05,
    shadowRadius: 4,
    elevation: 2,
  };

  return (
    <ScrollView 
      className="flex-1 bg-sand-100" 
      showsVerticalScrollIndicator={false}
      contentContainerStyle={{ paddingBottom: insets.bottom + 40 }}
    >
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f']}
        className="px-4 pb-6"
        style={{ paddingTop: insets.top + 8 }}
      >
        <View className="flex-row items-center">
          <TouchableOpacity
            onPress={() => router.back()}
            className="w-10 h-10 rounded-xl items-center justify-center mr-3"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </TouchableOpacity>
          <Text className="text-xl font-bold text-white flex-1">Booking Details</Text>
        </View>
      </LinearGradient>

      <View className="px-4 py-6 pb-8">
        {booking ? (
          <>
            {/* Property & Status Header Card */}
            <View className="bg-white rounded-2xl p-4 mb-4" style={cardStyle}>
              <View className="flex-row items-start justify-between mb-3">
                <View className="flex-row items-center">
                  <View className="bg-green-50 px-2 py-1 rounded-md mr-2">
                    <Text className="text-green-800 text-xs font-medium">
                      <Ionicons name="home" size={10} color="#166534" /> Stay
                    </Text>
                  </View>
                  <View className={`${(STATUS_COLORS[booking.status] || defaultStatusColor).bg} px-3 py-1 rounded-full`}>
                    <Text className={`${(STATUS_COLORS[booking.status] || defaultStatusColor).text} text-xs font-semibold`}>
                      {(STATUS_COLORS[booking.status] || defaultStatusColor).label}
                    </Text>
                  </View>
                </View>
                <View className="items-end">
                  <Text className="text-moss text-xs">Booking Ref</Text>
                  <Text className="text-forest font-bold font-mono">
                    {booking.booking_ref || `#${id}`}
                  </Text>
                </View>
              </View>
              <Text className="text-xl font-bold text-forest mb-1">
                {booking.property?.title || booking.property_title || 'Property'}
              </Text>
              <View className="flex-row items-center">
                <Ionicons name="location" size={14} color="#3A5C50" />
                <Text className="text-moss text-sm ml-1">
                  {booking.property?.city || booking.property?.location?.city || 'Unknown'},{' '}
                  {booking.property?.country || booking.property?.location?.country || 'Unknown'}
                </Text>
              </View>
            </View>

            {/* Stay Details */}
            <View className="bg-white rounded-2xl p-4 mb-4" style={cardStyle}>
              <Text className="text-lg font-bold text-forest mb-4">Stay Details</Text>
              
              {/* Dates */}
              <View className="flex-row items-center justify-between mb-4">
                <View className="flex-1">
                  <View className="flex-row items-center mb-1">
                    <Ionicons name="calendar" size={14} color="#3A5C50" />
                    <Text className="text-moss text-xs font-medium ml-1">Check-in</Text>
                  </View>
                  <Text className="text-forest font-bold">
                    {new Date(booking.check_in).toLocaleDateString('en-US', { 
                      weekday: 'short', month: 'short', day: 'numeric', year: 'numeric'
                    })}
                  </Text>
                </View>
                <Ionicons name="arrow-forward" size={20} color="#D9B168" />
                <View className="flex-1 items-end">
                  <View className="flex-row items-center mb-1">
                    <Ionicons name="calendar" size={14} color="#3A5C50" />
                    <Text className="text-moss text-xs font-medium ml-1">Check-out</Text>
                  </View>
                  <Text className="text-forest font-bold">
                    {new Date(booking.check_out).toLocaleDateString('en-US', { 
                      weekday: 'short', month: 'short', day: 'numeric', year: 'numeric'
                    })}
                  </Text>
                </View>
              </View>

              {/* Nights & Guests grid */}
              <View className="flex-row border-t border-sand-200 pt-4">
                <View className="flex-1">
                  <View className="flex-row items-center mb-1">
                    <Ionicons name="moon" size={14} color="#3A5C50" />
                    <Text className="text-moss text-xs font-medium ml-1">Nights</Text>
                  </View>
                  <Text className="text-forest font-bold">{booking.nights || booking.number_of_nights}</Text>
                </View>
                <View className="flex-1">
                  <View className="flex-row items-center mb-1">
                    <Ionicons name="people" size={14} color="#3A5C50" />
                    <Text className="text-moss text-xs font-medium ml-1">Guests</Text>
                  </View>
                  <Text className="text-forest font-bold">{booking.number_of_guests}</Text>
                </View>
              </View>

              {/* Special Requests */}
              {booking.special_requests && (
                <View className="border-t border-sand-200 pt-4 mt-4">
                  <Text className="text-moss text-xs font-medium mb-1">Special Requests</Text>
                  <Text className="text-forest">{booking.special_requests}</Text>
                </View>
              )}
            </View>

            {/* Price Breakdown */}
            <View className="bg-white rounded-2xl p-4 mb-4" style={cardStyle}>
              <Text className="text-lg font-bold text-forest mb-4">Price Breakdown</Text>
              
              <View className="flex-row justify-between mb-3">
                <Text className="text-moss">
                  Nightly total ({booking.nights || booking.number_of_nights} night{(booking.nights || booking.number_of_nights) !== 1 ? 's' : ''})
                </Text>
                <Text className="text-forest font-semibold">
                  {currency} {Number(booking.nightly_total || booking.total_before_tax || 0).toFixed(2)}
                </Text>
              </View>
              {booking.cleaning_fee && Number(booking.cleaning_fee) > 0 && (
                <View className="flex-row justify-between mb-3">
                  <Text className="text-moss">Cleaning fee</Text>
                  <Text className="text-forest font-semibold">
                    {currency} {Number(booking.cleaning_fee).toFixed(2)}
                  </Text>
                </View>
              )}
              <View className="flex-row justify-between mb-3">
                <Text className="text-moss">Service fee</Text>
                <Text className="text-forest font-semibold">
                  {currency} {Number(booking.service_fee || 0).toFixed(2)}
                </Text>
              </View>
              <View className="border-t border-sand-200 pt-3 mt-1">
                <View className="flex-row justify-between">
                  <Text className="text-forest font-bold text-lg">Total</Text>
                  <Text className="text-gold font-black text-xl">
                    {currency} {Number(booking.grand_total).toFixed(2)}
                  </Text>
                </View>
              </View>
            </View>

            {/* Payment Status */}
            <View className="bg-white rounded-2xl p-4 mb-4" style={cardStyle}>
              <Text className="text-lg font-bold text-forest mb-4">Payment</Text>

              {paymentLoading ? (
                <View className="flex-row items-center">
                  <ActivityIndicator size="small" color="#3A5C50" />
                  <Text className="text-moss ml-2">Checking payment status...</Text>
                </View>
              ) : paymentSuccess ? (
                <View className="bg-green-50 rounded-xl p-4 flex-row items-center">
                  <View className="bg-green-100 rounded-full p-2 mr-3">
                    <Ionicons name="checkmark-circle" size={22} color="#16a34a" />
                  </View>
                  <View className="flex-1">
                    <Text className="text-green-800 font-bold">Payment Successful</Text>
                    <Text className="text-green-600 text-xs mt-0.5">
                      Paid via {payment.provider}{payment.gateway_ref ? ` — Ref: ${payment.gateway_ref}` : ''}
                    </Text>
                  </View>
                </View>
              ) : paymentProcessing ? (
                <View className="bg-blue-50 rounded-xl p-4 flex-row items-center">
                  <View className="mr-3">
                    <ActivityIndicator size="small" color="#2563eb" />
                  </View>
                  <View className="flex-1">
                    <Text className="text-blue-800 font-bold">Payment Processing</Text>
                    <Text className="text-blue-600 text-xs mt-0.5">
                      Your payment is being processed. This will update automatically.
                    </Text>
                  </View>
                </View>
              ) : paymentFailed ? (
                <View>
                  <View className="bg-red-50 rounded-xl p-4 flex-row items-center mb-3">
                    <View className="bg-red-100 rounded-full p-2 mr-3">
                      <Ionicons name="close-circle" size={22} color="#dc2626" />
                    </View>
                    <View className="flex-1">
                      <Text className="text-red-800 font-bold">Payment Failed</Text>
                      <Text className="text-red-600 text-xs mt-0.5">
                        Your previous payment attempt was unsuccessful. You can try again below.
                      </Text>
                    </View>
                  </View>
                  <TouchableOpacity
                    onPress={() => router.push({
                      pathname: '/booking/payment',
                      params: {
                        bookingId: booking.id,
                        total: booking.grand_total,
                        propertyName: booking.property?.title || booking.property_title,
                        checkIn: booking.check_in,
                        checkOut: booking.check_out,
                        guests: booking.number_of_guests,
                      }
                    })}
                  >
                    <LinearGradient
                      colors={['#D9B168', '#bea04f']}
                      className="py-4 rounded-2xl items-center"
                      style={{
                        shadowColor: '#D9B168',
                        shadowOffset: { width: 0, height: 4 },
                        shadowOpacity: 0.3,
                        shadowRadius: 8,
                        elevation: 5,
                      }}
                    >
                      <View className="flex-row items-center">
                        <Ionicons name="refresh" size={18} color="#122F26" />
                        <Text className="text-forest font-bold ml-2">Retry Payment</Text>
                      </View>
                    </LinearGradient>
                  </TouchableOpacity>
                </View>
              ) : canPay ? (
                <View>
                  <View className="bg-yellow-50 rounded-xl p-4 flex-row items-center mb-3">
                    <View className="bg-yellow-100 rounded-full p-2 mr-3">
                      <Ionicons name="alert-circle" size={22} color="#d97706" />
                    </View>
                    <View className="flex-1">
                      <Text className="text-yellow-800 font-bold">Payment Required</Text>
                      <Text className="text-yellow-600 text-xs mt-0.5">
                        Complete your payment to confirm this booking.
                      </Text>
                    </View>
                  </View>
                  <TouchableOpacity
                    onPress={() => router.push({
                      pathname: '/booking/payment',
                      params: {
                        bookingId: booking.id,
                        total: booking.grand_total,
                        propertyName: booking.property?.title || booking.property_title,
                        checkIn: booking.check_in,
                        checkOut: booking.check_out,
                        guests: booking.number_of_guests,
                      }
                    })}
                  >
                    <LinearGradient
                      colors={['#D9B168', '#bea04f']}
                      className="py-4 rounded-2xl items-center"
                      style={{
                        shadowColor: '#D9B168',
                        shadowOffset: { width: 0, height: 4 },
                        shadowOpacity: 0.3,
                        shadowRadius: 8,
                        elevation: 5,
                      }}
                    >
                      <View className="flex-row items-center">
                        <Ionicons name="card" size={18} color="#122F26" />
                        <Text className="text-forest font-bold ml-2">Pay Now</Text>
                      </View>
                    </LinearGradient>
                  </TouchableOpacity>
                </View>
              ) : (
                <Text className="text-moss text-sm">No payment information available.</Text>
              )}
            </View>

            {/* Actions */}
            <View className="bg-white rounded-2xl p-4 mb-4" style={cardStyle}>
              {/* Contact Host */}
              {(booking.status === 'pending' || booking.status === 'confirmed') && (
                <TouchableOpacity
                  onPress={async () => {
                    try {
                      const propertyId = booking.property_id || booking.rental_property || booking.property?.id;
                      if (!propertyId) {
                        Alert.alert('Error', 'Unable to find property information');
                        return;
                      }
                      const conversation = await apiClient.createConversation(propertyId);
                      if (conversation && conversation.id) {
                        router.push(`/(tabs)/messages/${conversation.id}`);
                      } else {
                        router.push('/(tabs)/messages');
                      }
                    } catch (error: any) {
                      logApiError('/messaging/conversations/', error, { action: 'create conversation' });
                      router.push('/(tabs)/messages');
                    }
                  }}
                  className="mb-3"
                >
                  <View className="bg-forest py-3.5 rounded-2xl items-center">
                    <View className="flex-row items-center">
                      <Ionicons name="chatbubble-outline" size={18} color="#fff" />
                      <Text className="text-white font-bold ml-2">Contact Host</Text>
                    </View>
                  </View>
                </TouchableOpacity>
              )}

              {/* View Property and Get Directions */}
              <View className="flex-row gap-3">
                {booking.property?.id && (
                  <TouchableOpacity
                    onPress={() => router.push(`/(tabs)/explore/${booking.property.id}`)}
                    className="flex-1"
                  >
                    <View className="bg-white border border-sand-300 py-3 rounded-2xl items-center">
                      <View className="flex-row items-center">
                        <Ionicons name="home-outline" size={16} color="#3A5C50" />
                        <Text className="text-moss font-semibold ml-1.5 text-sm">View Property</Text>
                      </View>
                    </View>
                  </TouchableOpacity>
                )}

                {['confirmed', 'CONFIRMED', 'checked_in'].includes(booking.status) && (
                  <TouchableOpacity
                    onPress={() => {
                      const lat = booking.property?.location?.latitude || booking.property?.latitude;
                      const lng = booking.property?.location?.longitude || booking.property?.longitude;
                      const address = booking.property?.location?.address || booking.property?.address || '';
                      
                      if (lat && lng) {
                        const url = Platform.select({
                          ios: `maps:?q=${encodeURIComponent(address)}&ll=${lat},${lng}`,
                          android: `geo:${lat},${lng}?q=${lat},${lng}(${encodeURIComponent(address)})`,
                        });
                        if (url) {
                          Linking.canOpenURL(url).then((supported) => {
                            if (supported) Linking.openURL(url);
                            else Linking.openURL(`https://maps.google.com/?q=${lat},${lng}`);
                          });
                        }
                      } else {
                        const searchQuery = `${booking.property?.title || ''} ${booking.property?.city || ''} ${booking.property?.country || ''}`;
                        Linking.openURL(`https://maps.google.com/maps?q=${encodeURIComponent(searchQuery)}`);
                      }
                    }}
                    className="flex-1"
                  >
                    <View className="bg-white border border-sand-300 py-3 rounded-2xl items-center">
                      <View className="flex-row items-center">
                        <Ionicons name="navigate-outline" size={16} color="#3A5C50" />
                        <Text className="text-moss font-semibold ml-1.5 text-sm">Get Directions</Text>
                      </View>
                    </View>
                  </TouchableOpacity>
                )}
              </View>

              {/* Cancel Booking */}
              {(booking.status === 'pending' || booking.status === 'confirmed') && (
                <TouchableOpacity
                  onPress={() => {
                    Alert.alert(
                      'Cancel Booking',
                      'Are you sure you want to cancel this booking?',
                      [
                        { text: 'No', style: 'cancel' },
                        {
                          text: 'Yes, Cancel',
                          style: 'destructive',
                          onPress: async () => {
                            try {
                              await apiClient.cancelBooking(id as string);
                              Alert.alert('Cancelled', 'Your booking has been cancelled successfully.', [
                                { text: 'OK', onPress: () => router.back() }
                              ]);
                            } catch (error: any) {
                              const errorMessage = error?.response?.data?.detail || 
                                                  error?.response?.data?.message || 
                                                  'Failed to cancel booking. Please try again.';
                              Alert.alert('Error', errorMessage);
                            }
                          },
                        },
                      ]
                    );
                  }}
                  className="mt-3"
                >
                  <View className="bg-red-50 border border-red-200 py-3 rounded-2xl items-center">
                    <Text className="text-red-600 font-semibold">Cancel Booking</Text>
                  </View>
                </TouchableOpacity>
              )}
            </View>

            {/* Booking Metadata */}
            <Text className="text-center text-xs text-moss mt-2">
              Booked on {new Date(booking.created_at).toLocaleDateString()} — Last updated{' '}
              {new Date(booking.updated_at).toLocaleDateString()}
            </Text>
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
