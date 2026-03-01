import { View, Text, ScrollView, TouchableOpacity, Platform, ActivityIndicator, Image, Alert } from 'react-native';
import { useState } from 'react';
import { useRouter, useLocalSearchParams } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { useFeeConfiguration, calculateBookingCost } from '@/hooks/api-hooks';
import AsyncStorage from '@react-native-async-storage/async-storage';

export default function BookingConfirmScreen() {
  const router = useRouter();
  const { isAuthenticated, user } = useAuth();
  const params = useLocalSearchParams();
  
  const propertyId = params.propertyId as string;
  const checkIn = params.checkIn as string;
  const checkOut = params.checkOut as string;
  const guests = parseInt(params.guests as string || '1');

  const [agreedToTerms, setAgreedToTerms] = useState(false);
  const [isCreatingBooking, setIsCreatingBooking] = useState(false);
  const [contactingHost, setContactingHost] = useState(false);

  // Calculate nights
  const checkInDate = checkIn ? new Date(checkIn) : null;
  const checkOutDate = checkOut ? new Date(checkOut) : null;
  const nights = checkInDate && checkOutDate
    ? Math.ceil((checkOutDate.getTime() - checkInDate.getTime()) / (1000 * 60 * 60 * 24))
    : 0;

  const formatDate = (dateStr: string | null) => {
    if (!dateStr) return '—';
    return new Date(dateStr).toLocaleDateString('en-US', {
      month: 'short',
      day: 'numeric',
      year: 'numeric',
    });
  };

  // Fetch property details
  const { data: property, isLoading: loadingProperty } = useQuery({
    queryKey: ['property', propertyId],
    queryFn: async () => {
      const response = await apiClient.get(`/properties/${propertyId}/`);
      return response.data;
    },
    enabled: !!propertyId,
  });

  // Fetch fee configuration from backend
  const { data: feeConfig, isLoading: loadingFees } = useFeeConfiguration();

  // Calculate costs using backend fee config (matching web parity)
  const cleaningFee = property?.cleaning_fee || 0;
  let costs = { basePrice: 0, serviceFee: 0, commissionFee: 0, commissionRate: 0, cleaningFee: 0, total: 0 };
  if (property && feeConfig && nights > 0) {
    costs = calculateBookingCost(property.price_per_night, nights, feeConfig, cleaningFee);
  }
  const currency = property?.currency || 'USD';

  const contactHost = async () => {
    const hostId = property?.host?.id || property?.host_id;
    if (!property || !hostId) {
      Alert.alert('Error', 'Host information not available');
      return;
    }
    setContactingHost(true);
    try {
      await apiClient.createConversation(String(property.id));
      Alert.alert('Success', 'Conversation started!', [
        { text: 'OK', onPress: () => router.push('/(tabs)/messages') }
      ]);
    } catch (error: any) {
      const msg = error.response?.data?.error || error.response?.data?.detail || 'Failed to start conversation';
      Alert.alert('Error', msg);
    } finally {
      setContactingHost(false);
    }
  };

  const handleConfirmBooking = async () => {
    if (!agreedToTerms) {
      Alert.alert('Error', 'Please agree to the terms and conditions');
      return;
    }

    setIsCreatingBooking(true);
    try {
      const booking = await apiClient.createBooking({
        rental_property: Number(propertyId),
        check_in: checkIn,
        check_out: checkOut,
        number_of_guests: guests,
        cleaning_fee: costs.cleaningFee || 0,
      });

      router.push({
        pathname: '/booking/payment',
        params: {
          bookingId: String(booking.id),
          propertyName: property?.title || '',
          checkIn,
          checkOut,
          guests: guests.toString(),
          total: costs.total.toString(),
        },
      });
    } catch (error: any) {
      const msg =
        error.response?.data?.detail ||
        error.response?.data?.non_field_errors?.[0] ||
        error.response?.data?.error ||
        'Failed to create booking. Please try again.';
      Alert.alert('Booking Error', msg);
    } finally {
      setIsCreatingBooking(false);
    }
  };

  if (!isAuthenticated) {
    return (
      <View className="flex-1 bg-sand-100">
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
        >
          <TouchableOpacity onPress={() => router.back()} className="mb-4">
            <View className="w-10 h-10 rounded-xl items-center justify-center" style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}>
              <Ionicons name="arrow-back" size={24} color="#fff" />
            </View>
          </TouchableOpacity>
          <Text className="text-2xl font-bold text-white">Confirm Booking</Text>
        </LinearGradient>
        <View className="flex-1 items-center justify-center px-6">
          <Text className="text-forest text-lg">Please sign in to continue</Text>
          <TouchableOpacity
            onPress={async () => {
              // Save booking params so user returns here after login
              await AsyncStorage.setItem('auth_redirect', JSON.stringify({
                pathname: '/booking/confirm',
                params: { propertyId, checkIn, checkOut, guests: guests.toString() },
              }));
              router.push('/(auth)/login');
            }}
            className="mt-4"
          >
            <LinearGradient colors={['#D9B168', '#bea04f']} className="px-8 py-4 rounded-2xl">
              <Text className="text-forest font-bold">Sign In</Text>
            </LinearGradient>
          </TouchableOpacity>
        </View>
      </View>
    );
  }

  if (loadingProperty || loadingFees) {
    return (
      <View className="flex-1 bg-sand-100 items-center justify-center">
        <ActivityIndicator size="large" color="#D9B168" />
        <Text className="mt-4 text-moss">Loading booking details...</Text>
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
        <TouchableOpacity onPress={() => router.back()} className="mb-4">
          <View className="w-10 h-10 rounded-xl items-center justify-center" style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}>
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </View>
        </TouchableOpacity>
        <Text className="text-3xl font-black text-white tracking-tight">
          Confirm Booking
        </Text>
        <Text className="text-sand-200 text-sm mt-1">
          Review your trip details
        </Text>
      </LinearGradient>

      <View className="px-4 -mt-4">
        {/* Property Card */}
        <View className="bg-white rounded-2xl p-4 mb-4" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 4 },
          shadowOpacity: 0.08,
          shadowRadius: 8,
          elevation: 4,
        }}>
          <Text className="text-lg font-bold text-forest mb-3">Property Details</Text>
          <View className="flex-row gap-3">
            {property?.images?.[0]?.image && (
              <Image
                source={{ uri: property.images[0].image }}
                style={{ width: 80, height: 80, borderRadius: 12 }}
              />
            )}
            <View className="flex-1">
              <Text className="text-base font-semibold text-forest mb-1">
                {property?.title}
              </Text>
              <View className="flex-row items-center mb-1">
                <Ionicons name="location" size={14} color="#3A5C50" />
                <Text className="text-sm text-moss ml-1">
                  {property?.city}, {property?.country}
                </Text>
              </View>
              <Text className="text-xs text-moss">{property?.property_type}</Text>
            </View>
          </View>
        </View>

        {/* Contact Host */}
        <TouchableOpacity
          onPress={contactHost}
          disabled={contactingHost}
          className="bg-white rounded-2xl p-4 mb-4"
          style={{
            shadowColor: '#122F26',
            shadowOffset: { width: 0, height: 4 },
            shadowOpacity: 0.08,
            shadowRadius: 8,
            elevation: 4,
          }}
        >
          <View className="flex-row items-center justify-center">
            {contactingHost ? (
              <>
                <ActivityIndicator size="small" color="#122F26" />
                <Text className="text-forest font-semibold ml-2">Contacting Host...</Text>
              </>
            ) : (
              <>
                <Ionicons name="chatbubble-ellipses" size={20} color="#122F26" />
                <Text className="text-forest font-semibold ml-2">Contact Host</Text>
              </>
            )}
          </View>
        </TouchableOpacity>

        {/* Trip Details */}
        <View className="bg-white rounded-2xl p-4 mb-4" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 4 },
          shadowOpacity: 0.08,
          shadowRadius: 8,
          elevation: 4,
        }}>
          <Text className="text-lg font-bold text-forest mb-3">Your Trip</Text>
          <View className="space-y-3">
            <View className="flex-row items-start">
              <Ionicons name="calendar" size={20} color="#D9B168" />
              <View className="ml-3 flex-1">
                <Text className="font-semibold text-forest">Dates</Text>
                <Text className="text-sm text-moss">
                  {formatDate(checkIn)} - {formatDate(checkOut)}
                </Text>
                <Text className="text-sm text-moss">
                  {nights} {nights === 1 ? 'night' : 'nights'}
                </Text>
              </View>
            </View>
            <View className="flex-row items-start">
              <Ionicons name="people" size={20} color="#D9B168" />
              <View className="ml-3 flex-1">
                <Text className="font-semibold text-forest">Guests</Text>
                <Text className="text-sm text-moss">
                  {guests} {guests === 1 ? 'guest' : 'guests'}
                </Text>
              </View>
            </View>
          </View>
        </View>

        {/* Guest Information */}
        <View className="bg-white rounded-2xl p-4 mb-4" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 4 },
          shadowOpacity: 0.08,
          shadowRadius: 8,
          elevation: 4,
        }}>
          <Text className="text-lg font-bold text-forest mb-3">Guest Information</Text>
          <View className="space-y-2">
            <View className="flex-row justify-between">
              <Text className="text-moss">Name:</Text>
              <Text className="font-semibold text-forest">
                {user?.first_name} {user?.last_name}
              </Text>
            </View>
            <View className="flex-row justify-between">
              <Text className="text-moss">Email:</Text>
              <Text className="font-semibold text-forest">{user?.email}</Text>
            </View>
            {user?.phone_number && (
              <View className="flex-row justify-between">
                <Text className="text-moss">Phone:</Text>
                <Text className="font-semibold text-forest">{user.phone_number}</Text>
              </View>
            )}
          </View>
        </View>

        {/* Price Breakdown */}
        <View className="bg-white rounded-2xl p-4 mb-4" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 4 },
          shadowOpacity: 0.08,
          shadowRadius: 8,
          elevation: 4,
        }}>
          <Text className="text-lg font-bold text-forest mb-3">Price Details</Text>
          <View className="space-y-2 mb-3 pb-3 border-b border-sand-200">
            <View className="flex-row justify-between">
              <Text className="text-moss">{currency} {property?.price_per_night} × {nights} nights</Text>
              <Text className="font-semibold text-forest">{currency} {costs.basePrice.toFixed(2)}</Text>
            </View>
            <View className="flex-row justify-between">
              <Text className="text-moss">Service fee</Text>
              <Text className="font-semibold text-forest">{currency} {costs.serviceFee.toFixed(2)}</Text>
            </View>
            <View className="flex-row justify-between">
              <Text className="text-moss">Commission ({(costs.commissionRate * 100).toFixed(1)}%)</Text>
              <Text className="font-semibold text-forest">{currency} {costs.commissionFee.toFixed(2)}</Text>
            </View>
            {costs.cleaningFee > 0 && (
              <View className="flex-row justify-between">
                <Text className="text-moss">Cleaning fee</Text>
                <Text className="font-semibold text-forest">{currency} {costs.cleaningFee.toFixed(2)}</Text>
              </View>
            )}
          </View>
          <View className="flex-row justify-between">
            <Text className="text-lg font-bold text-forest">Total</Text>
            <Text className="text-lg font-bold text-forest">{currency} {costs.total.toFixed(2)}</Text>
          </View>
        </View>

        {/* Terms and Conditions */}
        <TouchableOpacity
          onPress={() => setAgreedToTerms(!agreedToTerms)}
          className="flex-row items-start bg-white rounded-2xl p-4 mb-4"
          style={{
            shadowColor: '#122F26',
            shadowOffset: { width: 0, height: 2 },
            shadowOpacity: 0.05,
            shadowRadius: 4,
            elevation: 2,
          }}
        >
          <View className={`w-6 h-6 rounded border-2 ${
            agreedToTerms ? 'bg-gold border-gold' : 'border-moss'
          } items-center justify-center mr-3`}>
            {agreedToTerms && <Ionicons name="checkmark" size={16} color="#122F26" />}
          </View>
          <Text className="flex-1 text-sm text-moss">
            I agree to the Terms and Conditions and Cancellation Policy
          </Text>
        </TouchableOpacity>

        {/* Confirm Button */}
        <TouchableOpacity
          onPress={handleConfirmBooking}
          disabled={!agreedToTerms || isCreatingBooking}
          className={`rounded-2xl overflow-hidden ${
            (!agreedToTerms || isCreatingBooking) ? 'opacity-50' : ''
          }`}
        >
          <LinearGradient
            colors={['#D9B168', '#bea04f']}
            className="py-4 items-center"
            style={{
              shadowColor: '#D9B168',
              shadowOffset: { width: 0, height: 4 },
              shadowOpacity: 0.3,
              shadowRadius: 8,
              elevation: 5,
            }}
          >
            <View className="flex-row items-center">
              {isCreatingBooking ? (
                <>
                  <ActivityIndicator size="small" color="#122F26" />
                  <Text className="text-forest font-bold text-base ml-2">Creating Booking...</Text>
                </>
              ) : (
                <>
                  <Ionicons name="checkmark-circle" size={20} color="#122F26" />
                  <Text className="text-forest font-bold text-base ml-2">Confirm Booking</Text>
                </>
              )}
            </View>
          </LinearGradient>
        </TouchableOpacity>

        <Text className="text-xs text-center text-moss mt-3">
          You won't be charged yet. Payment will be processed on the next page.
        </Text>
      </View>
    </ScrollView>
  );
}
