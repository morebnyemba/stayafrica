import { View, Text, ScrollView, TouchableOpacity, Platform, TextInput, Alert, KeyboardAvoidingView, TouchableWithoutFeedback, Keyboard } from 'react-native';
import { useState } from 'react';
import { useLocalSearchParams, useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';

export default function CreateBookingScreen() {
  const router = useRouter();
  const { propertyId } = useLocalSearchParams<{ propertyId: string }>();
  const { isAuthenticated } = useAuth();
  const [loading, setLoading] = useState(false);
  const [checkIn, setCheckIn] = useState('');
  const [checkOut, setCheckOut] = useState('');
  const [guests, setGuests] = useState('1');

  const handleCreateBooking = async () => {
    if (!checkIn || !checkOut) {
      Alert.alert('Error', 'Please select check-in and check-out dates');
      return;
    }

    setLoading(true);
    try {
      // TODO: Implement booking API call
      await new Promise(resolve => setTimeout(resolve, 1500));
      Alert.alert('Success', 'Booking request submitted!', [
        { text: 'OK', onPress: () => router.replace('/(tabs)/bookings') }
      ]);
    } catch (error) {
      Alert.alert('Error', 'Failed to create booking');
    } finally {
      setLoading(false);
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
          <View className="flex-row items-center">
            <TouchableOpacity
              onPress={() => router.back()}
              className="w-10 h-10 rounded-xl items-center justify-center mr-3"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="arrow-back" size={24} color="#fff" />
            </TouchableOpacity>
            <Text className="text-xl font-bold text-white">Book Property</Text>
          </View>
        </LinearGradient>

        <View className="flex-1 items-center justify-center px-6">
          <Text className="text-moss text-center">Please sign in to make a booking</Text>
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

  return (
    <KeyboardAvoidingView 
      behavior={Platform.OS === 'ios' ? 'padding' : 'height'}
      className="flex-1"
    >
      <TouchableWithoutFeedback onPress={Keyboard.dismiss}>
        <ScrollView 
          className="flex-1 bg-sand-100" 
          showsVerticalScrollIndicator={false}
          contentContainerStyle={{ paddingBottom: 40 }}
          keyboardShouldPersistTaps="handled"
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
          <Text className="text-xl font-bold text-white">Book Property</Text>
        </View>
      </LinearGradient>

      <View className="px-4 py-6">
        {/* Check-in Date */}
        <Text className="text-base font-semibold text-forest mb-2">Check-in Date</Text>
        <View className="bg-white rounded-2xl p-4 mb-4 flex-row items-center" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 2 },
          shadowOpacity: 0.05,
          shadowRadius: 4,
          elevation: 2,
        }}>
          <Ionicons name="calendar" size={24} color="#3A5C50" />
          <TextInput
            className="flex-1 ml-3 text-base text-forest"
            placeholder="Select check-in date"
            placeholderTextColor="#94a3b8"
            value={checkIn}
            onChangeText={setCheckIn}
          />
        </View>

        {/* Check-out Date */}
        <Text className="text-base font-semibold text-forest mb-2">Check-out Date</Text>
        <View className="bg-white rounded-2xl p-4 mb-4 flex-row items-center" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 2 },
          shadowOpacity: 0.05,
          shadowRadius: 4,
          elevation: 2,
        }}>
          <Ionicons name="calendar" size={24} color="#3A5C50" />
          <TextInput
            className="flex-1 ml-3 text-base text-forest"
            placeholder="Select check-out date"
            placeholderTextColor="#94a3b8"
            value={checkOut}
            onChangeText={setCheckOut}
          />
        </View>

        {/* Guests */}
        <Text className="text-base font-semibold text-forest mb-2">Number of Guests</Text>
        <View className="bg-white rounded-2xl p-4 mb-6 flex-row items-center" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 2 },
          shadowOpacity: 0.05,
          shadowRadius: 4,
          elevation: 2,
        }}>
          <Ionicons name="people" size={24} color="#3A5C50" />
          <TextInput
            className="flex-1 ml-3 text-base text-forest"
            placeholder="1"
            placeholderTextColor="#94a3b8"
            keyboardType="number-pad"
            value={guests}
            onChangeText={setGuests}
          />
        </View>

        {/* Book Button */}
        <TouchableOpacity
          onPress={handleCreateBooking}
          disabled={loading}
        >
          <LinearGradient
            colors={loading ? ['#94a3b8', '#94a3b8'] : ['#D9B168', '#bea04f']}
            className="py-4 rounded-2xl items-center"
          >
            <Text className="text-forest font-bold text-base">
              {loading ? 'Processing...' : 'Request to Book'}
            </Text>
          </LinearGradient>
        </TouchableOpacity>

        {/* Info */}
        <View className="mt-6 bg-blue-50 rounded-2xl p-4">
          <View className="flex-row items-center mb-2">
            <Ionicons name="information-circle" size={20} color="#3B82F6" />
            <Text className="text-blue-800 font-semibold ml-2">Booking Info</Text>
          </View>
          <Text className="text-blue-700 text-sm">
            You won't be charged until the host confirms your booking request.
          </Text>
        </View>
      </View>
        </ScrollView>
      </TouchableWithoutFeedback>
    </KeyboardAvoidingView>
  );
}
