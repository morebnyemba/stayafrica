import { View, Text, ScrollView, TouchableOpacity, Platform, Alert, Modal, ActivityIndicator } from 'react-native';
import { useState } from 'react';
import { useLocalSearchParams, useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { Calendar } from 'react-native-calendars';
import { apiClient } from '@/services/api-client';
import { format } from 'date-fns';
import { Input } from '@/components/common/Input';
import { SafeAreaView } from 'react-native-safe-area-context';

export default function CreateBookingScreen() {
  const router = useRouter();
  const { propertyId } = useLocalSearchParams<{ propertyId: string }>();
  const { isAuthenticated } = useAuth();
  const [loading, setLoading] = useState(false);
  const [checkIn, setCheckIn] = useState('');
  const [checkOut, setCheckOut] = useState('');
  const [guests, setGuests] = useState('1');
  const [showCheckInPicker, setShowCheckInPicker] = useState(false);
  const [showCheckOutPicker, setShowCheckOutPicker] = useState(false);
  const [markedDates, setMarkedDates] = useState<any>({});

  const handleCreateBooking = async () => {
    if (!checkIn || !checkOut) {
      Alert.alert('Error', 'Please select check-in and check-out dates');
      return;
    }

    if (new Date(checkOut) <= new Date(checkIn)) {
      Alert.alert('Error', 'Check-out date must be after check-in date');
      return;
    }

    setLoading(true);
    try {
      console.log('Creating booking with data:', {
        property_id: propertyId,
        check_in_date: checkIn,
        check_out_date: checkOut,
        number_of_guests: parseInt(guests),
      });

      const response = await apiClient.post('/bookings/', {
        property_id: propertyId,
        check_in_date: checkIn,
        check_out_date: checkOut,
        number_of_guests: parseInt(guests),
      });

      const bookingId = response.data.id;
      
      Alert.alert('Booking Created!', 'Proceed to payment to confirm your booking.', [
        { 
          text: 'Pay Now', 
          onPress: () => router.replace({
            pathname: '/booking/payment',
            params: {
              bookingId: bookingId,
              total: response.data.total_price || '0',
              checkIn: checkIn,
              checkOut: checkOut,
              guests: guests,
            }
          })
        }
      ]);
    } catch (error: any) {
      console.error('Error creating booking:', error);
      console.error('Error status:', error.response?.status);
      console.error('Error data:', error.response?.data);
      const errorMessage = error.response?.data?.detail || error.response?.data?.message || error.response?.data?.non_field_errors?.[0] || 'Failed to create booking. Please try again.';
      Alert.alert('Error', errorMessage);
    } finally {
      setLoading(false);
    }
  };

  const handleCheckInSelect = (date: string) => {
    setCheckIn(date);
    setShowCheckInPicker(false);
    const marks: any = { [date]: { startingDay: true, color: '#D9B168', textColor: 'white' } };
    if (checkOut && checkOut > date) {
      marks[checkOut] = { endingDay: true, color: '#D9B168', textColor: 'white' };
    }
    setMarkedDates(marks);
  };

  const handleCheckOutSelect = (date: string) => {
    if (checkIn && date > checkIn) {
      setCheckOut(date);
      setShowCheckOutPicker(false);
      const marks: any = {
        [checkIn]: { startingDay: true, color: '#D9B168', textColor: 'white' },
        [date]: { endingDay: true, color: '#D9B168', textColor: 'white' }
      };
      setMarkedDates(marks);
    } else {
      Alert.alert('Invalid Date', 'Check-out date must be after check-in date');
    }
  };

  if (!isAuthenticated) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100">
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
        >
          <View className="flex-row items-center">
            <TouchableOpacity onPress={() => router.back()} className="w-10 h-10 rounded-xl items-center justify-center mr-3" style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}>
              <Ionicons name="arrow-back" size={24} color="#fff" />
            </TouchableOpacity>
            <Text className="text-xl font-bold text-white">Book Property</Text>
          </View>
        </LinearGradient>
        <View className="flex-1 items-center justify-center px-6">
          <Text className="text-moss text-center">Please sign in to make a booking</Text>
          <TouchableOpacity onPress={() => router.push('/(auth)/login')} className="mt-4">
            <LinearGradient colors={['#D9B168', '#bea04f']} className="px-8 py-4 rounded-2xl">
              <Text className="text-forest font-bold">Sign In</Text>
            </LinearGradient>
          </TouchableOpacity>
        </View>
      </SafeAreaView>
    );
  }

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      <ScrollView className="flex-1 bg-sand-100" showsVerticalScrollIndicator={false} contentContainerStyle={{ paddingBottom: 40 }} keyboardShouldPersistTaps="handled">
        <LinearGradient colors={['#122F26', '#1d392f']} className="px-4 pb-6" style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}>
          <View className="flex-row items-center">
            <TouchableOpacity onPress={() => router.back()} className="w-10 h-10 rounded-xl items-center justify-center mr-3" style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}>
              <Ionicons name="arrow-back" size={24} color="#fff" />
            </TouchableOpacity>
            <Text className="text-xl font-bold text-white">Book Property</Text>
          </View>
        </LinearGradient>

        <View className="px-4 py-6">
          <Text className="text-base font-semibold text-forest mb-2">Check-in Date</Text>
          <TouchableOpacity onPress={() => setShowCheckInPicker(true)} className="bg-white rounded-2xl p-4 mb-4 flex-row items-center" style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 4, elevation: 2 }}>
            <Ionicons name="calendar" size={24} color="#3A5C50" />
            <Text className={`flex-1 ml-3 text-base ${checkIn ? 'text-forest' : 'text-[#94a3b8]'}`}>
              {checkIn ? format(new Date(checkIn), 'MMM dd, yyyy') : 'Select check-in date'}
            </Text>
          </TouchableOpacity>

          <Text className="text-base font-semibold text-forest mb-2">Check-out Date</Text>
          <TouchableOpacity onPress={() => { if (!checkIn) { Alert.alert('Select Check-in First', 'Please select a check-in date before selecting check-out'); return; } setShowCheckOutPicker(true); }} className="bg-white rounded-2xl p-4 mb-4 flex-row items-center" style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 4, elevation: 2 }}>
            <Ionicons name="calendar" size={24} color="#3A5C50" />
            <Text className={`flex-1 ml-3 text-base ${checkOut ? 'text-forest' : 'text-[#94a3b8]'}`}>
              {checkOut ? format(new Date(checkOut), 'MMM dd, yyyy') : 'Select check-out date'}
            </Text>
          </TouchableOpacity>

          <Text className="text-base font-semibold text-forest mb-2">Number of Guests</Text>
          <View className="mb-6">
            <Input value={guests} onChangeText={setGuests} placeholder="1" keyboardType="number-pad" leftIcon={<Ionicons name="people" size={20} color="#3A5C50" />} />
          </View>

          <TouchableOpacity onPress={handleCreateBooking} disabled={loading || !checkIn || !checkOut}>
            <LinearGradient colors={(loading || !checkIn || !checkOut) ? ['#94a3b8', '#94a3b8'] : ['#D9B168', '#bea04f']} className="py-4 rounded-2xl items-center">
              {loading ? (<ActivityIndicator size="small" color="#122F26" />) : (<Text className="text-forest font-bold text-base">Request to Book</Text>)}
            </LinearGradient>
          </TouchableOpacity>

          <View className="mt-6 bg-blue-50 rounded-2xl p-4">
            <View className="flex-row items-center mb-2">
              <Ionicons name="information-circle" size={20} color="#3B82F6" />
              <Text className="text-blue-800 font-semibold ml-2">Booking Info</Text>
            </View>
            <Text className="text-blue-700 text-sm">After creating your booking request, you'll be taken to payment. You won't be charged until the host confirms your booking.</Text>
          </View>
        </View>
      </ScrollView>

      <Modal visible={showCheckInPicker} animationType="slide" transparent={true} onRequestClose={() => setShowCheckInPicker(false)}>
        <View className="flex-1 justify-end bg-black/50">
          <View className="bg-white rounded-t-3xl" style={{ maxHeight: '80%' }}>
            <View className="flex-row justify-between items-center p-4 border-b border-sand-200">
              <Text className="text-lg font-bold text-forest">Select Check-in Date</Text>
              <TouchableOpacity onPress={() => setShowCheckInPicker(false)}><Ionicons name="close" size={28} color="#3A5C50" /></TouchableOpacity>
            </View>
            <Calendar minDate={new Date().toISOString().split('T')[0]} onDayPress={(day) => handleCheckInSelect(day.dateString)} markedDates={{ ...markedDates, [checkIn || '']: { selected: true, selectedColor: '#D9B168' } }} theme={{ selectedDayBackgroundColor: '#D9B168', todayTextColor: '#D9B168', arrowColor: '#D9B168' }} />
          </View>
        </View>
      </Modal>

      <Modal visible={showCheckOutPicker} animationType="slide" transparent={true} onRequestClose={() => setShowCheckOutPicker(false)}>
        <View className="flex-1 justify-end bg-black/50">
          <View className="bg-white rounded-t-3xl" style={{ maxHeight: '80%' }}>
            <View className="flex-row justify-between items-center p-4 border-b border-sand-200">
              <Text className="text-lg font-bold text-forest">Select Check-out Date</Text>
              <TouchableOpacity onPress={() => setShowCheckOutPicker(false)}><Ionicons name="close" size={28} color="#3A5C50" /></TouchableOpacity>
            </View>
            <Calendar minDate={checkIn ? new Date(new Date(checkIn).getTime() + 86400000).toISOString().split('T')[0] : new Date().toISOString().split('T')[0]} onDayPress={(day) => handleCheckOutSelect(day.dateString)} markedDates={{ ...markedDates, [checkOut || '']: { selected: true, selectedColor: '#D9B168' } }} theme={{ selectedDayBackgroundColor: '#D9B168', todayTextColor: '#D9B168', arrowColor: '#D9B168' }} />
          </View>
        </View>
      </Modal>
    </SafeAreaView>
  );
}

CreateBookingScreen.displayName = 'CreateBookingScreen';
