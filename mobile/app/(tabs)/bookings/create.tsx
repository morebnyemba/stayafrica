import { View, Text, ScrollView, TouchableOpacity, Platform, TextInput, Alert, KeyboardAvoidingView, TouchableWithoutFeedback, Keyboard, Modal } from 'react-native';
import { useState, useMemo } from 'react';
import { useLocalSearchParams, useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { Calendar } from 'react-native-calendars';
import { format, parseISO, addDays } from 'date-fns';
import { useAuth } from '@/context/auth-context';
import { useUnavailableDates } from '@/hooks/api-hooks';
import { isHostMode, promptSwitchToTravelMode } from '@/utils/helpers';

export default function CreateBookingScreen() {
  const router = useRouter();
  const { propertyId } = useLocalSearchParams<{ propertyId: string }>();
  const { isAuthenticated, user, switchProfile } = useAuth();
  const today = format(new Date(), 'yyyy-MM-dd');
  const tomorrow = format(addDays(new Date(), 1), 'yyyy-MM-dd');
  const [checkIn, setCheckIn] = useState('');
  const [checkOut, setCheckOut] = useState('');
  const [guests, setGuests] = useState('1');
  const [showCheckInCalendar, setShowCheckInCalendar] = useState(false);
  const [showCheckOutCalendar, setShowCheckOutCalendar] = useState(false);

  // Fetch unavailable dates for this property
  const { data: unavailableData } = useUnavailableDates(propertyId || '');
  const unavailableDates = unavailableData?.unavailable_dates || [];

  // Build disabled dates map for react-native-calendars
  const disabledMarkedDates = useMemo(() => {
    const marks: Record<string, any> = {};
    for (const date of unavailableDates) {
      marks[date] = {
        marked: true,
        disabled: true,
        disableTouchEvent: true,
        customStyles: {
          container: {
            backgroundColor: '#FEE2E2',
            borderRadius: 16,
            borderWidth: 1,
            borderColor: '#FCA5A5',
          },
          text: { color: '#B91C1C', fontWeight: '700' },
        },
      };
    }

    // Disable current date to align with web behavior (booking starts from tomorrow)
    marks[today] = {
      marked: true,
      disabled: true,
      disableTouchEvent: true,
      customStyles: {
        container: {
          backgroundColor: '#F1F5F9',
          borderRadius: 16,
          borderWidth: 1,
          borderColor: '#CBD5E1',
        },
        text: { color: '#94A3B8', fontWeight: '700' },
      },
    };

    return marks;
  }, [unavailableDates, today]);

  const handleCreateBooking = async () => {
    if (!checkIn || !checkOut) {
      Alert.alert('Error', 'Please select check-in and check-out dates');
      return;
    }

    if (!propertyId) {
      Alert.alert('Error', 'Property ID is missing');
      return;
    }

    const guestCount = parseInt(guests);
    if (isNaN(guestCount) || guestCount < 1) {
      Alert.alert('Error', 'Please enter a valid number of guests');
      return;
    }

    const continueToConfirm = () => {
      router.push({
        pathname: '/booking/confirm',
        params: {
          propertyId,
          checkIn,
          checkOut,
          guests: guestCount.toString(),
        },
      });
    };

    if (isHostMode(user)) {
      promptSwitchToTravelMode({
        onCancel: () => router.back(),
        onConfirm: async () => {
          try {
            await switchProfile('guest');
            continueToConfirm();
          } catch (error: any) {
            Alert.alert(
              'Unable to Switch Mode',
              error?.response?.data?.error || error?.message || 'Failed to switch to travel mode.'
            );
          }
        },
      });
      return;
    }

    continueToConfirm();
  };

  const handleCheckInDateSelect = (date: string) => {
    if (date <= today || unavailableDates.includes(date)) return; // Block current/past and unavailable dates
    setCheckIn(date);
    setShowCheckInCalendar(false);
    // Clear check-out if it's before or equal to the new check-in
    if (checkOut && date >= checkOut) {
      setCheckOut('');
    }
  };

  const handleCheckOutDateSelect = (date: string) => {
    if (date <= today || unavailableDates.includes(date)) return; // Block current/past and unavailable dates
    if (!checkIn || date <= checkIn) {
      Alert.alert('Invalid Date', 'Check-out must be after check-in.');
      return;
    }
    // Check if any unavailable date falls between check-in and check-out
    const hasConflict = unavailableDates.some(d => d > checkIn && d < date);
    if (hasConflict) {
      Alert.alert('Unavailable Dates', 'Some dates in your selected range are already booked. Please choose different dates.');
      return;
    }
    setCheckOut(date);
    setShowCheckOutCalendar(false);
  };

  const formatDate = (dateStr: string) => {
    if (!dateStr) return '';
    try {
      // Parse ISO date string safely and format it
      const date = parseISO(dateStr);
      return format(date, 'MMM dd, yyyy');
    } catch (error) {
      return dateStr;
    }
  };

  const getMinCheckOutDate = () => {
    if (!checkIn) return tomorrow;
    try {
      // Use date-fns to add 1 day safely
      const minDate = addDays(parseISO(checkIn), 1);
      return format(minDate, 'yyyy-MM-dd');
    } catch (error) {
      return tomorrow;
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
        <TouchableOpacity
          onPress={() => setShowCheckInCalendar(true)}
          className="bg-white rounded-2xl p-4 mb-4 flex-row items-center"
          style={{
            shadowColor: '#122F26',
            shadowOffset: { width: 0, height: 2 },
            shadowOpacity: 0.05,
            shadowRadius: 4,
            elevation: 2,
          }}
        >
          <Ionicons name="calendar" size={24} color="#3A5C50" />
          <Text className={`flex-1 ml-3 text-base ${checkIn ? 'text-forest' : 'text-slate-400'}`}>
            {checkIn ? formatDate(checkIn) : 'Select check-in date'}
          </Text>
        </TouchableOpacity>

        {/* Check-out Date */}
        <Text className="text-base font-semibold text-forest mb-2">Check-out Date</Text>
        <TouchableOpacity
          onPress={() => setShowCheckOutCalendar(true)}
          disabled={!checkIn}
          className="bg-white rounded-2xl p-4 mb-4 flex-row items-center"
          style={{
            shadowColor: '#122F26',
            shadowOffset: { width: 0, height: 2 },
            shadowOpacity: 0.05,
            shadowRadius: 4,
            elevation: 2,
            opacity: !checkIn ? 0.5 : 1,
          }}
        >
          <Ionicons name="calendar" size={24} color="#3A5C50" />
          <Text className={`flex-1 ml-3 text-base ${checkOut ? 'text-forest' : 'text-slate-400'}`}>
            {checkOut ? formatDate(checkOut) : 'Select check-out date'}
          </Text>
        </TouchableOpacity>

        {/* Guests */}
        <Text className="text-base font-semibold text-forest mb-2">Number of Guests</Text>
        <View className="bg-white rounded-2xl p-4 mb-4 flex-row items-center" style={{
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

        {/* Continue Button */}
        <TouchableOpacity
          onPress={handleCreateBooking}
          disabled={!checkIn || !checkOut}
        >
          <LinearGradient
            colors={(!checkIn || !checkOut) ? ['#94a3b8', '#94a3b8'] : ['#D9B168', '#bea04f']}
            className="py-4 rounded-2xl items-center"
          >
            <Text className="text-forest font-bold text-base">
              Continue
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
            You won't be charged yet. You'll review the price breakdown on the next page before confirming.
          </Text>
        </View>
      </View>
        </ScrollView>
      </TouchableWithoutFeedback>

      {/* Check-in Calendar Modal */}
      <Modal
        visible={showCheckInCalendar}
        animationType="slide"
        transparent={true}
        onRequestClose={() => setShowCheckInCalendar(false)}
      >
        <View className="flex-1 justify-center items-center bg-black/50">
          <View className="bg-white rounded-3xl p-4 mx-4 w-full max-w-md">
            <View className="flex-row justify-between items-center mb-4">
              <Text className="text-xl font-bold text-forest">Select Check-in Date</Text>
              <TouchableOpacity onPress={() => setShowCheckInCalendar(false)}>
                <Ionicons name="close" size={24} color="#122F26" />
              </TouchableOpacity>
            </View>
            {unavailableDates.length > 0 && (
              <View className="flex-row items-center mb-2 px-1">
                <View style={{ width: 12, height: 12, borderRadius: 6, backgroundColor: '#FEE2E2', borderWidth: 1, borderColor: '#FCA5A5', marginRight: 6 }} />
                <Text className="text-xs text-slate-500">Already booked</Text>
              </View>
            )}
            <Calendar
              markingType="custom"
              minDate={tomorrow}
              onDayPress={(day) => handleCheckInDateSelect(day.dateString)}
              markedDates={{
                ...disabledMarkedDates,
                ...(checkIn ? { [checkIn]: { customStyles: { container: { backgroundColor: '#D9B168', borderRadius: 16 }, text: { color: '#FFFFFF', fontWeight: 'bold' } } } } : {}),
              }}
              theme={{
                todayTextColor: '#D9B168',
                arrowColor: '#D9B168',
              }}
            />
          </View>
        </View>
      </Modal>

      {/* Check-out Calendar Modal */}
      <Modal
        visible={showCheckOutCalendar}
        animationType="slide"
        transparent={true}
        onRequestClose={() => setShowCheckOutCalendar(false)}
      >
        <View className="flex-1 justify-center items-center bg-black/50">
          <View className="bg-white rounded-3xl p-4 mx-4 w-full max-w-md">
            <View className="flex-row justify-between items-center mb-4">
              <Text className="text-xl font-bold text-forest">Select Check-out Date</Text>
              <TouchableOpacity onPress={() => setShowCheckOutCalendar(false)}>
                <Ionicons name="close" size={24} color="#122F26" />
              </TouchableOpacity>
            </View>
            {unavailableDates.length > 0 && (
              <View className="flex-row items-center mb-2 px-1">
                <View style={{ width: 12, height: 12, borderRadius: 6, backgroundColor: '#FEE2E2', borderWidth: 1, borderColor: '#FCA5A5', marginRight: 6 }} />
                <Text className="text-xs text-slate-500">Already booked</Text>
              </View>
            )}
            <Calendar
              markingType="custom"
              minDate={getMinCheckOutDate()}
              onDayPress={(day) => handleCheckOutDateSelect(day.dateString)}
              markedDates={{
                ...disabledMarkedDates,
                ...(checkOut ? { [checkOut]: { customStyles: { container: { backgroundColor: '#D9B168', borderRadius: 16 }, text: { color: '#FFFFFF', fontWeight: 'bold' } } } } : {}),
                ...(checkIn ? { [checkIn]: { customStyles: { container: { backgroundColor: '#10B981', borderRadius: 16 }, text: { color: '#FFFFFF', fontWeight: 'bold' } } } } : {}),
              }}
              theme={{
                todayTextColor: '#D9B168',
                arrowColor: '#D9B168',
              }}
            />
          </View>
        </View>
      </Modal>
    </KeyboardAvoidingView>
  );
}
