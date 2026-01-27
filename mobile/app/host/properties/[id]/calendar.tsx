import { View, Text, ScrollView, TouchableOpacity, Platform, ActivityIndicator } from 'react-native';
import { useState } from 'react';
import { useRouter, useLocalSearchParams } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { Calendar } from 'react-native-calendars';

export default function PropertyCalendarScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const params = useLocalSearchParams();
  
  const propertyId = params.id as string;
  const [selectedDate, setSelectedDate] = useState('');

  // Fetch property details
  const { data: property, isLoading: loadingProperty } = useQuery({
    queryKey: ['property', propertyId],
    queryFn: async () => {
      const response = await apiClient.get(`/properties/${propertyId}/`);
      return response.data;
    },
    enabled: !!propertyId,
  });

  // Fetch bookings for this property
  const { data: bookingsData, isLoading: loadingBookings } = useQuery({
    queryKey: ['property-bookings', propertyId],
    queryFn: async () => {
      const response = await apiClient.get(`/bookings/?property=${propertyId}`);
      return response.data;
    },
    enabled: !!propertyId,
  });

  const bookings = bookingsData?.results || [];

  // Build marked dates object for calendar
  const markedDates: any = {};
  bookings.forEach((booking: any) => {
    if (booking.check_in && booking.check_out) {
      const start = new Date(booking.check_in);
      const end = new Date(booking.check_out);
      const current = new Date(start);

      while (current < end) {
        const dateStr = current.toISOString().split('T')[0];
        markedDates[dateStr] = {
          marked: true,
          dotColor: booking.status === 'confirmed' ? '#10B981' : '#F59E0B',
          selected: selectedDate === dateStr,
          selectedColor: selectedDate === dateStr ? '#D9B168' : undefined,
        };
        current.setDate(current.getDate() + 1);
      }
    }
  });

  if (!isAuthenticated) {
    return (
      <View className="flex-1 bg-sand-100 items-center justify-center">
        <Text className="text-forest text-lg">Please sign in to view calendar</Text>
      </View>
    );
  }

  if (loadingProperty || loadingBookings) {
    return (
      <View className="flex-1 bg-sand-100 items-center justify-center">
        <ActivityIndicator size="large" color="#D9B168" />
        <Text className="mt-4 text-moss">Loading calendar...</Text>
      </View>
    );
  }

  return (
    <ScrollView 
      className="flex-1 bg-sand-100" 
      showsVerticalScrollIndicator={false}
      contentContainerStyle={{ paddingBottom: 40 }}
    >
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
        <Text className="text-2xl font-black text-white tracking-tight">
          {property?.title}
        </Text>
        <Text className="text-sand-200 text-sm mt-1">
          Booking Calendar
        </Text>
      </LinearGradient>

      <View className="px-4 -mt-4">
        {/* Calendar Card */}
        <View className="bg-white rounded-2xl p-4 mb-4" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 4 },
          shadowOpacity: 0.08,
          shadowRadius: 8,
          elevation: 4,
        }}>
          <Calendar
            markedDates={markedDates}
            onDayPress={(day: any) => setSelectedDate(day.dateString)}
            theme={{
              backgroundColor: '#ffffff',
              calendarBackground: '#ffffff',
              textSectionTitleColor: '#122F26',
              selectedDayBackgroundColor: '#D9B168',
              selectedDayTextColor: '#ffffff',
              todayTextColor: '#D9B168',
              dayTextColor: '#122F26',
              textDisabledColor: '#d9e1e8',
              dotColor: '#D9B168',
              selectedDotColor: '#ffffff',
              arrowColor: '#D9B168',
              monthTextColor: '#122F26',
              indicatorColor: '#D9B168',
              textDayFontFamily: 'System',
              textMonthFontFamily: 'System',
              textDayHeaderFontFamily: 'System',
              textDayFontWeight: '400',
              textMonthFontWeight: 'bold',
              textDayHeaderFontWeight: '400',
              textDayFontSize: 14,
              textMonthFontSize: 16,
              textDayHeaderFontSize: 12,
            }}
          />
        </View>

        {/* Legend */}
        <View className="bg-white rounded-2xl p-5 mb-4" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 4 },
          shadowOpacity: 0.08,
          shadowRadius: 8,
          elevation: 4,
        }}>
          <Text className="text-lg font-bold text-forest mb-3">Legend</Text>
          <View className="space-y-2">
            <View className="flex-row items-center">
              <View className="w-4 h-4 rounded-full bg-green-500 mr-3" />
              <Text className="text-moss">Confirmed Booking</Text>
            </View>
            <View className="flex-row items-center">
              <View className="w-4 h-4 rounded-full bg-amber-500 mr-3" />
              <Text className="text-moss">Pending Booking</Text>
            </View>
            <View className="flex-row items-center">
              <View className="w-4 h-4 rounded-full bg-gold mr-3" />
              <Text className="text-moss">Selected Date</Text>
            </View>
          </View>
        </View>

        {/* Upcoming Bookings */}
        <View className="bg-white rounded-2xl p-5" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 4 },
          shadowOpacity: 0.08,
          shadowRadius: 8,
          elevation: 4,
        }}>
          <Text className="text-lg font-bold text-forest mb-3">Upcoming Bookings</Text>
          {bookings.length > 0 ? (
            <View className="space-y-3">
              {bookings.slice(0, 5).map((booking: any) => (
                <View key={booking.id} className="p-3 bg-sand-50 rounded-xl">
                  <View className="flex-row justify-between items-start mb-1">
                    <Text className="font-semibold text-forest flex-1">
                      {booking.guest?.first_name} {booking.guest?.last_name}
                    </Text>
                    <View className={`px-2 py-1 rounded-full ${
                      booking.status === 'confirmed' ? 'bg-green-100' : 'bg-amber-100'
                    }`}>
                      <Text className={`text-xs font-semibold ${
                        booking.status === 'confirmed' ? 'text-green-800' : 'text-amber-800'
                      }`}>
                        {booking.status}
                      </Text>
                    </View>
                  </View>
                  <View className="flex-row items-center">
                    <Ionicons name="calendar" size={14} color="#3A5C50" />
                    <Text className="text-sm text-moss ml-1">
                      {new Date(booking.check_in).toLocaleDateString()} - {new Date(booking.check_out).toLocaleDateString()}
                    </Text>
                  </View>
                </View>
              ))}
            </View>
          ) : (
            <View className="py-8 items-center">
              <Ionicons name="calendar-outline" size={48} color="#cbd5e1" />
              <Text className="text-moss mt-2 text-center">No upcoming bookings</Text>
            </View>
          )}
        </View>
      </View>
    </ScrollView>
  );
}
