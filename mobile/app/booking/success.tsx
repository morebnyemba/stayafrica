import { View, Text, ScrollView, TouchableOpacity, Platform } from 'react-native';
import { useRouter, useLocalSearchParams } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';

export default function BookingSuccessScreen() {
  const router = useRouter();
  const params = useLocalSearchParams();

  const handleViewBookings = () => {
    router.replace('/(tabs)/bookings');
  };

  const handleBackToHome = () => {
    router.replace('/(tabs)/explore');
  };

  return (
    <ScrollView 
      className="flex-1 bg-sand-100" 
      showsVerticalScrollIndicator={false}
      contentContainerStyle={{ paddingBottom: 40 }}
    >
      <LinearGradient
        colors={['#10B981', '#059669']}
        className="px-4 pb-12"
        style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
      >
        <Text className="text-3xl font-black text-white tracking-tight text-center mt-8">
          Booking Confirmed!
        </Text>
        <Text className="text-white/80 text-center mt-2">
          Your payment was successful
        </Text>
      </LinearGradient>

      <View className="px-4 -mt-8">
        {/* Success Card */}
        <View className="bg-white rounded-2xl p-8 mb-4 items-center" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 8 },
          shadowOpacity: 0.1,
          shadowRadius: 16,
          elevation: 8,
        }}>
          <View className="bg-green-100 rounded-full p-6 mb-6">
            <Ionicons name="checkmark-circle" size={80} color="#10B981" />
          </View>

          <Text className="text-2xl font-bold text-forest mb-2 text-center">
            Payment Successful
          </Text>
          <Text className="text-moss text-center mb-6">
            Your booking has been confirmed. You'll receive a confirmation email shortly.
          </Text>

          <View className="w-full space-y-3 mb-6">
            <View className="flex-row items-center justify-between py-3 border-b border-sand-200">
              <Text className="text-moss">Booking Reference</Text>
              <Text className="font-semibold text-forest">#{Math.random().toString(36).substr(2, 9).toUpperCase()}</Text>
            </View>
            <View className="flex-row items-center justify-between py-3 border-b border-sand-200">
              <Text className="text-moss">Status</Text>
              <View className="bg-green-100 px-3 py-1 rounded-full">
                <Text className="text-green-800 font-semibold text-xs">Confirmed</Text>
              </View>
            </View>
          </View>

          {/* Action Buttons */}
          <TouchableOpacity
            onPress={handleViewBookings}
            className="w-full rounded-2xl overflow-hidden mb-3"
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
                <Ionicons name="list" size={20} color="#122F26" />
                <Text className="text-forest font-bold text-base ml-2">View My Bookings</Text>
              </View>
            </LinearGradient>
          </TouchableOpacity>

          <TouchableOpacity
            onPress={handleBackToHome}
            className="w-full"
          >
            <View className="py-3 items-center">
              <Text className="text-moss font-semibold">Back to Home</Text>
            </View>
          </TouchableOpacity>
        </View>

        {/* What's Next Card */}
        <View className="bg-white rounded-2xl p-6" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 4 },
          shadowOpacity: 0.08,
          shadowRadius: 8,
          elevation: 4,
        }}>
          <Text className="text-lg font-bold text-forest mb-4">What's Next?</Text>
          <View className="space-y-4">
            <View className="flex-row items-start">
              <View className="bg-gold/20 rounded-full p-2 mr-3">
                <Ionicons name="mail" size={20} color="#D9B168" />
              </View>
              <View className="flex-1">
                <Text className="font-semibold text-forest mb-1">Check Your Email</Text>
                <Text className="text-sm text-moss">
                  We've sent confirmation details and check-in instructions
                </Text>
              </View>
            </View>
            <View className="flex-row items-start">
              <View className="bg-gold/20 rounded-full p-2 mr-3">
                <Ionicons name="chatbubble-ellipses" size={20} color="#D9B168" />
              </View>
              <View className="flex-1">
                <Text className="font-semibold text-forest mb-1">Message Your Host</Text>
                <Text className="text-sm text-moss">
                  Coordinate check-in time and ask any questions
                </Text>
              </View>
            </View>
            <View className="flex-row items-start">
              <View className="bg-gold/20 rounded-full p-2 mr-3">
                <Ionicons name="calendar" size={20} color="#D9B168" />
              </View>
              <View className="flex-1">
                <Text className="font-semibold text-forest mb-1">Prepare for Your Trip</Text>
                <Text className="text-sm text-moss">
                  Review property details and local recommendations
                </Text>
              </View>
            </View>
          </View>
        </View>
      </View>
    </ScrollView>
  );
}
