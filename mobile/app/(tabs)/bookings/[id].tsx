import { View, Text, ScrollView, TouchableOpacity, Platform, ActivityIndicator } from 'react-native';
import { useLocalSearchParams, useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';

export default function BookingDetailScreen() {
  const router = useRouter();
  const { id } = useLocalSearchParams<{ id: string }>();
  const { isAuthenticated } = useAuth();

  // TODO: Fetch booking details from API
  const booking = null;
  const loading = false;

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
            {/* Property Info */}
            <View className="bg-white rounded-2xl p-4 mb-4" style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 2 },
              shadowOpacity: 0.05,
              shadowRadius: 4,
              elevation: 2,
            }}>
              <Text className="text-lg font-bold text-forest mb-2">Property Details</Text>
              {/* Add property details here */}
            </View>

            {/* Booking Details */}
            <View className="bg-white rounded-2xl p-4 mb-4" style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 2 },
              shadowOpacity: 0.05,
              shadowRadius: 4,
              elevation: 2,
            }}>
              <Text className="text-lg font-bold text-forest mb-2">Booking Details</Text>
              {/* Add booking dates, guests, etc */}
            </View>

            {/* Actions */}
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
