import { View, Text, ScrollView, TouchableOpacity, Platform, ActivityIndicator } from 'react-native';
import { useState } from 'react';
import { useRouter, useLocalSearchParams } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';

export default function PaymentScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const params = useLocalSearchParams();
  
  const [processing, setProcessing] = useState(false);
  
  const total = parseFloat(params.total as string || '0');
  const provider = params.provider as string;
  const propertyId = params.propertyId as string;
  const checkIn = params.checkIn as string;
  const checkOut = params.checkOut as string;
  const guests = params.guests as string;

  const handlePayment = async () => {
    setProcessing(true);
    
    // Simulate payment processing
    setTimeout(() => {
      // Navigate to success page
      router.replace({
        pathname: '/booking/success',
        params: { propertyId, checkIn, checkOut, guests },
      });
    }, 2000);
  };

  if (!isAuthenticated) {
    return (
      <View className="flex-1 bg-sand-100 items-center justify-center">
        <Text className="text-forest text-lg">Please sign in to continue</Text>
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
        <TouchableOpacity onPress={() => router.back()} className="mb-4" disabled={processing}>
          <View className="w-10 h-10 rounded-xl items-center justify-center" style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}>
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </View>
        </TouchableOpacity>
        <Text className="text-3xl font-black text-white tracking-tight">
          Payment
        </Text>
        <Text className="text-sand-200 text-sm mt-1">
          Complete your booking
        </Text>
      </LinearGradient>

      <View className="px-4 -mt-4">
        {/* Payment Card */}
        <View className="bg-white rounded-2xl p-6 mb-4" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 4 },
          shadowOpacity: 0.08,
          shadowRadius: 8,
          elevation: 4,
        }}>
          <Text className="text-lg font-bold text-forest mb-4">Payment Details</Text>
          
          <View className="mb-6">
            <Text className="text-sm text-moss mb-2">Selected Payment Method</Text>
            <View className="bg-sand-100 rounded-xl p-4">
              <Text className="font-semibold text-forest capitalize">{provider?.replace('-', ' ')}</Text>
            </View>
          </View>

          <View className="mb-6">
            <Text className="text-sm text-moss mb-2">Total Amount</Text>
            <Text className="text-3xl font-black text-forest">${total.toFixed(2)}</Text>
          </View>

          {processing ? (
            <View className="py-8 items-center">
              <ActivityIndicator size="large" color="#D9B168" />
              <Text className="mt-4 text-moss">Processing payment...</Text>
              <Text className="mt-2 text-sm text-moss/70 text-center">
                Please wait while we securely process your payment
              </Text>
            </View>
          ) : (
            <>
              <TouchableOpacity
                onPress={handlePayment}
                className="rounded-2xl overflow-hidden mb-3"
              >
                <LinearGradient
                  colors={['#10B981', '#059669']}
                  className="py-4 items-center"
                  style={{
                    shadowColor: '#10B981',
                    shadowOffset: { width: 0, height: 4 },
                    shadowOpacity: 0.3,
                    shadowRadius: 8,
                    elevation: 5,
                  }}
                >
                  <View className="flex-row items-center">
                    <Ionicons name="shield-checkmark" size={20} color="#fff" />
                    <Text className="text-white font-bold text-base ml-2">Pay Now</Text>
                  </View>
                </LinearGradient>
              </TouchableOpacity>

              <View className="bg-blue-50 rounded-xl p-4 border border-blue-200">
                <View className="flex-row items-start">
                  <Ionicons name="lock-closed" size={16} color="#2563EB" />
                  <Text className="flex-1 text-xs text-blue-800 ml-2 leading-5">
                    Your payment information is encrypted and secure. We use industry-standard security measures to protect your data.
                  </Text>
                </View>
              </View>
            </>
          )}
        </View>
      </View>
    </ScrollView>
  );
}
