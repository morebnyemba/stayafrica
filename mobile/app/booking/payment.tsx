import React from 'react';
import { View, Text, ScrollView, TouchableOpacity, Alert, ActivityIndicator } from 'react-native';
import { useState, useEffect } from 'react';
import { useFocusEffect } from '@react-navigation/native';
import { useRouter, useLocalSearchParams } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { useAuth } from '@/context/auth-context';
import { apiClient } from '@/services/api-client';

interface PaymentMethod {
  id: string;
  method_type: 'card' | 'mobile' | 'bank' | 'ussd';
  provider: string;
  name: string;
  last_four?: string;
  is_default: boolean;
}

interface PaymentResponse {
  status: 'pending' | 'completed' | 'failed';
  transaction_id: string;
  checkout_url?: string;
}

export default function PaymentScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const { isAuthenticated } = useAuth();
  const params = useLocalSearchParams();
  
  const [processing, setProcessing] = useState(false);
  const [loading, setLoading] = useState(true);
  const [paymentMethods, setPaymentMethods] = useState<PaymentMethod[]>([]);
  const [selectedMethodId, setSelectedMethodId] = useState<string | null>(null);
  
  const total = parseFloat(params.total as string || '0');
  const bookingId = params.bookingId as string;
  const propertyName = params.propertyName as string;
  const checkIn = params.checkIn as string;
  const checkOut = params.checkOut as string;
  const guests = params.guests as string;

  // Fetch payment methods when screen is focused
  useFocusEffect(
    React.useCallback(() => {
      if (isAuthenticated) {
        fetchPaymentMethods();
      }
    }, [isAuthenticated])
  );

  const fetchPaymentMethods = async () => {
    try {
      setLoading(true);
      const response = await apiClient.get('/payments/payment-methods/');
      const methods = response.data.results || [];
      setPaymentMethods(methods);
      
      // Auto-select default method
      const defaultMethod = methods.find((m: PaymentMethod) => m.is_default);
      if (defaultMethod) {
        setSelectedMethodId(defaultMethod.id);
      }
    } catch (error) {
      console.error('Error fetching payment methods:', error);
      Alert.alert('Error', 'Failed to load payment methods');
    } finally {
      setLoading(false);
    }
  };

  const handlePayment = async () => {
    if (!selectedMethodId) {
      Alert.alert('Error', 'Please select a payment method');
      return;
    }

    setProcessing(true);
    try {
      const response: PaymentResponse = await apiClient.post(
        `/bookings/${bookingId}/pay/`,
        { payment_method_id: selectedMethodId }
      );

      if (response.status === 'completed') {
        // Payment successful
        Alert.alert('Success', 'Payment processed successfully!', [
          {
            text: 'OK',
            onPress: () => router.replace({
              pathname: '/booking/[id]/success',
              params: { id: bookingId }
            })
          }
        ]);
      } else if (response.checkout_url) {
        // Redirect to payment provider
        Alert.alert('Redirecting', 'You will be redirected to complete the payment', [
          { text: 'OK' }
        ]);
        // In production: use linking.openURL(response.checkout_url)
        router.replace({
          pathname: '/booking/[id]/pending',
          params: { id: bookingId, transactionId: response.transaction_id }
        });
      } else {
        // Payment pending webhook confirmation
        router.replace({
          pathname: '/booking/[id]/pending',
          params: { id: bookingId, transactionId: response.transaction_id }
        });
      }
    } catch (error) {
      console.error('Payment error:', error);
      Alert.alert('Payment Failed', 'Unable to process payment. Please try again.');
    } finally {
      setProcessing(false);
    }
  };

  const handleAddPaymentMethod = () => {
    router.push('/wallet/add-payment-method');
  };

  if (!isAuthenticated) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100">
        <View className="flex-1 items-center justify-center">
          <Text className="text-forest text-lg font-semibold">Please sign in to continue</Text>
        </View>
      </SafeAreaView>
    );
  }

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f']}
        className="px-4 pb-6"
        style={{ paddingTop: insets.top + 12 }}
      >
        <View className="flex-row items-center mb-4">
          <TouchableOpacity
            onPress={() => router.back()}
            disabled={processing}
            className="w-10 h-10 rounded-xl items-center justify-center mr-3"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </TouchableOpacity>
          <View className="flex-1">
            <Text className="text-2xl font-bold text-white">Complete Payment</Text>
            <Text className="text-sand-200 text-xs mt-1">Confirm booking details</Text>
          </View>
        </View>
      </LinearGradient>

      {loading ? (
        <View className="flex-1 items-center justify-center">
          <ActivityIndicator size="large" color="#D9B168" />
          <Text className="text-moss font-semibold mt-4">Loading payment methods...</Text>
        </View>
      ) : (
        <ScrollView showsVerticalScrollIndicator={false} className="flex-1">
          {/* Booking Summary */}
          <View className="px-4 mt-4">
            <Text className="text-lg font-bold text-forest mb-3">Booking Summary</Text>
            <View
              className="bg-white rounded-2xl p-5 mb-4"
              style={{
                shadowColor: '#122F26',
                shadowOffset: { width: 0, height: 2 },
                shadowOpacity: 0.05,
                shadowRadius: 4,
                elevation: 2,
              }}
            >
              <View className="mb-3">
                <Text className="text-xs text-moss font-semibold mb-1">PROPERTY</Text>
                <Text className="text-base font-bold text-forest">{propertyName}</Text>
              </View>
              
              <View className="flex-row mb-3">
                <View className="flex-1">
                  <Text className="text-xs text-moss font-semibold mb-1">CHECK-IN</Text>
                  <Text className="text-base font-bold text-forest">{checkIn}</Text>
                </View>
                <View className="flex-1 ml-4">
                  <Text className="text-xs text-moss font-semibold mb-1">CHECK-OUT</Text>
                  <Text className="text-base font-bold text-forest">{checkOut}</Text>
                </View>
              </View>

              <View>
                <Text className="text-xs text-moss font-semibold mb-1">GUESTS</Text>
                <Text className="text-base font-bold text-forest">{guests} Guest{guests !== '1' ? 's' : ''}</Text>
              </View>
            </View>
          </View>

          {/* Total Amount */}
          <View className="px-4">
            <View className="bg-gradient-to-r from-gold/20 to-gold/10 rounded-2xl p-5 mb-6">
              <Text className="text-moss text-xs font-semibold mb-2">TOTAL AMOUNT</Text>
              <Text className="text-4xl font-black text-forest">
                ${total.toFixed(2)}
              </Text>
            </View>
          </View>

          {/* Payment Methods */}
          <View className="px-4 mb-6">
            <View className="flex-row items-center justify-between mb-4">
              <Text className="text-lg font-bold text-forest">Payment Method</Text>
              <TouchableOpacity
                onPress={handleAddPaymentMethod}
                className="flex-row items-center px-3 py-2 bg-gold/20 rounded-lg"
              >
                <Ionicons name="add" size={16} color="#D9B168" />
                <Text className="text-gold font-semibold text-xs ml-1">Add</Text>
              </TouchableOpacity>
            </View>

            {paymentMethods.length > 0 ? (
              <View>
                {paymentMethods.map((method) => (
                  <TouchableOpacity
                    key={method.id}
                    onPress={() => setSelectedMethodId(method.id)}
                    disabled={processing}
                    className={`mb-3 rounded-2xl p-4 flex-row items-center border-2 ${
                      selectedMethodId === method.id
                        ? 'bg-gold/20 border-gold'
                        : 'bg-white border-sand-200'
                    }`}
                  >
                    <View
                      className={`w-12 h-12 rounded-xl items-center justify-center mr-3 ${
                        selectedMethodId === method.id ? 'bg-gold' : 'bg-sand-100'
                      }`}
                    >
                      <Ionicons
                        name={
                          method.type === 'card'
                            ? 'card'
                            : method.type === 'mobile'
                            ? 'phone-portrait'
                            : 'business'
                        }
                        size={20}
                        color={selectedMethodId === method.id ? '#122F26' : '#3A5C50'}
                      />
                    </View>

                    <View className="flex-1">
                      <Text
                        className={`font-semibold ${
                          selectedMethodId === method.id ? 'text-gold' : 'text-forest'
                        }`}
                      >
                        {method.name}
                      </Text>
                      {method.last_four && (
                        <Text className="text-xs text-moss mt-1">•••• {method.last_four}</Text>
                      )}
                    </View>

                    {method.is_default && (
                      <View className="bg-gold/20 px-2 py-1 rounded-full">
                        <Text className="text-gold text-xs font-bold">Default</Text>
                      </View>
                    )}

                    {selectedMethodId === method.id && (
                      <Ionicons name="checkmark-circle" size={24} color="#D9B168" />
                    )}
                  </TouchableOpacity>
                ))}
              </View>
            ) : (
              <View className="bg-blue-50 rounded-2xl p-5 items-center">
                <Ionicons name="alert-circle" size={32} color="#3B82F6" />
                <Text className="text-blue-900 font-bold mt-3 mb-2">No Payment Methods</Text>
                <Text className="text-blue-800 text-sm text-center mb-4">
                  Add a payment method to complete your booking
                </Text>
                <TouchableOpacity onPress={handleAddPaymentMethod}>
                  <LinearGradient
                    colors={['#3B82F6', '#2563EB']}
                    className="px-6 py-2 rounded-xl"
                  >
                    <Text className="text-white font-bold">Add Payment Method</Text>
                  </LinearGradient>
                </TouchableOpacity>
              </View>
            )}
          </View>

          {/* Security Info */}
          <View className="px-4 mb-6">
            <View className="bg-green-50 rounded-2xl p-4 flex-row items-start">
              <Ionicons name="shield-checkmark" size={20} color="#10B981" />
              <View className="flex-1 ml-3">
                <Text className="text-green-900 font-semibold text-sm">Secure Payment</Text>
                <Text className="text-green-800 text-xs mt-1">
                  PCI DSS compliant. Your payment data is encrypted.
                </Text>
              </View>
            </View>
          </View>
        </ScrollView>
      )}

      {/* Footer Action */}
      {!loading && (
        <View className="px-4 pb-6 pt-4 border-t border-sand-200">
          <TouchableOpacity
            onPress={handlePayment}
            disabled={!selectedMethodId || processing || paymentMethods.length === 0}
          >
            <LinearGradient
              colors={
                !selectedMethodId || processing || paymentMethods.length === 0
                  ? ['#cbd5e1', '#cbd5e1']
                  : ['#D9B168', '#bea04f']
              }
              className="py-4 rounded-2xl items-center"
            >
              {processing ? (
                <View className="flex-row items-center">
                  <ActivityIndicator color="#122F26" />
                  <Text className="text-forest font-bold ml-2">Processing...</Text>
                </View>
              ) : (
                <View className="flex-row items-center">
                  <Ionicons name="shield-checkmark" size={20} color="#122F26" />
                  <Text className="text-forest font-bold text-base ml-2">
                    Pay ${total.toFixed(2)}
                  </Text>
                </View>
              )}
            </LinearGradient>
          </TouchableOpacity>
        </View>
      )}
    </SafeAreaView>
  );
}
