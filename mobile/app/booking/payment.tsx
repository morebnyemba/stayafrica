import { View, Text, ScrollView, TouchableOpacity, Alert, ActivityIndicator, Linking } from 'react-native';
import { useState } from 'react';
import { useRouter, useLocalSearchParams } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { useAuth } from '@/context/auth-context';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';

interface ProviderItem {
  id: string;
  name: string;
  description: string;
  icon: keyof typeof Ionicons.glyphMap;
  category: 'regional' | 'international';
}

const providerIcons: Record<string, keyof typeof Ionicons.glyphMap> = {
  paynow: 'phone-portrait',
  paystack: 'card',
  flutterwave: 'card',
  stripe: 'card',
  paypal: 'logo-paypal',
  cash_on_arrival: 'cash',
  mpesa: 'phone-portrait',
  ozow: 'swap-horizontal',
};

const providerDescriptions: Record<string, string> = {
  paynow: 'EcoCash, Visa, Mastercard',
  paystack: 'Cards & bank transfer',
  flutterwave: 'Cards & mobile money',
  stripe: 'Credit or debit card',
  paypal: 'Pay with PayPal',
  cash_on_arrival: 'Pay cash when you arrive',
  mpesa: 'M-Pesa mobile money',
  ozow: 'Instant EFT',
};

export default function PaymentScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const { isAuthenticated, user } = useAuth();
  const params = useLocalSearchParams();
  
  const [processing, setProcessing] = useState(false);
  const [selectedProvider, setSelectedProvider] = useState<string>('');
  
  const total = parseFloat(params.total as string || '0');
  const bookingId = params.bookingId as string;
  const propertyName = params.propertyName as string;
  const checkIn = params.checkIn as string;
  const checkOut = params.checkOut as string;
  const guests = params.guests as string;

  // Fetch available payment providers
  const { data: providersData, isLoading: loadingProviders } = useQuery({
    queryKey: ['payment-providers', user?.country_of_residence],
    queryFn: () => apiClient.getAvailableProviders(user?.country_of_residence),
    enabled: isAuthenticated,
  });

  const providers: ProviderItem[] = (providersData?.providers || []).map((p: { id: string; name: string; category: 'regional' | 'international' }) => ({
    id: p.id,
    name: p.name,
    description: providerDescriptions[p.id] || p.name,
    icon: providerIcons[p.id] || 'card',
    category: p.category,
  }));

  const regionalProviders = providers.filter(p => p.category === 'regional');
  const internationalProviders = providers.filter(p => p.category === 'international');

  const handlePayment = async () => {
    if (!selectedProvider) {
      Alert.alert('Error', 'Please select a payment method');
      return;
    }

    setProcessing(true);
    try {
      const response = await apiClient.post('/payments/initiate/', {
        booking_id: bookingId,
        provider: selectedProvider,
      });

      if (response.data?.status === 'completed' || response.data?.status === 'success') {
        Alert.alert('Success', 'Payment processed successfully!', [
          {
            text: 'OK',
            onPress: () => router.replace(`/booking/success?bookingId=${bookingId}`)
          }
        ]);
      } else if (response.data?.checkout_url || response.data?.redirect_url || response.data?.payment_link) {
        const paymentUrl = response.data.checkout_url || response.data.redirect_url || response.data.payment_link;
        Alert.alert(
          'Complete Payment',
          'You will be redirected to the payment gateway to complete your payment securely.',
          [
            {
              text: 'Continue',
              onPress: async () => {
                try {
                  await Linking.openURL(paymentUrl);
                } catch (e) {
                  Alert.alert('Error', 'Could not open payment page. Please try again.');
                }
              },
            },
            { text: 'Cancel', style: 'cancel' },
          ]
        );
      } else {
        // Payment initiated, pending webhook confirmation
        Alert.alert('Payment Pending', 'Your payment is being processed. You will be notified once confirmed.', [
          { text: 'OK', onPress: () => router.replace(`/booking/success?bookingId=${bookingId}&pending=true`) }
        ]);
      }
    } catch (error) {
      console.error('Payment error:', error);
      Alert.alert('Payment Failed', 'Unable to process payment. Please try again.', [
        { text: 'OK', onPress: () => router.replace(`/booking/failure?bookingId=${bookingId}`) }
      ]);
    } finally {
      setProcessing(false);
    }
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
            <Text className="text-2xl font-bold text-white">Choose Payment</Text>
            <Text className="text-sand-200 text-xs mt-1">Select a payment method</Text>
          </View>
        </View>
      </LinearGradient>

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
            {propertyName && (
              <View className="mb-3">
                <Text className="text-xs text-moss font-semibold mb-1">PROPERTY</Text>
                <Text className="text-base font-bold text-forest">{propertyName}</Text>
              </View>
            )}
            
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

        {/* Payment Method Selection */}
        <View className="px-4 mb-6">
          <Text className="text-lg font-bold text-forest mb-3">Payment Method</Text>

          {loadingProviders ? (
            <View className="bg-white rounded-2xl p-6 items-center">
              <ActivityIndicator color="#122F26" />
              <Text className="text-moss mt-2 text-sm">Loading payment methods...</Text>
            </View>
          ) : providers.length === 0 ? (
            <View className="bg-white rounded-2xl p-6 items-center">
              <Ionicons name="alert-circle" size={32} color="#D97706" />
              <Text className="text-moss text-center mt-2">No payment providers available for your region.</Text>
            </View>
          ) : (
            <>
              {regionalProviders.length > 0 && (
                <View className="mb-3">
                  <View className="flex-row items-center mb-2">
                    <Ionicons name="location" size={16} color="#5A7A6C" />
                    <Text className="text-sm font-semibold text-moss ml-1">Local Payment Methods</Text>
                  </View>
                  {regionalProviders.map((provider) => (
                    <TouchableOpacity
                      key={provider.id}
                      onPress={() => setSelectedProvider(provider.id)}
                      className={`bg-white rounded-2xl p-4 mb-2 ${
                        selectedProvider === provider.id ? 'border-2 border-gold' : ''
                      }`}
                      style={{
                        shadowColor: '#122F26',
                        shadowOffset: { width: 0, height: 2 },
                        shadowOpacity: 0.05,
                        shadowRadius: 4,
                        elevation: 2,
                      }}
                    >
                      <View className="flex-row justify-between items-center">
                        <View className="flex-row items-center flex-1">
                          <View className={`w-10 h-10 rounded-xl items-center justify-center mr-3 ${
                            selectedProvider === provider.id ? 'bg-gold' : 'bg-sand-100'
                          }`}>
                            <Ionicons name={provider.icon} size={18} color="#122F26" />
                          </View>
                          <View>
                            <Text className="font-semibold text-forest">{provider.name}</Text>
                            <Text className="text-xs text-moss">{provider.description}</Text>
                          </View>
                        </View>
                        {selectedProvider === provider.id && (
                          <Ionicons name="checkmark-circle" size={24} color="#D9B168" />
                        )}
                      </View>
                    </TouchableOpacity>
                  ))}
                </View>
              )}

              {internationalProviders.length > 0 && (
                <View>
                  <View className="flex-row items-center mb-2">
                    <Ionicons name="globe" size={16} color="#5A7A6C" />
                    <Text className="text-sm font-semibold text-moss ml-1">International Payment Methods</Text>
                  </View>
                  {internationalProviders.map((provider) => (
                    <TouchableOpacity
                      key={provider.id}
                      onPress={() => setSelectedProvider(provider.id)}
                      className={`bg-white rounded-2xl p-4 mb-2 ${
                        selectedProvider === provider.id ? 'border-2 border-gold' : ''
                      }`}
                      style={{
                        shadowColor: '#122F26',
                        shadowOffset: { width: 0, height: 2 },
                        shadowOpacity: 0.05,
                        shadowRadius: 4,
                        elevation: 2,
                      }}
                    >
                      <View className="flex-row justify-between items-center">
                        <View className="flex-row items-center flex-1">
                          <View className={`w-10 h-10 rounded-xl items-center justify-center mr-3 ${
                            selectedProvider === provider.id ? 'bg-gold' : 'bg-sand-100'
                          }`}>
                            <Ionicons name={provider.icon} size={18} color="#122F26" />
                          </View>
                          <View>
                            <Text className="font-semibold text-forest">{provider.name}</Text>
                            <Text className="text-xs text-moss">{provider.description}</Text>
                          </View>
                        </View>
                        {selectedProvider === provider.id && (
                          <Ionicons name="checkmark-circle" size={24} color="#D9B168" />
                        )}
                      </View>
                    </TouchableOpacity>
                  ))}
                </View>
              )}
            </>
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

      {/* Footer Action */}
      <View className="px-4 pb-6 pt-4 border-t border-sand-200">
        <TouchableOpacity
          onPress={handlePayment}
          disabled={!selectedProvider || processing}
        >
          <LinearGradient
            colors={
              !selectedProvider || processing
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
    </SafeAreaView>
  );
}
