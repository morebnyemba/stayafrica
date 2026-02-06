import { View, Text, ScrollView, TouchableOpacity, Platform, Alert, ActivityIndicator } from 'react-native';
import { useState, useEffect } from 'react';
import { useRouter, useFocusEffect } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { apiClient } from '@/services/api-client';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { useCallback } from 'react';

interface PaymentMethod {
  id: string;
  method_type: 'card' | 'mobile' | 'bank' | 'ussd';
  provider: 'stripe' | 'paynow' | 'flutterwave' | 'paystack';
  name: string;
  last_four?: string;
  is_default: boolean;
  created_at?: string;
}

export default function PaymentMethodsScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const [paymentMethods, setPaymentMethods] = useState<PaymentMethod[]>([]);
  const [loading, setLoading] = useState(true);

  useFocusEffect(
    useCallback(() => {
      fetchPaymentMethods();
    }, [])
  );

  const fetchPaymentMethods = async () => {
    try {
      setLoading(true);
      const response = await apiClient.get('/payments/payment-methods/');
      setPaymentMethods(response.data.results || []);
    } catch (error) {
      console.error('Error fetching payment methods:', error);
    } finally {
      setLoading(false);
    }
  };

  const getIcon = (type: string): keyof typeof Ionicons.glyphMap => {
    switch (type) {
      case 'card': return 'card';
      case 'mobile': return 'phone-portrait';
      case 'bank': return 'business';
      default: return 'wallet';
    }
  };

  const getProviderColor = (provider: string): string => {
    switch (provider) {
      case 'stripe': return '#635BFF';
      case 'paynow': return '#FF6B35';
      case 'flutterwave': return '#F10800';
      case 'paystack': return '#0066FF';
      default: return '#3A5C50';
    }
  };

  const handleSetDefault = async (id: string) => {
    try {
      await apiClient.patch(`/payments/payment-methods/${id}/set_default/`, {});
      setPaymentMethods(methods => 
        methods.map(m => ({ ...m, is_default: m.id === id }))
      );
      Alert.alert('Success', 'Default payment method updated');
    } catch (error) {
      console.error('Error setting default:', error);
      Alert.alert('Error', 'Failed to update default payment method');
    }
  };

  const handleDelete = (id: string) => {
    Alert.alert(
      'Delete Payment Method',
      'Are you sure you want to delete this payment method?',
      [
        { text: 'Cancel', style: 'cancel' },
        { 
          text: 'Delete', 
          style: 'destructive',
          onPress: async () => {
            try {
              await apiClient.delete(`/payments/payment-methods/${id}/`);
              setPaymentMethods(methods => methods.filter(m => m.id !== id));
              Alert.alert('Success', 'Payment method deleted');
            } catch (error) {
              console.error('Error deleting:', error);
              Alert.alert('Error', 'Failed to delete payment method');
            }
          }
        },
      ]
    );
  };

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f']}
        className="px-4 pb-6"
        style={{ paddingTop: insets.top + 12 }}
      >
        <View className="flex-row items-center justify-between">
          <View className="flex-row items-center flex-1">
            <TouchableOpacity
              onPress={() => router.back()}
              className="w-10 h-10 rounded-xl items-center justify-center mr-3 -ml-2"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="arrow-back" size={24} color="#fff" />
            </TouchableOpacity>
            <View>
              <Text className="text-xl font-bold text-white">Payment Methods</Text>
              <Text className="text-xs text-sand-300 mt-1">{paymentMethods.length} method{paymentMethods.length !== 1 ? 's' : ''}</Text>
            </View>
          </View>
          <TouchableOpacity
            onPress={() => router.push('/wallet/add-payment-method')}
            className="w-10 h-10 rounded-xl items-center justify-center"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="add" size={24} color="#fff" />
          </TouchableOpacity>
        </View>
      </LinearGradient>

      {loading ? (
        <View className="flex-1 items-center justify-center">
          <ActivityIndicator size="large" color="#D9B168" />
        </View>
      ) : (
        <ScrollView className="flex-1" showsVerticalScrollIndicator={false}>
          <View className="px-4 py-6">
            {paymentMethods.length > 0 ? (
              <>
                {paymentMethods.map((method) => (
                  <View
                    key={method.id}
                    className="bg-white rounded-2xl p-4 mb-3"
                    style={{
                      shadowColor: '#122F26',
                      shadowOffset: { width: 0, height: 2 },
                      shadowOpacity: 0.05,
                      shadowRadius: 4,
                      elevation: 2,
                    }}
                  >
                    <View className="flex-row items-center mb-3">
                      <View 
                        className="w-12 h-12 rounded-xl items-center justify-center mr-3"
                        style={{ backgroundColor: `${getProviderColor(method.provider)}20` }}
                      >
                        <Ionicons name={getIcon(method.method_type)} size={24} color={getProviderColor(method.provider)} />
                      </View>
                      <View className="flex-1">
                        <Text className="text-base font-semibold text-forest">{method.name}</Text>
                        {method.last_four && (
                          <Text className="text-sm text-moss">•••• {method.last_four}</Text>
                        )}
                        <Text className="text-xs text-gold mt-1 font-semibold uppercase">{method.provider}</Text>
                      </View>
                      {method.is_default && (
                        <View className="bg-gold/20 px-3 py-1 rounded-full">
                          <Text className="text-gold text-xs font-semibold">Default</Text>
                        </View>
                      )}
                    </View>
                    <View className="flex-row pt-3 border-t border-sand-100">
                      {!method.is_default && (
                        <TouchableOpacity 
                          className="flex-1 items-center py-2"
                          onPress={() => handleSetDefault(method.id)}
                        >
                          <Text className="text-gold font-semibold text-sm">Set as Default</Text>
                        </TouchableOpacity>
                      )}
                      <TouchableOpacity 
                        className={`flex-1 items-center py-2 ${!method.is_default ? 'border-l border-sand-100' : ''}`}
                        onPress={() => handleDelete(method.id)}
                      >
                        <Text className="text-red-500 font-semibold text-sm">Remove</Text>
                      </TouchableOpacity>
                    </View>
                  </View>
                ))}
              </>
            ) : (
              <View className="items-center py-16">
                <View className="bg-sand-200 rounded-full p-6 mb-4">
                  <Ionicons name="card-outline" size={48} color="#94a3b8" />
                </View>
                <Text className="text-xl font-bold text-forest mb-2">No Payment Methods</Text>
                <Text className="text-moss text-center mb-8 px-8 leading-5">
                  Add a payment method to make bookings and transactions easier
                </Text>
              </View>
            )}

            {/* Add New Method Button */}
            <TouchableOpacity 
              className="mt-4"
              onPress={() => router.push('/wallet/add-payment-method')}
            >
              <LinearGradient
                colors={['#D9B168', '#bea04f']}
                className="py-4 rounded-2xl flex-row items-center justify-center"
              >
                <Ionicons name="add" size={24} color="#122F26" />
                <Text className="text-forest font-bold text-base ml-2">Add Payment Method</Text>
              </LinearGradient>
            </TouchableOpacity>

            {/* Security Info */}
            {paymentMethods.length > 0 && (
              <View className="mt-6 bg-blue-50 rounded-2xl p-4">
                <View className="flex-row items-start">
                  <Ionicons name="shield-checkmark" size={20} color="#3B82F6" />
                  <View className="flex-1 ml-3">
                    <Text className="text-sm font-semibold text-blue-900 mb-1">Secure Payment</Text>
                    <Text className="text-xs text-blue-800">
                      Payment information is encrypted and secured. We never store full card details.
                    </Text>
                  </View>
                </View>
              </View>
            )}

            <View className="h-6" />
          </View>
        </ScrollView>
      )}
    </SafeAreaView>
  );
}

PaymentMethodsScreen.displayName = 'PaymentMethodsScreen';
