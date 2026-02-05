import React, { useState, useEffect } from 'react';
import {
  View,
  Text,
  ScrollView,
  TouchableOpacity,
  ActivityIndicator,
  RefreshControl,
  Alert,
} from 'react-native';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { router } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { apiClient } from '@/services/api-client';
import { useAuth } from '@/context/auth-context';

interface Payment {
  id: string;
  booking: string;
  amount: number;
  currency: string;
  provider: string;
  status: string;
  created_at: string;
  gateway_ref: string;
}

interface PaymentMethod {
  id: string;
  type: string;
  last4?: string;
  brand?: string;
  is_default: boolean;
}

export default function PaymentsScreen() {
  const { user } = useAuth();
  const insets = useSafeAreaInsets();
  const [payments, setPayments] = useState<Payment[]>([]);
  const [paymentMethods, setPaymentMethods] = useState<PaymentMethod[]>([]);
  const [loading, setLoading] = useState(true);
  const [refreshing, setRefreshing] = useState(false);
  const [activeTab, setActiveTab] = useState<'history' | 'methods'>('history');

  useEffect(() => {
    fetchData();
  }, []);

  const fetchData = async () => {
    try {
      // For now, use mock data since payment endpoints may not be ready
      // TODO: Replace with actual API calls when backend is ready
      setPayments([]);
      setPaymentMethods([]);
      
      // Uncomment when backend endpoints are available:
      // const [paymentsRes, methodsRes] = await Promise.all([
      //   apiClient.get('/api/v1/payments/'),
      //   apiClient.get('/api/v1/payments/methods/'),
      // ]);
      // setPayments(paymentsRes.data.results || paymentsRes.data || []);
      // setPaymentMethods(methodsRes.data.results || methodsRes.data || []);
    } catch (error) {
      console.error('Error fetching payment data:', error);
      setPayments([]);
      setPaymentMethods([]);
    } finally {
      setLoading(false);
      setRefreshing(false);
    }
  };

  const handleRefresh = () => {
    setRefreshing(true);
    fetchData();
  };

  const getStatusColor = (status: string) => {
    switch (status.toLowerCase()) {
      case 'success':
      case 'completed':
        return 'text-green-600 bg-green-100';
      case 'pending':
      case 'initiated':
        return 'text-yellow-600 bg-yellow-100';
      case 'failed':
      case 'cancelled':
        return 'text-red-600 bg-red-100';
      default:
        return 'text-gray-600 bg-gray-100';
    }
  };

  const getProviderIcon = (provider: string) => {
    switch (provider.toLowerCase()) {
      case 'paynow':
        return 'phone-portrait-outline';
      case 'payfast':
      case 'stripe':
        return 'card-outline';
      case 'cash_on_arrival':
        return 'cash-outline';
      default:
        return 'wallet-outline';
    }
  };

  const formatDate = (dateString: string) => {
    const date = new Date(dateString);
    return date.toLocaleDateString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric',
    });
  };

  const formatAmount = (amount: number, currency: string) => {
    return `${currency} ${amount.toFixed(2)}`;
  };

  const handleAddPaymentMethod = () => {
    router.push('/wallet/payment-methods');
  };

  const handleDeleteMethod = (methodId: string) => {
    Alert.alert(
      'Delete Payment Method',
      'Are you sure you want to remove this payment method?',
      [
        { text: 'Cancel', style: 'cancel' },
        {
          text: 'Delete',
          style: 'destructive',
          onPress: async () => {
            try {
              await apiClient.delete(`/api/v1/payments/methods/${methodId}/`);
              setPaymentMethods(paymentMethods.filter(m => m.id !== methodId));
              Alert.alert('Success', 'Payment method removed');
            } catch (error) {
              Alert.alert('Error', 'Failed to remove payment method');
            }
          },
        },
      ]
    );
  };

  if (loading) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100">
        <View className="flex-1 justify-center items-center">
          <ActivityIndicator size="large" color="#D9B168" />
          <Text className="text-moss mt-4">Loading payments...</Text>
        </View>
      </SafeAreaView>
    );
  }

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="px-4 pb-6"
        style={{ paddingTop: insets.top + 12 }}
      >
        <View className="flex-row items-center justify-between mb-4">
          <Text className="text-3xl font-black text-white tracking-tight">Payments</Text>
          <TouchableOpacity
            onPress={handleAddPaymentMethod}
            className="px-4 py-2 rounded-xl"
            style={{ backgroundColor: 'rgba(217, 177, 104, 0.2)' }}
          >
            <View className="flex-row items-center">
              <Ionicons name="add" size={18} color="#D9B168" />
              <Text className="text-gold font-semibold ml-1">Add</Text>
            </View>
          </TouchableOpacity>
        </View>

        {/* Tabs */}
        <View className="flex-row bg-white/10 rounded-xl p-1">
          <TouchableOpacity
            onPress={() => setActiveTab('history')}
            className={`flex-1 py-2.5 rounded-lg items-center ${
              activeTab === 'history' ? 'bg-white' : ''
            }`}
          >
            <Text
              className={`font-semibold ${
                activeTab === 'history' ? 'text-forest' : 'text-white'
              }`}
            >
              History
            </Text>
          </TouchableOpacity>

          <TouchableOpacity
            onPress={() => setActiveTab('methods')}
            className={`flex-1 py-2.5 rounded-lg items-center ${
              activeTab === 'methods' ? 'bg-white' : ''
            }`}
          >
            <Text
              className={`font-semibold ${
                activeTab === 'methods' ? 'text-forest' : 'text-white'
              }`}
            >
              Methods
            </Text>
          </TouchableOpacity>
        </View>
      </LinearGradient>

      <ScrollView
        className="flex-1"
        refreshControl={
          <RefreshControl refreshing={refreshing} onRefresh={handleRefresh} />
        }
      >
        {activeTab === 'history' ? (
          <View className="p-4">
            {payments.length === 0 ? (
              <View className="bg-white rounded-xl p-8 items-center">
                <Ionicons name="receipt-outline" size={48} color="#d1d5db" />
                <Text className="text-gray-500 text-center mt-4">
                  No payment history yet
                </Text>
              </View>
            ) : (
              payments.map((payment) => (
                <TouchableOpacity
                  key={payment.id}
                  className="bg-white rounded-xl p-4 mb-3"
                  onPress={() => router.push(`/bookings/${payment.booking}`)}
                >
                  <View className="flex-row items-start justify-between mb-3">
                    <View className="flex-1">
                      <View className="flex-row items-center mb-2">
                        <View className="w-10 h-10 bg-emerald-100 rounded-full items-center justify-center mr-3">
                          <Ionicons
                            name={getProviderIcon(payment.provider) as any}
                            size={20}
                            color="#059669"
                          />
                        </View>
                        <View className="flex-1">
                          <Text className="font-semibold text-gray-900">
                            {payment.provider.replace('_', ' ').toUpperCase()}
                          </Text>
                          <Text className="text-sm text-gray-500">
                            {formatDate(payment.created_at)}
                          </Text>
                        </View>
                      </View>
                    </View>

                    <View className="items-end">
                      <Text className="font-bold text-gray-900 text-lg">
                        {formatAmount(payment.amount, payment.currency)}
                      </Text>
                      <View
                        className={`px-3 py-1 rounded-full mt-1 ${getStatusColor(
                          payment.status
                        )}`}
                      >
                        <Text className="text-xs font-semibold capitalize">
                          {payment.status}
                        </Text>
                      </View>
                    </View>
                  </View>

                  {payment.gateway_ref && (
                    <Text className="text-xs text-gray-400 mt-2">
                      Ref: {payment.gateway_ref}
                    </Text>
                  )}
                </TouchableOpacity>
              ))
            )}
          </View>
        ) : (
          <View className="p-4">
            {paymentMethods.length === 0 ? (
              <View className="bg-white rounded-xl p-8 items-center">
                <Ionicons name="card-outline" size={48} color="#d1d5db" />
                <Text className="text-gray-500 text-center mt-4 mb-4">
                  No payment methods added
                </Text>
                <TouchableOpacity
                  onPress={handleAddPaymentMethod}
                  className="bg-emerald-600 px-6 py-3 rounded-lg"
                >
                  <Text className="text-white font-semibold">
                    Add Payment Method
                  </Text>
                </TouchableOpacity>
              </View>
            ) : (
              paymentMethods.map((method) => (
                <View
                  key={method.id}
                  className="bg-white rounded-xl p-4 mb-3 flex-row items-center justify-between"
                >
                  <View className="flex-row items-center flex-1">
                    <View className="w-12 h-12 bg-gray-100 rounded-lg items-center justify-center mr-3">
                      <Ionicons name="card" size={24} color="#6b7280" />
                    </View>
                    <View className="flex-1">
                      <Text className="font-semibold text-gray-900 capitalize">
                        {method.brand || method.type}
                      </Text>
                      <Text className="text-sm text-gray-500">
                        •••• {method.last4 || '****'}
                      </Text>
                      {method.is_default && (
                        <View className="bg-emerald-100 px-2 py-1 rounded mt-1 self-start">
                          <Text className="text-xs text-emerald-700 font-medium">
                            Default
                          </Text>
                        </View>
                      )}
                    </View>
                  </View>

                  <TouchableOpacity
                    onPress={() => handleDeleteMethod(method.id)}
                    className="p-2"
                  >
                    <Ionicons name="trash-outline" size={20} color="#ef4444" />
                  </TouchableOpacity>
                </View>
              ))
            )}
          </View>
        )}
      </ScrollView>
    </SafeAreaView>
  );
}
