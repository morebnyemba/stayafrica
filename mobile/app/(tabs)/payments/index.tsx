import { View, Text, ScrollView, TouchableOpacity, ActivityIndicator, RefreshControl } from 'react-native';
import { useState, useCallback } from 'react';
import { useRouter, useFocusEffect } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { apiClient } from '@/services/api-client';

interface Payment {
  id: string;
  booking: {
    id: string;
    booking_ref: string;
    rental_property: {
      name: string;
    };
  };
  amount: string;
  currency: string;
  provider: string;
  status: 'pending' | 'success' | 'failed' | 'refunded';
  created_at: string;
  gateway_ref?: string;
}

export default function PaymentHistoryScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const [loading, setLoading] = useState(true);
  const [refreshing, setRefreshing] = useState(false);
  const [payments, setPayments] = useState<Payment[]>([]);
  const [filter, setFilter] = useState<'all' | 'success' | 'pending' | 'failed'>('all');

  useFocusEffect(
    useCallback(() => {
      fetchPayments();
    }, [filter])
  );

  const fetchPayments = async () => {
    try {
      setLoading(true);
      const params: any = { page_size: 50, ordering: '-created_at' };
      if (filter !== 'all') {
        params.status = filter;
      }
      
      const response = await apiClient.get('/payments/', { params });
      setPayments(response.data.results || []);
    } catch (error) {
      console.error('Error fetching payments:', error);
    } finally {
      setLoading(false);
      setRefreshing(false);
    }
  };

  const onRefresh = () => {
    setRefreshing(true);
    fetchPayments();
  };

  const getStatusIcon = (status: string) => {
    switch (status) {
      case 'success':
        return 'checkmark-circle';
      case 'pending':
        return 'time';
      case 'failed':
        return 'close-circle';
      case 'refunded':
        return 'return-down-back';
      default:
        return 'help-circle';
    }
  };

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'success':
        return '#10B981';
      case 'pending':
        return '#F59E0B';
      case 'failed':
        return '#EF4444';
      case 'refunded':
        return '#8B5CF6';
      default:
        return '#6B7280';
    }
  };

  const getProviderIcon = (provider: string) => {
    switch (provider.toLowerCase()) {
      case 'stripe':
        return 'card';
      case 'paynow':
        return 'phone-portrait';
      case 'flutterwave':
      case 'paystack':
        return 'globe';
      case 'cash_on_arrival':
        return 'cash';
      default:
        return 'wallet';
    }
  };

  const filteredPayments = filter === 'all' 
    ? payments 
    : payments.filter(p => p.status === filter);

  if (loading) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100">
        <View className="flex-1 items-center justify-center">
          <ActivityIndicator size="large" color="#D9B168" />
          <Text className="text-moss font-semibold mt-4">Loading payments...</Text>
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
        <View className="flex-row items-center justify-between mb-6">
          <View>
            <Text className="text-2xl font-bold text-white">Payment History</Text>
            <Text className="text-sand-200 text-xs mt-1">
              {payments.length} payment{payments.length !== 1 ? 's' : ''} total
            </Text>
          </View>
        </View>

        {/* Filter Pills */}
        <ScrollView
          horizontal
          showsHorizontalScrollIndicator={false}
          className="flex-row"
          contentContainerStyle={{ gap: 8 }}
        >
          <TouchableOpacity
            onPress={() => setFilter('all')}
            className={`px-4 py-2 rounded-full ${
              filter === 'all' ? 'bg-gold' : 'bg-white/20'
            }`}
          >
            <Text className={`font-semibold ${filter === 'all' ? 'text-forest' : 'text-white'}`}>
              All
            </Text>
          </TouchableOpacity>
          <TouchableOpacity
            onPress={() => setFilter('success')}
            className={`px-4 py-2 rounded-full ${
              filter === 'success' ? 'bg-gold' : 'bg-white/20'
            }`}
          >
            <Text className={`font-semibold ${filter === 'success' ? 'text-forest' : 'text-white'}`}>
              Success
            </Text>
          </TouchableOpacity>
          <TouchableOpacity
            onPress={() => setFilter('pending')}
            className={`px-4 py-2 rounded-full ${
              filter === 'pending' ? 'bg-gold' : 'bg-white/20'
            }`}
          >
            <Text className={`font-semibold ${filter === 'pending' ? 'text-forest' : 'text-white'}`}>
              Pending
            </Text>
          </TouchableOpacity>
          <TouchableOpacity
            onPress={() => setFilter('failed')}
            className={`px-4 py-2 rounded-full ${
              filter === 'failed' ? 'bg-gold' : 'bg-white/20'
            }`}
          >
            <Text className={`font-semibold ${filter === 'failed' ? 'text-forest' : 'text-white'}`}>
              Failed
            </Text>
          </TouchableOpacity>
        </ScrollView>
      </LinearGradient>

      {/* Payment List */}
      <ScrollView
        className="flex-1 px-4 pt-4"
        showsVerticalScrollIndicator={false}
        refreshControl={
          <RefreshControl refreshing={refreshing} onRefresh={onRefresh} colors={['#D9B168']} />
        }
      >
        {filteredPayments.length > 0 ? (
          filteredPayments.map((payment) => (
            <TouchableOpacity
              key={payment.id}
              onPress={() => router.push(`/booking/${payment.booking.id}`)}
              className="bg-white rounded-2xl p-4 mb-3"
              style={{
                shadowColor: '#122F26',
                shadowOffset: { width: 0, height: 2 },
                shadowOpacity: 0.05,
                shadowRadius: 4,
                elevation: 2,
              }}
            >
              <View className="flex-row items-start justify-between mb-3">
                <View className="flex-1 mr-3">
                  <Text className="text-forest font-bold text-base mb-1">
                    {payment.booking.rental_property.name}
                  </Text>
                  <Text className="text-moss text-xs">
                    Booking: {payment.booking.booking_ref}
                  </Text>
                  <Text className="text-moss text-xs mt-1">
                    {new Date(payment.created_at).toLocaleDateString()} {new Date(payment.created_at).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })}
                  </Text>
                </View>
                <View className="items-end">
                  <Text className="text-2xl font-black text-forest">
                    {payment.currency} {parseFloat(payment.amount).toFixed(2)}
                  </Text>
                  <View
                    className="flex-row items-center mt-2 px-2 py-1 rounded-full"
                    style={{ backgroundColor: `${getStatusColor(payment.status)}20` }}
                  >
                    <Ionicons
                      name={getStatusIcon(payment.status)}
                      size={14}
                      color={getStatusColor(payment.status)}
                    />
                    <Text
                      className="text-xs font-bold ml-1 capitalize"
                      style={{ color: getStatusColor(payment.status) }}
                    >
                      {payment.status}
                    </Text>
                  </View>
                </View>
              </View>

              <View className="flex-row items-center pt-3 border-t border-sand-100">
                <View
                  className="w-8 h-8 rounded-full items-center justify-center mr-2"
                  style={{ backgroundColor: '#D9B16820' }}
                >
                  <Ionicons name={getProviderIcon(payment.provider)} size={16} color="#D9B168" />
                </View>
                <Text className="text-moss text-sm font-semibold capitalize">
                  {payment.provider.replace('_', ' ')}
                </Text>
                {payment.gateway_ref && (
                  <Text className="text-moss text-xs ml-2">• {payment.gateway_ref.substring(0, 12)}...</Text>
                )}
              </View>
            </TouchableOpacity>
          ))
        ) : (
          <View className="items-center py-16">
            <View className="bg-sand-200 rounded-full p-6 mb-4">
              <Ionicons name="card-outline" size={48} color="#94a3b8" />
            </View>
            <Text className="text-xl font-bold text-forest mb-2">
              {filter === 'all' ? 'No Payments' : `No ${filter} Payments`}
            </Text>
            <Text className="text-moss text-center px-8">
              {filter === 'all'
                ? 'Your payment history will appear here'
                : `You have no ${filter} payments`}
            </Text>
          </View>
        )}

        <View className="h-8" />
      </ScrollView>
    </SafeAreaView>
  );
}
