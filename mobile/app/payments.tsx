import { View, Text, ScrollView, TouchableOpacity, ActivityIndicator, RefreshControl } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { useSafeAreaInsets } from 'react-native-safe-area-context';
import { useState } from 'react';

export default function PaymentsScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const insets = useSafeAreaInsets();
  const [refreshing, setRefreshing] = useState(false);
  const [loading, setLoading] = useState(false);

  // TODO: Fetch payment history from API
  // For now, using placeholder data
  const payments = [
    {
      id: '1',
      provider: 'Stripe',
      amount: 350.00,
      status: 'success',
      booking_ref: 'BK-2026-001',
      created_at: '2026-02-01T10:30:00Z',
    },
    {
      id: '2',
      provider: 'PayPal',
      amount: 520.00,
      status: 'success',
      booking_ref: 'BK-2026-002',
      created_at: '2026-01-28T14:20:00Z',
    },
    {
      id: '3',
      provider: 'Stripe',
      amount: 180.00,
      status: 'pending',
      booking_ref: 'BK-2026-003',
      created_at: '2026-01-25T09:15:00Z',
    },
  ];

  const onRefresh = async () => {
    setRefreshing(true);
    // TODO: Implement refresh logic
    setTimeout(() => setRefreshing(false), 1000);
  };

  const getStatusIcon = (status: string) => {
    switch (status) {
      case 'success':
        return <Ionicons name="checkmark-circle" size={24} color="#10B981" />;
      case 'pending':
        return <Ionicons name="time" size={24} color="#F59E0B" />;
      case 'failed':
        return <Ionicons name="close-circle" size={24} color="#EF4444" />;
      default:
        return <Ionicons name="help-circle" size={24} color="#6B7280" />;
    }
  };

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'success':
        return 'text-green-600';
      case 'pending':
        return 'text-yellow-600';
      case 'failed':
        return 'text-red-600';
      default:
        return 'text-gray-600';
    }
  };

  const PaymentCard = ({ payment }: any) => (
    <View
      className="bg-white rounded-2xl p-5 mb-4"
      style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 4 },
        shadowOpacity: 0.08,
        shadowRadius: 8,
        elevation: 4,
      }}
    >
      <View className="flex-row items-start justify-between mb-3">
        <View className="flex-1">
          <Text className="text-lg font-bold text-forest mb-1">
            {payment.provider}
          </Text>
          <Text className="text-sm text-moss mb-1">
            Booking Ref: {payment.booking_ref}
          </Text>
          <Text className="text-xs text-moss/70">
            {new Date(payment.created_at).toLocaleDateString()}{' '}
            {new Date(payment.created_at).toLocaleTimeString([], { 
              hour: '2-digit', 
              minute: '2-digit' 
            })}
          </Text>
        </View>
        <View className="items-end">
          <Text className="text-xl font-bold text-forest mb-2">
            ${payment.amount.toFixed(2)}
          </Text>
          {getStatusIcon(payment.status)}
        </View>
      </View>
      
      <View className="border-t border-sand-200 pt-3 mt-2">
        <View className="flex-row items-center justify-between">
          <View className="flex-row items-center">
            <Text className="text-xs text-moss mr-2">Status:</Text>
            <Text className={`text-xs font-semibold ${getStatusColor(payment.status)} capitalize`}>
              {payment.status}
            </Text>
          </View>
          <TouchableOpacity
            onPress={() => {/* TODO: View payment details */}}
            className="flex-row items-center"
          >
            <Text className="text-xs font-semibold text-gold mr-1">Details</Text>
            <Ionicons name="chevron-forward" size={16} color="#D9B168" />
          </TouchableOpacity>
        </View>
      </View>
    </View>
  );

  if (!isAuthenticated) {
    return (
      <View className="flex-1 bg-sand-100">
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: insets.top + 12 }}
        >
          <View className="flex-row items-center justify-between">
            <TouchableOpacity
              onPress={() => router.back()}
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="arrow-back" size={24} color="#fff" />
            </TouchableOpacity>
            <Text className="text-white font-bold text-lg flex-1 text-center mr-10">
              Payment History
            </Text>
          </View>
        </LinearGradient>

        <View className="flex-1 items-center justify-center px-6">
          <View className="bg-white rounded-3xl p-8 items-center" style={{ 
            shadowColor: '#000', 
            shadowOffset: { width: 0, height: 8 }, 
            shadowOpacity: 0.1, 
            shadowRadius: 16, 
            elevation: 8 
          }}>
            <View className="bg-sand-200 rounded-full p-8 mb-6">
              <Ionicons name="card-outline" size={48} color="#3A5C50" />
            </View>
            <Text className="text-2xl font-bold text-forest mb-3">Sign In Required</Text>
            <Text className="text-moss text-center mb-8 px-4 leading-6">
              Sign in to view your payment history
            </Text>
            <TouchableOpacity onPress={() => router.replace('/(auth)/login')}>
              <LinearGradient
                colors={['#D9B168', '#bea04f']}
                className="px-8 py-4 rounded-2xl"
                style={{
                  shadowColor: '#D9B168',
                  shadowOffset: { width: 0, height: 4 },
                  shadowOpacity: 0.3,
                  shadowRadius: 8,
                  elevation: 5,
                }}
              >
                <Text className="text-forest font-bold text-base">Sign In</Text>
              </LinearGradient>
            </TouchableOpacity>
          </View>
        </View>
      </View>
    );
  }

  return (
    <View className="flex-1 bg-sand-100">
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f']}
        className="px-4 pb-6"
        style={{ paddingTop: insets.top + 12 }}
      >
        <View className="flex-row items-center justify-between mb-4">
          <TouchableOpacity
            onPress={() => router.back()}
            className="w-10 h-10 rounded-xl items-center justify-center"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </TouchableOpacity>
          <Text className="text-white font-bold text-lg flex-1 text-center">
            Payment History
          </Text>
          <View className="w-10" />
        </View>
        <Text className="text-sand-200 text-sm text-center">
          View all your payment transactions
        </Text>
      </LinearGradient>

      {/* Summary Cards */}
      <View className="px-4 pt-6 pb-4">
        <View className="flex-row space-x-3">
          <View
            className="flex-1 bg-white rounded-2xl p-4"
            style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 4 },
              shadowOpacity: 0.08,
              shadowRadius: 8,
              elevation: 4,
            }}
          >
            <View className="bg-green-100 rounded-full p-2 w-10 h-10 items-center justify-center mb-2">
              <Ionicons name="checkmark-circle" size={20} color="#10B981" />
            </View>
            <Text className="text-2xl font-black text-forest mb-1">
              {payments.filter(p => p.status === 'success').length}
            </Text>
            <Text className="text-xs text-moss">Successful</Text>
          </View>

          <View
            className="flex-1 bg-white rounded-2xl p-4"
            style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 4 },
              shadowOpacity: 0.08,
              shadowRadius: 8,
              elevation: 4,
            }}
          >
            <View className="bg-yellow-100 rounded-full p-2 w-10 h-10 items-center justify-center mb-2">
              <Ionicons name="time" size={20} color="#F59E0B" />
            </View>
            <Text className="text-2xl font-black text-forest mb-1">
              {payments.filter(p => p.status === 'pending').length}
            </Text>
            <Text className="text-xs text-moss">Pending</Text>
          </View>

          <View
            className="flex-1 bg-white rounded-2xl p-4"
            style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 4 },
              shadowOpacity: 0.08,
              shadowRadius: 8,
              elevation: 4,
            }}
          >
            <View className="bg-gold/20 rounded-full p-2 w-10 h-10 items-center justify-center mb-2">
              <Ionicons name="cash" size={20} color="#D9B168" />
            </View>
            <Text className="text-2xl font-black text-forest mb-1">
              ${payments.reduce((sum, p) => sum + p.amount, 0).toFixed(0)}
            </Text>
            <Text className="text-xs text-moss">Total</Text>
          </View>
        </View>
      </View>

      {/* Payment List */}
      <ScrollView
        className="flex-1 px-4"
        showsVerticalScrollIndicator={false}
        refreshControl={
          <RefreshControl refreshing={refreshing} onRefresh={onRefresh} tintColor="#D9B168" />
        }
      >
        <Text className="text-lg font-bold text-forest mb-4 px-1">
          Recent Transactions
        </Text>

        {loading ? (
          <View className="flex-1 items-center justify-center py-12">
            <ActivityIndicator size="large" color="#D9B168" />
          </View>
        ) : payments.length === 0 ? (
          <View
            className="bg-white rounded-3xl p-8 items-center"
            style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 4 },
              shadowOpacity: 0.08,
              shadowRadius: 8,
              elevation: 4,
            }}
          >
            <View className="bg-sand-200 rounded-full p-6 mb-4">
              <Ionicons name="card-outline" size={40} color="#3A5C50" />
            </View>
            <Text className="text-xl font-bold text-forest mb-2">No Payments Found</Text>
            <Text className="text-moss text-center">
              You have not made any payments yet.
            </Text>
          </View>
        ) : (
          <View className="pb-6">
            {payments.map((payment) => (
              <PaymentCard key={payment.id} payment={payment} />
            ))}
          </View>
        )}
      </ScrollView>
    </View>
  );
}
