import { View, Text, ScrollView, TouchableOpacity, ActivityIndicator } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { useAuth } from '@/context/auth-context';
import { useHostEarnings } from '@/hooks/api-hooks';

export default function HostEarningsScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const { data: earnings, isLoading } = useHostEarnings();

  if (!isAuthenticated) {
    return (
      <View className="flex-1 items-center justify-center bg-white px-6">
        <Ionicons name="cash-outline" size={64} color="#ddd" />
        <Text className="text-xl font-bold text-gray-800 mt-4">Sign In Required</Text>
        <Text className="text-gray-600 text-center mt-2 mb-6">
          Please sign in to view your earnings
        </Text>
        <TouchableOpacity
          className="bg-primary-600 px-8 py-3 rounded-lg"
          onPress={() => router.push('/(auth)/login')}
        >
          <Text className="text-white font-semibold">Sign In</Text>
        </TouchableOpacity>
      </View>
    );
  }

  const StatCard = ({ title, value, subtitle, icon, color }: any) => (
    <View className="bg-white rounded-xl p-4 mb-3 shadow-sm border border-gray-100">
      <View className="flex-row items-center justify-between mb-2">
        <Text className="text-sm text-gray-600">{title}</Text>
        <View className={`w-10 h-10 rounded-full items-center justify-center`} style={{ backgroundColor: `${color}20` }}>
          <Ionicons name={icon} size={20} color={color} />
        </View>
      </View>
      <Text className="text-3xl font-extrabold text-gray-900 mb-1">{value}</Text>
      {subtitle && <Text className="text-xs text-gray-500">{subtitle}</Text>}
    </View>
  );

  const PayoutItem = ({ date, amount, status, property }: any) => (
    <View className="bg-white rounded-xl p-4 mb-3 shadow-sm border border-gray-100">
      <View className="flex-row items-center justify-between mb-2">
        <View className="flex-1">
          <Text className="text-base font-semibold text-gray-900">{property}</Text>
          <Text className="text-sm text-gray-600 mt-1">{date}</Text>
        </View>
        <View className="items-end">
          <Text className="text-xl font-bold text-green-600">${amount}</Text>
          <View className={`px-2 py-1 rounded-full mt-1 ${
            status === 'paid' ? 'bg-green-100' : 'bg-yellow-100'
          }`}>
            <Text className={`text-xs font-semibold ${
              status === 'paid' ? 'text-green-800' : 'text-yellow-800'
            }`}>
              {status === 'paid' ? 'Paid' : 'Pending'}
            </Text>
          </View>
        </View>
      </View>
    </View>
  );

  return (
    <ScrollView className="flex-1 bg-gray-50" showsVerticalScrollIndicator={false}>
      {/* Header */}
      <View className="bg-primary-600 px-4 pt-12 pb-8">
        <TouchableOpacity onPress={() => router.back()} className="mb-4">
          <Ionicons name="arrow-back" size={24} color="white" />
        </TouchableOpacity>
        <Text className="text-3xl font-extrabold text-white mb-2 tracking-tight">
          Earnings & Payouts
        </Text>
        <Text className="text-base text-white/80">
          Track your income and payments
        </Text>
      </View>

      {isLoading ? (
        <View className="flex-1 justify-center items-center py-12">
          <ActivityIndicator size="large" color="#3A5C50" />
          <Text className="mt-2 text-gray-600">Loading earnings...</Text>
        </View>
      ) : (
        <View className="px-4 -mt-6">
          {/* Total Earnings Card */}
          <View className="bg-gradient-to-br from-primary-600 to-primary-700 rounded-2xl p-6 shadow-lg mb-4">
            <Text className="text-white/80 text-sm mb-2">Total Earnings</Text>
            <Text className="text-white text-5xl font-extrabold mb-4">
              ${earnings?.total_earnings || '0.00'}
            </Text>
            <View className="flex-row justify-between">
              <View>
                <Text className="text-white/80 text-xs">Available</Text>
                <Text className="text-white text-xl font-bold">
                  ${earnings?.available_balance || '0.00'}
                </Text>
              </View>
              <View>
                <Text className="text-white/80 text-xs">Pending</Text>
                <Text className="text-white text-xl font-bold">
                  ${earnings?.pending_balance || '0.00'}
                </Text>
              </View>
            </View>
          </View>

          {/* Stats */}
          <StatCard
            title="This Month"
            value={`$${earnings?.monthly_earnings || '0.00'}`}
            subtitle="Earned this month"
            icon="trending-up"
            color="#10B981"
          />

          <StatCard
            title="Last Month"
            value={`$${earnings?.last_month_earnings || '0.00'}`}
            subtitle="Previous month earnings"
            icon="calendar"
            color="#6366F1"
          />

          <StatCard
            title="Average per Booking"
            value={`$${earnings?.average_per_booking || '0.00'}`}
            subtitle="Based on completed bookings"
            icon="analytics"
            color="#F59E0B"
          />

          {/* Payout Button */}
          <TouchableOpacity
            className="bg-primary-600 py-4 rounded-xl mb-4 shadow-sm flex-row items-center justify-center"
            onPress={() => router.push('/wallet')}
          >
            <Ionicons name="wallet" size={20} color="white" />
            <Text className="text-white font-semibold text-base ml-2">Request Payout</Text>
          </TouchableOpacity>

          {/* Recent Payouts */}
          <View className="mb-6">
            <View className="flex-row items-center justify-between mb-3">
              <Text className="text-lg font-bold text-gray-900">Recent Payouts</Text>
              <TouchableOpacity>
                <Text className="text-primary-600 font-semibold">See All</Text>
              </TouchableOpacity>
            </View>

            {earnings?.recent_payouts && earnings.recent_payouts.length > 0 ? (
              earnings.recent_payouts.map((payout: any, index: number) => (
                <PayoutItem key={index} {...payout} />
              ))
            ) : (
              <View className="bg-white rounded-xl p-8 items-center">
                <Ionicons name="cash-outline" size={48} color="#ddd" />
                <Text className="text-gray-600 mt-4">No payouts yet</Text>
                <Text className="text-gray-500 text-sm mt-2 text-center">
                  Your payout history will appear here
                </Text>
              </View>
            )}
          </View>
        </View>
      )}
    </ScrollView>
  );
}
