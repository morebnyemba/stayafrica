import { View, Text, ScrollView, TouchableOpacity, ActivityIndicator, Platform } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { useHostEarnings } from '@/hooks/api-hooks';

export default function HostEarningsScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const { data: earnings, isLoading } = useHostEarnings();

  if (!isAuthenticated) {
    return (
      <View className="flex-1 bg-sand-100">
        {/* Header */}
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
        >
          <Text className="text-3xl font-black text-white tracking-tight">
            Earnings & Payouts
          </Text>
          <Text className="text-sand-200 text-sm mt-1">
            Track your income and payments
          </Text>
        </LinearGradient>

        <View className="flex-1 items-center justify-center px-6">
          <View className="bg-white rounded-3xl p-8 items-center" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}>
            <View className="bg-sand-200 rounded-full p-8 mb-6">
              <Ionicons name="cash-outline" size={72} color="#D9B168" />
            </View>
            <Text className="text-2xl font-bold text-forest mb-3">Sign In Required</Text>
            <Text className="text-moss text-center mb-8 px-4 leading-6">
              Please sign in to view your earnings
            </Text>
            <TouchableOpacity
              onPress={() => router.push('/(auth)/login')}
            >
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

  const StatCard = ({ title, value, subtitle, icon, color }: any) => (
    <View className="bg-white rounded-2xl p-4 mb-3" style={{
      shadowColor: '#122F26',
      shadowOffset: { width: 0, height: 4 },
      shadowOpacity: 0.08,
      shadowRadius: 8,
      elevation: 4,
    }}>
      <View className="flex-row items-center justify-between mb-2">
        <Text className="text-sm text-moss font-medium">{title}</Text>
        <LinearGradient
          colors={[`${color}30`, `${color}15`]}
          className="w-10 h-10 rounded-full items-center justify-center"
        >
          <Ionicons name={icon} size={20} color={color} />
        </LinearGradient>
      </View>
      <Text className="text-3xl font-black text-forest mb-1">{value}</Text>
      {subtitle && <Text className="text-xs text-moss/70">{subtitle}</Text>}
    </View>
  );

  const PayoutItem = ({ date, amount, status, property }: any) => (
    <View className="bg-white rounded-2xl p-4 mb-3" style={{
      shadowColor: '#122F26',
      shadowOffset: { width: 0, height: 2 },
      shadowOpacity: 0.05,
      shadowRadius: 4,
      elevation: 2,
    }}>
      <View className="flex-row items-center justify-between">
        <View className="flex-1">
          <Text className="text-base font-semibold text-forest">{property}</Text>
          <Text className="text-sm text-moss mt-1">{date}</Text>
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
    <ScrollView 
      className="flex-1 bg-sand-100" 
      showsVerticalScrollIndicator={false}
      contentContainerStyle={{ paddingBottom: 40 }}
    >
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="px-4 pb-8"
        style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
      >
        <TouchableOpacity onPress={() => router.back()} className="mb-4">
          <View className="w-10 h-10 rounded-xl items-center justify-center" style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}>
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </View>
        </TouchableOpacity>
        <Text className="text-3xl font-black text-white tracking-tight mb-2">
          Earnings & Payouts
        </Text>
        <View className="flex-row items-center">
          <Ionicons name="cash" size={16} color="#D9B168" />
          <Text className="text-sand-100 ml-2">
            Track your income and payments
          </Text>
        </View>
      </LinearGradient>

      {isLoading ? (
        <View className="flex-1 justify-center items-center py-12">
          <ActivityIndicator size="large" color="#3A5C50" />
          <Text className="mt-2 text-moss">Loading earnings...</Text>
        </View>
      ) : (
        <View className="px-4 -mt-4">
          {/* Total Earnings Card */}
          <LinearGradient
            colors={['#10B981', '#059669']}
            start={{ x: 0, y: 0 }}
            end={{ x: 1, y: 1 }}
            className="rounded-2xl p-6 mb-4"
            style={{
              shadowColor: '#10B981',
              shadowOffset: { width: 0, height: 8 },
              shadowOpacity: 0.25,
              shadowRadius: 12,
              elevation: 8,
            }}
          >
            <Text className="text-white/80 text-sm mb-2">Total Earnings</Text>
            <Text className="text-white text-5xl font-black mb-4">
              ${earnings?.total_earnings || '0.00'}
            </Text>
            <View className="flex-row justify-between mt-2">
              <View>
                <Text className="text-white/80 text-xs">Available</Text>
                <Text className="text-white text-xl font-bold">
                  ${earnings?.available_balance || '0.00'}
                </Text>
              </View>
              <View className="items-end">
                <Text className="text-white/80 text-xs">Pending</Text>
                <Text className="text-white text-xl font-bold">
                  ${earnings?.pending_balance || '0.00'}
                </Text>
              </View>
            </View>
          </LinearGradient>

          {/* Stats Grid */}
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
            className="mb-4"
            onPress={() => router.push('/(tabs)/wallet')}
          >
            <LinearGradient
              colors={['#D9B168', '#bea04f']}
              className="py-4 rounded-2xl flex-row items-center justify-center"
              style={{
                shadowColor: '#D9B168',
                shadowOffset: { width: 0, height: 4 },
                shadowOpacity: 0.3,
                shadowRadius: 8,
                elevation: 5,
              }}
            >
              <Ionicons name="wallet" size={20} color="#122F26" />
              <Text className="text-forest font-bold text-base ml-2">Request Payout</Text>
            </LinearGradient>
          </TouchableOpacity>

          {/* Charges Breakdown */}
          {earnings?.gross_earnings && (
            <View className="bg-white rounded-2xl p-5 mb-4" style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 4 },
              shadowOpacity: 0.08,
              shadowRadius: 8,
              elevation: 4,
            }}>
              <Text className="text-lg font-bold text-forest mb-4">Charges & Fees Breakdown</Text>
              
              <View className="space-y-3">
                <View className="flex-row justify-between items-center">
                  <Text className="text-forest font-medium">Gross Revenue</Text>
                  <Text className="text-forest font-bold text-lg">${earnings.gross_earnings || '0.00'}</Text>
                </View>
                
                <View className="h-px bg-sand-200" />
                
                <View className="flex-row justify-between items-center">
                  <Text className="text-red-600 pl-3">Platform Commission (15%)</Text>
                  <Text className="text-red-600 font-semibold">-${earnings.total_commission || '0.00'}</Text>
                </View>
                
                {earnings.total_service_fee > 0 && (
                  <View className="flex-row justify-between items-center">
                    <Text className="text-moss pl-3">Service Fees</Text>
                    <Text className="text-moss font-medium">${earnings.total_service_fee || '0.00'}</Text>
                  </View>
                )}
                
                {earnings.total_taxes > 0 && (
                  <View className="flex-row justify-between items-center">
                    <Text className="text-moss pl-3">Taxes Collected</Text>
                    <Text className="text-moss font-medium">${earnings.total_taxes || '0.00'}</Text>
                  </View>
                )}
                
                <View className="h-px bg-sand-200" />
                
                <View className="flex-row justify-between items-center">
                  <Text className="text-green-600 font-bold">Net Earnings to You</Text>
                  <Text className="text-green-600 font-black text-xl">${earnings.total_earnings || '0.00'}</Text>
                </View>
              </View>
              
              <View className="mt-4 bg-blue-50 rounded-xl p-3 border border-blue-200">
                <Text className="text-xs text-blue-800 leading-5">
                  <Text className="font-bold">Note:</Text> The 15% platform commission covers secure payments, support, marketing, and maintenance.
                </Text>
              </View>
            </View>
          )}

          {/* Recent Payouts */}
          <View className="mb-8">
            <View className="flex-row items-center justify-between mb-3">
              <Text className="text-lg font-bold text-forest">Recent Payouts</Text>
              <TouchableOpacity>
                <Text className="text-gold font-semibold">See All</Text>
              </TouchableOpacity>
            </View>

            {earnings?.recent_payouts && earnings.recent_payouts.length > 0 ? (
              earnings.recent_payouts.map((payout: any, index: number) => (
                <PayoutItem key={index} {...payout} />
              ))
            ) : (
              <View className="bg-white rounded-2xl p-8 items-center" style={{
                shadowColor: '#122F26',
                shadowOffset: { width: 0, height: 4 },
                shadowOpacity: 0.08,
                shadowRadius: 8,
                elevation: 4,
              }}>
                <View className="bg-sand-200 rounded-full p-6 mb-4">
                  <Ionicons name="cash-outline" size={40} color="#3A5C50" />
                </View>
                <Text className="text-forest font-semibold">No payouts yet</Text>
                <Text className="text-moss text-sm mt-2 text-center">
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
