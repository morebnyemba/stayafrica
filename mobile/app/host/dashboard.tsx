import { View, Text, ScrollView, TouchableOpacity, Dimensions } from 'react-native';
import { useState } from 'react';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { useSafeAreaInsets } from 'react-native-safe-area-context';
import { 
  useHostAnalytics, 
  usePendingActions,
  usePropertyPerformance 
} from '@/hooks/api-hooks';

const SCREEN_WIDTH = Dimensions.get('window').width;

export default function HostDashboardScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const { user, isAuthenticated } = useAuth();
  const [timeRange, setTimeRange] = useState<'7' | '30' | '90'>('30');
  
  // Fetch host data
  const { data: analyticsData } = useHostAnalytics();
  const { data: pendingData } = usePendingActions();
  const { data: performanceData } = usePropertyPerformance();
  
  // Calculate stats
  const totalEarnings = analyticsData?.total_earnings || 0;
  const totalBookings = analyticsData?.total_bookings || 0;
  const avgRating = analyticsData?.average_rating || 4.5;
  const occupancyRate = analyticsData?.occupancy_rate || 0;
  const responseRate = analyticsData?.response_rate || 0;
  const bookingRate = analyticsData?.booking_conversion_rate || 0;
  const pendingRequests = pendingData?.pending_bookings || 0;
  const unreadMessages = pendingData?.unread_messages || 0;
  const properties = performanceData?.properties || [];

  const StatCard = ({ icon, label, value, color, trend, onPress }: any) => (
    <TouchableOpacity
      onPress={onPress}
      disabled={!onPress}
      className="bg-white rounded-2xl p-5 mb-4"
      style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 4 },
        shadowOpacity: 0.08,
        shadowRadius: 8,
        elevation: 4,
        width: SCREEN_WIDTH / 2 - 24,
      }}
    >
      <View className={`bg-${color}-100 rounded-full p-3 w-12 h-12 items-center justify-center mb-3`}>
        <Ionicons name={icon} size={24} color={color === 'gold' ? '#D9B168' : `#${color}`} />
      </View>
      <Text className="text-2xl font-black text-forest mb-1">{value}</Text>
      <Text className="text-xs text-moss mb-2">{label}</Text>
      {trend && (
        <View className="flex-row items-center">
          <Ionicons 
            name={trend > 0 ? 'trending-up' : 'trending-down'} 
            size={12} 
            color={trend > 0 ? '#10B981' : '#EF4444'} 
          />
          <Text className={`text-xs ml-1 ${trend > 0 ? 'text-green-600' : 'text-red-600'}`}>
            {Math.abs(trend)}%
          </Text>
        </View>
      )}
    </TouchableOpacity>
  );

  const PerformanceCard = ({ property }: any) => (
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
      <Text className="text-lg font-bold text-forest mb-3">{property.title}</Text>
      <View className="flex-row justify-between mb-2">
        <Text className="text-sm text-moss">Occupancy Rate</Text>
        <Text className="text-sm font-semibold text-forest">{property.occupancy_rate}%</Text>
      </View>
      <View className="flex-row justify-between mb-2">
        <Text className="text-sm text-moss">Total Bookings</Text>
        <Text className="text-sm font-semibold text-forest">{property.total_bookings}</Text>
      </View>
      <View className="flex-row justify-between mb-2">
        <Text className="text-sm text-moss">Revenue</Text>
        <Text className="text-sm font-semibold text-forest">${property.total_revenue}</Text>
      </View>
      <View className="flex-row justify-between">
        <Text className="text-sm text-moss">Avg Rating</Text>
        <View className="flex-row items-center">
          <Ionicons name="star" size={14} color="#F59E0B" />
          <Text className="text-sm font-semibold text-forest ml-1">{property.average_rating}</Text>
        </View>
      </View>
    </View>
  );

  if (!isAuthenticated || user?.role !== 'host') {
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
              Host Dashboard
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
            <View className="bg-yellow-100 rounded-full p-8 mb-6">
              <Ionicons name="alert-circle" size={48} color="#F59E0B" />
            </View>
            <Text className="text-2xl font-bold text-forest mb-3">Host Access Required</Text>
            <Text className="text-moss text-center mb-8 px-4 leading-6">
              You need to have a host account to access this dashboard.
            </Text>
            <TouchableOpacity onPress={() => router.push('/(tabs)/host')}>
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
                <Text className="text-forest font-bold text-base">Become a Host</Text>
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
            Host Dashboard
          </Text>
          <TouchableOpacity
            onPress={() => router.push('/host/settings')}
            className="w-10 h-10 rounded-xl items-center justify-center"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="settings-outline" size={24} color="#fff" />
          </TouchableOpacity>
        </View>
        <Text className="text-sand-200 text-sm text-center">
          Analytics and performance insights
        </Text>
      </LinearGradient>

      {/* Time Range Selector */}
      <View className="px-4 pt-6 pb-4">
        <View className="flex-row bg-white rounded-2xl p-1" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 2 },
          shadowOpacity: 0.05,
          shadowRadius: 4,
          elevation: 2,
        }}>
          {['7', '30', '90'].map((days) => (
            <TouchableOpacity
              key={days}
              onPress={() => setTimeRange(days as any)}
              className={`flex-1 py-2 rounded-xl ${timeRange === days ? '' : ''}`}
            >
              <LinearGradient
                colors={timeRange === days ? ['#D9B168', '#bea04f'] : ['transparent', 'transparent']}
                className="py-2 rounded-xl"
              >
                <Text className={`text-center font-semibold ${
                  timeRange === days ? 'text-forest' : 'text-moss'
                }`}>
                  {days} Days
                </Text>
              </LinearGradient>
            </TouchableOpacity>
          ))}
        </View>
      </View>

      <ScrollView className="flex-1" showsVerticalScrollIndicator={false}>
        {/* Key Metrics */}
        <View className="px-4 pb-4">
          <Text className="text-lg font-bold text-forest mb-4">Key Metrics</Text>
          <View className="flex-row flex-wrap justify-between">
            <StatCard
              icon="cash-outline"
              label="Total Earnings"
              value={`$${totalEarnings.toLocaleString()}`}
              color="green"
              trend={12}
            />
            <StatCard
              icon="calendar-outline"
              label="Total Bookings"
              value={totalBookings.toString()}
              color="blue"
              trend={8}
            />
            <StatCard
              icon="star-outline"
              label="Average Rating"
              value={avgRating.toFixed(1)}
              color="yellow"
            />
            <StatCard
              icon="trending-up-outline"
              label="Occupancy Rate"
              value={`${occupancyRate}%`}
              color="purple"
              trend={5}
            />
          </View>
        </View>

        {/* Performance Indicators */}
        <View className="px-4 pb-4">
          <Text className="text-lg font-bold text-forest mb-4">Performance</Text>
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
            <View className="flex-row justify-between items-center mb-4">
              <Text className="text-sm text-moss">Response Rate</Text>
              <Text className="text-lg font-bold text-forest">{responseRate}%</Text>
            </View>
            <View className="bg-sand-200 rounded-full h-2 mb-4">
              <View 
                className="bg-green-500 rounded-full h-2" 
                style={{ width: `${responseRate}%` }}
              />
            </View>

            <View className="flex-row justify-between items-center mb-4">
              <Text className="text-sm text-moss">Booking Rate</Text>
              <Text className="text-lg font-bold text-forest">{bookingRate}%</Text>
            </View>
            <View className="bg-sand-200 rounded-full h-2">
              <View 
                className="bg-blue-500 rounded-full h-2" 
                style={{ width: `${bookingRate}%` }}
              />
            </View>
          </View>
        </View>

        {/* Pending Actions */}
        {(pendingRequests > 0 || unreadMessages > 0) && (
          <View className="px-4 pb-4">
            <Text className="text-lg font-bold text-forest mb-4">Pending Actions</Text>
            {pendingRequests > 0 && (
              <TouchableOpacity
                onPress={() => router.push('/host/bookings')}
                className="bg-white rounded-2xl p-5 mb-3 flex-row items-center justify-between"
                style={{
                  shadowColor: '#122F26',
                  shadowOffset: { width: 0, height: 4 },
                  shadowOpacity: 0.08,
                  shadowRadius: 8,
                  elevation: 4,
                }}
              >
                <View className="flex-row items-center flex-1">
                  <View className="bg-yellow-100 rounded-full p-3 mr-4">
                    <Ionicons name="time" size={24} color="#F59E0B" />
                  </View>
                  <View>
                    <Text className="text-base font-bold text-forest">{pendingRequests} Pending Requests</Text>
                    <Text className="text-sm text-moss">Review booking requests</Text>
                  </View>
                </View>
                <Ionicons name="chevron-forward" size={24} color="#3A5C50" />
              </TouchableOpacity>
            )}
            {unreadMessages > 0 && (
              <TouchableOpacity
                onPress={() => router.push('/(tabs)/messages')}
                className="bg-white rounded-2xl p-5 flex-row items-center justify-between"
                style={{
                  shadowColor: '#122F26',
                  shadowOffset: { width: 0, height: 4 },
                  shadowOpacity: 0.08,
                  shadowRadius: 8,
                  elevation: 4,
                }}
              >
                <View className="flex-row items-center flex-1">
                  <View className="bg-blue-100 rounded-full p-3 mr-4">
                    <Ionicons name="chatbubbles" size={24} color="#3B82F6" />
                  </View>
                  <View>
                    <Text className="text-base font-bold text-forest">{unreadMessages} Unread Messages</Text>
                    <Text className="text-sm text-moss">Respond to guest inquiries</Text>
                  </View>
                </View>
                <Ionicons name="chevron-forward" size={24} color="#3A5C50" />
              </TouchableOpacity>
            )}
          </View>
        )}

        {/* Property Performance */}
        {properties.length > 0 && (
          <View className="px-4 pb-8">
            <Text className="text-lg font-bold text-forest mb-4">Property Performance</Text>
            {properties.map((property: any) => (
              <PerformanceCard key={property.id} property={property} />
            ))}
          </View>
        )}
      </ScrollView>
    </View>
  );
}
