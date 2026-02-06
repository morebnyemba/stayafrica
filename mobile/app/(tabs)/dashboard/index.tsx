import { View, Text, ScrollView, TouchableOpacity, RefreshControl } from 'react-native';
import { useState } from 'react';
import { Redirect, useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { AnimatedCompassIcon } from '@/components/common/AnimatedCompassIcon';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { Sidebar } from '@/components/common/Sidebar';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';

interface StatCardProps {
  icon: keyof typeof Ionicons.glyphMap;
  label: string;
  value: string | number;
  color: string;
  onPress?: () => void;
}

interface QuickActionProps {
  icon: keyof typeof Ionicons.glyphMap;
  title: string;
  description: string;
  onPress: () => void;
  color: string;
}

interface ActivityItemProps {
  type: 'booking' | 'review' | 'payment' | 'wishlist';
  title: string;
  subtitle: string;
  time: string;
}

export default function GuestDashboardScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const { user, isAuthenticated, isLoading, refreshUser } = useAuth();
  const [sidebarVisible, setSidebarVisible] = useState(false);
  const [refreshing, setRefreshing] = useState(false);

  const onRefresh = async () => {
    setRefreshing(true);
    try {
      await refreshUser();
    } catch (error) {
      console.error('Error refreshing dashboard:', error);
    } finally {
      setRefreshing(false);
    }
  };

  if (isAuthenticated && user?.role === 'host') {
    return <Redirect href="/(tabs)/host" />;
  }

  if (isLoading) {
    return null;
  }

  if (!isAuthenticated) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100">
        {/* Sidebar */}
        <Sidebar
          isVisible={sidebarVisible}
          onClose={() => setSidebarVisible(false)}
        />

        {/* Header */}
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: insets.top + 12 }}
        >
          {/* Top Navigation Bar with Menu */}
          <View className="flex-row items-center justify-between mb-4">
            <TouchableOpacity
              onPress={() => setSidebarVisible(true)}
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="menu" size={24} color="#fff" />
            </TouchableOpacity>
          </View>

          <Text className="text-3xl font-black text-white tracking-tight">
            Dashboard
          </Text>
          <Text className="text-sand-200 text-sm mt-1">
            Your travel hub
          </Text>
        </LinearGradient>

        <View className="flex-1 items-center justify-center px-6">
          <View className="bg-white rounded-3xl p-8 items-center" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}>
            <View className="bg-sand-200 rounded-full p-8 mb-6">
              <AnimatedCompassIcon size={72} color="#D9B168" />
            </View>
            <Text className="text-2xl font-bold text-forest mb-3">Welcome to StayAfrica</Text>
            <Text className="text-moss text-center mb-8 px-4 leading-6">
              Sign in to access your personalized travel dashboard
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
      </SafeAreaView>
    );
  }

  // Verification Status Banner
  const VerificationBanner = () => {
    if (user?.is_verified) {
      return (
        <View className="mx-4 mt-4">
          <LinearGradient
            colors={['#10B981', '#059669']}
            className="p-4 bg-white rounded-2xl flex-row items-center"
            start={{ x: 0, y: 0 }}
            end={{ x: 1, y: 0 }}
          >
            <View className="bg-white/20 rounded-full p-2 mr-3">
              <Ionicons name="shield-checkmark" size={24} color="#fff" />
            </View>
            <View className="flex-1">
              <Text className="text-white font-bold text-base">Verified Traveler</Text>
              <Text className="text-white/80 text-sm mt-0.5">Your identity has been verified</Text>
            </View>
            <Ionicons name="checkmark-circle" size={28} color="#fff" />
          </LinearGradient>
        </View>
      );
    }

    return (
      <View className="mx-4 mt-4">
        <TouchableOpacity onPress={() => router.push('/profile/verification')}>
          <LinearGradient
            colors={['#F59E0B', '#D97706']}
            className="p-4 bg-white rounded-2xl flex-row items-center"
            start={{ x: 0, y: 0 }}
            end={{ x: 1, y: 0 }}
          >
            <View className="bg-white/20 rounded-full p-2 mr-3">
              <Ionicons name="alert-circle" size={24} color="#fff" />
            </View>
            <View className="flex-1">
              <Text className="text-white font-bold text-base">Verify Your Identity</Text>
              <Text className="text-white/80 text-sm mt-0.5">Get verified for faster bookings</Text>
            </View>
            <Ionicons name="chevron-forward" size={24} color="#fff" />
          </LinearGradient>
        </TouchableOpacity>
      </View>
    );
  };

  // Stats Card Component
  const StatCard = ({ icon, label, value, color, onPress }: StatCardProps) => (
    <TouchableOpacity 
      className="flex-1 bg-white rounded-2xl p-4 bg-white rounded-2xl m-1.5" 
      onPress={onPress}
      style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 4 },
        shadowOpacity: 0.08,
        shadowRadius: 8,
        elevation: 4,
      }}
    >
      <LinearGradient
        colors={[`${color}30`, `${color}15`]}
        className="w-11 h-11 rounded-full items-center justify-center mb-3"
      >
        <Ionicons name={icon} size={22} color={color} />
      </LinearGradient>
      <Text className="text-2xl font-black text-forest">{value}</Text>
      <Text className="text-sm text-moss mt-1">{label}</Text>
    </TouchableOpacity>
  );

  // Quick Action Component
  const QuickAction = ({ icon, title, description, onPress, color }: QuickActionProps) => (
    <TouchableOpacity
      className="bg-white rounded-2xl p-4 bg-white rounded-2xl mb-3 flex-row items-center"
      onPress={onPress}
      style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 2 },
        shadowOpacity: 0.05,
        shadowRadius: 4,
        elevation: 2,
      }}
    >
      <LinearGradient
        colors={[`${color}`, `${color}dd`]}
        className="w-12 h-12 rounded-xl items-center justify-center"
      >
        {icon === 'compass' ? (
          <AnimatedCompassIcon size={24} color="#fff" filled />
        ) : (
          <Ionicons name={icon} size={24} color="#fff" />
        )}
      </LinearGradient>
      <View className="flex-1 ml-4">
        <Text className="text-base font-semibold text-forest">{title}</Text>
        <Text className="text-sm text-moss mt-0.5">{description}</Text>
      </View>
      <Ionicons name="chevron-forward" size={20} color="#3A5C50" />
    </TouchableOpacity>
  );

  // Recent Activity Item
  const ActivityItem = ({ type, title, subtitle, time }: ActivityItemProps) => {
    const getTypeIcon = (): keyof typeof Ionicons.glyphMap => {
      switch (type) {
        case 'booking': return 'calendar';
        case 'review': return 'star';
        case 'payment': return 'card';
        case 'wishlist': return 'heart';
        default: return 'ellipse';
      }
    };

    const getTypeColor = () => {
      switch (type) {
        case 'booking': return '#3B82F6';
        case 'review': return '#EC4899';
        case 'payment': return '#8B5CF6';
        case 'wishlist': return '#EF4444';
        default: return '#6B7280';
      }
    };

    return (
      <View className="flex-row items-center py-3 border-b border-sand-100">
        <View 
          className="w-10 h-10 rounded-full items-center justify-center mr-3"
          style={{ backgroundColor: `${getTypeColor()}20` }}
        >
          <Ionicons name={getTypeIcon()} size={18} color={getTypeColor()} />
        </View>
        <View className="flex-1">
          <Text className="text-sm font-semibold text-forest">{title}</Text>
          <Text className="text-xs text-moss mt-0.5">{subtitle}</Text>
        </View>
        <Text className="text-xs text-moss">{time}</Text>
      </View>
    );
  };

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      <ScrollView 
        className="flex-1" 
        showsVerticalScrollIndicator={false}
        refreshControl={
          <RefreshControl
            refreshing={refreshing}
            onRefresh={onRefresh}
            tintColor="#D9B168"
            colors={['#D9B168']}
          />
        }
      >
      {/* Sidebar */}
      <Sidebar
        isVisible={sidebarVisible}
        onClose={() => setSidebarVisible(false)}
      />

      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="px-4 pb-6"
        style={{ paddingTop: insets.top + 12 }}
      >
        {/* Top Navigation */}
        <View className="flex-row items-center justify-between mb-4">
          <TouchableOpacity
            onPress={() => setSidebarVisible(true)}
            className="w-10 h-10 rounded-xl items-center justify-center"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="menu" size={24} color="#fff" />
          </TouchableOpacity>

          <TouchableOpacity
            onPress={() => router.push('/(tabs)/messages')}
            className="w-10 h-10 rounded-xl items-center justify-center"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="notifications-outline" size={22} color="#fff" />
          </TouchableOpacity>
        </View>

        {/* Welcome Message */}
        <Text className="text-2xl font-black text-white tracking-tight mb-1">
          Welcome back, {user?.first_name}! 👋
        </Text>
        <Text className="text-sand-200 text-sm">
          Here's what's happening with your travels
        </Text>
      </LinearGradient>

      {/* Verification Banner */}
      <VerificationBanner />

      {/* Stats Grid */}
      <View className="px-2.5 mt-4">
        <View className="flex-row">
          <StatCard
            icon="calendar"
            label="Upcoming Trips"
            value="0"
            color="#3B82F6"
            onPress={() => router.push('/(tabs)/bookings')}
          />
          <StatCard
            icon="heart"
            label="Saved Properties"
            value="0"
            color="#EF4444"
            onPress={() => router.push('/(tabs)/wishlist')}
          />
        </View>
        <View className="flex-row">
          <StatCard
            icon="checkmark-circle"
            label="Total Bookings"
            value="0"
            color="#10B981"
            onPress={() => router.push('/(tabs)/bookings')}
          />
          <StatCard
            icon="chatbubbles"
            label="Messages"
            value="0"
            color="#8B5CF6"
            onPress={() => router.push('/(tabs)/messages')}
          />
        </View>
      </View>

      {/* Quick Actions */}
      <View className="px-4 mt-6">
        <Text className="text-lg font-bold text-forest mb-3">Quick Actions</Text>

        <QuickAction
          icon="compass"
          title="Explore Properties"
          description="Discover new places to stay"
          onPress={() => router.push('/(tabs)/explore')}
          color="#D9B168"
        />

        <QuickAction
          icon="calendar"
          title="View Bookings"
          description="Manage your reservations"
          onPress={() => router.push('/(tabs)/bookings')}
          color="#3B82F6"
        />

        <QuickAction
          icon="person"
          title="My Profile"
          description="Update your information"
          onPress={() => router.push('/(tabs)/profile')}
          color="#10B981"
        />

        <QuickAction
          icon="card"
          title="Payment Methods"
          description="Manage payment options"
          onPress={() => router.push('/(tabs)/wallet')}
          color="#8B5CF6"
        />

        <QuickAction
          icon="star"
          title="My Reviews"
          description="See reviews you wrote"
          onPress={() => router.push('/reviews/my-reviews')}
          color="#EC4899"
        />

        <QuickAction
          icon="shield-checkmark"
          title="Verify Identity"
          description="Get verified for more trust"
          onPress={() => router.push('/profile/verification')}
          color="#6366F1"
        />
      </View>

      {/* Recent Activity */}
      <View className="px-4 mt-6 mb-8">
        <View className="flex-row items-center justify-between mb-3">
          <Text className="text-lg font-bold text-forest">Recent Activity</Text>
          <TouchableOpacity>
            <Text className="text-gold font-semibold text-sm">View All</Text>
          </TouchableOpacity>
        </View>

        <View className="bg-white rounded-2xl p-4 bg-white rounded-2xl" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 4 },
          shadowOpacity: 0.08,
          shadowRadius: 8,
          elevation: 4,
        }}>
          {/* Empty state */}
          <View className="items-center py-6">
            <View className="bg-sand-100 rounded-full p-4 bg-white rounded-2xl mb-3">
              <Ionicons name="time-outline" size={32} color="#94a3b8" />
            </View>
            <Text className="text-forest font-semibold">No Recent Activity</Text>
            <Text className="text-moss text-sm text-center mt-1 px-4">
              Your bookings, reviews, and activity will appear here
            </Text>
          </View>
        </View>
      </View>

      {/* Tips Section */}
      <View className="mx-4 mb-8 rounded-2xl overflow-hidden" style={{
        shadowColor: '#3B82F6',
        shadowOffset: { width: 0, height: 4 },
        shadowOpacity: 0.15,
        shadowRadius: 8,
        elevation: 4,
      }}>
        <LinearGradient
          colors={['#DBEAFE', '#BFDBFE']}
          className="p-4 bg-white rounded-2xl"
        >
          <View className="flex-row items-center mb-2">
            <Ionicons name="bulb" size={20} color="#3B82F6" />
            <Text className="text-base font-semibold text-blue-900 ml-2">Travel Tip</Text>
          </View>
          <Text className="text-sm text-blue-800">
            Complete your profile and verify your identity to unlock faster bookings and exclusive deals!
          </Text>
        </LinearGradient>
      </View>
      </ScrollView>
    </SafeAreaView>
  );
}

GuestDashboardScreen.displayName = 'GuestDashboardScreen';
