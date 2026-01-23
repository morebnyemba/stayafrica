import { View, Text, ScrollView, TouchableOpacity, Platform } from 'react-native';
import { useState } from 'react';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';

interface StatCardProps {
  icon: keyof typeof Ionicons.glyphMap;
  label: string;
  value: string;
  color: string;
  onPress?: () => void;
}

interface MenuItemProps {
  icon: keyof typeof Ionicons.glyphMap;
  title: string;
  description: string;
  onPress: () => void;
  color?: string;
  badge?: string;
}

export default function HostScreen() {
  const router = useRouter();
  const { user, isAuthenticated } = useAuth();
  const [activeTab, setActiveTab] = useState<'overview' | 'analytics'>('overview');

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
            Become a Host
          </Text>
          <Text className="text-sand-200 text-sm mt-1">
            Share your property and earn
          </Text>
        </LinearGradient>

        <View className="flex-1 items-center justify-center px-6">
          <View className="bg-white rounded-3xl p-8 items-center" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}>
            <View className="bg-sand-200 rounded-full p-8 mb-6">
              <Ionicons name="home-outline" size={72} color="#D9B168" />
            </View>
            <Text className="text-2xl font-bold text-forest mb-3">Become a Host</Text>
            <Text className="text-moss text-center mb-8 px-4 leading-6">
              Sign in to start hosting your properties and earn income from your spaces
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
                <Text className="text-forest font-bold text-base">Sign In Now</Text>
              </LinearGradient>
            </TouchableOpacity>
          </View>
        </View>
      </View>
    );
  }

  // Check if user is a host
  const isHost = user?.role === 'host';

  // Verification Status Banner
  const VerificationBanner = () => {
    if (user?.is_verified) {
      return null; // Don't show banner if verified
    }

    return (
      <View className="mx-4 mt-4">
        <TouchableOpacity onPress={() => router.push('/host/verification')}>
          <LinearGradient
            colors={['#F59E0B', '#D97706']}
            className="p-4 rounded-2xl flex-row items-center"
            start={{ x: 0, y: 0 }}
            end={{ x: 1, y: 0 }}
          >
            <View className="bg-white/20 rounded-full p-2 mr-3">
              <Ionicons name="alert-circle" size={24} color="#fff" />
            </View>
            <View className="flex-1">
              <Text className="text-white font-bold text-base">Verification Required</Text>
              <Text className="text-white/80 text-sm mt-0.5">Verify your identity to list properties</Text>
            </View>
            <Ionicons name="chevron-forward" size={24} color="#fff" />
          </LinearGradient>
        </TouchableOpacity>
      </View>
    );
  };

  // Pending Actions Alert
  const PendingActionsAlert = () => (
    <View className="mx-4 mt-4">
      <LinearGradient
        colors={['#FEF3C7', '#FDE68A']}
        className="p-4 rounded-2xl"
      >
        <View className="flex-row items-start">
          <View className="bg-yellow-500/20 rounded-full p-2 mr-3">
            <Ionicons name="alert-circle" size={20} color="#D97706" />
          </View>
          <View className="flex-1">
            <Text className="text-yellow-900 font-bold mb-2">Pending Actions</Text>
            <View className="flex-row items-center mb-1">
              <Ionicons name="ellipse" size={6} color="#D97706" />
              <Text className="text-yellow-800 text-sm ml-2">0 booking requests awaiting response</Text>
            </View>
            <View className="flex-row items-center mb-1">
              <Ionicons name="ellipse" size={6} color="#D97706" />
              <Text className="text-yellow-800 text-sm ml-2">0 unread messages</Text>
            </View>
          </View>
        </View>
      </LinearGradient>
    </View>
  );

  const StatCard = ({ icon, label, value, color, onPress }: StatCardProps) => (
    <TouchableOpacity 
      className="flex-1 bg-white rounded-2xl p-4 m-2" 
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
        colors={[`${color}20`, `${color}10`]}
        className="w-12 h-12 rounded-full items-center justify-center mb-3"
      >
        <Ionicons name={icon} size={24} color={color} />
      </LinearGradient>
      <Text className="text-2xl font-bold text-forest">{value}</Text>
      <Text className="text-sm text-moss mt-1">{label}</Text>
    </TouchableOpacity>
  );

  const MenuItem = ({ icon, title, description, onPress, color = '#3A5C50', badge }: MenuItemProps) => (
    <TouchableOpacity
      className="bg-white rounded-2xl p-4 mb-3 flex-row items-center"
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
        colors={[`${color}20`, `${color}10`]}
        className="w-12 h-12 rounded-full items-center justify-center"
      >
        <Ionicons name={icon} size={24} color={color} />
      </LinearGradient>
      <View className="flex-1 ml-4">
        <View className="flex-row items-center">
          <Text className="text-base font-semibold text-forest">{title}</Text>
          {badge && (
            <View className="bg-red-500 rounded-full px-2 py-0.5 ml-2">
              <Text className="text-white text-xs font-bold">{badge}</Text>
            </View>
          )}
        </View>
        <Text className="text-sm text-moss mt-1">{description}</Text>
      </View>
      <Ionicons name="chevron-forward" size={20} color="#3A5C50" />
    </TouchableOpacity>
  );

  // Analytics Tab Content
  const AnalyticsContent = () => (
    <View className="px-4 mt-4">
      {/* Revenue Chart Placeholder */}
      <View className="bg-white rounded-2xl p-4 mb-4" style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 4 },
        shadowOpacity: 0.08,
        shadowRadius: 8,
        elevation: 4,
      }}>
        <Text className="text-lg font-bold text-forest mb-4">Revenue Overview</Text>
        <View className="h-40 bg-sand-100 rounded-xl items-center justify-center">
          <Ionicons name="bar-chart-outline" size={48} color="#94a3b8" />
          <Text className="text-moss mt-2">No earnings data yet</Text>
          <Text className="text-sm text-moss/70">Start hosting to see analytics</Text>
        </View>
      </View>

      {/* Performance Metrics */}
      <View className="bg-white rounded-2xl p-4 mb-4" style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 4 },
        shadowOpacity: 0.08,
        shadowRadius: 8,
        elevation: 4,
      }}>
        <Text className="text-lg font-bold text-forest mb-4">Performance Metrics</Text>
        
        <View className="flex-row justify-between mb-3">
          <View className="flex-1 items-center p-3 bg-blue-50 rounded-xl mr-2">
            <Text className="text-2xl font-bold text-blue-600">0%</Text>
            <Text className="text-xs text-blue-700 mt-1">Occupancy Rate</Text>
          </View>
          <View className="flex-1 items-center p-3 bg-green-50 rounded-xl mr-2">
            <Text className="text-2xl font-bold text-green-600">0%</Text>
            <Text className="text-xs text-green-700 mt-1">Response Rate</Text>
          </View>
          <View className="flex-1 items-center p-3 bg-purple-50 rounded-xl">
            <Text className="text-2xl font-bold text-purple-600">0%</Text>
            <Text className="text-xs text-purple-700 mt-1">Booking Rate</Text>
          </View>
        </View>
      </View>

      {/* Property Performance Table */}
      <View className="bg-white rounded-2xl p-4 mb-6" style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 4 },
        shadowOpacity: 0.08,
        shadowRadius: 8,
        elevation: 4,
      }}>
        <Text className="text-lg font-bold text-forest mb-4">Property Performance</Text>
        <View className="h-24 bg-sand-100 rounded-xl items-center justify-center">
          <Ionicons name="home-outline" size={32} color="#94a3b8" />
          <Text className="text-moss mt-2 text-sm">No properties listed yet</Text>
        </View>
      </View>
    </View>
  );

  return (
    <ScrollView className="flex-1 bg-sand-100" showsVerticalScrollIndicator={false}>
      {/* Modern Header with Tabs */}
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="px-4 pb-6"
        style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
      >
        <View className="flex-row items-center justify-between mb-4">
          <View>
            <Text className="text-2xl font-black text-white tracking-tight mb-1">
              Welcome back, {user?.first_name}! 🏠
            </Text>
            <View className="flex-row items-center">
              <Ionicons name="home" size={14} color="#D9B168" />
              <Text className="text-sand-100 text-sm ml-2">
                Manage your properties and bookings
              </Text>
            </View>
          </View>
        </View>

        {/* Tab Navigation */}
        <View className="flex-row bg-white/10 rounded-xl p-1 mt-2">
          <TouchableOpacity
            onPress={() => setActiveTab('overview')}
            className={`flex-1 py-2.5 rounded-lg items-center ${
              activeTab === 'overview' ? 'bg-white' : ''
            }`}
          >
            <Text className={`font-semibold ${
              activeTab === 'overview' ? 'text-forest' : 'text-white'
            }`}>
              Overview
            </Text>
          </TouchableOpacity>
          <TouchableOpacity
            onPress={() => setActiveTab('analytics')}
            className={`flex-1 py-2.5 rounded-lg items-center flex-row justify-center ${
              activeTab === 'analytics' ? 'bg-white' : ''
            }`}
          >
            <Ionicons 
              name="bar-chart-outline" 
              size={16} 
              color={activeTab === 'analytics' ? '#122F26' : '#fff'} 
            />
            <Text className={`font-semibold ml-1.5 ${
              activeTab === 'analytics' ? 'text-forest' : 'text-white'
            }`}>
              Analytics
            </Text>
          </TouchableOpacity>
        </View>
      </LinearGradient>

      {/* Verification Banner */}
      {activeTab === 'overview' && <VerificationBanner />}

      {/* Stats Cards */}
      {activeTab === 'overview' && (
        <>
          <View className="flex-row px-2 mt-4">
            <StatCard
              icon="home"
              label="Properties"
              value="0"
              color="#3A5C50"
              onPress={() => router.push('/host/properties')}
            />
            <StatCard
              icon="calendar"
              label="Bookings"
              value="0"
              color="#D9B168"
              onPress={() => router.push('/host/bookings')}
            />
          </View>

          <View className="flex-row px-2">
            <StatCard
              icon="cash"
              label="Earnings"
              value="$0"
              color="#10B981"
              onPress={() => router.push('/host/earnings')}
            />
            <StatCard
              icon="star"
              label="Avg Rating"
              value="N/A"
              color="#F59E0B"
            />
          </View>

          {/* Quick Actions */}
          <View className="px-4 mt-6">
            <Text className="text-lg font-bold text-forest mb-3">Quick Actions</Text>
            
            <MenuItem
              icon="add-circle"
              title="List New Property"
              description="Add a new property to your listings"
              onPress={() => router.push('/host/properties/new')}
              color="#3A5C50"
            />
            
            <MenuItem
              icon="home"
              title="My Properties"
              description="View and manage your properties"
              onPress={() => router.push('/host/properties')}
              color="#D9B168"
            />
            
            <MenuItem
              icon="calendar"
              title="Bookings"
              description="View property bookings and reservations"
              onPress={() => router.push('/host/bookings')}
              color="#6366F1"
            />
            
            <MenuItem
              icon="cash"
              title="Earnings & Payouts"
              description="Track your income and payments"
              onPress={() => router.push('/host/earnings')}
              color="#10B981"
            />
            
            <MenuItem
              icon="chatbubbles"
              title="Messages"
              description="Communicate with your guests"
              onPress={() => router.push('/(tabs)/messages')}
              color="#8B5CF6"
            />
            
            <MenuItem
              icon="star"
              title="Reviews"
              description="View guest reviews and ratings"
              onPress={() => router.push('/host/reviews')}
              color="#F59E0B"
            />

            <MenuItem
              icon="trending-up"
              title="Dynamic Pricing"
              description="Manage pricing rules"
              onPress={() => router.push('/host/pricing')}
              color="#EC4899"
            />

            <MenuItem
              icon="receipt"
              title="Tax Reports"
              description="View tax documents"
              onPress={() => router.push('/host/tax-reports')}
              color="#14B8A6"
            />

            {!user?.is_verified && (
              <MenuItem
                icon="shield-checkmark"
                title="Verification"
                description="Verify your identity"
                onPress={() => router.push('/host/verification')}
                color="#6366F1"
                badge="Required"
              />
            )}
            
            <MenuItem
              icon="settings"
              title="Host Settings"
              description="Manage your hosting preferences"
              onPress={() => router.push('/host/settings')}
              color="#6B7280"
            />
          </View>

          {/* Tips Section */}
          <View className="mx-4 mt-4 mb-6 rounded-2xl overflow-hidden" style={{
            shadowColor: '#3B82F6',
            shadowOffset: { width: 0, height: 4 },
            shadowOpacity: 0.15,
            shadowRadius: 8,
            elevation: 4,
          }}>
            <LinearGradient
              colors={['#DBEAFE', '#BFDBFE']}
              className="p-4"
            >
              <View className="flex-row items-center mb-2">
                <Ionicons name="bulb" size={20} color="#3B82F6" />
                <Text className="text-base font-semibold text-blue-900 ml-2">Hosting Tip</Text>
              </View>
              <Text className="text-sm text-blue-800">
                High-quality photos and detailed descriptions help attract more guests. 
                Complete your property profile to increase bookings!
              </Text>
            </LinearGradient>
          </View>
        </>
      )}

      {/* Analytics Tab */}
      {activeTab === 'analytics' && <AnalyticsContent />}
    </ScrollView>
  );
}
