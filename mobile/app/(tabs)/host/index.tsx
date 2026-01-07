import { View, Text, ScrollView, TouchableOpacity, ActivityIndicator } from 'react-native';
import { useRouter } from 'expo-router';
import { useState } from 'react';
import { Ionicons } from '@expo/vector-icons';
import { useAuth } from '@/context/auth-context';

export default function HostScreen() {
  const router = useRouter();
  const { user, isAuthenticated } = useAuth();
  const [isLoading, setIsLoading] = useState(false);

  if (!isAuthenticated) {
    return (
      <View className="flex-1 items-center justify-center bg-white px-6">
        <Ionicons name="home-outline" size={64} color="#ddd" />
        <Text className="text-xl font-bold text-gray-800 mt-4">Become a Host</Text>
        <Text className="text-gray-600 text-center mt-2 mb-6">
          Sign in to start hosting your properties and earn income
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

  const StatCard = ({ icon, label, value, color }: any) => (
    <View className={`flex-1 bg-white rounded-xl p-4 m-2 shadow-sm border border-gray-100`}>
      <View className={`w-10 h-10 rounded-full items-center justify-center mb-2`} style={{ backgroundColor: `${color}20` }}>
        <Ionicons name={icon} size={20} color={color} />
      </View>
      <Text className="text-2xl font-bold text-gray-900">{value}</Text>
      <Text className="text-sm text-gray-600 mt-1">{label}</Text>
    </View>
  );

  const MenuItem = ({ icon, title, description, onPress, color = '#3A5C50' }: any) => (
    <TouchableOpacity
      className="bg-white rounded-xl p-4 mb-3 shadow-sm border border-gray-100 flex-row items-center"
      onPress={onPress}
    >
      <View className={`w-12 h-12 rounded-full items-center justify-center`} style={{ backgroundColor: `${color}20` }}>
        <Ionicons name={icon} size={24} color={color} />
      </View>
      <View className="flex-1 ml-4">
        <Text className="text-base font-semibold text-gray-900">{title}</Text>
        <Text className="text-sm text-gray-600 mt-1">{description}</Text>
      </View>
      <Ionicons name="chevron-forward" size={20} color="#999" />
    </TouchableOpacity>
  );

  return (
    <ScrollView className="flex-1 bg-gray-50" showsVerticalScrollIndicator={false}>
      {/* Header */}
      <View className="bg-primary-600 px-4 pt-12 pb-8">
        <Text className="text-3xl font-extrabold text-white mb-2 tracking-tight">
          Host Dashboard
        </Text>
        <Text className="text-base text-white/80">
          Manage your properties and bookings
        </Text>
      </View>

      {/* Stats Cards */}
      <View className="flex-row px-2 -mt-6">
        <StatCard
          icon="home"
          label="Properties"
          value="0"
          color="#3A5C50"
        />
        <StatCard
          icon="calendar"
          label="Bookings"
          value="0"
          color="#D9B168"
        />
      </View>

      <View className="flex-row px-2">
        <StatCard
          icon="cash"
          label="Earnings"
          value="$0"
          color="#10B981"
        />
        <StatCard
          icon="star"
          label="Rating"
          value="N/A"
          color="#F59E0B"
        />
      </View>

      {/* Menu Items */}
      <View className="px-4 mt-4">
        <Text className="text-lg font-bold text-gray-900 mb-3">Quick Actions</Text>
        
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
          icon="star"
          title="Reviews"
          description="View guest reviews and ratings"
          onPress={() => router.push('/host/reviews')}
          color="#F59E0B"
        />
        
        <MenuItem
          icon="settings"
          title="Host Settings"
          description="Manage your hosting preferences"
          onPress={() => router.push('/host/settings')}
          color="#6B7280"
        />
      </View>

      {/* Tips Section */}
      <View className="mx-4 mt-4 mb-6 bg-blue-50 rounded-xl p-4 border border-blue-100">
        <View className="flex-row items-center mb-2">
          <Ionicons name="bulb" size={20} color="#3B82F6" />
          <Text className="text-base font-semibold text-blue-900 ml-2">Hosting Tip</Text>
        </View>
        <Text className="text-sm text-blue-800">
          High-quality photos and detailed descriptions help attract more guests. 
          Complete your property profile to increase bookings!
        </Text>
      </View>
    </ScrollView>
  );
}
