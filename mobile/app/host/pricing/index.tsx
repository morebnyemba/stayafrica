import { View, Text, ScrollView, TouchableOpacity, ActivityIndicator, FlatList } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { useHostProperties } from '@/hooks/api-hooks';
import type { Property } from '@/types';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';

export default function HostPricingScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const insets = useSafeAreaInsets();
  const { data: propertiesData, isLoading } = useHostProperties();
  const properties = propertiesData?.results || [];

  if (!isAuthenticated) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100">
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: insets.top + 12 }}
        >
          <TouchableOpacity onPress={() => router.back()} className="mb-4">
            <View className="w-10 h-10 rounded-xl items-center justify-center" style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}>
              <Ionicons name="arrow-back" size={24} color="#fff" />
            </View>
          </TouchableOpacity>
          <Text className="text-3xl font-black text-white tracking-tight">
            Dynamic Pricing
          </Text>
        </LinearGradient>
        <View className="flex-1 items-center justify-center px-6">
          <Text className="text-forest text-lg">Please sign in to access pricing</Text>
        </View>
      </SafeAreaView>
    );
  }

  const PricingFeatureCard = ({ icon, title, description, color }: any) => (
    <View className="bg-white rounded-xl p-4 flex-1 mr-2" style={{
      shadowColor: '#122F26',
      shadowOffset: { width: 0, height: 2 },
      shadowOpacity: 0.05,
      shadowRadius: 4,
      elevation: 2,
      minWidth: 160,
    }}>
      <View className="items-center">
        <LinearGradient
          colors={[`${color}30`, `${color}20`]}
          className="w-12 h-12 rounded-full items-center justify-center mb-3"
        >
          <Ionicons name={icon} size={24} color={color} />
        </LinearGradient>
        <Text className="text-sm font-bold text-forest text-center mb-1">{title}</Text>
        <Text className="text-xs text-moss text-center">{description}</Text>
      </View>
    </View>
  );

  const PropertyItem = ({ property }: { property: Property }) => (
    <TouchableOpacity
      className="bg-white rounded-2xl p-4 mb-3 flex-row items-center justify-between"
      onPress={() => router.push(`/host/properties/${property.id}/pricing`)}
      style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 4 },
        shadowOpacity: 0.08,
        shadowRadius: 8,
        elevation: 4,
      }}
    >
      <View className="flex-row items-center flex-1">
        <LinearGradient
          colors={['#3A5C50', '#2d4a40']}
          className="w-12 h-12 rounded-xl items-center justify-center"
        >
          <Ionicons name="home" size={24} color="#D9B168" />
        </LinearGradient>
        <View className="ml-3 flex-1">
          <Text className="text-base font-bold text-forest mb-1" numberOfLines={1}>
            {property.title}
          </Text>
          <Text className="text-sm text-moss" numberOfLines={1}>
            {property.location?.city}, {property.location?.country}
          </Text>
          <View className="flex-row items-center mt-1">
            <Text className="text-sm font-semibold text-gold">
              ${property.price_per_night}/night
            </Text>
            <View className={`ml-2 px-2 py-0.5 rounded-full ${
              property.status === 'active' ? 'bg-green-100' : 'bg-gray-100'
            }`}>
              <Text className={`text-xs font-medium ${
                property.status === 'active' ? 'text-green-800' : 'text-gray-800'
              }`}>
                {property.status}
              </Text>
            </View>
          </View>
        </View>
      </View>
      <Ionicons name="chevron-forward" size={20} color="#3A5C50" />
    </TouchableOpacity>
  );

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
          <TouchableOpacity onPress={() => router.back()}>
            <View className="w-10 h-10 rounded-xl items-center justify-center" style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}>
              <Ionicons name="arrow-back" size={24} color="#fff" />
            </View>
          </TouchableOpacity>
        </View>
        <Text className="text-3xl font-black text-white tracking-tight mb-2">
          Dynamic Pricing
        </Text>
        <Text className="text-sand-200 text-sm">
          Manage pricing rules to maximize your earnings
        </Text>
      </LinearGradient>

      <ScrollView 
        className="flex-1" 
        showsVerticalScrollIndicator={false}
        contentContainerStyle={{ paddingHorizontal: 16 }}
      >
        {/* Pricing Overview Cards */}
        <View className="mb-4 -mt-4">
          <ScrollView horizontal showsHorizontalScrollIndicator={false}>
            <PricingFeatureCard
              icon="trending-up"
              title="Seasonal Pricing"
              description="Peak season rates"
              color="#10B981"
            />
            <PricingFeatureCard
              icon="calendar-outline"
              title="Weekend Premium"
              description="Higher weekend rates"
              color="#3B82F6"
            />
            <PricingFeatureCard
              icon="pricetag-outline"
              title="Length Discounts"
              description="Longer stay deals"
              color="#8B5CF6"
            />
          </ScrollView>
        </View>

        {/* Available Rule Types */}
        <View className="bg-white rounded-2xl p-4 mb-4" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 4 },
          shadowOpacity: 0.08,
          shadowRadius: 8,
          elevation: 4,
        }}>
          <Text className="text-lg font-bold text-forest mb-3">
            Available Pricing Rules
          </Text>
          <View className="flex-row flex-wrap justify-between">
            <View className="items-center mb-3" style={{ width: '30%' }}>
              <View className="bg-sand-100 rounded-xl p-3 w-full items-center">
                <Ionicons name="trending-up" size={24} color="#10B981" />
                <Text className="text-xs font-semibold text-forest mt-1">Seasonal</Text>
              </View>
            </View>
            <View className="items-center mb-3" style={{ width: '30%' }}>
              <View className="bg-sand-100 rounded-xl p-3 w-full items-center">
                <Ionicons name="calendar" size={24} color="#3B82F6" />
                <Text className="text-xs font-semibold text-forest mt-1">Weekend</Text>
              </View>
            </View>
            <View className="items-center mb-3" style={{ width: '30%' }}>
              <View className="bg-sand-100 rounded-xl p-3 w-full items-center">
                <Ionicons name="pricetag" size={24} color="#8B5CF6" />
                <Text className="text-xs font-semibold text-forest mt-1">Length</Text>
              </View>
            </View>
            <View className="items-center mb-3" style={{ width: '30%' }}>
              <View className="bg-sand-100 rounded-xl p-3 w-full items-center">
                <Ionicons name="cash" size={24} color="#F59E0B" />
                <Text className="text-xs font-semibold text-forest mt-1">Early Bird</Text>
              </View>
            </View>
            <View className="items-center" style={{ width: '30%' }}>
              <View className="bg-sand-100 rounded-xl p-3 w-full items-center">
                <Ionicons name="flash" size={24} color="#EF4444" />
                <Text className="text-xs font-semibold text-forest mt-1">Last Minute</Text>
              </View>
            </View>
          </View>
        </View>

        {/* Property Pricing List */}
        <View className="bg-white rounded-2xl overflow-hidden mb-4" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 4 },
          shadowOpacity: 0.08,
          shadowRadius: 8,
          elevation: 4,
        }}>
          <View className="p-4 border-b border-sand-200">
            <Text className="text-lg font-bold text-forest">Property Pricing</Text>
            <Text className="text-sm text-moss mt-1">
              Select a property to manage its pricing rules
            </Text>
          </View>

          {isLoading ? (
            <View className="p-6">
              <ActivityIndicator size="large" color="#3A5C50" />
              <Text className="text-center text-moss mt-4">Loading properties...</Text>
            </View>
          ) : properties.length > 0 ? (
            <View className="p-4">
              {properties.filter((p: any) => p?.id).map((property: any) => (
                <PropertyItem key={property.id} property={property} />
              ))}
            </View>
          ) : (
            <View className="p-8 items-center">
              <View className="bg-sand-200 rounded-full p-8 mb-4">
                <Ionicons name="home-outline" size={48} color="#3A5C50" />
              </View>
              <Text className="text-lg font-bold text-forest mb-2">No Properties Yet</Text>
              <Text className="text-moss text-center mb-6">
                Add a property to start managing its pricing rules
              </Text>
              <TouchableOpacity
                onPress={() => router.push('/host/properties/new')}
              >
                <LinearGradient
                  colors={['#D9B168', '#bea04f']}
                  className="px-6 py-3 rounded-xl"
                  style={{
                    shadowColor: '#D9B168',
                    shadowOffset: { width: 0, height: 4 },
                    shadowOpacity: 0.3,
                    shadowRadius: 8,
                    elevation: 5,
                  }}
                >
                  <Text className="text-forest font-bold">Add Property</Text>
                </LinearGradient>
              </TouchableOpacity>
            </View>
          )}
        </View>

        {/* Tips Section */}
        <View className="mb-6 rounded-2xl overflow-hidden" style={{
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
            <View className="flex-row items-center mb-3">
              <Ionicons name="bulb" size={20} color="#3B82F6" />
              <Text className="text-base font-bold text-blue-900 ml-2">💡 Pricing Tips</Text>
            </View>
            <View className="space-y-2">
              <Text className="text-xs text-blue-800 mb-1">• Use seasonal pricing to capture peak demand during holidays</Text>
              <Text className="text-xs text-blue-800 mb-1">• Offer early bird discounts (10-15%) to secure bookings in advance</Text>
              <Text className="text-xs text-blue-800 mb-1">• Apply last-minute discounts to fill vacant dates</Text>
              <Text className="text-xs text-blue-800 mb-1">• Weekend premiums of 10-20% are common in most markets</Text>
              <Text className="text-xs text-blue-800">• Length of stay discounts encourage longer bookings</Text>
            </View>
          </LinearGradient>
        </View>
      </ScrollView>
    </SafeAreaView>
  );
}

HostPricingScreen.displayName = 'HostPricingScreen';
