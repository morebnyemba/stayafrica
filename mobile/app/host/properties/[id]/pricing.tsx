import { View, Text, ScrollView, TouchableOpacity, Platform, ActivityIndicator } from 'react-native';
import { useLocalSearchParams, useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { usePropertyById } from '@/hooks/api-hooks';
import PricingCalendar from '@/components/pricing/PricingCalendar';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';

export default function PropertyPricingPage() {
  const router = useRouter();
  const { id } = useLocalSearchParams<{ id: string }>();
  const { isAuthenticated } = useAuth();
  const insets = useSafeAreaInsets();
  
  const { data: property, isLoading } = usePropertyById(id || '');

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
            Property Pricing
          </Text>
        </LinearGradient>
        <View className="flex-1 items-center justify-center px-6">
          <Text className="text-forest text-lg">Please sign in to access pricing</Text>
        </View>
      </SafeAreaView>
    );
  }

  if (isLoading) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100">
        <LinearGradient
          colors={['#122F26', '#1d392f', '#2d4a40']}
          start={{ x: 0, y: 0 }}
          end={{ x: 1, y: 1 }}
          className="px-4 pb-6"
          style={{ paddingTop: insets.top + 12 }}
        >
          <TouchableOpacity onPress={() => router.back()} className="mb-4">
            <View className="w-10 h-10 rounded-xl items-center justify-center" style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}>
              <Ionicons name="arrow-back" size={24} color="#fff" />
            </View>
          </TouchableOpacity>
          <Text className="text-3xl font-black text-white tracking-tight">
            Property Pricing
          </Text>
        </LinearGradient>
        <View className="flex-1 items-center justify-center">
          <ActivityIndicator size="large" color="#3A5C50" />
          <Text className="text-moss mt-4">Loading property...</Text>
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
          <TouchableOpacity onPress={() => router.back()}>
            <View className="w-10 h-10 rounded-xl items-center justify-center" style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}>
              <Ionicons name="arrow-back" size={24} color="#fff" />
            </View>
          </TouchableOpacity>
          <TouchableOpacity onPress={() => router.push('/host/pricing')}>
            <View className="px-3 py-1.5 rounded-lg" style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}>
              <Text className="text-white text-xs font-medium">All Properties</Text>
            </View>
          </TouchableOpacity>
        </View>
        <Text className="text-2xl font-black text-white tracking-tight mb-2" numberOfLines={2}>
          {property?.title || 'Property'} - Pricing
        </Text>
        <View className="flex-row items-center">
          <Ionicons name="location" size={14} color="#D9B168" />
          <Text className="text-sand-200 text-sm ml-1">
            {property?.location?.city}, {property?.location?.country}
          </Text>
        </View>
        <View className="flex-row items-center mt-2">
          <View className="bg-white/10 rounded-lg px-3 py-1.5">
            <Text className="text-gold font-semibold">
              Base: ${property?.price_per_night}/night
            </Text>
          </View>
        </View>
      </LinearGradient>

      <ScrollView 
        className="flex-1" 
        showsVerticalScrollIndicator={false}
        contentContainerStyle={{ paddingHorizontal: 16, paddingTop: 16 }}
      >
        {/* Pricing Calendar */}
        <View className="mb-4">
          <PricingCalendar propertyId={id || ''} />
        </View>

        {/* How It Works Card */}
        <View className="bg-white rounded-2xl p-5 mb-4" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 4 },
          shadowOpacity: 0.08,
          shadowRadius: 8,
          elevation: 4,
        }}>
          <View className="flex-row items-center mb-4">
            <View className="bg-sand-100 rounded-full p-2">
              <Ionicons name="information-circle" size={24} color="#3A5C50" />
            </View>
            <Text className="text-xl font-bold text-forest ml-3">
              How Dynamic Pricing Works
            </Text>
          </View>

          <View className="mb-4">
            <View className="flex-row items-start mb-3">
              <View className="bg-green-100 rounded-full p-2 mr-3">
                <Ionicons name="trending-up" size={16} color="#10B981" />
              </View>
              <View className="flex-1">
                <Text className="font-semibold text-forest mb-1">Price Adjustments</Text>
                <Text className="text-sm text-moss leading-5">
                  Pricing rules automatically adjust your nightly rate based on demand, 
                  seasonality, and booking patterns. Green indicates increased prices, 
                  red indicates discounted prices.
                </Text>
              </View>
            </View>

            <View className="flex-row items-start">
              <View className="bg-blue-100 rounded-full p-2 mr-3">
                <Ionicons name="layers" size={16} color="#3B82F6" />
              </View>
              <View className="flex-1">
                <Text className="font-semibold text-forest mb-1">Stacking Rules</Text>
                <Text className="text-sm text-moss leading-5">
                  Multiple pricing rules can apply to the same date. Rules are applied 
                  in priority order, with higher priority rules taking precedence.
                </Text>
              </View>
            </View>
          </View>
        </View>

        {/* Pricing Rules Card */}
        <View className="bg-white rounded-2xl p-5 mb-4" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 4 },
          shadowOpacity: 0.08,
          shadowRadius: 8,
          elevation: 4,
        }}>
          <View className="flex-row items-center justify-between mb-4">
            <Text className="text-xl font-bold text-forest">Active Rules</Text>
            <TouchableOpacity
              onPress={() => {
                // Navigate to rules management (future implementation)
              }}
            >
              <View className="flex-row items-center">
                <Text className="text-sm text-gold font-medium mr-1">Manage</Text>
                <Ionicons name="chevron-forward" size={16} color="#D9B168" />
              </View>
            </TouchableOpacity>
          </View>

          <View className="space-y-3">
            <View className="bg-sand-50 rounded-xl p-3">
              <View className="flex-row items-center justify-between">
                <View className="flex-row items-center flex-1">
                  <View className="bg-green-100 rounded-full p-2">
                    <Ionicons name="sunny" size={16} color="#10B981" />
                  </View>
                  <View className="ml-3 flex-1">
                    <Text className="font-semibold text-forest">Seasonal Pricing</Text>
                    <Text className="text-xs text-moss">Peak season rates</Text>
                  </View>
                </View>
                <Text className="text-sm font-bold text-green-600">+20%</Text>
              </View>
            </View>

            <View className="bg-sand-50 rounded-xl p-3">
              <View className="flex-row items-center justify-between">
                <View className="flex-row items-center flex-1">
                  <View className="bg-blue-100 rounded-full p-2">
                    <Ionicons name="calendar" size={16} color="#3B82F6" />
                  </View>
                  <View className="ml-3 flex-1">
                    <Text className="font-semibold text-forest">Weekend Premium</Text>
                    <Text className="text-xs text-moss">Fri-Sat nights</Text>
                  </View>
                </View>
                <Text className="text-sm font-bold text-blue-600">+15%</Text>
              </View>
            </View>

            <View className="bg-sand-50 rounded-xl p-3">
              <View className="flex-row items-center justify-between">
                <View className="flex-row items-center flex-1">
                  <View className="bg-purple-100 rounded-full p-2">
                    <Ionicons name="pricetag" size={16} color="#8B5CF6" />
                  </View>
                  <View className="ml-3 flex-1">
                    <Text className="font-semibold text-forest">Length Discount</Text>
                    <Text className="text-xs text-moss">7+ nights</Text>
                  </View>
                </View>
                <Text className="text-sm font-bold text-purple-600">-10%</Text>
              </View>
            </View>
          </View>

          <View className="mt-4 pt-4 border-t border-sand-200">
            <Text className="text-xs text-moss text-center">
              Tap 'Manage' to create, edit, or remove pricing rules
            </Text>
          </View>
        </View>

        {/* Quick Actions */}
        <View className="mb-6">
          <Text className="text-lg font-bold text-forest mb-3">Quick Actions</Text>
          
          <TouchableOpacity
            className="bg-white rounded-2xl p-4 mb-3 flex-row items-center justify-between"
            onPress={() => {
              // Navigate to create rule
            }}
            style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 2 },
              shadowOpacity: 0.05,
              shadowRadius: 4,
              elevation: 2,
            }}
          >
            <View className="flex-row items-center">
              <LinearGradient
                colors={['#10B981', '#059669']}
                className="w-12 h-12 rounded-xl items-center justify-center"
              >
                <Ionicons name="add-circle" size={24} color="#fff" />
              </LinearGradient>
              <View className="ml-3">
                <Text className="font-semibold text-forest">Create Pricing Rule</Text>
                <Text className="text-xs text-moss">Add seasonal or event-based pricing</Text>
              </View>
            </View>
            <Ionicons name="chevron-forward" size={20} color="#3A5C50" />
          </TouchableOpacity>

          <TouchableOpacity
            className="bg-white rounded-2xl p-4 flex-row items-center justify-between"
            onPress={() => {
              // Navigate to bulk pricing
            }}
            style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 2 },
              shadowOpacity: 0.05,
              shadowRadius: 4,
              elevation: 2,
            }}
          >
            <View className="flex-row items-center">
              <LinearGradient
                colors={['#3B82F6', '#2563EB']}
                className="w-12 h-12 rounded-xl items-center justify-center"
              >
                <Ionicons name="calendar" size={24} color="#fff" />
              </LinearGradient>
              <View className="ml-3">
                <Text className="font-semibold text-forest">Bulk Date Pricing</Text>
                <Text className="text-xs text-moss">Set prices for multiple dates</Text>
              </View>
            </View>
            <Ionicons name="chevron-forward" size={20} color="#3A5C50" />
          </TouchableOpacity>
        </View>
      </ScrollView>
    </SafeAreaView>
  );
}
