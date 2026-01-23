import { View, Text, ScrollView, TouchableOpacity, Platform, Switch } from 'react-native';
import { useState } from 'react';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';

export default function HostPricingScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const [weekendPricing, setWeekendPricing] = useState(false);
  const [seasonalPricing, setSeasonalPricing] = useState(false);
  const [smartPricing, setSmartPricing] = useState(false);

  if (!isAuthenticated) {
    return (
      <View className="flex-1 bg-sand-100">
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
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
      </View>
    );
  }

  const PricingToggle = ({ icon, title, description, value, onToggle, color = '#3A5C50' }: any) => (
    <View className="bg-white rounded-2xl p-4 mb-3 flex-row items-center" style={{
      shadowColor: '#122F26',
      shadowOffset: { width: 0, height: 2 },
      shadowOpacity: 0.05,
      shadowRadius: 4,
      elevation: 2,
    }}>
      <LinearGradient
        colors={[`${color}20`, `${color}10`]}
        className="w-12 h-12 rounded-full items-center justify-center"
      >
        <Ionicons name={icon} size={24} color={color} />
      </LinearGradient>
      <View className="flex-1 ml-4">
        <Text className="text-base font-semibold text-forest">{title}</Text>
        <Text className="text-sm text-moss mt-1">{description}</Text>
      </View>
      <Switch
        value={value}
        onValueChange={onToggle}
        trackColor={{ false: '#e5dfd0', true: '#10B981' }}
        thumbColor={value ? '#fff' : '#fff'}
      />
    </View>
  );

  return (
    <ScrollView className="flex-1 bg-sand-100" showsVerticalScrollIndicator={false}>
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
          Dynamic Pricing
        </Text>
        <View className="flex-row items-center">
          <Ionicons name="trending-up" size={16} color="#D9B168" />
          <Text className="text-sand-100 ml-2">
            Optimize your pricing strategy
          </Text>
        </View>
      </LinearGradient>

      <View className="px-4 -mt-4">
        {/* Smart Pricing Card */}
        <LinearGradient
          colors={['#6366F1', '#8B5CF6']}
          start={{ x: 0, y: 0 }}
          end={{ x: 1, y: 1 }}
          className="rounded-2xl p-5 mb-4"
          style={{
            shadowColor: '#6366F1',
            shadowOffset: { width: 0, height: 8 },
            shadowOpacity: 0.25,
            shadowRadius: 12,
            elevation: 8,
          }}
        >
          <View className="flex-row items-center mb-3">
            <View className="bg-white/20 rounded-full p-2">
              <Ionicons name="sparkles" size={24} color="#fff" />
            </View>
            <Text className="text-white text-xl font-bold ml-3">Smart Pricing</Text>
          </View>
          <Text className="text-white/80 mb-4">
            Let StayAfrica automatically adjust your prices based on demand, seasonality, and local events
          </Text>
          <View className="flex-row items-center justify-between">
            <Text className="text-white font-semibold">Enable Smart Pricing</Text>
            <Switch
              value={smartPricing}
              onValueChange={setSmartPricing}
              trackColor={{ false: 'rgba(255,255,255,0.3)', true: '#10B981' }}
              thumbColor="#fff"
            />
          </View>
        </LinearGradient>

        {/* Pricing Rules */}
        <Text className="text-lg font-bold text-forest mb-3 mt-4">Pricing Rules</Text>

        <PricingToggle
          icon="calendar-outline"
          title="Weekend Pricing"
          description="Charge more on Friday and Saturday nights"
          value={weekendPricing}
          onToggle={setWeekendPricing}
          color="#F59E0B"
        />

        <PricingToggle
          icon="sunny-outline"
          title="Seasonal Pricing"
          description="Adjust prices for peak and off-peak seasons"
          value={seasonalPricing}
          onToggle={setSeasonalPricing}
          color="#10B981"
        />

        {/* Discounts Section */}
        <Text className="text-lg font-bold text-forest mb-3 mt-6">Discounts</Text>

        <View className="bg-white rounded-2xl p-4 mb-3" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 2 },
          shadowOpacity: 0.05,
          shadowRadius: 4,
          elevation: 2,
        }}>
          <View className="flex-row items-center justify-between mb-4">
            <View className="flex-row items-center">
              <View className="bg-blue-100 rounded-full p-2">
                <Ionicons name="calendar" size={20} color="#3B82F6" />
              </View>
              <View className="ml-3">
                <Text className="font-semibold text-forest">Weekly Discount</Text>
                <Text className="text-sm text-moss">7+ nights</Text>
              </View>
            </View>
            <View className="bg-sand-100 rounded-xl px-4 py-2">
              <Text className="text-forest font-bold">0%</Text>
            </View>
          </View>

          <View className="flex-row items-center justify-between mb-4">
            <View className="flex-row items-center">
              <View className="bg-purple-100 rounded-full p-2">
                <Ionicons name="calendar" size={20} color="#8B5CF6" />
              </View>
              <View className="ml-3">
                <Text className="font-semibold text-forest">Monthly Discount</Text>
                <Text className="text-sm text-moss">28+ nights</Text>
              </View>
            </View>
            <View className="bg-sand-100 rounded-xl px-4 py-2">
              <Text className="text-forest font-bold">0%</Text>
            </View>
          </View>

          <View className="flex-row items-center justify-between">
            <View className="flex-row items-center">
              <View className="bg-green-100 rounded-full p-2">
                <Ionicons name="flash" size={20} color="#10B981" />
              </View>
              <View className="ml-3">
                <Text className="font-semibold text-forest">Last Minute Discount</Text>
                <Text className="text-sm text-moss">Bookings within 48hrs</Text>
              </View>
            </View>
            <View className="bg-sand-100 rounded-xl px-4 py-2">
              <Text className="text-forest font-bold">0%</Text>
            </View>
          </View>
        </View>

        {/* Tips */}
        <View className="mt-4 mb-8 rounded-2xl overflow-hidden" style={{
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
              <Text className="text-base font-semibold text-blue-900 ml-2">Pricing Tip</Text>
            </View>
            <Text className="text-sm text-blue-800">
              Properties with competitive pricing get 40% more bookings. Consider enabling smart pricing to maximize your revenue.
            </Text>
          </LinearGradient>
        </View>
      </View>
    </ScrollView>
  );
}
