import { View, Text, ScrollView, TouchableOpacity, ActivityIndicator } from 'react-native';
import { useLocalSearchParams, useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { usePropertyById } from '@/hooks/api-hooks';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';

export default function PropertyDetailScreen() {
  const router = useRouter();
  const { id } = useLocalSearchParams<{ id: string }>();
  const { isAuthenticated } = useAuth();
  const insets = useSafeAreaInsets();

  const { data: property, isLoading: loading } = usePropertyById(id as string);

  if (!isAuthenticated) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100">
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: insets.top + 12 }}
        >
          <View className="flex-row items-center">
            <TouchableOpacity
              onPress={() => router.back()}
              className="w-10 h-10 rounded-xl items-center justify-center mr-3"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="arrow-back" size={24} color="#fff" />
            </TouchableOpacity>
            <Text className="text-xl font-bold text-white">Property Details</Text>
          </View>
        </LinearGradient>

        <View className="flex-1 items-center justify-center px-6">
          <Text className="text-moss text-center">Please sign in to view property details</Text>
          <TouchableOpacity 
            onPress={() => router.push('/(auth)/login')}
            className="mt-4"
          >
            <LinearGradient
              colors={['#D9B168', '#bea04f']}
              className="px-8 py-4 rounded-2xl"
            >
              <Text className="text-forest font-bold">Sign In</Text>
            </LinearGradient>
          </TouchableOpacity>
        </View>
      </SafeAreaView>
    );
  }

  if (loading) {
    return (
      <View className="flex-1 bg-sand-100 items-center justify-center">
        <ActivityIndicator size="large" color="#D9B168" />
      </View>
    );
  }

  return (
    <SafeAreaView className="flex-1 bg-[#122F26]" edges={['top', 'bottom']}>
      <ScrollView className="flex-1 bg-sand-100" showsVerticalScrollIndicator={false}>
        {/* Header */}
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: insets.top + 12 }}
        >
          <View className="flex-row items-center justify-between">
            <View className="flex-row items-center">
              <TouchableOpacity
                onPress={() => router.back()}
                className="w-10 h-10 rounded-xl items-center justify-center mr-3"
                style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
              >
                <Ionicons name="arrow-back" size={24} color="#fff" />
              </TouchableOpacity>
              <Text className="text-xl font-bold text-white">Property #{id}</Text>
            </View>
            <TouchableOpacity
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="create-outline" size={24} color="#fff" />
            </TouchableOpacity>
          </View>
        </LinearGradient>

        <View className="px-4 py-6">
          {property ? (
            <>
              {/* Property Info */}
              <View
                className="bg-white rounded-2xl p-4 mb-4"
                style={{
                  shadowColor: '#122F26',
                  shadowOffset: { width: 0, height: 2 },
                  shadowOpacity: 0.05,
                  shadowRadius: 4,
                  elevation: 2,
                }}
              >
                <Text className="text-lg font-bold text-forest mb-2">Property Information</Text>
                <Text className="text-base font-semibold text-forest">{property.title}</Text>
                <Text className="text-moss mb-2">{property.city}, {property.country}</Text>
                <Text className="text-moss mb-4">{property.description}</Text>

                <View className="flex-row justify-between mb-2">
                  <Text className="text-moss">Price per night</Text>
                  <Text className="text-forest font-bold">${property.price_per_night}</Text>
                </View>
                <View className="flex-row justify-between mb-2">
                  <Text className="text-moss">Type</Text>
                  <Text className="text-forest capitalize">{property.property_type}</Text>
                </View>
                <View className="flex-row justify-between">
                  <Text className="text-moss">Capacity</Text>
                  <Text className="text-forest">
                    {property.max_guests} guests • {property.bedrooms} beds
                  </Text>
                </View>
              </View>

              {/* Stats */}
              <View
                className="bg-white rounded-2xl p-4 mb-4"
                style={{
                  shadowColor: '#122F26',
                  shadowOffset: { width: 0, height: 2 },
                  shadowOpacity: 0.05,
                  shadowRadius: 4,
                  elevation: 2,
                }}
              >
                <Text className="text-lg font-bold text-forest mb-2">Performance</Text>
                {/* Add booking stats, revenue, etc */}
              </View>

              {/* Actions */}
              <View className="flex-row gap-3">
                <TouchableOpacity
                  className="flex-1"
                  onPress={() => router.push(`/host/properties/edit?id=${property.id}`)}
                >
                  <LinearGradient
                    colors={['#D9B168', '#bea04f']}
                    className="py-4 rounded-2xl items-center"
                  >
                    <Text className="text-forest font-bold">Edit Property</Text>
                  </LinearGradient>
                </TouchableOpacity>
              </View>
            </>
          ) : (
            <View className="items-center py-12">
              <View className="bg-sand-200 rounded-full p-6 mb-4">
                <Ionicons name="home-outline" size={48} color="#94a3b8" />
              </View>
              <Text className="text-xl font-bold text-forest mb-2">Property Not Found</Text>
              <Text className="text-moss text-center">
                This property doesn't exist or has been removed
              </Text>
              <TouchableOpacity
                onPress={() => router.back()}
                className="mt-6"
              >
                <LinearGradient
                  colors={['#D9B168', '#bea04f']}
                  className="px-8 py-4 rounded-2xl"
                >
                  <Text className="text-forest font-bold">Go Back</Text>
                </LinearGradient>
              </TouchableOpacity>
            </View>
          )}
        </View>
      </ScrollView>
    </SafeAreaView>
  );
}
