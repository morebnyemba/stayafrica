import { View, Text, FlatList, TouchableOpacity, Image, ActivityIndicator, Platform } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { useHostProperties } from '@/hooks/api-hooks';
import type { Property } from '@/types';

export default function HostPropertiesScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const { data: propertiesData, isLoading } = useHostProperties();
  const properties = propertiesData?.results || [];

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
            My Properties
          </Text>
          <Text className="text-sand-200 text-sm mt-1">
            Manage your listings
          </Text>
        </LinearGradient>

        <View className="flex-1 items-center justify-center px-6">
          <View className="bg-white rounded-3xl p-8 items-center" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}>
            <View className="bg-sand-200 rounded-full p-8 mb-6">
              <Ionicons name="home-outline" size={72} color="#D9B168" />
            </View>
            <Text className="text-2xl font-bold text-forest mb-3">Sign In Required</Text>
            <Text className="text-moss text-center mb-8 px-4 leading-6">
              Please sign in to manage your properties
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

  const PropertyItem = ({ property }: { property: Property }) => (
    <TouchableOpacity
      className="mb-3 mx-4"
      onPress={() => router.push(`/host/properties/${property.id}`)}
    >
      <View
        className="overflow-hidden"
        style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 4 },
          shadowOpacity: 0.08,
          shadowRadius: 8,
          elevation: 4,
        }}
      >
        <View className="flex-row">
          {property.image_urls?.[0] ? (
            <Image
              source={{ uri: property.image_urls[0] }}
              className="w-28 h-28"
              resizeMode="cover"
            />
          ) : (
            <View className="w-28 h-28 bg-sand-200 items-center justify-center">
              <Ionicons name="image-outline" size={32} color="#3A5C50" />
            </View>
          )}
          
          <View className="flex-1 p-3">
            <Text className="text-base font-bold text-forest mb-1" numberOfLines={1}>
              {property.title}
            </Text>
            <View className="flex-row items-center mb-2">
              <Ionicons name="location" size={14} color="#3A5C50" />
              <Text className="text-sm text-moss ml-1" numberOfLines={1}>
                {property.location?.city}, {property.location?.country}
              </Text>
            </View>
            
            <View className="flex-row items-center mb-2">
              <View className="flex-row items-center mr-4">
                <Ionicons name="bed" size={14} color="#3A5C50" />
                <Text className="text-xs text-moss ml-1">{property.number_of_beds}</Text>
              </View>
              <View className="flex-row items-center">
                <Ionicons name="people" size={14} color="#3A5C50" />
                <Text className="text-xs text-moss ml-1">{property.max_guests}</Text>
              </View>
            </View>
            
            <View className="flex-row items-center justify-between">
              <Text className="text-lg font-bold text-gold">
                ${property.price_per_night}/night
              </Text>
              <View className={`px-2 py-1 rounded-full ${
                property.is_available ? 'bg-green-100' : 'bg-red-100'
              }`}>
                <Text className={`text-xs font-semibold ${
                  property.is_available ? 'text-green-800' : 'text-red-800'
                }`}>
                  {property.is_available ? 'Available' : 'Unavailable'}
                </Text>
              </View>
            </View>
          </View>
        </View>
      </View>
    </TouchableOpacity>
  );

  return (
    <View className="flex-1 bg-sand-100">
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="px-4 pb-6"
        style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
      >
        <View className="flex-row items-center justify-between mb-4">
          <TouchableOpacity 
            onPress={() => router.back()}
            className="w-10 h-10 rounded-xl items-center justify-center"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </TouchableOpacity>
          
          <TouchableOpacity
            onPress={() => router.push('/host/properties/new')}
          >
            <LinearGradient
              colors={['#D9B168', '#bea04f']}
              className="px-4 py-2 rounded-xl flex-row items-center"
            >
              <Ionicons name="add" size={20} color="#122F26" />
              <Text className="text-forest font-bold ml-1">Add</Text>
            </LinearGradient>
          </TouchableOpacity>
        </View>

        <Text className="text-3xl font-black text-white tracking-tight mb-2">
          My Properties
        </Text>
        <View className="flex-row items-center">
          <Ionicons name="home" size={16} color="#D9B168" />
          <Text className="text-sand-100 ml-2">
            {properties.length} {properties.length === 1 ? 'property' : 'properties'}
          </Text>
        </View>
      </LinearGradient>

      {/* Properties List */}
      {isLoading ? (
        <View className="flex-1 justify-center items-center">
          <ActivityIndicator size="large" color="#3A5C50" />
          <Text className="mt-2 text-moss">Loading properties...</Text>
        </View>
      ) : properties.length === 0 ? (
        <View className="flex-1 justify-center items-center px-6">
          <View className="bg-white rounded-3xl p-8 items-center" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}>
            <View className="bg-sand-200 rounded-full p-8 mb-6">
              <Ionicons name="home-outline" size={72} color="#3A5C50" />
            </View>
            <Text className="text-2xl font-bold text-forest mb-3">No Properties Yet</Text>
            <Text className="text-moss text-center mb-8 px-4 leading-6">
              Start earning by listing your first property
            </Text>
            <TouchableOpacity
              onPress={() => router.push('/host/properties/new')}
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
                <Text className="text-forest font-bold text-base">List a Property</Text>
              </LinearGradient>
            </TouchableOpacity>
          </View>
        </View>
      ) : (
        <FlatList
          data={properties}
          renderItem={({ item }) => <PropertyItem property={item} />}
          keyExtractor={(item) => item.id}
          contentContainerStyle={{ paddingVertical: 16 }}
        />
      )}
    </View>
  );
}
