import { View, Text, FlatList, TouchableOpacity, Image, ActivityIndicator } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
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
      <View className="flex-1 items-center justify-center bg-white px-6">
        <Ionicons name="home-outline" size={64} color="#ddd" />
        <Text className="text-xl font-bold text-gray-800 mt-4">Sign In Required</Text>
        <Text className="text-gray-600 text-center mt-2 mb-6">
          Please sign in to manage your properties
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

  const PropertyItem = ({ property }: { property: Property }) => (
    <TouchableOpacity
      className="bg-white rounded-xl mb-3 shadow-sm border border-gray-100 overflow-hidden"
      onPress={() => router.push(`/host/properties/${property.id}`)}
    >
      <View className="flex-row">
        {property.image_urls?.[0] ? (
          <Image
            source={{ uri: property.image_urls[0] }}
            className="w-28 h-28 bg-gray-200"
            resizeMode="cover"
          />
        ) : (
          <View className="w-28 h-28 bg-gray-200 items-center justify-center">
            <Ionicons name="image-outline" size={32} color="#999" />
          </View>
        )}
        
        <View className="flex-1 p-3">
          <Text className="text-base font-bold text-gray-900 mb-1" numberOfLines={1}>
            {property.title}
          </Text>
          <Text className="text-sm text-gray-600 mb-2" numberOfLines={1}>
            {property.location?.city}, {property.location?.country}
          </Text>
          
          <View className="flex-row items-center mb-2">
            <View className="flex-row items-center mr-4">
              <Ionicons name="bed" size={14} color="#666" />
              <Text className="text-xs text-gray-600 ml-1">{property.number_of_beds}</Text>
            </View>
            <View className="flex-row items-center">
              <Ionicons name="people" size={14} color="#666" />
              <Text className="text-xs text-gray-600 ml-1">{property.max_guests}</Text>
            </View>
          </View>
          
          <View className="flex-row items-center justify-between">
            <Text className="text-lg font-bold text-primary-600">
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
    </TouchableOpacity>
  );

  return (
    <View className="flex-1 bg-gray-50">
      {/* Header */}
      <View className="bg-white px-4 pt-12 pb-4 border-b border-gray-100 shadow-sm flex-row items-center justify-between">
        <View className="flex-1">
          <TouchableOpacity onPress={() => router.back()} className="mb-2">
            <Ionicons name="arrow-back" size={24} color="#333" />
          </TouchableOpacity>
          <Text className="text-2xl font-extrabold text-primary-900">My Properties</Text>
          <Text className="text-sm text-gray-500 mt-1">
            {properties.length} {properties.length === 1 ? 'property' : 'properties'}
          </Text>
        </View>
        
        <TouchableOpacity
          className="bg-primary-600 px-4 py-3 rounded-lg flex-row items-center"
          onPress={() => router.push('/host/properties/new')}
        >
          <Ionicons name="add" size={20} color="white" />
          <Text className="text-white font-semibold ml-1">Add</Text>
        </TouchableOpacity>
      </View>

      {/* Properties List */}
      {isLoading ? (
        <View className="flex-1 justify-center items-center">
          <ActivityIndicator size="large" color="#3A5C50" />
          <Text className="mt-2 text-gray-600">Loading properties...</Text>
        </View>
      ) : properties.length === 0 ? (
        <View className="flex-1 justify-center items-center px-6">
          <Ionicons name="home-outline" size={64} color="#ddd" />
          <Text className="text-xl font-bold text-gray-800 mt-4">No Properties Yet</Text>
          <Text className="text-gray-600 text-center mt-2 mb-6">
            Start earning by listing your first property
          </Text>
          <TouchableOpacity
            className="bg-primary-600 px-8 py-3 rounded-lg"
            onPress={() => router.push('/host/properties/new')}
          >
            <Text className="text-white font-semibold">List a Property</Text>
          </TouchableOpacity>
        </View>
      ) : (
        <FlatList
          data={properties}
          renderItem={({ item }) => <PropertyItem property={item} />}
          keyExtractor={(item) => item.id}
          contentContainerStyle={{ padding: 16 }}
        />
      )}
    </View>
  );
}
