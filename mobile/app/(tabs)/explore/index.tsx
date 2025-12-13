import { View, Text, FlatList, TouchableOpacity, TextInput, Image, ActivityIndicator } from 'react-native';
import { useRouter } from 'expo-router';
import { useState } from 'react';
import { Ionicons } from '@expo/vector-icons';
import { useProperties } from '@/hooks/api-hooks';

interface Property {
  id: string;
  title: string;
  description: string;
  price_per_night: number;
  city: string;
  rating?: number;
  image?: string;
}

export default function ExploreScreen() {
  const router = useRouter();
  const [searchQuery, setSearchQuery] = useState('');
  const { data: propertiesData, isLoading } = useProperties();
  const [savedProperties, setSavedProperties] = useState<string[]>([]);

  const properties = propertiesData?.results || [];

  const filteredProperties = properties.filter((property: Property) => {
    const matchesSearch = property.title.toLowerCase().includes(searchQuery.toLowerCase()) ||
      property.city.toLowerCase().includes(searchQuery.toLowerCase());
    return matchesSearch;
  });

  const toggleSaveProperty = (propertyId: string) => {
    setSavedProperties(prev =>
      prev.includes(propertyId)
        ? prev.filter(id => id !== propertyId)
        : [...prev, propertyId]
    );
  };

  const PropertyCard = ({ property }: { property: Property }) => (
    <TouchableOpacity
      className="mx-4 mb-4 bg-white rounded-xl overflow-hidden shadow-sm"
      onPress={() => router.push(`/(tabs)/explore/${property.id}`)}
    >
      {/* Image Placeholder */}
      <View className="h-48 bg-primary-200 justify-center items-center relative">
        {property.image ? (
          <Image source={{ uri: property.image }} className="w-full h-full" />
        ) : (
          <Text className="text-primary-600 text-sm">Property Image</Text>
        )}
        
        {/* Save Button */}
        <TouchableOpacity
          className="absolute top-3 right-3 bg-white rounded-full p-2"
          onPress={(e) => {
            e.stopPropagation();
            toggleSaveProperty(property.id);
          }}
        >
          <Ionicons
            name={savedProperties.includes(property.id) ? 'heart' : 'heart-outline'}
            size={20}
            color={savedProperties.includes(property.id) ? '#FF6B6B' : '#999'}
          />
        </TouchableOpacity>
      </View>

      {/* Content */}
      <View className="p-3">
        <View className="flex-row justify-between items-start mb-2">
          <View className="flex-1">
            <Text className="font-bold text-lg text-primary-900">{property.title}</Text>
            <View className="flex-row items-center mt-1">
              <Ionicons name="location" size={14} color="#666" />
              <Text className="text-gray-600 text-sm ml-1">{property.city}</Text>
            </View>
          </View>
          {property.rating && (
            <View className="flex-row items-center">
              <Ionicons name="star" size={14} color="#FFA500" />
              <Text className="text-sm font-semibold ml-1">{property.rating}</Text>
            </View>
          )}
        </View>

        {/* Price */}
        <View className="flex-row items-baseline">
          <Text className="text-lg font-bold text-secondary-600">${property.price_per_night}</Text>
          <Text className="text-gray-600 ml-1">/night</Text>
        </View>
      </View>
    </TouchableOpacity>
  );

  return (
    <View className="flex-1 bg-gray-50">
      {/* Header */}
      <View className="bg-white px-4 pt-4 pb-4 border-b border-gray-200">
        <Text className="text-3xl font-bold text-primary-900 mb-4">Explore</Text>

        {/* Search Bar */}
        <View className="flex-row items-center bg-gray-100 rounded-lg px-3 py-2">
          <Ionicons name="search" size={20} color="#999" />
          <TextInput
            className="flex-1 py-2 ml-2"
            placeholder="Search properties or cities..."
            value={searchQuery}
            onChangeText={setSearchQuery}
            placeholderTextColor="#999"
          />
        </View>
      </View>

      {/* Properties List */}
      {isLoading ? (
        <View className="flex-1 justify-center items-center">
          <ActivityIndicator size="large" color="#D4A574" />
          <Text className="mt-2 text-gray-600">Loading properties...</Text>
        </View>
      ) : (
        <FlatList
          data={filteredProperties}
          renderItem={({ item }) => <PropertyCard property={item} />}
          keyExtractor={(item) => item.id}
          contentContainerStyle={{ paddingVertical: 8 }}
          ListEmptyComponent={
            <View className="flex-1 justify-center items-center py-12">
              <Text className="text-gray-600 text-base">No properties found</Text>
              <Text className="text-gray-500 text-sm mt-1">Try adjusting your search</Text>
            </View>
          }
        />
      )}
    </View>
  );
}
