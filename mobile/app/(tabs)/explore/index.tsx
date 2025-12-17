import { View, Text, FlatList, TextInput, ActivityIndicator } from 'react-native';
import { useRouter } from 'expo-router';
import { useState } from 'react';
import { Ionicons } from '@expo/vector-icons';
import { useProperties } from '@/hooks/api-hooks';
import { PropertyCard } from '@/components/property/PropertyCard';

export default function ExploreScreen() {
  const router = useRouter();
  const [searchQuery, setSearchQuery] = useState('');
  const { data: propertiesData, isLoading } = useProperties();
  const properties = propertiesData?.results || [];
  const filteredProperties = properties.filter((property: any) => {
    const matchesSearch = property.title.toLowerCase().includes(searchQuery.toLowerCase()) ||
      property.location?.city?.toLowerCase().includes(searchQuery.toLowerCase());
    return matchesSearch;
  });
  const handlePropertyPress = (id: string) => {
    router.push(`/(tabs)/explore/${id}`);
  };

  return (
    <View className="flex-1 bg-gradient-to-b from-primary-50 to-white">
      {/* Header */}
      <View className="bg-white px-4 pt-8 pb-4 border-b border-gray-100 shadow-sm">
        <Text className="text-3xl font-extrabold text-primary-900 mb-2 tracking-tight">Explore</Text>
        <Text className="text-base text-gray-500 mb-4">Find your next stay from our curated selection of properties.</Text>
        {/* Search Bar */}
        <View className="flex-row items-center bg-gray-100 rounded-xl px-3 py-2 shadow-sm">
          <Ionicons name="search" size={20} color="#999" />
          <TextInput
            className="flex-1 py-2 ml-2 text-base"
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
          renderItem={({ item }) => (
            <View className="px-4">
              <PropertyCard property={item} onPress={handlePropertyPress} />
            </View>
          )}
          keyExtractor={(item) => item.id}
          contentContainerStyle={{ paddingVertical: 12 }}
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
