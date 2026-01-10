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
    <View className="flex-1 bg-[#0f172a]">
      {/* Hero / Header */}
      <View className="px-4 pt-12 pb-6 bg-gradient-to-b from-[#0f172a] via-[#0f172a] to-[#111827] border-b border-white/5">
        <Text className="text-4xl font-black text-white tracking-tight mb-2">StayAfrica</Text>
        <Text className="text-lg text-slate-200 mb-5">Curated stays across Africafind your next escape.</Text>
        {/* Search Bar */}
        <View className="flex-row items-center bg-white/10 rounded-2xl px-3 py-3 border border-white/15">
          <Ionicons name="search" size={20} color="#cbd5e1" />
          <TextInput
            className="flex-1 py-2 ml-3 text-base text-white"
            placeholder="Search properties or cities..."
            value={searchQuery}
            onChangeText={setSearchQuery}
            placeholderTextColor="#94a3b8"
          />
        </View>
      </View>

      {/* Properties List */}
      {isLoading ? (
        <View className="flex-1 justify-center items-center bg-[#0f172a]">
          <ActivityIndicator size="large" color="#fbbf24" />
          <Text className="mt-2 text-slate-200">Loading properties...</Text>
        </View>
      ) : (
        <FlatList
          data={filteredProperties}
          renderItem={({ item }) => (
            <View className="px-4 py-2">
              <PropertyCard property={item} onPress={handlePropertyPress} />
            </View>
          )}
          keyExtractor={(item) => item.id}
          contentContainerStyle={{ paddingVertical: 12, paddingBottom: 40, backgroundColor: '#0f172a' }}
          ListEmptyComponent={
            <View className="flex-1 justify-center items-center py-12">
              <Text className="text-slate-200 text-base">No properties found</Text>
              <Text className="text-slate-400 text-sm mt-1">Try adjusting your search</Text>
            </View>
          }
        />
      )}
    </View>
  );
}
