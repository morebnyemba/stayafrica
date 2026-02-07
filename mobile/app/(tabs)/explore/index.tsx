import { View, Text, FlatList, TextInput, ActivityIndicator, TouchableOpacity, ScrollView } from 'react-native';
import { useRouter } from 'expo-router';
import { useMemo, useState } from 'react';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import Animated, { FadeInRight } from 'react-native-reanimated';
import { useProperties } from '@/hooks/api-hooks';
import { PropertyCard } from '@/components/property/PropertyCard';
import { PropertyCardSkeleton } from '@/components/common/Skeletons';
import { Sidebar } from '@/components/common/Sidebar';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';

const CATEGORIES = [
  { id: 'all', label: 'All', icon: 'apps' },
  { id: 'villa', label: 'Villas', icon: 'home' },
  { id: 'apartment', label: 'Apartments', icon: 'business' },
  { id: 'cabin', label: 'Cabins', icon: 'bed' },
  { id: 'beach', label: 'Beach', icon: 'water' },
  { id: 'safari', label: 'Safari', icon: 'leaf' },
];

export default function ExploreScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const [searchQuery, setSearchQuery] = useState('');
  const [selectedCategory, setSelectedCategory] = useState('all');
  const [sidebarVisible, setSidebarVisible] = useState(false);
  const { data: propertiesData, isLoading } = useProperties();
  const properties = propertiesData?.results || [];
  
  const filteredProperties = properties.filter((property: any) => {
    const city = property.location?.city || property.city || '';
    const matchesSearch = property.title.toLowerCase().includes(searchQuery.toLowerCase()) ||
      city.toLowerCase().includes(searchQuery.toLowerCase());
    return matchesSearch;
  });

  const featuredSections = useMemo(() => {
    const counts = filteredProperties.reduce<Record<string, number>>((acc, property: any) => {
      const city = (property.location?.city || property.city || '').trim();
      if (!city) return acc;
      acc[city] = (acc[city] || 0) + 1;
      return acc;
    }, {});

    const topCities = Object.entries(counts)
      .sort((a, b) => b[1] - a[1])
      .slice(0, 2)
      .map(([city]) => city);

    return topCities.map((city) => ({
      id: city,
      title: `Popular homes in ${city}`,
      city,
      data: filteredProperties.filter((property: any) => {
        const propertyCity = property.location?.city || property.city;
        return propertyCity === city;
      }).slice(0, 8),
    }));
  }, [filteredProperties]);

  // Get IDs of properties already shown in featured sections to avoid duplicates
  const featuredPropertyIds = useMemo(() => {
    return new Set(featuredSections.flatMap(section => section.data.map((p: any) => p.id)));
  }, [featuredSections]);

  // Filter out properties already shown in featured sections
  const gridProperties = useMemo(() => {
    return filteredProperties.filter((property: any) => !featuredPropertyIds.has(property.id));
  }, [filteredProperties, featuredPropertyIds]);

  const handlePropertyPress = (id: string) => {
    router.push(`/(tabs)/explore/${id}`);
  };

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      {/* Sidebar */}
      <Sidebar
        isVisible={sidebarVisible}
        onClose={() => setSidebarVisible(false)}
      />
      
      <View className="flex-1 bg-sand-100">
        {/* Consistent Header with Sidebar */}
        <LinearGradient
          colors={['#122F26', '#1d392f', '#2d4a40']}
          start={{ x: 0, y: 0 }}
          end={{ x: 1, y: 1 }}
          className="pb-6"
          style={{ paddingTop: insets.top + 12 }}
        >
          {/* Top Navigation Bar with Menu */}
          <View className="flex-row items-center justify-between px-4 mb-4">
            {/* Hamburger Menu */}
            <TouchableOpacity
              onPress={() => setSidebarVisible(true)}
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="menu" size={24} color="#fff" />
            </TouchableOpacity>
          </View>
          
          <View className="px-4">
            {/* Title */}
            <Text className="text-3xl font-black text-white tracking-tight mb-1">
              Explore
            </Text>
            <Text className="text-sand-200 text-sm mb-4">
              Discover amazing places
            </Text>

            {/* Search Bar */}
            <View 
              className="flex-row items-center px-4 py-3"
              style={{ 
                backgroundColor: 'rgba(255, 255, 255, 0.1)',
                borderRadius: 16
              }}
            >
              <Ionicons name="search" size={22} color="#D9B168" />
              <TextInput
                className="flex-1 ml-3 text-base text-white"
                placeholder="Search destinations, cities..."
                value={searchQuery}
                onChangeText={setSearchQuery}
                placeholderTextColor="rgba(255, 255, 255, 0.5)"
              />
              {searchQuery.length > 0 && (
                <TouchableOpacity onPress={() => setSearchQuery('')}>
                  <Ionicons name="close-circle" size={20} color="rgba(255, 255, 255, 0.5)" />
                </TouchableOpacity>
              )}
            </View>
          </View>
        </LinearGradient>

        {/* Categories Filter */}
        <View className="bg-white border-b border-sand-200" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 4, elevation: 2 }}>
          <ScrollView 
            horizontal 
            showsHorizontalScrollIndicator={false}
            contentContainerStyle={{ paddingHorizontal: 16, paddingVertical: 12 }}
          >
            {CATEGORIES.map((category, index) => (
              <Animated.View 
                key={category.id}
                entering={FadeInRight.delay(index * 50).springify()}
              >
                <TouchableOpacity
                  onPress={() => setSelectedCategory(category.id)}
                  className={`mr-3 px-5 py-2.5 rounded-full flex-row items-center ${
                    selectedCategory === category.id 
                      ? 'bg-primary-800' 
                      : 'bg-sand-100'
                  }`}
                  style={{
                    shadowColor: selectedCategory === category.id ? '#122F26' : '#000',
                    shadowOffset: { width: 0, height: 2 },
                    shadowOpacity: selectedCategory === category.id ? 0.2 : 0.05,
                    shadowRadius: 4,
                    elevation: selectedCategory === category.id ? 3 : 1,
                  }}
                >
                  <Ionicons 
                    name={category.icon as any} 
                    size={18} 
                    color={selectedCategory === category.id ? '#D9B168' : '#3A5C50'} 
                  />
                  <Text className={`ml-2 font-semibold text-sm ${
                    selectedCategory === category.id 
                      ? 'text-gold' 
                      : 'text-primary-800'
                  }`}>
                    {category.label}
                  </Text>
                </TouchableOpacity>
              </Animated.View>
            ))}
          </ScrollView>
        </View>

        {/* Properties List */}
        {isLoading ? (
          <View className="pt-4">
            {[1, 2, 3, 4].map((i) => (
              <PropertyCardSkeleton key={i} />
            ))}
          </View>
        ) : (
          <FlatList
            data={gridProperties}
            numColumns={2}
            renderItem={({ item }) => (
              <View style={{ width: '48%' }} className="mb-4">
                <PropertyCard property={item} onPress={handlePropertyPress} variant="grid" />
              </View>
            )}
            keyExtractor={(item) => item.id}
            columnWrapperStyle={{ paddingHorizontal: 16, justifyContent: 'space-between' }}
            contentContainerStyle={{ paddingVertical: 12, paddingBottom: 40 }}
            ListHeaderComponent={
              featuredSections.length > 0 ? (
                <View className="pt-2">
                  {featuredSections.map((section) => (
                    <View key={section.id} className="mb-6">
                      <View className="flex-row items-center justify-between px-4 mb-3">
                        <Text className="text-lg font-bold text-forest">{section.title}</Text>
                        <TouchableOpacity onPress={() => setSearchQuery(section.city)}>
                          <Text className="text-sm font-semibold text-primary-800">View more</Text>
                        </TouchableOpacity>
                      </View>
                      <FlatList
                        horizontal
                        showsHorizontalScrollIndicator={false}
                        data={section.data}
                        keyExtractor={(item) => `${section.id}-${item.id}`}
                        contentContainerStyle={{ paddingHorizontal: 16 }}
                        renderItem={({ item }) => (
                          <View style={{ width: 220 }} className="mr-4">
                            <PropertyCard property={item} onPress={handlePropertyPress} variant="compact" />
                          </View>
                        )}
                      />
                    </View>
                  ))}
                </View>
              ) : null
            }
            ListEmptyComponent={
              <View className="flex-1 justify-center items-center py-20 px-6">
                <View className="bg-white rounded-3xl p-8 items-center" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 4 }, shadowOpacity: 0.1, shadowRadius: 12, elevation: 5 }}>
                  <View className="bg-sand-200 rounded-full p-6 mb-4">
                    <Ionicons name="search" size={48} color="#3A5C50" />
                  </View>
                  <Text className="text-primary-900 text-xl font-bold mb-2">No properties found</Text>
                  <Text className="text-primary-600 text-center text-sm">
                    Try adjusting your search or explore different categories
                  </Text>
                </View>
              </View>
            }
          />
        )}
      </View>
    </SafeAreaView>
  );
}
