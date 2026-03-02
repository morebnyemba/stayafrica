import { View, Text, FlatList, TextInput, TouchableOpacity, ScrollView, RefreshControl } from 'react-native';
import { useRouter } from 'expo-router';
import { useCallback, useMemo, useRef, useState } from 'react';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import Animated, { FadeInRight } from 'react-native-reanimated';
import { useProperties, type PropertyFilters } from '@/hooks/api-hooks';
import { PropertyCard } from '@/components/property/PropertyCard';
import { PropertyCardSkeleton } from '@/components/common/Skeletons';
import { Sidebar } from '@/components/common/Sidebar';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';

const CATEGORIES = [
  { id: 'all', label: 'All', icon: 'apps', type: undefined },
  { id: 'villa', label: 'Villas', icon: 'home', type: 'villa' },
  { id: 'apartment', label: 'Apartments', icon: 'business', type: 'apartment' },
  { id: 'cabin', label: 'Cabins', icon: 'bed', type: 'cabin' },
  { id: 'house', label: 'Houses', icon: 'home-outline', type: 'house' },
  { id: 'lodge', label: 'Lodges', icon: 'leaf', type: 'lodge' },
];

const SORT_OPTIONS = [
  { id: 'default', label: 'Best Match', value: undefined },
  { id: 'price_asc', label: 'Price: Low → High', value: 'price_per_night' },
  { id: 'price_desc', label: 'Price: High → Low', value: '-price_per_night' },
  { id: 'newest', label: 'Newest', value: '-created_at' },
];

export default function ExploreScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const [searchInput, setSearchInput] = useState('');
  const [debouncedSearch, setDebouncedSearch] = useState('');
  const [selectedCategory, setSelectedCategory] = useState('all');
  const [selectedSort, setSelectedSort] = useState('default');
  const [showSortMenu, setShowSortMenu] = useState(false);
  const [sidebarVisible, setSidebarVisible] = useState(false);
  const debounceRef = useRef<ReturnType<typeof setTimeout> | null>(null);

  // Build server-side filters
  const filters: PropertyFilters = useMemo(() => {
    const f: PropertyFilters = { page_size: 50 };
    if (debouncedSearch) f.search = debouncedSearch;
    const cat = CATEGORIES.find((c) => c.id === selectedCategory);
    if (cat?.type) f.property_type = cat.type;
    const sort = SORT_OPTIONS.find((s) => s.id === selectedSort);
    if (sort?.value) f.ordering = sort.value;
    return f;
  }, [debouncedSearch, selectedCategory, selectedSort]);

  const { data: propertiesData, isLoading, refetch, isRefetching } = useProperties(filters);
  const properties = propertiesData?.results || [];

  // Debounced search handler
  const handleSearchChange = useCallback((text: string) => {
    setSearchInput(text);
    if (debounceRef.current) clearTimeout(debounceRef.current);
    debounceRef.current = setTimeout(() => {
      setDebouncedSearch(text.trim());
    }, 400);
  }, []);

  const clearSearch = useCallback(() => {
    setSearchInput('');
    setDebouncedSearch('');
    if (debounceRef.current) clearTimeout(debounceRef.current);
  }, []);

  // Featured sections (top 2 cities) — only when no active filters
  const featuredSections = useMemo(() => {
    if (debouncedSearch || selectedCategory !== 'all' || selectedSort !== 'default') return [];
    const counts = properties.reduce<Record<string, number>>((acc, property: any) => {
      const city = (property.location?.city || property.city || '').trim();
      if (!city) return acc;
      acc[city] = (acc[city] || 0) + 1;
      return acc;
    }, {});

    return Object.entries(counts)
      .sort((a, b) => b[1] - a[1])
      .slice(0, 2)
      .map(([city]) => ({
        id: city,
        title: `Popular in ${city}`,
        city,
        data: properties
          .filter((p: any) => (p.location?.city || p.city) === city)
          .slice(0, 8),
      }));
  }, [properties, debouncedSearch, selectedCategory, selectedSort]);

  const featuredPropertyIds = useMemo(
    () => new Set(featuredSections.flatMap((s) => s.data.map((p: any) => p.id))),
    [featuredSections]
  );

  const gridProperties = useMemo(
    () => properties.filter((p: any) => !featuredPropertyIds.has(p.id)),
    [properties, featuredPropertyIds]
  );

  const handlePropertyPress = (id: string) => {
    router.push(`/(tabs)/explore/${id}`);
  };

  const activeSort = SORT_OPTIONS.find((s) => s.id === selectedSort);
  const hasActiveFilters = debouncedSearch || selectedCategory !== 'all' || selectedSort !== 'default';

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      <Sidebar isVisible={sidebarVisible} onClose={() => setSidebarVisible(false)} />

      <View className="flex-1 bg-sand-100">
        {/* Header */}
        <LinearGradient
          colors={['#122F26', '#1d392f', '#2d4a40']}
          start={{ x: 0, y: 0 }}
          end={{ x: 1, y: 1 }}
          className="pb-3"
          style={{ paddingTop: insets.top + 4 }}
        >
          <View className="flex-row items-center justify-between px-4 mb-2">
            <TouchableOpacity
              onPress={() => setSidebarVisible(true)}
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="menu" size={24} color="#fff" />
            </TouchableOpacity>
            <Text className="text-xl font-bold text-white">Explore</Text>
            {/* Sort button */}
            <TouchableOpacity
              onPress={() => setShowSortMenu(!showSortMenu)}
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{ backgroundColor: selectedSort !== 'default' ? 'rgba(217, 177, 104, 0.3)' : 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="swap-vertical" size={22} color={selectedSort !== 'default' ? '#D9B168' : '#fff'} />
            </TouchableOpacity>
          </View>

          <View className="px-4">
            <View
              className="flex-row items-center px-4 py-2.5"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.1)', borderRadius: 12 }}
            >
              <Ionicons name="search" size={22} color="#D9B168" />
              <TextInput
                className="flex-1 ml-3 text-base text-white"
                placeholder="Search destinations, cities..."
                value={searchInput}
                onChangeText={handleSearchChange}
                placeholderTextColor="rgba(255, 255, 255, 0.5)"
                returnKeyType="search"
              />
              {searchInput.length > 0 && (
                <TouchableOpacity onPress={clearSearch}>
                  <Ionicons name="close-circle" size={20} color="rgba(255, 255, 255, 0.5)" />
                </TouchableOpacity>
              )}
            </View>
          </View>
        </LinearGradient>

        {/* Sort dropdown */}
        {showSortMenu && (
          <View className="bg-white border-b border-sand-200 px-4 py-2" style={{ elevation: 4, zIndex: 10 }}>
            {SORT_OPTIONS.map((opt) => (
              <TouchableOpacity
                key={opt.id}
                className={`flex-row items-center justify-between py-3 px-3 rounded-xl mb-1 ${
                  selectedSort === opt.id ? 'bg-primary-800' : ''
                }`}
                onPress={() => { setSelectedSort(opt.id); setShowSortMenu(false); }}
              >
                <Text className={`text-sm font-semibold ${selectedSort === opt.id ? 'text-gold' : 'text-forest'}`}>
                  {opt.label}
                </Text>
                {selectedSort === opt.id && <Ionicons name="checkmark" size={18} color="#D9B168" />}
              </TouchableOpacity>
            ))}
          </View>
        )}

        {/* Categories */}
        <View className="bg-white border-b border-sand-200" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 4, elevation: 2 }}>
          <ScrollView
            horizontal
            showsHorizontalScrollIndicator={false}
            contentContainerStyle={{ paddingHorizontal: 16, paddingVertical: 12 }}
          >
            {CATEGORIES.map((category, index) => (
              <Animated.View key={category.id} entering={FadeInRight.delay(index * 50).springify()}>
                <TouchableOpacity
                  onPress={() => setSelectedCategory(category.id)}
                  className={`mr-3 px-5 py-2.5 rounded-full flex-row items-center ${
                    selectedCategory === category.id ? 'bg-primary-800' : 'bg-sand-100'
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
                    selectedCategory === category.id ? 'text-gold' : 'text-primary-800'
                  }`}>
                    {category.label}
                  </Text>
                </TouchableOpacity>
              </Animated.View>
            ))}
          </ScrollView>
        </View>

        {/* Active filter indicator */}
        {hasActiveFilters && (
          <View className="flex-row items-center justify-between px-4 py-2 bg-sand-100">
            <Text className="text-xs text-moss">
              {properties.length} {properties.length === 1 ? 'property' : 'properties'} found
              {activeSort?.id !== 'default' ? ` · ${activeSort?.label}` : ''}
            </Text>
            <TouchableOpacity
              onPress={() => { clearSearch(); setSelectedCategory('all'); setSelectedSort('default'); }}
              className="flex-row items-center"
            >
              <Text className="text-xs font-semibold text-primary-800 mr-1">Clear all</Text>
              <Ionicons name="close-circle" size={14} color="#3A5C50" />
            </TouchableOpacity>
          </View>
        )}

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
            refreshControl={
              <RefreshControl
                refreshing={isRefetching}
                onRefresh={() => refetch()}
                tintColor="#D9B168"
                colors={['#D9B168', '#122F26']}
              />
            }
            ListHeaderComponent={
              featuredSections.length > 0 ? (
                <View className="pt-2">
                  {featuredSections.map((section) => (
                    <View key={section.id} className="mb-6">
                      <View className="flex-row items-center justify-between px-4 mb-3">
                        <Text className="text-lg font-bold text-forest">{section.title}</Text>
                        <TouchableOpacity onPress={() => handleSearchChange(section.city)}>
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
                  <Text className="text-primary-600 text-center text-sm mb-4">
                    Try adjusting your search or explore different categories
                  </Text>
                  {hasActiveFilters && (
                    <TouchableOpacity
                      onPress={() => { clearSearch(); setSelectedCategory('all'); setSelectedSort('default'); }}
                      className="bg-primary-800 px-6 py-3 rounded-full"
                    >
                      <Text className="text-gold font-semibold text-sm">Clear Filters</Text>
                    </TouchableOpacity>
                  )}
                </View>
              </View>
            }
          />
        )}
      </View>
    </SafeAreaView>
  );
}
