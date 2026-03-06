import {
  View,
  Text,
  FlatList,
  TextInput,
  TouchableOpacity,
  ScrollView,
  RefreshControl,
  Modal,
  Pressable,
  Dimensions,
} from 'react-native';
import { useRouter } from 'expo-router';
import { useCallback, useMemo, useRef, useState } from 'react';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import Animated, {
  FadeInRight,
  FadeInDown,
  FadeIn,
  SlideInDown,
  useSharedValue,
  useAnimatedStyle,
  withTiming,
} from 'react-native-reanimated';
import { useProperties, type PropertyFilters } from '@/hooks/api-hooks';
import { PropertyCard } from '@/components/property/PropertyCard';
import { PropertyCardGridSkeletonRow } from '@/components/common/Skeletons';
import { EmptyState } from '@/components/common/EmptyState';
import { Sidebar } from '@/components/common/Sidebar';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';

const SCREEN_WIDTH = Dimensions.get('window').width;
const GRID_GAP = 12;
const GRID_PADDING = 16;
const GRID_CARD_WIDTH = (SCREEN_WIDTH - GRID_PADDING * 2 - GRID_GAP) / 2;
const FEATURED_CARD_WIDTH = 200;

const CATEGORIES = [
  { id: 'all', label: 'All', icon: 'apps' as const, type: undefined },
  { id: 'villa', label: 'Villas', icon: 'home' as const, type: 'villa' },
  { id: 'apartment', label: 'Apartments', icon: 'business' as const, type: 'apartment' },
  { id: 'cabin', label: 'Cabins', icon: 'bed' as const, type: 'cabin' },
  { id: 'house', label: 'Houses', icon: 'home-outline' as const, type: 'house' },
  { id: 'lodge', label: 'Lodges', icon: 'leaf' as const, type: 'lodge' },
] as const;

const SORT_OPTIONS = [
  { id: 'default', label: 'Best Match', icon: 'sparkles' as const, value: undefined },
  { id: 'price_asc', label: 'Price: Low → High', icon: 'trending-down' as const, value: 'price_per_night' },
  { id: 'price_desc', label: 'Price: High → Low', icon: 'trending-up' as const, value: '-price_per_night' },
  { id: 'newest', label: 'Newest First', icon: 'time' as const, value: '-created_at' },
] as const;

export default function ExploreScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const [searchInput, setSearchInput] = useState('');
  const [debouncedSearch, setDebouncedSearch] = useState('');
  const [selectedCategory, setSelectedCategory] = useState('all');
  const [selectedSort, setSelectedSort] = useState('default');
  const [showSortSheet, setShowSortSheet] = useState(false);
  const [sidebarVisible, setSidebarVisible] = useState(false);
  const [isSearchFocused, setIsSearchFocused] = useState(false);
  const debounceRef = useRef<ReturnType<typeof setTimeout> | null>(null);
  const flatListRef = useRef<FlatList>(null);

  // Search bar focus animation
  const searchBorderOpacity = useSharedValue(0);
  const searchAnimStyle = useAnimatedStyle(() => ({
    borderWidth: 1.5,
    borderColor: `rgba(217, 177, 104, ${searchBorderOpacity.value})`,
  }));

  const handleSearchFocus = () => {
    setIsSearchFocused(true);
    searchBorderOpacity.value = withTiming(0.8, { duration: 200 });
  };

  const handleSearchBlur = () => {
    setIsSearchFocused(false);
    searchBorderOpacity.value = withTiming(0, { duration: 200 });
  };

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

  const clearAllFilters = useCallback(() => {
    clearSearch();
    setSelectedCategory('all');
    setSelectedSort('default');
  }, [clearSearch]);

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
    <SafeAreaView className="flex-1 bg-sand-50">
      <Sidebar isVisible={sidebarVisible} onClose={() => setSidebarVisible(false)} />

      <View className="flex-1 bg-sand-50">
        {/* ── Header ── */}
        <LinearGradient
          colors={['#122F26', '#1a3a2f', '#244438']}
          start={{ x: 0, y: 0 }}
          end={{ x: 1, y: 1 }}
          className="pb-4"
          style={{ paddingTop: insets.top + 4 }}
        >
          {/* Top row */}
          <View className="flex-row items-center justify-between px-4 mb-3">
            <TouchableOpacity
              onPress={() => setSidebarVisible(true)}
              accessibilityRole="button"
              accessibilityLabel="Open navigation menu"
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.12)' }}
            >
              <Ionicons name="menu" size={22} color="#fff" />
            </TouchableOpacity>

            <View className="items-center">
              <Text className="text-lg font-bold text-white tracking-wide">Explore</Text>
              {!isLoading && properties.length > 0 && (
                <Text className="text-[10px] text-white/50 font-medium mt-0.5">
                  {properties.length} {properties.length === 1 ? 'place' : 'places'}
                </Text>
              )}
            </View>

            <TouchableOpacity
              onPress={() => setShowSortSheet(true)}
              accessibilityRole="button"
              accessibilityLabel={`Sort properties. Current: ${activeSort?.label}`}
              accessibilityHint="Opens sort options"
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{
                backgroundColor: selectedSort !== 'default'
                  ? 'rgba(217, 177, 104, 0.25)'
                  : 'rgba(255, 255, 255, 0.12)',
              }}
            >
              <Ionicons
                name="swap-vertical"
                size={20}
                color={selectedSort !== 'default' ? '#D9B168' : '#fff'}
              />
            </TouchableOpacity>
          </View>

          {/* Search bar */}
          <View className="px-4">
            <Animated.View
              className="flex-row items-center px-4 py-2.5 rounded-2xl"
              style={[
                { backgroundColor: isSearchFocused ? 'rgba(255,255,255,0.15)' : 'rgba(255,255,255,0.1)' },
                searchAnimStyle,
              ]}
            >
              <Ionicons name="search" size={20} color={isSearchFocused ? '#D9B168' : 'rgba(255,255,255,0.6)'} />
              <TextInput
                className="flex-1 ml-3 text-base text-white"
                placeholder="Where are you going?"
                value={searchInput}
                onChangeText={handleSearchChange}
                onFocus={handleSearchFocus}
                onBlur={handleSearchBlur}
                placeholderTextColor="rgba(255, 255, 255, 0.45)"
                returnKeyType="search"
                accessibilityLabel="Search properties"
                accessibilityHint="Search by destination, city, or property name"
              />
              {searchInput.length > 0 && (
                <TouchableOpacity
                  onPress={clearSearch}
                  accessibilityRole="button"
                  accessibilityLabel="Clear search"
                  hitSlop={{ top: 8, bottom: 8, left: 8, right: 8 }}
                >
                  <Ionicons name="close-circle" size={20} color="rgba(255,255,255,0.5)" />
                </TouchableOpacity>
              )}
            </Animated.View>
          </View>
        </LinearGradient>

        {/* ── Sort Bottom Sheet Modal ── */}
        <Modal
          visible={showSortSheet}
          transparent
          animationType="none"
          statusBarTranslucent
          onRequestClose={() => setShowSortSheet(false)}
        >
          <Pressable
            className="flex-1 justify-end"
            style={{ backgroundColor: 'rgba(0,0,0,0.4)' }}
            onPress={() => setShowSortSheet(false)}
          >
            <Pressable onPress={(e) => e.stopPropagation()}>
              <Animated.View
                entering={SlideInDown.springify().damping(18)}
                className="bg-white rounded-t-3xl px-5 pb-8"
                style={{ paddingTop: 16 }}
              >
                {/* Handle */}
                <View className="items-center mb-4">
                  <View className="w-10 h-1 rounded-full bg-sand-300" />
                </View>
                <Text className="text-lg font-bold text-forest mb-4">Sort By</Text>

                {SORT_OPTIONS.map((opt) => {
                  const isActive = selectedSort === opt.id;
                  return (
                    <TouchableOpacity
                      key={opt.id}
                      className={`flex-row items-center justify-between py-3.5 px-4 rounded-2xl mb-2 ${
                        isActive ? 'bg-primary-800' : 'bg-sand-50'
                      }`}
                      accessibilityRole="radio"
                      accessibilityState={{ selected: isActive }}
                      accessibilityLabel={opt.label}
                      onPress={() => {
                        setSelectedSort(opt.id);
                        setShowSortSheet(false);
                      }}
                    >
                      <View className="flex-row items-center">
                        <Ionicons
                          name={opt.icon as any}
                          size={18}
                          color={isActive ? '#D9B168' : '#3A5C50'}
                        />
                        <Text
                          className={`ml-3 text-sm font-semibold ${
                            isActive ? 'text-gold' : 'text-forest'
                          }`}
                        >
                          {opt.label}
                        </Text>
                      </View>
                      {isActive && (
                        <View className="bg-gold/20 rounded-full p-1">
                          <Ionicons name="checkmark" size={16} color="#D9B168" />
                        </View>
                      )}
                    </TouchableOpacity>
                  );
                })}
              </Animated.View>
            </Pressable>
          </Pressable>
        </Modal>

        {/* ── Categories ── */}
        <View
          className="bg-white border-b border-sand-200"
          style={{
            shadowColor: '#000',
            shadowOffset: { width: 0, height: 1 },
            shadowOpacity: 0.04,
            shadowRadius: 3,
            elevation: 2,
          }}
        >
          <ScrollView
            horizontal
            showsHorizontalScrollIndicator={false}
            contentContainerStyle={{ paddingHorizontal: 16, paddingVertical: 10 }}
          >
            {CATEGORIES.map((category, index) => {
              const isActive = selectedCategory === category.id;
              return (
                <Animated.View key={category.id} entering={FadeInRight.delay(index * 40).springify()}>
                  <TouchableOpacity
                    onPress={() => setSelectedCategory(category.id)}
                    accessibilityRole="tab"
                    accessibilityState={{ selected: isActive }}
                    accessibilityLabel={`Filter by ${category.label}`}
                    className={`mr-2.5 px-4 py-2 rounded-full flex-row items-center ${
                      isActive ? 'bg-primary-800' : 'bg-sand-100'
                    }`}
                    style={{
                      shadowColor: isActive ? '#122F26' : 'transparent',
                      shadowOffset: { width: 0, height: 2 },
                      shadowOpacity: isActive ? 0.15 : 0,
                      shadowRadius: 4,
                      elevation: isActive ? 3 : 0,
                    }}
                  >
                    <Ionicons
                      name={category.icon as any}
                      size={16}
                      color={isActive ? '#D9B168' : '#3A5C50'}
                    />
                    <Text
                      className={`ml-1.5 font-semibold text-[13px] ${
                        isActive ? 'text-gold' : 'text-primary-700'
                      }`}
                    >
                      {category.label}
                    </Text>
                  </TouchableOpacity>
                </Animated.View>
              );
            })}
          </ScrollView>
        </View>

        {/* ── Active filter bar ── */}
        {hasActiveFilters && (
          <Animated.View
            entering={FadeIn.duration(200)}
            className="flex-row items-center justify-between px-4 py-2 bg-sand-100 border-b border-sand-200"
          >
            <Text className="text-xs text-moss font-medium">
              {isLoading ? 'Searching…' : (
                <>
                  {properties.length} {properties.length === 1 ? 'property' : 'properties'}
                  {activeSort?.id !== 'default' ? ` · ${activeSort?.label}` : ''}
                </>
              )}
            </Text>
            <TouchableOpacity
              onPress={clearAllFilters}
              accessibilityRole="button"
              accessibilityLabel="Clear all filters"
              className="flex-row items-center bg-white px-3 py-1.5 rounded-full"
              style={{
                shadowColor: '#000',
                shadowOffset: { width: 0, height: 1 },
                shadowOpacity: 0.06,
                shadowRadius: 2,
                elevation: 1,
              }}
            >
              <Text className="text-xs font-semibold text-primary-800 mr-1">Clear all</Text>
              <Ionicons name="close" size={12} color="#3A5C50" />
            </TouchableOpacity>
          </Animated.View>
        )}

        {/* ── Content ── */}
        {isLoading ? (
          <Animated.View entering={FadeIn.duration(300)} className="pt-4">
            {[1, 2, 3].map((i) => (
              <PropertyCardGridSkeletonRow key={i} />
            ))}
          </Animated.View>
        ) : (
          <FlatList
            ref={flatListRef}
            data={gridProperties}
            numColumns={2}
            renderItem={({ item, index }) => (
              <View style={{ width: GRID_CARD_WIDTH, marginBottom: GRID_GAP }}>
                <PropertyCard
                  property={item}
                  onPress={handlePropertyPress}
                  variant="grid"
                  index={index}
                />
              </View>
            )}
            keyExtractor={(item) => item.id}
            columnWrapperStyle={{
              paddingHorizontal: GRID_PADDING,
              gap: GRID_GAP,
            }}
            contentContainerStyle={{
              paddingTop: 12,
              paddingBottom: insets.bottom + 60,
            }}
            refreshControl={
              <RefreshControl
                refreshing={isRefetching}
                onRefresh={() => refetch()}
                tintColor="#D9B168"
                colors={['#D9B168', '#122F26']}
                progressViewOffset={8}
              />
            }
            ListHeaderComponent={
              featuredSections.length > 0 ? (
                <View className="mb-2">
                  {featuredSections.map((section, sIdx) => (
                    <Animated.View
                      key={section.id}
                      entering={FadeInDown.delay(sIdx * 100).springify()}
                      className="mb-5"
                    >
                      <View className="flex-row items-center justify-between px-4 mb-3">
                        <View className="flex-row items-center">
                          <Ionicons name="flame" size={16} color="#D9B168" />
                          <Text className="text-base font-bold text-forest ml-1.5">
                            {section.title}
                          </Text>
                        </View>
                        <TouchableOpacity
                          onPress={() => handleSearchChange(section.city)}
                          accessibilityRole="button"
                          accessibilityLabel={`View more properties in ${section.city}`}
                          className="flex-row items-center"
                        >
                          <Text className="text-xs font-semibold text-primary-600 mr-0.5">
                            See all
                          </Text>
                          <Ionicons name="chevron-forward" size={14} color="#3A5C50" />
                        </TouchableOpacity>
                      </View>
                      <FlatList
                        horizontal
                        showsHorizontalScrollIndicator={false}
                        data={section.data}
                        keyExtractor={(item) => `${section.id}-${item.id}`}
                        contentContainerStyle={{ paddingHorizontal: 16 }}
                        snapToInterval={FEATURED_CARD_WIDTH + 12}
                        decelerationRate="fast"
                        renderItem={({ item, index }) => (
                          <View style={{ width: FEATURED_CARD_WIDTH }} className="mr-3">
                            <PropertyCard
                              property={item}
                              onPress={handlePropertyPress}
                              variant="compact"
                              index={index}
                            />
                          </View>
                        )}
                      />
                    </Animated.View>
                  ))}

                  {/* Divider between featured and grid */}
                  <View className="flex-row items-center px-4 mb-2 mt-1">
                    <View className="flex-1 h-px bg-sand-200" />
                    <Text className="text-[11px] font-semibold text-sand-500 mx-3 uppercase tracking-wider">
                      All Properties
                    </Text>
                    <View className="flex-1 h-px bg-sand-200" />
                  </View>
                </View>
              ) : null
            }
            ListEmptyComponent={
              <EmptyState
                icon={hasActiveFilters ? 'filter' : 'compass'}
                title={hasActiveFilters ? 'No matches found' : 'No properties yet'}
                description={
                  hasActiveFilters
                    ? 'Try broadening your search or clearing filters to see more results.'
                    : 'New stays are being added all the time. Pull down to refresh.'
                }
                actionLabel={hasActiveFilters ? 'Clear Filters' : undefined}
                onAction={hasActiveFilters ? clearAllFilters : undefined}
                variant="compact"
              />
            }
          />
        )}
      </View>
    </SafeAreaView>
  );
}
