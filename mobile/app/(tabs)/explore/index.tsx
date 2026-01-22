import { View, Text, FlatList, TextInput, ActivityIndicator, TouchableOpacity, ScrollView, Platform } from 'react-native';
import { useRouter } from 'expo-router';
import { useState } from 'react';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import Animated, { FadeInRight } from 'react-native-reanimated';
import { useProperties } from '@/hooks/api-hooks';
import { PropertyCard } from '@/components/property/PropertyCard';
import { PropertyCardSkeleton } from '@/components/common/Skeletons';
import { Avatar } from '@/components/common/Avatar';
import { Sidebar } from '@/components/common/Sidebar';
import { useAuth } from '@/context/auth-context';

const CATEGORIES = [
  { id: 'all', label: 'All', icon: 'apps' },
  { id: 'villa', label: 'Villas', icon: 'home' },
  { id: 'apartment', label: 'Apartments', icon: 'business' },
  { id: 'cabin', label: 'Cabins', icon: 'log-cabin' },
  { id: 'beach', label: 'Beach', icon: 'water' },
  { id: 'safari', label: 'Safari', icon: 'leaf' },
];

export default function ExploreScreen() {
  const router = useRouter();
  const [searchQuery, setSearchQuery] = useState('');
  const [selectedCategory, setSelectedCategory] = useState('all');
  const [sidebarVisible, setSidebarVisible] = useState(false);
  const { data: propertiesData, isLoading } = useProperties();
  const { user, isAuthenticated } = useAuth();
  const properties = propertiesData?.results || [];
  
  const filteredProperties = properties.filter((property: any) => {
    const matchesSearch = property.title.toLowerCase().includes(searchQuery.toLowerCase()) ||
      property.location?.city?.toLowerCase().includes(searchQuery.toLowerCase());
    return matchesSearch;
  });

  const handleAvatarPress = () => {
    if (isAuthenticated) {
      router.push('/(tabs)/profile');
    } else {
      router.push('/(auth)/login');
    }
  };

  const handlePropertyPress = (id: string) => {
    router.push(`/(tabs)/explore/${id}`);
  };

  return (
    <View className="flex-1 bg-sand-100">
      {/* Sidebar */}
      <Sidebar
        isVisible={sidebarVisible}
        onClose={() => setSidebarVisible(false)}
      />

      {/* Modern Header with Gradient */}
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="px-4 pb-6"
        style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
      >
        {/* Top Navigation Bar with Menu and Avatar */}
        <View className="flex-row items-center justify-between mb-4">
          {/* Hamburger Menu */}
          <TouchableOpacity
            onPress={() => setSidebarVisible(true)}
            className="w-10 h-10 rounded-xl items-center justify-center"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="menu" size={24} color="#fff" />
          </TouchableOpacity>

          {/* Avatar for Auth Options */}
          <Avatar
            uri={null}
            firstName={user?.first_name}
            lastName={user?.last_name}
            size="small"
            onPress={handleAvatarPress}
            showBadge={isAuthenticated}
          />
        </View>

        {/* Title Section */}
        <View className="mb-5">
          <Text className="text-4xl font-black text-white tracking-tight mb-2">
            Discover Africa
          </Text>
          <View className="flex-row items-center">
            <Ionicons name="location" size={16} color="#D9B168" />
            <Text className="text-base text-sand-100 ml-2">
              {properties.length} unique stays waiting for you
            </Text>
          </View>
        </View>

        {/* Modern Search Bar */}
        <View className="relative">
          <LinearGradient
            colors={['rgba(255, 255, 255, 0.15)', 'rgba(255, 255, 255, 0.08)']}
            start={{ x: 0, y: 0 }}
            end={{ x: 1, y: 1 }}
            className="rounded-2xl overflow-hidden"
            style={{
              shadowColor: '#000',
              shadowOffset: { width: 0, height: 4 },
              shadowOpacity: 0.2,
              shadowRadius: 8,
              elevation: 5,
            }}
          >
            <View className="flex-row items-center px-4 py-4 border border-white/20">
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
          </LinearGradient>
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
          data={filteredProperties}
          renderItem={({ item }) => (
            <View className="px-4 py-2">
              <PropertyCard property={item} onPress={handlePropertyPress} />
            </View>
          )}
          keyExtractor={(item) => item.id}
          contentContainerStyle={{ paddingVertical: 12, paddingBottom: 40 }}
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
  );
}
