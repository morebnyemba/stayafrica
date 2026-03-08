import { View, Text, FlatList, TouchableOpacity, Dimensions } from 'react-native';
import { useRouter } from 'expo-router';
import { useState, useCallback } from 'react';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { PropertyCard } from '@/components/property/PropertyCard';
import { Skeleton } from '@/components/common/Skeletons';
import type { Property } from '@/types';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { Sidebar } from '@/components/common/Sidebar';
import { useWishlist, useRemoveFromWishlist } from '@/hooks/api-hooks';
import Animated, { FadeInDown } from 'react-native-reanimated';

const { width: SCREEN_WIDTH } = Dimensions.get('window');
const GRID_CARD_WIDTH = (SCREEN_WIDTH - 48) / 2;

export default function WishlistScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const { isAuthenticated } = useAuth();
  const [sidebarVisible, setSidebarVisible] = useState(false);
  const [viewMode, setViewMode] = useState<'list' | 'grid'>('list');

  const { data: wishlistData, isLoading } = useWishlist();
  const removeFromWishlist = useRemoveFromWishlist();
  const wishlistProperties: Property[] = wishlistData?.results || [];

  const handlePropertyPress = useCallback((id: string) => {
    router.push(`/(tabs)/explore/${id}`);
  }, [router]);

  const handleRemoveFromWishlist = useCallback((id: string) => {
    removeFromWishlist.mutate(id);
  }, [removeFromWishlist]);

  if (!isAuthenticated) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100">
        <Sidebar isVisible={sidebarVisible} onClose={() => setSidebarVisible(false)} />
        <LinearGradient colors={['#122F26', '#1d392f']} className="px-4 pb-6" style={{ paddingTop: insets.top + 12 }}>
          <View className="flex-row items-center justify-between mb-4">
            <TouchableOpacity
              onPress={() => setSidebarVisible(true)}
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="menu" size={24} color="#fff" />
            </TouchableOpacity>
          </View>
          <Text className="text-3xl font-black text-white tracking-tight">My Wishlist</Text>
          <Text className="text-sand-200 text-sm mt-1">Save your favorite properties</Text>
        </LinearGradient>
        <View className="flex-1 items-center justify-center px-6">
          <View className="bg-white rounded-3xl p-8 items-center" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}>
            <View className="bg-sand-200 rounded-full p-8 mb-6">
              <Ionicons name="heart-outline" size={72} color="#D9B168" />
            </View>
            <Text className="text-2xl font-bold text-forest mb-3">Sign In to Save</Text>
            <Text className="text-moss text-center mb-8 px-4 leading-6">Create an account to save your favorite properties</Text>
            <View className="flex-row gap-3">
              <TouchableOpacity className="flex-1" onPress={() => router.push('/(auth)/login')}>
                <LinearGradient colors={['#D9B168', '#bea04f']} className="px-6 py-4 rounded-2xl items-center" style={{ shadowColor: '#D9B168', shadowOffset: { width: 0, height: 4 }, shadowOpacity: 0.3, shadowRadius: 8, elevation: 5 }}>
                  <Text className="text-forest font-bold text-base">Log In</Text>
                </LinearGradient>
              </TouchableOpacity>
              <TouchableOpacity className="flex-1" onPress={() => router.push('/(auth)/register')}>
                <LinearGradient colors={['#122F26', '#1d392f']} className="px-6 py-4 rounded-2xl items-center" style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 4 }, shadowOpacity: 0.3, shadowRadius: 8, elevation: 5 }}>
                  <Text className="text-gold font-bold text-base">Sign Up</Text>
                </LinearGradient>
              </TouchableOpacity>
            </View>
          </View>
        </View>
      </SafeAreaView>
    );
  }

  // Skeleton loading
  const WishlistSkeleton = () => (
    <View className="px-4 pt-3">
      {viewMode === 'list' ? (
        [1, 2, 3].map((i) => (
          <View key={i} className="bg-white rounded-2xl mb-3 overflow-hidden" style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 4, elevation: 2 }}>
            <Skeleton height={180} width="100%" borderRadius={0} />
            <View className="p-3">
              <Skeleton height={16} width="70%" className="mb-2" />
              <Skeleton height={12} width="50%" className="mb-2" />
              <Skeleton height={14} width="30%" />
            </View>
          </View>
        ))
      ) : (
        <View className="flex-row flex-wrap" style={{ gap: 12 }}>
          {[1, 2, 3, 4].map((i) => (
            <View key={i} className="bg-white rounded-2xl overflow-hidden" style={{ width: GRID_CARD_WIDTH, shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 4, elevation: 2 }}>
              <Skeleton height={120} width="100%" borderRadius={0} />
              <View className="p-2">
                <Skeleton height={14} width="70%" className="mb-1" />
                <Skeleton height={12} width="40%" />
              </View>
            </View>
          ))}
        </View>
      )}
    </View>
  );

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      <Sidebar isVisible={sidebarVisible} onClose={() => setSidebarVisible(false)} />

      <View className="flex-1 bg-sand-100">
        {/* Header */}
        <LinearGradient
          colors={['#122F26', '#1d392f', '#2d4a40']}
          start={{ x: 0, y: 0 }}
          end={{ x: 1, y: 1 }}
          className="px-4 pb-5"
          style={{ paddingTop: insets.top + 12 }}
        >
          <View className="flex-row items-center justify-between mb-3">
            <TouchableOpacity
              onPress={() => setSidebarVisible(true)}
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
              accessibilityRole="button"
              accessibilityLabel="Open menu"
            >
              <Ionicons name="menu" size={24} color="#fff" />
            </TouchableOpacity>

            {/* View toggle */}
            {wishlistProperties.length > 0 && (
              <View className="flex-row bg-white/12 rounded-lg p-0.5">
                <TouchableOpacity
                  onPress={() => setViewMode('list')}
                  className={`px-3 py-1.5 rounded-md ${viewMode === 'list' ? 'bg-gold' : ''}`}
                  accessibilityRole="tab"
                  accessibilityState={{ selected: viewMode === 'list' }}
                >
                  <Ionicons name="list" size={16} color={viewMode === 'list' ? '#122F26' : '#fff'} />
                </TouchableOpacity>
                <TouchableOpacity
                  onPress={() => setViewMode('grid')}
                  className={`px-3 py-1.5 rounded-md ${viewMode === 'grid' ? 'bg-gold' : ''}`}
                  accessibilityRole="tab"
                  accessibilityState={{ selected: viewMode === 'grid' }}
                >
                  <Ionicons name="grid" size={16} color={viewMode === 'grid' ? '#122F26' : '#fff'} />
                </TouchableOpacity>
              </View>
            )}
          </View>

          <Text className="text-2xl font-black text-white tracking-tight mb-1">My Wishlist</Text>
          <View className="flex-row items-center">
            <Ionicons name="heart" size={14} color="#D9B168" />
            <Text className="text-sand-200 text-sm ml-1.5">
              {wishlistProperties.length} saved {wishlistProperties.length === 1 ? 'property' : 'properties'}
            </Text>
          </View>
        </LinearGradient>

        {/* Content */}
        {isLoading ? (
          <WishlistSkeleton />
        ) : wishlistProperties.length === 0 ? (
          <View className="flex-1 justify-center items-center px-6">
            <Animated.View
              entering={FadeInDown.duration(400)}
              className="bg-white rounded-3xl p-8 items-center"
              style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}
            >
              <View className="bg-sand-200 rounded-full p-8 mb-6">
                <Ionicons name="heart-outline" size={72} color="#3A5C50" />
              </View>
              <Text className="text-2xl font-bold text-forest mb-3">Start Your Collection</Text>
              <Text className="text-moss text-center mb-8 px-4 leading-6">
                Explore amazing properties and tap the heart icon to save your favorites
              </Text>
              <TouchableOpacity onPress={() => router.push('/(tabs)/explore')}>
                <LinearGradient
                  colors={['#122F26', '#1d392f']}
                  className="px-8 py-4 rounded-2xl"
                  style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 4 }, shadowOpacity: 0.3, shadowRadius: 8, elevation: 5 }}
                >
                  <Text className="text-gold font-bold text-base">Explore Properties</Text>
                </LinearGradient>
              </TouchableOpacity>
            </Animated.View>
          </View>
        ) : viewMode === 'grid' ? (
          <FlatList
            data={wishlistProperties}
            numColumns={2}
            columnWrapperStyle={{ paddingHorizontal: 16, gap: 12 }}
            renderItem={({ item, index }) => (
              <Animated.View entering={FadeInDown.delay(index * 60).duration(350)} style={{ width: GRID_CARD_WIDTH }}>
                <PropertyCard
                  property={item}
                  variant="grid"
                  onPress={handlePropertyPress}
                  showRemoveButton={true}
                  onRemove={() => handleRemoveFromWishlist(item.id)}
                />
              </Animated.View>
            )}
            keyExtractor={(item) => item.id}
            contentContainerStyle={{ paddingTop: 12, paddingBottom: 40 }}
          />
        ) : (
          <FlatList
            data={wishlistProperties}
            renderItem={({ item, index }) => (
              <Animated.View entering={FadeInDown.delay(index * 60).duration(350)} className="px-4">
                <PropertyCard
                  property={item}
                  onPress={handlePropertyPress}
                  showRemoveButton={true}
                  onRemove={() => handleRemoveFromWishlist(item.id)}
                />
              </Animated.View>
            )}
            keyExtractor={(item) => item.id}
            contentContainerStyle={{ paddingTop: 12, paddingBottom: 40 }}
          />
        )}
      </View>
    </SafeAreaView>
  );
}
