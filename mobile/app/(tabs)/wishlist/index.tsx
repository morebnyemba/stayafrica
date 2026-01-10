import { View, Text, FlatList, TouchableOpacity, ActivityIndicator } from 'react-native';
import { useRouter } from 'expo-router';
import { useState } from 'react';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { PropertyCard } from '@/components/property/PropertyCard';
import type { Property } from '@/types';

export default function WishlistScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const [wishlistProperties, setWishlistProperties] = useState<Property[]>([]);
  const [isLoading, setIsLoading] = useState(false);

  if (!isAuthenticated) {
    return (
      <View className="flex-1 bg-sand-100">
        {/* Header */}
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pt-12 pb-6"
        >
          <Text className="text-3xl font-black text-white tracking-tight">
            My Wishlist
          </Text>
          <Text className="text-sand-200 text-sm mt-1">
            Save your favorite properties
          </Text>
        </LinearGradient>

        {/* Empty State for Unauthenticated */}
        <View className="flex-1 items-center justify-center px-6">
          <View className="bg-white rounded-3xl p-8 items-center" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}>
            <View className="bg-sand-200 rounded-full p-8 mb-6">
              <Ionicons name="heart-outline" size={72} color="#D9B168" />
            </View>
            <Text className="text-2xl font-bold text-forest mb-3">Sign In to Save</Text>
            <Text className="text-moss text-center mb-8 px-4 leading-6">
              Create an account to save your favorite properties and access them anytime
            </Text>
            <TouchableOpacity
              onPress={() => router.push('/(auth)/login')}
            >
              <LinearGradient
                colors={['#D9B168', '#bea04f']}
                start={{ x: 0, y: 0 }}
                end={{ x: 1, y: 1 }}
                className="px-8 py-4 rounded-2xl"
                style={{
                  shadowColor: '#D9B168',
                  shadowOffset: { width: 0, height: 4 },
                  shadowOpacity: 0.3,
                  shadowRadius: 8,
                  elevation: 5,
                }}
              >
                <Text className="text-forest font-bold text-base">Sign In Now</Text>
              </LinearGradient>
            </TouchableOpacity>
          </View>
        </View>
      </View>
    );
  }

  const handlePropertyPress = (id: string) => {
    router.push(`/(tabs)/explore/${id}`);
  };

  const handleRemoveFromWishlist = (id: string) => {
    setWishlistProperties(prev => prev.filter(p => p.id !== id));
  };

  return (
    <View className="flex-1 bg-sand-100">
      {/* Modern Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="px-4 pt-12 pb-6"
      >
        <Text className="text-3xl font-black text-white tracking-tight mb-2">
          My Wishlist
        </Text>
        <View className="flex-row items-center">
          <Ionicons name="heart" size={16} color="#D9B168" />
          <Text className="text-sand-100 ml-2">
            {wishlistProperties.length} saved {wishlistProperties.length === 1 ? 'property' : 'properties'}
          </Text>
        </View>
      </LinearGradient>

      {/* Wishlist Content */}
      {isLoading ? (
        <View className="flex-1 justify-center items-center">
          <ActivityIndicator size="large" color="#D9B168" />
          <Text className="mt-4 text-primary-700 font-medium">Loading wishlist...</Text>
        </View>
      ) : wishlistProperties.length === 0 ? (
        <View className="flex-1 justify-center items-center px-6">
          <View className="bg-white rounded-3xl p-8 items-center" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}>
            <View className="bg-sand-200 rounded-full p-8 mb-6">
              <Ionicons name="heart-outline" size={72} color="#3A5C50" />
            </View>
            <Text className="text-2xl font-bold text-forest mb-3">Start Your Collection</Text>
            <Text className="text-moss text-center mb-8 px-4 leading-6">
              Explore amazing properties and tap the heart icon to save your favorites here
            </Text>
            <TouchableOpacity
              onPress={() => router.push('/(tabs)/explore')}
            >
              <LinearGradient
                colors={['#122F26', '#1d392f']}
                className="px-8 py-4 rounded-2xl"
                style={{
                  shadowColor: '#122F26',
                  shadowOffset: { width: 0, height: 4 },
                  shadowOpacity: 0.3,
                  shadowRadius: 8,
                  elevation: 5,
                }}
              >
                <Text className="text-gold font-bold text-base">Explore Properties</Text>
              </LinearGradient>
            </TouchableOpacity>
          </View>
        </View>
      ) : (
        <FlatList
          data={wishlistProperties}
          renderItem={({ item }) => (
            <View className="px-4">
              <PropertyCard
                property={item}
                onPress={handlePropertyPress}
                showRemoveButton={true}
                onRemove={() => handleRemoveFromWishlist(item.id)}
              />
            </View>
          )}
          keyExtractor={(item) => item.id}
          contentContainerStyle={{ paddingVertical: 12, paddingBottom: 40 }}
        />
      )}
    </View>
  );
}
