import { View, Text, FlatList, TouchableOpacity, ActivityIndicator } from 'react-native';
import { useRouter } from 'expo-router';
import { useState } from 'react';
import { Ionicons } from '@expo/vector-icons';
import { useAuth } from '@/context/auth-context';
import { PropertyCard } from '@/components/property/PropertyCard';

export default function WishlistScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const [wishlistProperties, setWishlistProperties] = useState<any[]>([]);
  const [isLoading, setIsLoading] = useState(false);

  if (!isAuthenticated) {
    return (
      <View className="flex-1 items-center justify-center bg-white px-6">
        <Ionicons name="heart-outline" size={64} color="#ddd" />
        <Text className="text-xl font-bold text-gray-800 mt-4">Your Wishlist is Empty</Text>
        <Text className="text-gray-600 text-center mt-2 mb-6">
          Sign in to save your favorite properties
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

  const handlePropertyPress = (id: string) => {
    router.push(`/(tabs)/explore/${id}`);
  };

  const handleRemoveFromWishlist = (id: string) => {
    // Remove property from wishlist
    setWishlistProperties(prev => prev.filter(p => p.id !== id));
  };

  return (
    <View className="flex-1 bg-gray-50">
      {/* Header */}
      <View className="bg-white px-4 pt-12 pb-4 border-b border-gray-100 shadow-sm">
        <Text className="text-3xl font-extrabold text-primary-900 mb-2 tracking-tight">
          My Wishlist
        </Text>
        <Text className="text-base text-gray-500">
          {wishlistProperties.length} saved {wishlistProperties.length === 1 ? 'property' : 'properties'}
        </Text>
      </View>

      {/* Wishlist Content */}
      {isLoading ? (
        <View className="flex-1 justify-center items-center">
          <ActivityIndicator size="large" color="#3A5C50" />
          <Text className="mt-2 text-gray-600">Loading wishlist...</Text>
        </View>
      ) : wishlistProperties.length === 0 ? (
        <View className="flex-1 justify-center items-center px-6">
          <Ionicons name="heart-outline" size={64} color="#ddd" />
          <Text className="text-xl font-bold text-gray-800 mt-4">No Saved Properties</Text>
          <Text className="text-gray-600 text-center mt-2 mb-6">
            Start exploring and tap the heart icon to save your favorite properties
          </Text>
          <TouchableOpacity
            className="bg-primary-600 px-8 py-3 rounded-lg"
            onPress={() => router.push('/(tabs)/explore')}
          >
            <Text className="text-white font-semibold">Explore Properties</Text>
          </TouchableOpacity>
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
          contentContainerStyle={{ paddingVertical: 12 }}
        />
      )}
    </View>
  );
}
