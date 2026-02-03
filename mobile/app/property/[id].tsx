import { View, Text, ScrollView, TouchableOpacity, ActivityIndicator, Image, Dimensions } from 'react-native';
import { useLocalSearchParams, useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { useSafeAreaInsets } from 'react-native-safe-area-context';
import { useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';

const SCREEN_WIDTH = Dimensions.get('window').width;

export default function PropertyDetailScreen() {
  const router = useRouter();
  const { id } = useLocalSearchParams<{ id: string }>();
  const { isAuthenticated } = useAuth();
  const insets = useSafeAreaInsets();
  const [isWishlisted, setIsWishlisted] = useState(false);

  // Fetch property details
  const { data: property, isLoading, error } = useQuery({
    queryKey: ['property', id],
    queryFn: async () => {
      const response = await apiClient.getPropertyDetails(id as string);
      return response.data;
    },
    enabled: !!id,
  });

  // Fetch property reviews
  const { data: reviewsData } = useQuery({
    queryKey: ['property-reviews', id],
    queryFn: async () => {
      const response = await apiClient.getPropertyReviews(id as string);
      return response.data;
    },
    enabled: !!id,
  });

  const reviews = reviewsData?.reviews || [];
  const averageRating = reviewsData?.average_rating || 0;
  const totalReviews = reviewsData?.count || 0;

  const handleWishlist = () => {
    setIsWishlisted(!isWishlisted);
    // TODO: Call API to add/remove from wishlist
  };

  const handleBookNow = () => {
    if (!isAuthenticated) {
      router.push('/(auth)/login');
      return;
    }
    router.push(`/booking/confirm?propertyId=${id}`);
  };

  const AmenityItem = ({ icon, label }: { icon: string; label: string }) => (
    <View className="flex-row items-center mb-3">
      <View className="bg-sand-200 rounded-full p-2 mr-3">
        <Ionicons name={icon as any} size={16} color="#3A5C50" />
      </View>
      <Text className="text-sm text-moss">{label}</Text>
    </View>
  );

  const ReviewCard = ({ review }: any) => (
    <View
      className="bg-white rounded-2xl p-4 mb-3"
      style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 2 },
        shadowOpacity: 0.05,
        shadowRadius: 4,
        elevation: 2,
      }}
    >
      <View className="flex-row items-center mb-2">
        <View className="bg-sand-200 rounded-full w-10 h-10 items-center justify-center mr-3">
          <Text className="text-forest font-bold">
            {review.guest_name?.charAt(0) || 'G'}
          </Text>
        </View>
        <View className="flex-1">
          <Text className="text-sm font-bold text-forest">{review.guest_name}</Text>
          <View className="flex-row items-center">
            {[...Array(5)].map((_, i) => (
              <Ionicons
                key={i}
                name={i < review.rating ? 'star' : 'star-outline'}
                size={12}
                color="#F59E0B"
              />
            ))}
          </View>
        </View>
        <Text className="text-xs text-moss">
          {new Date(review.created_at).toLocaleDateString()}
        </Text>
      </View>
      <Text className="text-sm text-moss">{review.comment}</Text>
    </View>
  );

  if (isLoading) {
    return (
      <View className="flex-1 bg-sand-100 items-center justify-center">
        <ActivityIndicator size="large" color="#D9B168" />
        <Text className="text-moss mt-4">Loading property...</Text>
      </View>
    );
  }

  if (error || !property) {
    return (
      <View className="flex-1 bg-sand-100">
        <View
          className="px-4"
          style={{ paddingTop: insets.top + 12 }}
        >
          <TouchableOpacity
            onPress={() => router.back()}
            className="w-10 h-10 rounded-xl items-center justify-center mb-4"
            style={{ backgroundColor: 'rgba(18, 47, 38, 0.15)' }}
          >
            <Ionicons name="arrow-back" size={24} color="#122F26" />
          </TouchableOpacity>
        </View>
        <View className="flex-1 items-center justify-center px-6">
          <View className="bg-white rounded-3xl p-8 items-center" style={{
            shadowColor: '#000',
            shadowOffset: { width: 0, height: 8 },
            shadowOpacity: 0.1,
            shadowRadius: 16,
            elevation: 8
          }}>
            <View className="bg-red-100 rounded-full p-6 mb-4">
              <Ionicons name="alert-circle" size={48} color="#EF4444" />
            </View>
            <Text className="text-xl font-bold text-forest mb-2">Property Not Found</Text>
            <Text className="text-moss text-center mb-6">
              Unable to load property details.
            </Text>
            <TouchableOpacity onPress={() => router.back()}>
              <LinearGradient
                colors={['#D9B168', '#bea04f']}
                className="px-8 py-4 rounded-2xl"
              >
                <Text className="text-forest font-bold">Go Back</Text>
              </LinearGradient>
            </TouchableOpacity>
          </View>
        </View>
      </View>
    );
  }

  return (
    <View className="flex-1 bg-sand-100">
      <ScrollView showsVerticalScrollIndicator={false}>
        {/* Image Gallery */}
        <View className="relative">
          {property.images && property.images.length > 0 ? (
            <Image
              source={{ uri: property.images[0] }}
              style={{ width: SCREEN_WIDTH, height: 300 }}
              resizeMode="cover"
            />
          ) : (
            <View
              className="bg-sand-200 items-center justify-center"
              style={{ width: SCREEN_WIDTH, height: 300 }}
            >
              <Ionicons name="home-outline" size={64} color="#3A5C50" />
            </View>
          )}

          {/* Header Overlay */}
          <View
            className="absolute top-0 left-0 right-0 flex-row items-center justify-between px-4"
            style={{ paddingTop: insets.top + 12 }}
          >
            <TouchableOpacity
              onPress={() => router.back()}
              className="w-10 h-10 rounded-full items-center justify-center bg-white/90"
            >
              <Ionicons name="arrow-back" size={24} color="#122F26" />
            </TouchableOpacity>
            <View className="flex-row">
              <TouchableOpacity
                onPress={handleWishlist}
                className="w-10 h-10 rounded-full items-center justify-center bg-white/90 mr-2"
              >
                <Ionicons
                  name={isWishlisted ? 'heart' : 'heart-outline'}
                  size={24}
                  color={isWishlisted ? '#EF4444' : '#122F26'}
                />
              </TouchableOpacity>
              <TouchableOpacity className="w-10 h-10 rounded-full items-center justify-center bg-white/90">
                <Ionicons name="share-outline" size={24} color="#122F26" />
              </TouchableOpacity>
            </View>
          </View>
        </View>

        {/* Property Info */}
        <View className="px-4 py-6">
          {/* Title and Price */}
          <View className="mb-4">
            <View className="flex-row items-start justify-between mb-2">
              <Text className="text-2xl font-black text-forest flex-1 mr-4">
                {property.title}
              </Text>
              <View>
                <Text className="text-2xl font-black text-forest">
                  ${property.price_per_night}
                </Text>
                <Text className="text-xs text-moss text-right">per night</Text>
              </View>
            </View>
            <View className="flex-row items-center">
              <Ionicons name="location" size={16} color="#6B8E7F" />
              <Text className="text-sm text-moss ml-1">
                {property.city}, {property.country}
              </Text>
            </View>
            {totalReviews > 0 && (
              <View className="flex-row items-center mt-2">
                <Ionicons name="star" size={16} color="#F59E0B" />
                <Text className="text-sm font-bold text-forest ml-1">
                  {averageRating.toFixed(1)}
                </Text>
                <Text className="text-sm text-moss ml-1">
                  ({totalReviews} {totalReviews === 1 ? 'review' : 'reviews'})
                </Text>
              </View>
            )}
          </View>

          {/* Quick Stats */}
          <View
            className="bg-white rounded-2xl p-4 mb-4 flex-row justify-around"
            style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 4 },
              shadowOpacity: 0.08,
              shadowRadius: 8,
              elevation: 4,
            }}
          >
            <View className="items-center">
              <Ionicons name="bed-outline" size={24} color="#3A5C50" />
              <Text className="text-xs text-moss mt-1">{property.bedrooms} Beds</Text>
            </View>
            <View className="items-center">
              <Ionicons name="water-outline" size={24} color="#3A5C50" />
              <Text className="text-xs text-moss mt-1">{property.bathrooms} Baths</Text>
            </View>
            <View className="items-center">
              <Ionicons name="people-outline" size={24} color="#3A5C50" />
              <Text className="text-xs text-moss mt-1">{property.max_guests} Guests</Text>
            </View>
          </View>

          {/* Description */}
          <View className="mb-4">
            <Text className="text-lg font-bold text-forest mb-2">About this place</Text>
            <View
              className="bg-white rounded-2xl p-4"
              style={{
                shadowColor: '#122F26',
                shadowOffset: { width: 0, height: 4 },
                shadowOpacity: 0.08,
                shadowRadius: 8,
                elevation: 4,
              }}
            >
              <Text className="text-sm text-moss leading-6">{property.description}</Text>
            </View>
          </View>

          {/* Amenities */}
          {property.amenities && property.amenities.length > 0 && (
            <View className="mb-4">
              <Text className="text-lg font-bold text-forest mb-2">Amenities</Text>
              <View
                className="bg-white rounded-2xl p-4"
                style={{
                  shadowColor: '#122F26',
                  shadowOffset: { width: 0, height: 4 },
                  shadowOpacity: 0.08,
                  shadowRadius: 8,
                  elevation: 4,
                }}
              >
                {property.amenities.slice(0, 6).map((amenity: string, index: number) => (
                  <AmenityItem
                    key={index}
                    icon="checkmark-circle"
                    label={amenity}
                  />
                ))}
              </View>
            </View>
          )}

          {/* Host Info */}
          {property.host && (
            <View className="mb-4">
              <Text className="text-lg font-bold text-forest mb-2">Your Host</Text>
              <View
                className="bg-white rounded-2xl p-4 flex-row items-center"
                style={{
                  shadowColor: '#122F26',
                  shadowOffset: { width: 0, height: 4 },
                  shadowOpacity: 0.08,
                  shadowRadius: 8,
                  elevation: 4,
                }}
              >
                <View className="bg-sand-200 rounded-full w-16 h-16 items-center justify-center mr-4">
                  <Text className="text-forest font-bold text-2xl">
                    {property.host.first_name?.charAt(0)}
                  </Text>
                </View>
                <View className="flex-1">
                  <Text className="text-base font-bold text-forest">
                    {property.host.first_name} {property.host.last_name}
                  </Text>
                  <Text className="text-sm text-moss">Host since {new Date(property.host.date_joined).getFullYear()}</Text>
                </View>
                <TouchableOpacity
                  onPress={() => router.push(`/(tabs)/messages?hostId=${property.host.id}`)}
                  className="bg-sand-200 rounded-full p-2"
                >
                  <Ionicons name="chatbubble-outline" size={20} color="#3A5C50" />
                </TouchableOpacity>
              </View>
            </View>
          )}

          {/* Reviews */}
          {reviews.length > 0 && (
            <View className="mb-4">
              <View className="flex-row items-center justify-between mb-2">
                <Text className="text-lg font-bold text-forest">Reviews</Text>
                {reviews.length > 3 && (
                  <TouchableOpacity>
                    <Text className="text-sm text-gold font-semibold">See all</Text>
                  </TouchableOpacity>
                )}
              </View>
              {reviews.slice(0, 3).map((review: any) => (
                <ReviewCard key={review.id} review={review} />
              ))}
            </View>
          )}
        </View>
      </ScrollView>

      {/* Booking Footer */}
      <View
        className="bg-white px-4 py-4 flex-row items-center justify-between"
        style={{
          paddingBottom: insets.bottom + 16,
          shadowColor: '#000',
          shadowOffset: { width: 0, height: -4 },
          shadowOpacity: 0.1,
          shadowRadius: 8,
          elevation: 8,
        }}
      >
        <View>
          <Text className="text-xl font-black text-forest">
            ${property.price_per_night}
            <Text className="text-sm font-normal text-moss"> / night</Text>
          </Text>
          {property.cleaning_fee && (
            <Text className="text-xs text-moss">+ ${property.cleaning_fee} cleaning fee</Text>
          )}
        </View>
        <TouchableOpacity onPress={handleBookNow}>
          <LinearGradient
            colors={['#D9B168', '#bea04f']}
            className="px-8 py-4 rounded-2xl"
            style={{
              shadowColor: '#D9B168',
              shadowOffset: { width: 0, height: 4 },
              shadowOpacity: 0.3,
              shadowRadius: 8,
              elevation: 5,
            }}
          >
            <Text className="text-forest font-bold text-base">Book Now</Text>
          </LinearGradient>
        </TouchableOpacity>
      </View>
    </View>
  );
}
