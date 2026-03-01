import { View, Text, ScrollView, TouchableOpacity, Image, ActivityIndicator, Alert, Dimensions } from 'react-native';
import { useLocalSearchParams, useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { usePropertyById, useCreateConversation } from '@/hooks/api-hooks';
import { Skeleton } from '@/components/common/Skeletons';
import { useAuth } from '@/context/auth-context';
import { useState } from 'react';
import { useSafeAreaInsets } from 'react-native-safe-area-context';

export default function PropertyDetailsScreen() {
  const router = useRouter();
  const { id } = useLocalSearchParams();
  const { data: property, isLoading } = usePropertyById(id as string);
  const { mutate: createConversation, isPending: isCreatingConversation } = useCreateConversation();
  const { isAuthenticated } = useAuth();
  const { width } = Dimensions.get('window');
  const [activeImageIndex, setActiveImageIndex] = useState(0);
  const insets = useSafeAreaInsets();

  const handleMessageHost = () => {
    if (!isAuthenticated) {
      router.push('/(auth)/login');
      return;
    }

    if (!property?.id) return;

    createConversation(property.id, {
      onSuccess: (data) => {
        // If we get a conversation ID back, navigate to it
        // Depending on API response structure, adjust access
        if (!data) {
          Alert.alert('Unavailable', 'Messaging is not available right now.');
          return;
        }

        const conversationId = data?.id;
        if (conversationId) {
          // Navigate to specific conversation (assuming route exists)
           // For now, if route is not clear, go to messages list
           // But assuming /messages/[id] standard
           // Since we see messages/new.tsx and messages/[id].tsx in file list earlier
          // router.push(`/(tabs)/messages/${conversationId}`);
           // Actually listing messages/[id].tsx showed it exists.
           // But let's check if the path is correct.
           // mobile/app/(tabs)/messages/[id].tsx
           router.push(`/(tabs)/messages/${conversationId}`);
        } else {
          router.push('/(tabs)/messages');
        }
      },
      onError: () => {
        Alert.alert('Error', 'Unable to start conversation with host.');
      }
    });
  };

  const PropertyDetailSkeleton = () => (
    <ScrollView className="flex-1 bg-sand-100">
      <Skeleton height={300} width="100%" borderRadius={0} />
      <View className="p-4">
        <Skeleton height={32} width="80%" className="mb-2" />
        <Skeleton height={16} width="60%" className="mb-4" />
        <View className="bg-white rounded-2xl p-4 mb-4">
          <Skeleton height={40} width={150} className="mb-2" />
          <Skeleton height={16} width={200} />
        </View>
        <Skeleton height={100} width="100%" className="mb-4" />
        <View className="flex-row gap-3 mb-4">
          <Skeleton height={60} width="48%" borderRadius={12} />
          <Skeleton height={60} width="48%" borderRadius={12} />
        </View>
      </View>
    </ScrollView>
  );

  if (isLoading) {
    return (
      <View className="flex-1 bg-sand-100">
        {/* Header Skeleton */}
        <View className="bg-white px-4 py-3 border-b border-sand-200 flex-row items-center justify-between">
          <Skeleton height={24} width={24} borderRadius={12} />
          <Skeleton height={20} width={120} />
          <View style={{ width: 24 }} />
        </View>
        <PropertyDetailSkeleton />
      </View>
    );
  }

  if (!property) {
    return (
      <View className="flex-1 bg-sand-100">
        {/* Header */}
        <View className="bg-white px-4 py-3 border-b border-sand-200 flex-row items-center justify-between">
          <TouchableOpacity onPress={() => router.back()}>
            <Ionicons name="arrow-back" size={24} color="#122F26" />
          </TouchableOpacity>
          <Text className="text-lg font-semibold text-forest">Property Details</Text>
          <View style={{ width: 24 }} />
        </View>
        
        <View className="flex-1 justify-center items-center px-6">
          <View className="bg-white rounded-3xl p-8 items-center" style={{ 
            shadowColor: '#000', 
            shadowOffset: { width: 0, height: 8 }, 
            shadowOpacity: 0.1, 
            shadowRadius: 16, 
            elevation: 8 
          }}>
            <View className="bg-sand-200 rounded-full p-8 mb-6">
              <Ionicons name="home-outline" size={72} color="#3A5C50" />
            </View>
            <Text className="text-2xl font-bold text-forest mb-3">Property Not Found</Text>
            <Text className="text-moss text-center px-4 leading-6">
              This property could not be found or no longer exists
            </Text>
          </View>
        </View>
      </View>
    );
  }

  const amenitiesMap = [
    { icon: 'wifi', label: 'WiFi' },
    { icon: 'flash', label: 'Electricity' },
    { icon: 'restaurant', label: 'Kitchen' },
    { icon: 'people', label: 'Common Area' },
  ];

  const pricePerNight =
    property?.price_per_night ??
    (property as any)?.price ??
    (property as any)?.pricePerNight ??
    0;

  const imageUrls = (() => {
    const urls = [
      ...(property?.image_urls || []),
      ...((property as any)?.images?.map((img: any) => img?.image_url).filter(Boolean) || []),
    ].filter(Boolean);

    if (urls.length > 0) return urls as string[];

    const fallback = (property as any)?.main_image || (property as any)?.image;
    return fallback ? [fallback] : [];
  })();

  return (
    <View className="flex-1 bg-sand-100">
      {/* Header */}
      <View
        className="bg-white px-4 py-3 border-b border-sand-200 flex-row items-center justify-between"
        style={{ paddingTop: insets.top + 8 }}
      >
        <TouchableOpacity onPress={() => router.back()}>
          <Ionicons name="arrow-back" size={24} color="#122F26" />
        </TouchableOpacity>
        <Text className="text-lg font-semibold text-forest">Property Details</Text>
        <TouchableOpacity>
          <Ionicons name="heart-outline" size={24} color="#D9B168" />
        </TouchableOpacity>
      </View>

      <ScrollView className="flex-1" showsVerticalScrollIndicator={false} contentContainerStyle={{ paddingBottom: 24 }}>
        {/* Image Carousel */}
        <View className="h-80 bg-sand-200">
          {imageUrls.length > 0 ? (
            <View>
              <ScrollView
                horizontal
                pagingEnabled
                showsHorizontalScrollIndicator={false}
                onScroll={(event) => {
                  const index = Math.round(event.nativeEvent.contentOffset.x / width);
                  setActiveImageIndex(index);
                }}
                scrollEventThrottle={16}
              >
                {imageUrls.map((url, index) => (
                  <Image
                    key={`${url}-${index}`}
                    source={{ uri: url }}
                    style={{ width, height: 320 }}
                    resizeMode="cover"
                  />
                ))}
              </ScrollView>

              {/* Dots */}
              {imageUrls.length > 1 && (
                <View className="absolute bottom-3 w-full flex-row justify-center">
                  {imageUrls.map((_, index) => (
                    <View
                      key={`dot-${index}`}
                      className={
                        `mx-1 h-2 w-2 rounded-full ${
                          index === activeImageIndex ? 'bg-gold' : 'bg-white/60'
                        }`
                      }
                    />
                  ))}
                </View>
              )}
            </View>
          ) : (
            <View className="h-80 bg-sand-200 justify-center items-center">
              <Ionicons name="image-outline" size={64} color="#3A5C50" />
              <Text className="text-moss mt-2">No image available</Text>
            </View>
          )}
        </View>

        {/* Content */}
        <View className="p-4">
          {/* Title and Rating */}
          <View className="flex-row justify-between items-start mb-4">
            <View className="flex-1">
              <Text className="text-3xl font-black text-forest mb-2">{property.title}</Text>
              <View className="flex-row items-center">
                <Ionicons name="location" size={18} color="#D9B168" />
                <Text className="text-moss ml-2 text-base">{property.city}, {property.country}</Text>
              </View>
            </View>
            {property.rating && (
              <View className="bg-gold/20 px-3 py-2 rounded-xl flex-row items-center">
                <Ionicons name="star" size={18} color="#F59E0B" />
                <Text className="font-bold ml-1 text-forest">{property.rating}</Text>
              </View>
            )}
          </View>

          {/* Price Card */}
          <LinearGradient
            colors={['#D9B168', '#bea04f']}
            className="p-5 rounded-2xl mb-6"
            style={{
              shadowColor: '#D9B168',
              shadowOffset: { width: 0, height: 4 },
              shadowOpacity: 0.2,
              shadowRadius: 8,
              elevation: 5,
            }}
          >
            <View className="flex-row items-baseline">
              <Text className="text-4xl font-black text-forest">${pricePerNight}</Text>
              <Text className="text-forest/80 ml-2 text-lg font-semibold">per night</Text>
            </View>
            {property.cleaning_fee && (
              <Text className="text-sm text-forest/70 mt-2">+ ${property.cleaning_fee} cleaning fee</Text>
            )}
          </LinearGradient>

          {/* Description */}
          <View className="mb-6">
            <Text className="text-xl font-bold text-forest mb-3">About this property</Text>
            <Text className="text-moss leading-6 text-base">{property.description}</Text>
          </View>

          {/* Property Type and Guests */}
          <View className="flex-row gap-3 mb-6">
            {property.property_type && (
              <View className="flex-1 bg-white p-4 rounded-2xl" style={{
                shadowColor: '#122F26',
                shadowOffset: { width: 0, height: 2 },
                shadowOpacity: 0.05,
                shadowRadius: 4,
                elevation: 2,
              }}>
                <Ionicons name="home" size={24} color="#D9B168" className="mb-2" />
                <Text className="text-xs text-moss mb-1 font-semibold">Type</Text>
                <Text className="font-bold text-forest">{property.property_type}</Text>
              </View>
            )}
            {property.max_guests && (
              <View className="flex-1 bg-white p-4 rounded-2xl" style={{
                shadowColor: '#122F26',
                shadowOffset: { width: 0, height: 2 },
                shadowOpacity: 0.05,
                shadowRadius: 4,
                elevation: 2,
              }}>
                <Ionicons name="people" size={24} color="#D9B168" className="mb-2" />
                <Text className="text-xs text-moss mb-1 font-semibold">Guests</Text>
                <Text className="font-bold text-forest">{property.max_guests}</Text>
              </View>
            )}
          </View>

          {/* Amenities */}
          {amenitiesMap.length > 0 && (
            <View className="mb-6">
              <Text className="text-xl font-bold text-forest mb-3">Amenities</Text>
              <View className="flex-row flex-wrap">
                {amenitiesMap.map((amenity, index) => (
                  <View
                    key={index}
                    className="flex-row items-center bg-white px-4 py-3 rounded-xl mr-2 mb-2"
                    style={{
                      shadowColor: '#122F26',
                      shadowOffset: { width: 0, height: 2 },
                      shadowOpacity: 0.05,
                      shadowRadius: 4,
                      elevation: 2,
                    }}
                  >
                    <Ionicons name={amenity.icon as any} size={18} color="#3A5C50" />
                    <Text className="ml-2 text-moss text-sm font-semibold">{amenity.label}</Text>
                  </View>
                ))}
              </View>
            </View>
          )}

          {/* Message Host Button */}
          <TouchableOpacity
            onPress={handleMessageHost}
            disabled={isCreatingConversation}
            className="mb-4 bg-white border border-sand-300 py-4 rounded-2xl flex-row justify-center items-center"
            style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 2 },
              shadowOpacity: 0.05,
              shadowRadius: 4,
              elevation: 2,
            }}
          >
            {isCreatingConversation ? (
              <ActivityIndicator color="#D9B168" />
            ) : (
              <>
                <Ionicons name="chatbubble-ellipses-outline" size={20} color="#3A5C50" className="mr-2" />
                <Text className="text-forest font-bold text-base ml-2">Message Host</Text>
              </>
            )}
          </TouchableOpacity>

          {/* Booking Button */}
          <TouchableOpacity
            onPress={() => router.push(`/(tabs)/bookings/create?propertyId=${property.id}`)}
            style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 6 },
              shadowOpacity: 0.3,
              shadowRadius: 12,
              elevation: 8,
            }}
          >
            <LinearGradient
              colors={['#122F26', '#1d392f']}
              className="py-5 rounded-2xl mb-4"
            >
              <Text className="text-gold font-bold text-center text-lg">Book Now</Text>
            </LinearGradient>
          </TouchableOpacity>
        </View>
      </ScrollView>
    </View>
  );
}
