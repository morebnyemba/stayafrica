import { View, Text, ScrollView, TouchableOpacity, Image, ActivityIndicator } from 'react-native';
import { useLocalSearchParams, useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { usePropertyById } from '@/hooks/api-hooks';

export default function PropertyDetailsScreen() {
  const router = useRouter();
  const { id } = useLocalSearchParams();
  const { data: property, isLoading } = usePropertyById(id as string);

  if (isLoading) {
    return (
      <View className="flex-1 justify-center items-center bg-white">
        <ActivityIndicator size="large" color="#D4A574" />
      </View>
    );
  }

  if (!property) {
    return (
      <View className="flex-1 justify-center items-center bg-white">
        <Text className="text-gray-600">Property not found</Text>
      </View>
    );
  }

  const amenitiesMap = [
    { icon: 'wifi', label: 'WiFi' },
    { icon: 'flash', label: 'Electricity' },
    { icon: 'restaurant', label: 'Kitchen' },
    { icon: 'people', label: 'Common Area' },
  ];

  return (
    <View className="flex-1 bg-white">
      {/* Header */}
      <View className="flex-row items-center justify-between px-4 py-3 border-b border-gray-200">
        <TouchableOpacity onPress={() => router.back()}>
          <Ionicons name="arrow-back" size={24} color="#333" />
        </TouchableOpacity>
        <Text className="text-lg font-semibold text-primary-900">Property Details</Text>
        <View style={{ width: 24 }} />
      </View>

      <ScrollView className="flex-1" showsVerticalScrollIndicator={false}>
        {/* Image */}
        <View className="h-64 bg-primary-200 justify-center items-center">
          {property.image ? (
            <Image source={{ uri: property.image }} className="w-full h-full" />
          ) : (
            <Text className="text-primary-600">Property Image</Text>
          )}
        </View>

        {/* Content */}
        <View className="p-4">
          {/* Title and Rating */}
          <View className="flex-row justify-between items-start mb-2">
            <View className="flex-1">
              <Text className="text-2xl font-bold text-primary-900">{property.title}</Text>
              <View className="flex-row items-center mt-2">
                <Ionicons name="location" size={16} color="#666" />
                <Text className="text-gray-600 ml-2">{property.city}, {property.country}</Text>
              </View>
            </View>
            {property.rating && (
              <View className="flex-row items-center bg-yellow-50 px-2 py-1 rounded-lg">
                <Ionicons name="star" size={16} color="#FFA500" />
                <Text className="font-semibold ml-1">{property.rating}</Text>
              </View>
            )}
          </View>

          {/* Price */}
          <View className="bg-secondary-50 p-3 rounded-lg mb-4">
            <View className="flex-row items-baseline">
              <Text className="text-3xl font-bold text-secondary-600">${property.price_per_night}</Text>
              <Text className="text-gray-600 ml-2 text-lg">/night</Text>
            </View>
            {property.cleaning_fee && (
              <Text className="text-sm text-gray-600 mt-1">+ ${property.cleaning_fee} cleaning fee</Text>
            )}
          </View>

          {/* Description */}
          <View className="mb-6">
            <Text className="text-lg font-semibold text-primary-900 mb-2">About this property</Text>
            <Text className="text-gray-700 leading-6">{property.description}</Text>
          </View>

          {/* Property Type and Guests */}
          <View className="flex-row gap-3 mb-6">
            {property.property_type && (
              <View className="flex-1 bg-gray-50 p-3 rounded-lg">
                <Text className="text-xs text-gray-600">Type</Text>
                <Text className="font-semibold text-primary-900">{property.property_type}</Text>
              </View>
            )}
            {property.max_guests && (
              <View className="flex-1 bg-gray-50 p-3 rounded-lg flex-row items-center">
                <Ionicons name="people" size={16} color="#333" />
                <View className="ml-2">
                  <Text className="text-xs text-gray-600">Guests</Text>
                  <Text className="font-semibold text-primary-900">{property.max_guests}</Text>
                </View>
              </View>
            )}
          </View>

          {/* Amenities */}
          {amenitiesMap.length > 0 && (
            <View className="mb-6">
              <Text className="text-lg font-semibold text-primary-900 mb-3">Amenities</Text>
              <View className="flex-row flex-wrap">
                {amenitiesMap.map((amenity, index) => (
                  <View
                    key={index}
                    className="flex-row items-center bg-gray-50 px-3 py-2 rounded-lg mr-2 mb-2"
                  >
                    <Ionicons name={amenity.icon as any} size={16} color="#666" />
                    <Text className="ml-2 text-gray-700 text-sm">{amenity.label}</Text>
                  </View>
                ))}
              </View>
            </View>
          )}

          {/* Booking Button */}
          <TouchableOpacity
            className="bg-secondary-600 py-4 rounded-lg mb-4"
            onPress={() => router.push(`/(tabs)/bookings/create?propertyId=${property.id}`)}
          >
            <Text className="text-white font-semibold text-center text-lg">Book Now</Text>
          </TouchableOpacity>
        </View>
      </ScrollView>
    </View>
  );
}
