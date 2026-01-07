import { View, Text, Image, TouchableOpacity } from 'react-native';
import { Ionicons } from '@expo/vector-icons';
import { Property } from '@/types';

interface PropertyCardProps {
  property: Property;
  onPress: (id: string) => void;
  showRemoveButton?: boolean;
  onRemove?: () => void;
}

export function PropertyCard({ property, onPress, showRemoveButton = false, onRemove }: PropertyCardProps) {
  return (
    <TouchableOpacity
      className="mb-6 rounded-2xl overflow-hidden bg-white shadow-lg border border-gray-100"
      onPress={() => onPress(property.id)}
      activeOpacity={0.92}
    >
      {property.image_urls[0] ? (
        <Image
          source={{ uri: property.image_urls[0] }}
          style={{ width: '100%', height: 180 }}
          className="bg-gray-200"
          resizeMode="cover"
        />
      ) : (
        <View className="w-full h-44 bg-gray-100 items-center justify-center">
          <Ionicons name="image-outline" size={48} color="#d1d5db" />
        </View>
      )}

      {/* Wishlist Remove Button */}
      {showRemoveButton && onRemove && (
        <TouchableOpacity
          className="absolute top-3 right-3 bg-white/90 rounded-full p-2 shadow-md"
          onPress={(e) => {
            e.stopPropagation();
            onRemove();
          }}
        >
          <Ionicons name="heart" size={24} color="#EF4444" />
        </TouchableOpacity>
      )}

      <View className="p-4">
        <Text className="text-xl font-extrabold mb-1 text-primary-900" numberOfLines={1}>{property.title}</Text>
        <Text className="text-gray-500 text-sm mb-2" numberOfLines={1}>{property.location.city}</Text>

        <View className="flex-row justify-between items-center mb-3">
          <Text className="text-primary-600 font-bold text-lg">
            ${property.price_per_night}
            <Text className="text-gray-500 text-xs"> /night</Text>
          </Text>
          <View className="flex-row items-center">
            <Ionicons name="star" size={16} color="#f59e0b" />
            <Text className="ml-1 font-semibold text-gray-700">{property.rating?.toFixed(1) || 'N/A'}</Text>
          </View>
        </View>

        <View className="flex-row justify-between text-xs text-gray-500">
          <Text>🛏️ {property.number_of_beds} beds</Text>
          <Text>🚿 {property.number_of_bathrooms} baths</Text>
          <Text>👥 {property.max_guests} guests</Text>
        </View>
      </View>
    </TouchableOpacity>
  );
}
