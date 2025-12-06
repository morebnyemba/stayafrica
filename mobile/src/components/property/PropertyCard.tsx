import { View, Text, Image, TouchableOpacity } from 'react-native';
import { Ionicons } from '@expo/vector-icons';
import { Property } from '@/types';

interface PropertyCardProps {
  property: Property;
  onPress: (id: string) => void;
}

export function PropertyCard({ property, onPress }: PropertyCardProps) {
  return (
    <TouchableOpacity
      className="mb-4 rounded-lg overflow-hidden bg-white shadow"
      onPress={() => onPress(property.id)}
    >
      {property.image_urls[0] && (
        <Image
          source={{ uri: property.image_urls[0] }}
          style={{ width: '100%', height: 200 }}
        />
      )}

      <View className="p-3">
        <Text className="text-lg font-bold mb-1">{property.title}</Text>
        <Text className="text-gray-600 text-sm mb-2">{property.location.city}</Text>

        <View className="flex-row justify-between items-center mb-2">
          <Text className="text-primary-600 font-bold">
            ${property.price_per_night}
            <Text className="text-gray-600 text-xs">/night</Text>
          </Text>
          <View className="flex-row items-center">
            <Ionicons name="star" size={16} color="#f59e0b" />
            <Text className="ml-1 font-semibold">{property.rating}</Text>
          </View>
        </View>

        <View className="flex-row justify-between text-xs text-gray-600">
          <Text>🛏️ {property.number_of_beds} beds</Text>
          <Text>🚿 {property.number_of_bathrooms} baths</Text>
          <Text>👥 {property.max_guests} guests</Text>
        </View>
      </View>
    </TouchableOpacity>
  );
}
