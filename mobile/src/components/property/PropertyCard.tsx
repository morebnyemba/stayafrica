import { View, Text, Image, TouchableOpacity } from 'react-native';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import Animated, { useAnimatedStyle, useSharedValue, withSpring, FadeIn } from 'react-native-reanimated';
import { GlassmorphicView } from '@/components/common/GlassmorphicView';
import { Property } from '@/types';
import { memo } from 'react';

interface PropertyCardProps {
  property: Property;
  onPress: (id: string) => void;
  showRemoveButton?: boolean;
  onRemove?: () => void;
  variant?: 'list' | 'grid' | 'compact';
  index?: number;
}

const currencySymbol = (code?: string) =>
  code === 'ZAR' ? 'R' : code === 'EUR' ? '€' : code === 'GBP' ? '£' : '$';

export function PropertyCard({
  property,
  onPress,
  showRemoveButton = false,
  onRemove,
  variant = 'list',
  index = 0,
}: PropertyCardProps) {
  const scale = useSharedValue(1);

  const animatedStyle = useAnimatedStyle(() => ({
    transform: [{ scale: scale.value }],
  }));

  const handlePressIn = () => {
    scale.value = withSpring(0.97, { damping: 15, stiffness: 150 });
  };

  const handlePressOut = () => {
    scale.value = withSpring(1, { damping: 15, stiffness: 150 });
  };

  if (!property) return null;

  const imageUrl =
    property.image_urls?.[0] ||
    (property as any).images?.[0]?.image_url ||
    (property as any).main_image ||
    (property as any).image ||
    null;

  const cityName =
    property.location?.city || (property as any).city || 'Unknown location';
  const countryName =
    property.location?.country || (property as any).country || '';

  const imageHeight = variant === 'list' ? 220 : variant === 'grid' ? 150 : 140;
  const containerMargin = variant === 'list' ? 'mb-6' : '';
  const isCompact = variant !== 'list';
  const isGrid = variant === 'grid';

  return (
    <Animated.View
      entering={FadeIn.delay(index * 60).duration(300)}
      style={animatedStyle}
    >
      <TouchableOpacity
        className={`${containerMargin} rounded-2xl overflow-hidden bg-white`}
        onPress={() => onPress(property.id)}
        onPressIn={handlePressIn}
        onPressOut={handlePressOut}
        activeOpacity={1}
        accessibilityRole="button"
        accessibilityLabel={`${property.title || 'Property'} in ${cityName}, ${currencySymbol(property.currency)}${property.price_per_night ?? 0} per night`}
        accessibilityHint="Double tap to view property details"
        style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: isCompact ? 4 : 8 },
          shadowOpacity: isCompact ? 0.1 : 0.15,
          shadowRadius: isCompact ? 8 : 16,
          elevation: isCompact ? 4 : 8,
        }}
      >
        {/* Image */}
        <View className="relative">
          {imageUrl ? (
            <Image
              source={{ uri: imageUrl }}
              style={{ width: '100%', height: imageHeight }}
              className="bg-sand-200"
              resizeMode="cover"
            />
          ) : (
            <LinearGradient
              colors={['#e3ece7', '#c5d8cf']}
              className="w-full items-center justify-center"
              style={{ height: imageHeight }}
            >
              <View className="bg-white/40 rounded-full p-3">
                <Ionicons name="image-outline" size={isCompact ? 28 : 40} color="#3A5C50" />
              </View>
            </LinearGradient>
          )}

          <LinearGradient
            colors={['transparent', 'rgba(0, 0, 0, 0.35)']}
            className="absolute bottom-0 left-0 right-0"
            style={{ height: isCompact ? 40 : 60 }}
          />

          {showRemoveButton && onRemove && (
            <GlassmorphicView
              intensity={60}
              tint="light"
              borderRadius={20}
              style={{ position: 'absolute', top: 10, right: 10 }}
            >
              <TouchableOpacity
                className="p-2"
                accessibilityRole="button"
                accessibilityLabel="Remove from wishlist"
                onPress={(e) => { e.stopPropagation(); onRemove(); }}
              >
                <Ionicons name="heart" size={20} color="#EF4444" />
              </TouchableOpacity>
            </GlassmorphicView>
          )}

          {/* Rating */}
          {property.rating ? (
            <View className="absolute top-2.5 left-2.5">
              <LinearGradient
                colors={['#D9B168', '#bea04f']}
                start={{ x: 0, y: 0 }}
                end={{ x: 1, y: 1 }}
                className="flex-row items-center rounded-full"
                style={{
                  paddingHorizontal: isGrid ? 8 : 12,
                  paddingVertical: isGrid ? 4 : 6,
                  shadowColor: '#000',
                  shadowOffset: { width: 0, height: 2 },
                  shadowOpacity: 0.25,
                  shadowRadius: 4,
                  elevation: 3,
                }}
              >
                <Ionicons name="star" size={isGrid ? 11 : 14} color="#122F26" />
                <Text className={`ml-0.5 font-bold text-forest ${isGrid ? 'text-xs' : 'text-sm'}`}>
                  {property.rating.toFixed(1)}
                </Text>
              </LinearGradient>
            </View>
          ) : null}

          {/* Property type badge on grid cards */}
          {isGrid && property.property_type && (
            <View className="absolute bottom-2 left-2.5">
              <View
                className="px-2 py-1 rounded-md"
                style={{ backgroundColor: 'rgba(18, 47, 38, 0.7)' }}
              >
                <Text className="text-white text-[10px] font-semibold capitalize">
                  {property.property_type}
                </Text>
              </View>
            </View>
          )}
        </View>

        {/* Content */}
        <View className={isCompact ? 'p-2.5' : 'p-5'}>
          <Text
            className={isCompact
              ? 'text-sm font-bold text-forest'
              : 'text-xl font-black mb-1 text-forest'
            }
            numberOfLines={1}
          >
            {property.title || 'Untitled Property'}
          </Text>

          <View className={`flex-row items-center ${isCompact ? 'mt-0.5 mb-1.5' : 'mb-3'}`}>
            <Ionicons name="location" size={isGrid ? 12 : 14} color="#3A5C50" />
            <Text
              className={isCompact ? 'text-moss text-[11px] ml-1 flex-1' : 'text-moss text-sm ml-1'}
              numberOfLines={1}
            >
              {countryName ? `${cityName}, ${countryName}` : cityName}
            </Text>
          </View>

          {/* Compact amenity dots for grid cards */}
          {isGrid && (
            <View className="flex-row items-center mb-2">
              <Ionicons name="bed-outline" size={12} color="#789c8d" />
              <Text className="text-primary-400 text-[10px] ml-0.5 mr-2 font-medium">
                {property.number_of_beds ?? 0}
              </Text>
              <Ionicons name="water-outline" size={12} color="#789c8d" />
              <Text className="text-primary-400 text-[10px] ml-0.5 mr-2 font-medium">
                {property.number_of_bathrooms ?? 0}
              </Text>
              <Ionicons name="people-outline" size={12} color="#789c8d" />
              <Text className="text-primary-400 text-[10px] ml-0.5 font-medium">
                {property.max_guests ?? 0}
              </Text>
            </View>
          )}

          {/* Full amenities row for list variant */}
          {variant === 'list' && (
            <View className="flex-row items-center mb-4 py-3 px-4 bg-sand-100 rounded-xl">
              <View className="flex-row items-center mr-4">
                <Ionicons name="bed-outline" size={18} color="#3A5C50" />
                <Text className="ml-1 text-sm font-medium text-moss">
                  {property.number_of_beds ?? 0}
                </Text>
              </View>
              <View className="flex-row items-center mr-4">
                <Ionicons name="water-outline" size={18} color="#3A5C50" />
                <Text className="ml-1 text-sm font-medium text-moss">
                  {property.number_of_bathrooms ?? 0}
                </Text>
              </View>
              <View className="flex-row items-center">
                <Ionicons name="people-outline" size={18} color="#3A5C50" />
                <Text className="ml-1 text-sm font-medium text-moss">
                  {property.max_guests ?? 0}
                </Text>
              </View>
            </View>
          )}

          {/* Price */}
          <View className="flex-row justify-between items-center">
            <View className="flex-row items-baseline">
              <Text className={isCompact ? 'text-base font-black text-gold' : 'text-2xl font-black text-gold'}>
                {currencySymbol(property.currency)}{property.price_per_night ?? 0}
              </Text>
              <Text className={isCompact ? 'text-moss text-[10px] font-medium ml-1' : 'text-moss text-xs font-medium ml-1'}>
                /night
              </Text>
            </View>
            {variant === 'list' && (
              <LinearGradient
                colors={['#122F26', '#1d392f']}
                start={{ x: 0, y: 0 }}
                end={{ x: 1, y: 1 }}
                className="px-5 py-3 rounded-xl"
              >
                <Text className="text-gold font-bold text-sm">View Details</Text>
              </LinearGradient>
            )}
          </View>
        </View>
      </TouchableOpacity>
    </Animated.View>
  );
}

// Wrap with React.memo to prevent unnecessary re-renders
export default memo(PropertyCard);
