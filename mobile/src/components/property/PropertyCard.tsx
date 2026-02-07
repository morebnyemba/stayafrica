import { View, Text, Image, TouchableOpacity } from 'react-native';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import Animated, { useAnimatedStyle, useSharedValue, withSpring } from 'react-native-reanimated';
import { GlassmorphicView } from '@/components/common/GlassmorphicView';
import { Property } from '@/types';

interface PropertyCardProps {
  property: Property;
  onPress: (id: string) => void;
  showRemoveButton?: boolean;
  onRemove?: () => void;
  variant?: 'list' | 'grid' | 'compact';
}

export function PropertyCard({
  property,
  onPress,
  showRemoveButton = false,
  onRemove,
  variant = 'list',
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

  // Guard against undefined property
  if (!property) {
    return null;
  }

  // Safe access to nested properties
  const imageUrl =
    property.image_urls?.[0] ||
    (property as any).images?.[0]?.image_url ||
    (property as any).main_image ||
    (property as any).image ||
    null;
  
  // Handle both nested location object and flat structure from backend
  const cityName = 
    property.location?.city || 
    (property as any).city || 
    'Unknown location';
  const countryName = 
    property.location?.country || 
    (property as any).country || 
    '';
  
  const imageHeight = variant === 'list' ? 220 : variant === 'grid' ? 160 : 140;
  const containerMargin = variant === 'list' ? 'mb-6' : 'mb-4';
  const isCompact = variant !== 'list';

  return (
    <Animated.View style={animatedStyle}>
      <TouchableOpacity
        className={`${containerMargin} rounded-3xl overflow-hidden bg-white`}
        onPress={() => onPress(property.id)}
        onPressIn={handlePressIn}
        onPressOut={handlePressOut}
        activeOpacity={1}
        style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 8 },
          shadowOpacity: 0.15,
          shadowRadius: 16,
          elevation: 8,
        }}
      >
        {/* Image Section */}
        <View className="relative">
          {imageUrl ? (
            <Image
              source={{ uri: imageUrl }}
              style={{ width: '100%', height: imageHeight }}
              className="bg-sand-200"
              resizeMode="cover"
            />
          ) : (
            <View className="w-full bg-sand-200 items-center justify-center" style={{ height: imageHeight }}>
              <Ionicons name="image-outline" size={56} color="#3A5C50" />
            </View>
          )}
          
          {/* Gradient Overlay on Image */}
          <LinearGradient
            colors={['transparent', 'rgba(0, 0, 0, 0.4)']}
            className="absolute bottom-0 left-0 right-0 h-20"
          />

          {/* Wishlist Remove Button with Glassmorphism */}
          {showRemoveButton && onRemove && (
            <GlassmorphicView
              intensity={60}
              tint="light"
              borderRadius={20}
              style={{
                position: 'absolute',
                top: 12,
                right: 12,
              }}
            >
              <TouchableOpacity
                className="p-2.5"
                onPress={(e) => {
                  e.stopPropagation();
                  onRemove();
                }}
              >
                <Ionicons name="heart" size={22} color="#EF4444" />
              </TouchableOpacity>
            </GlassmorphicView>
          )}

          {/* Rating Badge */}
          {property.rating && (
            <View className="absolute top-3 left-3">
              <LinearGradient
                colors={['#D9B168', '#bea04f']}
                start={{ x: 0, y: 0 }}
                end={{ x: 1, y: 1 }}
                className="flex-row items-center px-3 py-2 rounded-full"
                style={{
                  shadowColor: '#000',
                  shadowOffset: { width: 0, height: 2 },
                  shadowOpacity: 0.3,
                  shadowRadius: 4,
                  elevation: 4,
                }}
              >
                <Ionicons name="star" size={14} color="#122F26" />
                <Text className="ml-1 font-bold text-sm text-forest">
                  {property.rating.toFixed(1)}
                </Text>
              </LinearGradient>
            </View>
          )}
        </View>

        {/* Content Section */}
        <View className={isCompact ? 'p-3' : 'p-5'}>
          <Text className={isCompact ? 'text-base font-bold mb-1 text-forest' : 'text-xl font-black mb-1 text-forest'} numberOfLines={1}>
            {property.title || 'Untitled Property'}
          </Text>
          <View className={isCompact ? 'flex-row items-center mb-2' : 'flex-row items-center mb-3'}>
            <Ionicons name="location" size={14} color="#3A5C50" />
            <Text className={isCompact ? 'text-moss text-xs ml-1' : 'text-moss text-sm ml-1'} numberOfLines={1}>
              {countryName ? `${cityName}, ${countryName}` : cityName}
            </Text>
          </View>

          {/* Amenities Row */}
          {!isCompact && (
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

          {/* Price Section */}
          <View className="flex-row justify-between items-center">
            <View>
              <Text className={isCompact ? 'text-lg font-black text-gold' : 'text-2xl font-black text-gold'}>
                ${property.price_per_night ?? 0}
              </Text>
              <Text className={isCompact ? 'text-moss text-[10px] font-medium' : 'text-moss text-xs font-medium'}>
                per night
              </Text>
            </View>
            {!isCompact && (
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
