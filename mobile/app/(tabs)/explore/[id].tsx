import {
  View,
  Text,
  ScrollView,
  TouchableOpacity,
  Image,
  ActivityIndicator,
  Alert,
  Dimensions,
  Share,
  Modal,
  Pressable,
} from 'react-native';
import { useLocalSearchParams, useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import Animated, { FadeIn, FadeInDown, SlideInDown } from 'react-native-reanimated';
import {
  usePropertyById,
  useCreateConversation,
  useAddToWishlist,
  useRemoveFromWishlist,
  useNearbyPOIs,
  usePropertyReviews,
} from '@/hooks/api-hooks';
import { Skeleton } from '@/components/common/Skeletons';
import { EmptyState } from '@/components/common/EmptyState';
import { useAuth } from '@/context/auth-context';
import { useState, useCallback, useMemo } from 'react';
import { useSafeAreaInsets } from 'react-native-safe-area-context';

const { width: SCREEN_WIDTH } = Dimensions.get('window');

// ── Helpers ──────────────────────────────────────────────────

const AMENITY_ICONS: Record<string, string> = {
  wifi: 'wifi', pool: 'water', parking: 'car', kitchen: 'restaurant',
  tv: 'tv', ac: 'snow', heating: 'flame', washer: 'water-outline',
  dryer: 'sunny', gym: 'barbell', elevator: 'arrow-up', security: 'shield-checkmark',
  garden: 'leaf', balcony: 'resize', bbq: 'flame-outline', fireplace: 'bonfire',
  electricity: 'flash', common_area: 'people', hot_water: 'thermometer',
};

const POI_ICONS: Record<string, string> = {
  restaurant: 'restaurant', cafe: 'cafe', shopping: 'cart',
  attraction: 'camera', transport: 'bus', park: 'leaf',
  hospital: 'medkit', bank: 'cash', default: 'location',
};

function getCurrencySymbol(code?: string) {
  if (code === 'ZAR') return 'R';
  if (code === 'EUR') return '€';
  if (code === 'GBP') return '£';
  return '$';
}

function parseAmenity(a: any) {
  const name = typeof a === 'string' ? a : a.name || '';
  const key = name.toLowerCase().replace(/[\s-]/g, '_');
  return {
    icon: AMENITY_ICONS[key] || 'checkmark-circle',
    label: name.charAt(0).toUpperCase() + name.slice(1).replace(/_/g, ' '),
  };
}

function getImageUrls(property: any): string[] {
  const urls = [
    ...(property?.image_urls || []),
    ...((property as any)?.images?.map((img: any) => img?.image_url).filter(Boolean) || []),
  ].filter(Boolean);
  if (urls.length > 0) return urls;
  const fallback = property?.main_image || property?.image;
  return fallback ? [fallback] : [];
}

// ── Divider ─────────────────────────────────────────────────

function SectionDivider() {
  return <View className="h-px bg-sand-200 mx-4 my-1" />;
}

// ── Star row ────────────────────────────────────────────────

function StarRow({ rating, size = 12 }: { rating: number; size?: number }) {
  return (
    <View className="flex-row">
      {[1, 2, 3, 4, 5].map((i) => (
        <Ionicons
          key={i}
          name={i <= Math.round(rating) ? 'star' : 'star-outline'}
          size={size}
          color={i <= Math.round(rating) ? '#D9B168' : '#c5d8cf'}
          style={{ marginRight: 1 }}
        />
      ))}
    </View>
  );
}

// ── Component ───────────────────────────────────────────────

export default function PropertyDetailsScreen() {
  const router = useRouter();
  const { id } = useLocalSearchParams();
  const insets = useSafeAreaInsets();

  // Data
  const { data: property, isLoading } = usePropertyById(id as string);
  const { data: nearbyPOIs } = useNearbyPOIs(id as string);
  const { data: reviewsData } = usePropertyReviews(id as string);
  const { mutate: createConversation, isPending: isCreatingConversation } = useCreateConversation();
  const { isAuthenticated } = useAuth();
  const { mutate: addToWishlist, isPending: isAdding } = useAddToWishlist();
  const { mutate: removeFromWishlist, isPending: isRemoving } = useRemoveFromWishlist();

  // State
  const [activeImageIndex, setActiveImageIndex] = useState(0);
  const [isSaved, setIsSaved] = useState(false);
  const [showFullDescription, setShowFullDescription] = useState(false);
  const [showAllAmenities, setShowAllAmenities] = useState(false);
  const [showAllReviews, setShowAllReviews] = useState(false);

  const reviews = reviewsData?.results || [];

  // Computed
  const imageUrls = useMemo(() => (property ? getImageUrls(property) : []), [property]);
  const amenities = useMemo(
    () => (property?.amenities || []).map(parseAmenity),
    [property?.amenities],
  );
  const currSymbol = getCurrencySymbol(property?.currency);
  const pricePerNight = property?.price_per_night ?? (property as any)?.price ?? 0;
  const cityName = property?.location?.city || (property as any)?.city || '';
  const countryName = property?.location?.country || (property as any)?.country || '';
  const locationText = [cityName, countryName].filter(Boolean).join(', ');
  const isLongDescription = (property?.description?.length || 0) > 200;
  const displayedAmenities = amenities.slice(0, 6);
  const hasMoreAmenities = amenities.length > 6;

  // Rating breakdown
  const { avgRating, ratingBreakdown } = useMemo(() => {
    if (!reviews.length) return { avgRating: null, ratingBreakdown: {} as Record<number, number> };
    const breakdown: Record<number, number> = {};
    let sum = 0;
    reviews.forEach((r: any) => {
      const star = Math.min(5, Math.max(1, Math.round(r.overall_rating || r.rating || 0)));
      breakdown[star] = (breakdown[star] || 0) + 1;
      sum += r.overall_rating || r.rating || 0;
    });
    return { avgRating: (sum / reviews.length).toFixed(1), ratingBreakdown: breakdown };
  }, [reviews]);

  const displayedReviews = showAllReviews ? reviews : reviews.slice(0, 3);

  // ── Handlers ────────────────────────────────────────────

  const handleShare = async () => {
    try {
      await Share.share({
        message: `Check out ${property?.title || 'this property'} on StayAfrica!`,
        url: `https://zimlegend.online/property/${id}`,
      });
    } catch { /* user cancelled */ }
  };

  const handleToggleSave = useCallback(() => {
    if (!isAuthenticated) { router.push('/(auth)/login'); return; }
    if (!property?.id || isAdding || isRemoving) return;
    if (isSaved) {
      removeFromWishlist(String(property.id), {
        onSuccess: () => setIsSaved(false),
        onError: () => Alert.alert('Error', 'Failed to remove from wishlist'),
      });
    } else {
      addToWishlist(String(property.id), {
        onSuccess: () => setIsSaved(true),
        onError: () => Alert.alert('Error', 'Failed to add to wishlist'),
      });
    }
  }, [isAuthenticated, property?.id, isSaved, isAdding, isRemoving, addToWishlist, removeFromWishlist, router]);

  const handleMessageHost = () => {
    if (!isAuthenticated) { router.push('/(auth)/login'); return; }
    if (!property?.id) return;
    createConversation(property.id, {
      onSuccess: (data) => {
        if (!data) { Alert.alert('Unavailable', 'Messaging is not available right now.'); return; }
        const conversationId = data?.id;
        router.push(conversationId ? `/(tabs)/messages/${conversationId}` : '/(tabs)/messages');
      },
      onError: () => Alert.alert('Error', 'Unable to start conversation with host.'),
    });
  };

  // ── Loading skeleton ────────────────────────────────────

  if (isLoading) {
    return (
      <View className="flex-1 bg-sand-50">
        <Skeleton height={360} width="100%" borderRadius={0} />
        <View className="p-5">
          <Skeleton height={28} width="80%" className="mb-3" />
          <Skeleton height={16} width="55%" className="mb-5" />
          <Skeleton height={80} width="100%" borderRadius={16} className="mb-5" />
          <Skeleton height={14} width="100%" className="mb-2" />
          <Skeleton height={14} width="90%" className="mb-2" />
          <Skeleton height={14} width="75%" className="mb-6" />
          <View className="flex-row gap-3 mb-5">
            <Skeleton height={80} width="48%" borderRadius={16} />
            <Skeleton height={80} width="48%" borderRadius={16} />
          </View>
          <View className="flex-row gap-3">
            <Skeleton height={80} width="48%" borderRadius={16} />
            <Skeleton height={80} width="48%" borderRadius={16} />
          </View>
        </View>
      </View>
    );
  }

  // ── Not found ───────────────────────────────────────────

  if (!property) {
    return (
      <View className="flex-1 bg-sand-50">
        <View
          className="px-4 py-3 flex-row items-center"
          style={{ paddingTop: insets.top + 8 }}
        >
          <TouchableOpacity
            onPress={() => router.back()}
            accessibilityRole="button"
            accessibilityLabel="Go back"
          >
            <Ionicons name="arrow-back" size={24} color="#122F26" />
          </TouchableOpacity>
        </View>
        <EmptyState
          icon="home-outline"
          title="Property Not Found"
          description="This property could not be found or no longer exists."
          actionLabel="Back to Explore"
          onAction={() => router.back()}
        />
      </View>
    );
  }

  // ── Main render ─────────────────────────────────────────

  return (
    <View className="flex-1 bg-sand-50">
      {/* Amenities Modal */}
      <Modal
        visible={showAllAmenities}
        transparent
        animationType="none"
        statusBarTranslucent
        onRequestClose={() => setShowAllAmenities(false)}
      >
        <Pressable
          className="flex-1 justify-end"
          style={{ backgroundColor: 'rgba(0,0,0,0.45)' }}
          onPress={() => setShowAllAmenities(false)}
        >
          <Pressable onPress={(e) => e.stopPropagation()}>
            <Animated.View
              entering={SlideInDown.springify().damping(18)}
              className="bg-white rounded-t-3xl px-5 pb-8"
              style={{ paddingTop: 16, maxHeight: '75%' }}
            >
              <View className="items-center mb-3">
                <View className="w-10 h-1 rounded-full bg-sand-300" />
              </View>
              <Text className="text-lg font-bold text-forest mb-4">
                What this place offers
              </Text>
              <ScrollView showsVerticalScrollIndicator={false}>
                {amenities.map((amenity, i) => (
                  <View
                    key={i}
                    className="flex-row items-center py-3.5 border-b border-sand-100"
                  >
                    <View className="w-10 h-10 rounded-xl bg-sand-100 items-center justify-center">
                      <Ionicons name={amenity.icon as any} size={20} color="#3A5C50" />
                    </View>
                    <Text className="ml-3 text-[15px] text-forest font-medium">
                      {amenity.label}
                    </Text>
                  </View>
                ))}
              </ScrollView>
            </Animated.View>
          </Pressable>
        </Pressable>
      </Modal>

      <ScrollView
        className="flex-1"
        showsVerticalScrollIndicator={false}
        contentContainerStyle={{ paddingBottom: insets.bottom + 90 }}
      >
        {/* ── Image Carousel ─────────────────────────────── */}
        <View style={{ height: 360 }} className="bg-sand-200 relative">
          {imageUrls.length > 0 ? (
            <ScrollView
              horizontal
              pagingEnabled
              showsHorizontalScrollIndicator={false}
              onScroll={(e) => {
                const idx = Math.round(e.nativeEvent.contentOffset.x / SCREEN_WIDTH);
                setActiveImageIndex(idx);
              }}
              scrollEventThrottle={16}
            >
              {imageUrls.map((url, i) => (
                <Image
                  key={`${url}-${i}`}
                  source={{ uri: url }}
                  style={{ width: SCREEN_WIDTH, height: 360 }}
                  resizeMode="cover"
                  accessibilityLabel={`Property photo ${i + 1} of ${imageUrls.length}`}
                />
              ))}
            </ScrollView>
          ) : (
            <LinearGradient
              colors={['#e3ece7', '#c5d8cf']}
              className="flex-1 items-center justify-center"
            >
              <View className="bg-white/40 rounded-full p-5">
                <Ionicons name="image-outline" size={48} color="#3A5C50" />
              </View>
              <Text className="text-moss mt-3 text-sm">No photos available</Text>
            </LinearGradient>
          )}

          {/* Top gradient */}
          <LinearGradient
            colors={['rgba(0,0,0,0.45)', 'transparent']}
            className="absolute top-0 left-0 right-0"
            style={{ height: 100 }}
            pointerEvents="none"
          />

          {/* Floating nav bar */}
          <View
            className="absolute left-0 right-0 flex-row items-center justify-between px-4"
            style={{ top: insets.top + 8 }}
          >
            <TouchableOpacity
              onPress={() => router.back()}
              accessibilityRole="button"
              accessibilityLabel="Go back"
              className="w-10 h-10 rounded-full items-center justify-center"
              style={{ backgroundColor: 'rgba(0,0,0,0.35)' }}
            >
              <Ionicons name="arrow-back" size={22} color="#fff" />
            </TouchableOpacity>
            <View className="flex-row items-center gap-2">
              <TouchableOpacity
                onPress={handleShare}
                accessibilityRole="button"
                accessibilityLabel="Share property"
                className="w-10 h-10 rounded-full items-center justify-center"
                style={{ backgroundColor: 'rgba(0,0,0,0.35)' }}
              >
                <Ionicons name="share-outline" size={20} color="#fff" />
              </TouchableOpacity>
              <TouchableOpacity
                onPress={handleToggleSave}
                disabled={isAdding || isRemoving}
                accessibilityRole="button"
                accessibilityLabel={isSaved ? 'Remove from wishlist' : 'Save to wishlist'}
                className="w-10 h-10 rounded-full items-center justify-center"
                style={{ backgroundColor: 'rgba(0,0,0,0.35)' }}
              >
                {isAdding || isRemoving ? (
                  <ActivityIndicator size="small" color="#fff" />
                ) : (
                  <Ionicons
                    name={isSaved ? 'heart' : 'heart-outline'}
                    size={22}
                    color={isSaved ? '#EF4444' : '#fff'}
                  />
                )}
              </TouchableOpacity>
            </View>
          </View>

          {/* Image counter badge */}
          {imageUrls.length > 1 && (
            <View
              className="absolute bottom-4 right-4 rounded-full px-3 py-1.5 flex-row items-center"
              style={{ backgroundColor: 'rgba(0,0,0,0.55)' }}
            >
              <Ionicons name="images-outline" size={14} color="#fff" />
              <Text className="text-white text-xs font-semibold ml-1.5">
                {activeImageIndex + 1}/{imageUrls.length}
              </Text>
            </View>
          )}

          {/* Dot indicators */}
          {imageUrls.length > 1 && imageUrls.length <= 8 && (
            <View className="absolute bottom-4 left-0 right-0 flex-row justify-center" pointerEvents="none">
              {imageUrls.map((_, i) => (
                <View
                  key={`dot-${i}`}
                  className={`mx-1 rounded-full ${i === activeImageIndex ? 'bg-white' : 'bg-white/40'}`}
                  style={{ width: i === activeImageIndex ? 20 : 6, height: 6 }}
                />
              ))}
            </View>
          )}
        </View>

        {/* ── Title + Location + Rating ──────────────────── */}
        <View className="px-5 pt-5 pb-4">
          <Animated.View entering={FadeIn.duration(300)}>
            <Text className="text-2xl font-bold text-forest mb-2">
              {property.title}
            </Text>
            <View className="flex-row items-center justify-between">
              <View className="flex-row items-center flex-1 mr-3">
                <Ionicons name="location" size={16} color="#D9B168" />
                <Text className="text-moss ml-1.5 text-sm" numberOfLines={1}>
                  {locationText}
                </Text>
              </View>
              {(property.rating || avgRating) && (
                <TouchableOpacity
                  className="flex-row items-center"
                  accessibilityRole="button"
                  accessibilityLabel={`${avgRating || property.rating} rating, ${reviews.length} reviews`}
                >
                  <Ionicons name="star" size={16} color="#D9B168" />
                  <Text className="font-bold text-forest ml-1 text-sm">
                    {avgRating || property.rating?.toFixed(1)}
                  </Text>
                  {reviews.length > 0 && (
                    <Text className="text-moss text-sm ml-1">
                      ({reviews.length})
                    </Text>
                  )}
                </TouchableOpacity>
              )}
            </View>
          </Animated.View>
        </View>

        <SectionDivider />

        {/* ── Property Highlights ────────────────────────── */}
        <Animated.View entering={FadeInDown.delay(50).duration(300)} className="px-5 py-4">
          <Text className="text-sm text-moss mb-0.5">
            {property.property_type && (
              <Text className="capitalize">{property.property_type}</Text>
            )}
            {property.host?.first_name && (
              <Text> hosted by <Text className="font-bold text-forest">{property.host.first_name}</Text></Text>
            )}
          </Text>
          <Text className="text-xs text-primary-400">
            {[
              property.max_guests && `${property.max_guests} guest${property.max_guests !== 1 ? 's' : ''}`,
              (property.number_of_beds ?? (property as any).bedrooms) && `${property.number_of_beds ?? (property as any).bedrooms} bed${(property.number_of_beds ?? (property as any).bedrooms) !== 1 ? 's' : ''}`,
              (property.number_of_bathrooms ?? (property as any).bathrooms) && `${property.number_of_bathrooms ?? (property as any).bathrooms} bath${(property.number_of_bathrooms ?? (property as any).bathrooms) !== 1 ? 's' : ''}`,
            ].filter(Boolean).join(' · ')}
          </Text>

          {/* Highlight badges */}
          <View className="mt-4 gap-3">
            {property.host?.is_verified && (
              <View className="flex-row items-center">
                <View className="w-10 h-10 rounded-xl bg-green-50 items-center justify-center">
                  <Ionicons name="shield-checkmark" size={20} color="#10B981" />
                </View>
                <View className="ml-3 flex-1">
                  <Text className="text-sm font-semibold text-forest">Verified Host</Text>
                  <Text className="text-xs text-moss">Identity and credentials verified</Text>
                </View>
              </View>
            )}
            {property.instant_booking_enabled && (
              <View className="flex-row items-center">
                <View className="w-10 h-10 rounded-xl bg-amber-50 items-center justify-center">
                  <Ionicons name="flash" size={20} color="#D9B168" />
                </View>
                <View className="ml-3 flex-1">
                  <Text className="text-sm font-semibold text-forest">Instant Booking</Text>
                  <Text className="text-xs text-moss">Book right away without waiting</Text>
                </View>
              </View>
            )}
          </View>
        </Animated.View>

        <SectionDivider />

        {/* ── Description ────────────────────────────────── */}
        {property.description ? (
          <>
            <Animated.View entering={FadeInDown.delay(100).duration(300)} className="px-5 py-4">
              <View className={!showFullDescription && isLongDescription ? 'max-h-[100px] overflow-hidden' : ''}>
                <Text className="text-moss leading-6 text-[15px]">
                  {property.description}
                </Text>
              </View>
              {!showFullDescription && isLongDescription && (
                <LinearGradient
                  colors={['transparent', '#fbf7f1']}
                  className="absolute bottom-0 left-5 right-5 h-10"
                  pointerEvents="none"
                />
              )}
              {isLongDescription && (
                <TouchableOpacity
                  onPress={() => setShowFullDescription(!showFullDescription)}
                  accessibilityRole="button"
                  accessibilityLabel={showFullDescription ? 'Show less description' : 'Show more description'}
                  className="mt-2 flex-row items-center"
                >
                  <Text className="text-forest font-semibold text-sm underline">
                    {showFullDescription ? 'Show less' : 'Show more'}
                  </Text>
                  <Ionicons
                    name={showFullDescription ? 'chevron-up' : 'chevron-down'}
                    size={16}
                    color="#122F26"
                    style={{ marginLeft: 2 }}
                  />
                </TouchableOpacity>
              )}
            </Animated.View>
            <SectionDivider />
          </>
        ) : null}

        {/* ── Property Specs ─────────────────────────────── */}
        <Animated.View entering={FadeInDown.delay(150).duration(300)} className="px-5 py-4">
          <Text className="text-lg font-bold text-forest mb-3">Property Details</Text>
          <View className="flex-row flex-wrap" style={{ gap: 10 }}>
            {[
              { icon: 'home', label: 'Type', value: property.property_type, capitalize: true },
              { icon: 'bed', label: 'Beds', value: property.number_of_beds ?? (property as any).bedrooms },
              { icon: 'water', label: 'Baths', value: property.number_of_bathrooms ?? (property as any).bathrooms },
              { icon: 'people', label: 'Guests', value: property.max_guests },
            ].filter((s) => s.value != null).map((spec) => (
              <View
                key={spec.label}
                className="bg-white p-3.5 rounded-2xl items-center"
                style={{
                  width: (SCREEN_WIDTH - 50) / 2 - 5,
                  shadowColor: '#122F26',
                  shadowOffset: { width: 0, height: 2 },
                  shadowOpacity: 0.05,
                  shadowRadius: 4,
                  elevation: 2,
                }}
              >
                <View className="w-10 h-10 rounded-xl bg-gold/10 items-center justify-center mb-2">
                  <Ionicons name={spec.icon as any} size={22} color="#D9B168" />
                </View>
                <Text className="text-xs text-moss font-medium">{spec.label}</Text>
                <Text className={`text-base font-bold text-forest mt-0.5 ${spec.capitalize ? 'capitalize' : ''}`}>
                  {spec.value}
                </Text>
              </View>
            ))}
          </View>
        </Animated.View>

        <SectionDivider />

        {/* ── Amenities ──────────────────────────────────── */}
        {amenities.length > 0 && (
          <>
            <Animated.View entering={FadeInDown.delay(200).duration(300)} className="px-5 py-4">
              <Text className="text-lg font-bold text-forest mb-3">What this place offers</Text>
              <View className="gap-2">
                {displayedAmenities.map((amenity, i) => (
                  <View key={i} className="flex-row items-center py-2">
                    <View className="w-9 h-9 rounded-xl bg-sand-100 items-center justify-center">
                      <Ionicons name={amenity.icon as any} size={18} color="#3A5C50" />
                    </View>
                    <Text className="ml-3 text-[15px] text-forest">{amenity.label}</Text>
                  </View>
                ))}
              </View>
              {hasMoreAmenities && (
                <TouchableOpacity
                  onPress={() => setShowAllAmenities(true)}
                  accessibilityRole="button"
                  accessibilityLabel={`Show all ${amenities.length} amenities`}
                  className="mt-3 py-3 border border-forest rounded-xl items-center"
                >
                  <Text className="text-forest font-semibold text-sm">
                    Show all {amenities.length} amenities
                  </Text>
                </TouchableOpacity>
              )}
            </Animated.View>
            <SectionDivider />
          </>
        )}

        {/* ── Host Card ──────────────────────────────────── */}
        {property.host && (
          <>
            <Animated.View entering={FadeInDown.delay(250).duration(300)} className="px-5 py-4">
              <Text className="text-lg font-bold text-forest mb-4">
                Hosted by {property.host.first_name}
              </Text>
              <View className="flex-row items-center">
                {property.host.profile_picture ? (
                  <View className="relative">
                    <Image
                      source={{ uri: property.host.profile_picture }}
                      className="w-16 h-16 rounded-full"
                      resizeMode="cover"
                    />
                    {property.host.is_verified && (
                      <View className="absolute -bottom-1 -right-1 bg-white rounded-full p-0.5">
                        <Ionicons name="checkmark-circle" size={18} color="#10B981" />
                      </View>
                    )}
                  </View>
                ) : (
                  <View className="relative">
                    <View className="w-16 h-16 rounded-full bg-primary-800 items-center justify-center">
                      <Text className="text-white text-xl font-bold">
                        {property.host.first_name?.[0] || 'H'}
                      </Text>
                    </View>
                    {property.host.is_verified && (
                      <View className="absolute -bottom-1 -right-1 bg-white rounded-full p-0.5">
                        <Ionicons name="checkmark-circle" size={18} color="#10B981" />
                      </View>
                    )}
                  </View>
                )}
                <View className="ml-4 flex-1">
                  <Text className="font-bold text-forest text-base">
                    {property.host.first_name} {property.host.last_name}
                  </Text>
                  <Text className="text-xs text-moss mt-0.5">
                    {property.host.is_online
                      ? 'Online now'
                      : property.host.response_time
                        ? `Responds within ${property.host.response_time}`
                        : 'Host'}
                  </Text>
                </View>
              </View>

              {/* Host stats */}
              <View className="flex-row items-center mt-4 gap-4">
                {property.host.average_rating > 0 && (
                  <View className="flex-row items-center">
                    <Ionicons name="star" size={14} color="#D9B168" />
                    <Text className="font-bold text-forest text-sm ml-1">
                      {property.host.average_rating.toFixed(1)}
                    </Text>
                    <Text className="text-xs text-moss ml-1">rating</Text>
                  </View>
                )}
                {property.host.total_listings > 0 && (
                  <View className="flex-row items-center">
                    <Text className="font-bold text-forest text-sm">
                      {property.host.total_listings}
                    </Text>
                    <Text className="text-xs text-moss ml-1">
                      listing{property.host.total_listings !== 1 ? 's' : ''}
                    </Text>
                  </View>
                )}
                {property.host.response_rate != null && (
                  <View className="flex-row items-center">
                    <Text className="font-bold text-forest text-sm">
                      {property.host.response_rate}%
                    </Text>
                    <Text className="text-xs text-moss ml-1">response</Text>
                  </View>
                )}
              </View>

              {/* Contact host */}
              <TouchableOpacity
                onPress={handleMessageHost}
                disabled={isCreatingConversation}
                accessibilityRole="button"
                accessibilityLabel="Message host"
                className="mt-4 py-3 border border-forest rounded-xl flex-row justify-center items-center"
              >
                {isCreatingConversation ? (
                  <ActivityIndicator color="#122F26" size="small" />
                ) : (
                  <>
                    <Ionicons name="chatbubble-ellipses-outline" size={18} color="#122F26" />
                    <Text className="text-forest font-semibold text-sm ml-2">Contact Host</Text>
                  </>
                )}
              </TouchableOpacity>

              {/* Safety note */}
              <View className="flex-row items-start mt-4 pt-4 border-t border-sand-200">
                <Ionicons name="shield-outline" size={16} color="#789c8d" style={{ marginTop: 1 }} />
                <Text className="text-xs text-primary-400 ml-2 leading-4 flex-1">
                  To protect your payment, never transfer money or communicate outside of StayAfrica.
                </Text>
              </View>
            </Animated.View>
            <SectionDivider />
          </>
        )}

        {/* ── Reviews ────────────────────────────────────── */}
        {reviews.length > 0 && (
          <>
            <Animated.View entering={FadeInDown.delay(300).duration(300)} className="px-5 py-4">
              {/* Rating summary */}
              <View className="flex-row items-center mb-4">
                <Ionicons name="star" size={22} color="#D9B168" />
                <Text className="text-xl font-bold text-forest ml-2">
                  {avgRating}
                </Text>
                <Text className="text-moss text-lg mx-2">·</Text>
                <Text className="text-xl font-bold text-forest">
                  {reviews.length} review{reviews.length !== 1 ? 's' : ''}
                </Text>
              </View>

              {/* Rating breakdown bars */}
              <View className="mb-5">
                {[5, 4, 3, 2, 1].map((star) => {
                  const count = ratingBreakdown[star] || 0;
                  const pct = reviews.length > 0 ? (count / reviews.length) * 100 : 0;
                  return (
                    <View key={star} className="flex-row items-center mb-1.5">
                      <Text className="text-xs text-forest w-3 text-right">{star}</Text>
                      <View className="flex-1 h-1.5 bg-sand-200 rounded-full mx-2 overflow-hidden">
                        <View
                          className="h-full bg-gold rounded-full"
                          style={{ width: `${pct}%` }}
                        />
                      </View>
                      <Text className="text-xs text-moss w-5">{count}</Text>
                    </View>
                  );
                })}
              </View>

              {/* Review cards */}
              {displayedReviews.map((review: any, idx: number) => (
                <View
                  key={review.id || idx}
                  className={`pb-4 ${idx < displayedReviews.length - 1 ? 'mb-4 border-b border-sand-100' : ''}`}
                >
                  <View className="flex-row items-center mb-2">
                    <View className="w-10 h-10 rounded-full bg-primary-800 items-center justify-center">
                      <Text className="text-white text-sm font-bold">
                        {(review.guest?.first_name?.[0] || review.reviewer_name?.[0] || 'G').toUpperCase()}
                      </Text>
                    </View>
                    <View className="ml-3 flex-1">
                      <Text className="font-semibold text-forest text-sm">
                        {review.guest?.first_name || review.reviewer_name || 'Guest'}
                      </Text>
                      {review.created_at && (
                        <Text className="text-xs text-moss">
                          {new Date(review.created_at).toLocaleDateString('en-US', {
                            month: 'long',
                            year: 'numeric',
                          })}
                        </Text>
                      )}
                    </View>
                  </View>
                  <StarRow rating={review.overall_rating || review.rating || 0} />
                  {review.comment && (
                    <Text className="text-moss text-sm leading-5 mt-2" numberOfLines={showAllReviews ? undefined : 4}>
                      {review.comment}
                    </Text>
                  )}
                </View>
              ))}

              {reviews.length > 3 && (
                <TouchableOpacity
                  onPress={() => setShowAllReviews(!showAllReviews)}
                  accessibilityRole="button"
                  accessibilityLabel={showAllReviews ? 'Show fewer reviews' : `Show all ${reviews.length} reviews`}
                  className="mt-2 py-3 border border-forest rounded-xl items-center"
                >
                  <Text className="text-forest font-semibold text-sm">
                    {showAllReviews ? 'Show less' : `Show all ${reviews.length} reviews`}
                  </Text>
                </TouchableOpacity>
              )}
            </Animated.View>
            <SectionDivider />
          </>
        )}

        {/* ── Nearby POIs ────────────────────────────────── */}
        {nearbyPOIs?.pois_by_category && nearbyPOIs.total_pois > 0 && (
          <Animated.View entering={FadeInDown.delay(350).duration(300)} className="px-5 py-4">
            <Text className="text-lg font-bold text-forest mb-1">What's Nearby</Text>
            <Text className="text-xs text-moss mb-4">
              {nearbyPOIs.total_pois} places within {nearbyPOIs.radius_km}km
            </Text>
            {Object.entries(nearbyPOIs.pois_by_category).map(
              ([category, pois]: [string, any[]]) =>
                pois.length > 0 && (
                  <View key={category} className="mb-4">
                    <Text className="text-sm font-bold text-forest mb-2 capitalize">
                      {category.replace(/_/g, ' ')}
                    </Text>
                    {pois.slice(0, 3).map((item: any) => {
                      const poiIcon = POI_ICONS[category] || POI_ICONS.default;
                      return (
                        <View
                          key={item.id}
                          className="flex-row items-center py-2.5 border-b border-sand-100"
                        >
                          <View className="w-9 h-9 rounded-xl bg-gold/10 items-center justify-center">
                            <Ionicons name={poiIcon as any} size={16} color="#D9B168" />
                          </View>
                          <View className="ml-3 flex-1">
                            <Text className="font-medium text-forest text-sm">
                              {item.poi?.name || item.name}
                            </Text>
                            <Text className="text-[11px] text-moss capitalize">
                              {(item.poi?.poi_type || category).replace(/_/g, ' ')}
                            </Text>
                          </View>
                          <View className="items-end">
                            <Text className="text-xs text-moss font-medium">
                              {item.distance_display}
                            </Text>
                            {item.walking_time_minutes && (
                              <Text className="text-[11px] text-primary-400">
                                {item.walking_time_minutes} min walk
                              </Text>
                            )}
                          </View>
                        </View>
                      );
                    })}
                  </View>
                ),
            )}
          </Animated.View>
        )}
      </ScrollView>

      {/* ── Sticky Bottom Booking Bar ────────────────────── */}
      <View
        className="absolute bottom-0 left-0 right-0 bg-white border-t border-sand-200 px-5 flex-row items-center justify-between"
        style={{
          paddingBottom: insets.bottom + 8,
          paddingTop: 12,
          shadowColor: '#000',
          shadowOffset: { width: 0, height: -3 },
          shadowOpacity: 0.08,
          shadowRadius: 6,
          elevation: 10,
        }}
      >
        <View>
          <View className="flex-row items-baseline">
            <Text className="text-xl font-black text-forest">
              {currSymbol}{pricePerNight}
            </Text>
            <Text className="text-moss text-sm ml-1">/ night</Text>
          </View>
          {property.cleaning_fee ? (
            <Text className="text-[11px] text-primary-400">
              + {currSymbol}{property.cleaning_fee} cleaning fee
            </Text>
          ) : null}
        </View>
        <TouchableOpacity
          onPress={() => router.push(`/(tabs)/bookings/create?propertyId=${property.id}`)}
          accessibilityRole="button"
          accessibilityLabel={`Book this property for ${currSymbol}${pricePerNight} per night`}
          style={{
            shadowColor: '#122F26',
            shadowOffset: { width: 0, height: 4 },
            shadowOpacity: 0.25,
            shadowRadius: 8,
            elevation: 6,
          }}
        >
          <LinearGradient
            colors={['#122F26', '#1d392f']}
            className="px-8 py-3.5 rounded-xl"
          >
            <Text className="text-gold font-bold text-base">Book Now</Text>
          </LinearGradient>
        </TouchableOpacity>
      </View>
    </View>
  );
}
