import { View, Text, ScrollView, TouchableOpacity, FlatList, TextInput, ActivityIndicator } from 'react-native';
import { useRouter } from 'expo-router';
import { useState } from 'react';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { useHostReviews, useRespondToReview } from '@/hooks/api-hooks';
import { Skeleton } from '@/components/common/Skeletons';
import type { Review } from '@/types';

export default function HostReviewsScreen() {
  const router = useRouter();
  const { isAuthenticated, user } = useAuth();
  const insets = useSafeAreaInsets();
  const { data, isLoading } = useHostReviews();
  const { mutate: respondToReview, isPending: isResponding } = useRespondToReview();
  const [respondingTo, setRespondingTo] = useState<string | null>(null);
  const [responseText, setResponseText] = useState('');

  const reviews = data?.results || [];

  if (!isAuthenticated) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100">
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: insets.top + 12 }}
        >
          <TouchableOpacity 
            onPress={() => router.back()} 
            className="mb-4 w-10 h-10 rounded-xl items-center justify-center"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </TouchableOpacity>
          <Text className="text-3xl font-black text-white tracking-tight">
            Reviews
          </Text>
        </LinearGradient>
        <View className="flex-1 items-center justify-center px-6">
          <Text className="text-forest text-lg">Please sign in to view reviews</Text>
        </View>
      </SafeAreaView>
    );
  }

  const avgRating = reviews.length > 0
    ? (reviews.reduce((sum: number, r: Review) => sum + r.rating, 0) / reviews.length).toFixed(1)
    : '0.0';

  const ratingCounts = [5, 4, 3, 2, 1].map(star => ({
    star,
    count: reviews.filter((r: Review) => Math.round(r.rating) === star).length,
  }));

  const handleSubmitResponse = (reviewId: string) => {
    if (!responseText.trim()) return;
    respondToReview({ reviewId, response: responseText.trim() }, {
      onSuccess: () => {
        setRespondingTo(null);
        setResponseText('');
      },
    });
  };

  const RatingStats = () => (
    <View
      className="p-5 mx-4 -mt-4 bg-white rounded-2xl"
      style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 4 },
        shadowOpacity: 0.08,
        shadowRadius: 8,
        elevation: 4,
      }}
    >
      <View className="flex-row items-center justify-between">
        <View className="items-center">
          <Text className="text-4xl font-black text-forest">{avgRating}</Text>
          <View className="flex-row mt-1">
            {[1, 2, 3, 4, 5].map((star) => (
              <Ionicons key={star} name={Number(avgRating) >= star ? 'star' : 'star-outline'} size={16} color="#D9B168" />
            ))}
          </View>
          <Text className="text-sm text-moss mt-1">{reviews.length} reviews</Text>
        </View>

        <View className="flex-1 ml-6">
          {ratingCounts.map(({ star, count }) => (
            <View key={star} className="flex-row items-center mb-1">
              <Text className="text-xs text-moss w-3">{star}</Text>
              <Ionicons name="star" size={10} color="#D9B168" className="ml-1" />
              <View className="flex-1 h-2 bg-sand-200 rounded-full ml-2">
                <View className="h-2 bg-gold rounded-full" style={{ width: reviews.length > 0 ? `${(count / reviews.length) * 100}%` : '0%' }} />
              </View>
              <Text className="text-xs text-moss w-6 text-right">{count}</Text>
            </View>
          ))}
        </View>
      </View>
    </View>
  );

  const ReviewItem = ({ review }: { review: Review }) => (
    <View
      className="p-4 bg-white rounded-2xl mb-3 mx-4"
      style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 2 },
        shadowOpacity: 0.05,
        shadowRadius: 4,
        elevation: 2,
      }}
    >
      <View className="flex-row items-center mb-3">
        <View className="w-12 h-12 rounded-full bg-gold items-center justify-center">
          <Text className="text-xl font-bold text-forest">
            {review.guest_name?.[0] || review.reviewer?.first_name?.[0] || 'G'}
          </Text>
        </View>
        <View className="flex-1 ml-3">
          <Text className="font-semibold text-forest">
            {review.guest_name || `${review.reviewer?.first_name || ''} ${review.reviewer?.last_name || ''}`}
          </Text>
          <Text className="text-xs text-moss">{review.property_title}</Text>
        </View>
        <View className="flex-row items-center bg-gold/20 px-2 py-1 rounded-full">
          <Ionicons name="star" size={14} color="#D9B168" />
          <Text className="text-sm font-bold text-forest ml-1">{review.rating}</Text>
        </View>
      </View>
      <Text className="text-moss">{review.text || review.comment}</Text>
      <Text className="text-xs text-moss/70 mt-2">
        {new Date(review.created_at).toLocaleDateString()}
      </Text>

      {/* Host Response */}
      {review.host_response ? (
        <View className="mt-3 pt-3 border-t border-sand-200 bg-sand-100 rounded-xl p-3">
          <Text className="text-xs font-semibold text-moss mb-1">Your Response:</Text>
          <Text className="text-sm text-forest">{review.host_response}</Text>
        </View>
      ) : respondingTo === review.id ? (
        <View className="mt-3 pt-3 border-t border-sand-200">
          <TextInput
            className="bg-sand-100 rounded-xl p-3 text-forest min-h-[80px]"
            placeholder="Write your response..."
            placeholderTextColor="#7A8F85"
            value={responseText}
            onChangeText={setResponseText}
            multiline
            textAlignVertical="top"
          />
          <View className="flex-row gap-2 mt-2">
            <TouchableOpacity
              onPress={() => { setRespondingTo(null); setResponseText(''); }}
              className="flex-1 py-2 rounded-xl bg-sand-200"
            >
              <Text className="text-center text-moss font-semibold">Cancel</Text>
            </TouchableOpacity>
            <TouchableOpacity
              onPress={() => handleSubmitResponse(review.id)}
              disabled={isResponding || !responseText.trim()}
              className="flex-1 py-2 rounded-xl bg-forest"
            >
              {isResponding ? (
                <ActivityIndicator color="#D9B168" size="small" />
              ) : (
                <Text className="text-center text-gold font-semibold">Submit</Text>
              )}
            </TouchableOpacity>
          </View>
        </View>
      ) : (
        <TouchableOpacity
          onPress={() => setRespondingTo(review.id)}
          className="mt-3 pt-3 border-t border-sand-200"
        >
          <Text className="text-forest font-semibold text-sm">Respond to review →</Text>
        </TouchableOpacity>
      )}
    </View>
  );

  const ReviewSkeleton = () => (
    <View className="p-4 bg-white rounded-2xl mb-3 mx-4">
      <View className="flex-row items-center mb-3">
        <Skeleton height={48} width={48} borderRadius={24} />
        <View className="flex-1 ml-3">
          <Skeleton height={16} width="60%" className="mb-2" />
          <Skeleton height={12} width="40%" />
        </View>
      </View>
      <Skeleton height={40} width="100%" />
    </View>
  );

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="px-4 pb-8"
        style={{ paddingTop: insets.top + 12 }}
      >
        <TouchableOpacity onPress={() => router.back()} className="mb-4">
          <View className="w-10 h-10 rounded-xl items-center justify-center" style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}>
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </View>
        </TouchableOpacity>
        <Text className="text-3xl font-black text-white tracking-tight mb-2">
          Guest Reviews
        </Text>
        <View className="flex-row items-center">
          <Ionicons name="star" size={16} color="#D9B168" />
          <Text className="text-sand-100 ml-2">
            See what guests say about your properties
          </Text>
        </View>
      </LinearGradient>

      <ScrollView showsVerticalScrollIndicator={false}>
        <RatingStats />

        <View className="mt-6 mb-8">
          <Text className="text-lg font-bold text-forest mb-3 mx-4">All Reviews</Text>

          {isLoading ? (
            [1, 2, 3].map((i) => <ReviewSkeleton key={i} />)
          ) : reviews.length === 0 ? (
            <View className="bg-white rounded-2xl p-8 mx-4 items-center" style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 4 },
              shadowOpacity: 0.08,
              shadowRadius: 8,
              elevation: 4,
            }}>
              <View className="bg-sand-200 rounded-full p-6 mb-4">
                <Ionicons name="star-outline" size={40} color="#3A5C50" />
              </View>
              <Text className="text-forest font-semibold text-lg">No Reviews Yet</Text>
              <Text className="text-moss text-sm mt-2 text-center">
                Reviews from your guests will appear here after their stay
              </Text>
            </View>
          ) : (
            reviews.map((review: Review) => (
              <ReviewItem key={review.id} review={review} />
            ))
          )}
        </View>
      </ScrollView>
    </SafeAreaView>
  );
}
