import { View, Text, FlatList, TouchableOpacity, ActivityIndicator } from 'react-native';
import { useRouter } from 'expo-router';
import { useState } from 'react';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { Skeleton } from '@/components/common/Skeletons';
import { format } from 'date-fns';
import type { Review } from '@/types';

export default function ReviewsScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const [reviews, setReviews] = useState<Review[]>([]);
  const [isLoading, setIsLoading] = useState(false);

  if (!isAuthenticated) {
    return (
      <View className="flex-1 bg-sand-100">
        {/* Header */}
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pt-12 pb-6"
        >
          <Text className="text-3xl font-black text-white tracking-tight">
            Reviews
          </Text>
          <Text className="text-sand-200 text-sm mt-1">
            Guest feedback and ratings
          </Text>
        </LinearGradient>

        <View className="flex-1 items-center justify-center px-6">
          <View className="bg-white rounded-3xl p-8 items-center" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}>
            <View className="bg-sand-200 rounded-full p-8 mb-6">
              <Ionicons name="star-outline" size={72} color="#D9B168" />
            </View>
            <Text className="text-2xl font-bold text-forest mb-3">Sign In Required</Text>
            <Text className="text-moss text-center mb-8 px-4 leading-6">
              Please sign in to view your reviews and ratings
            </Text>
            <TouchableOpacity
              onPress={() => router.push('/(auth)/login')}
            >
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
                <Text className="text-forest font-bold text-base">Sign In Now</Text>
              </LinearGradient>
            </TouchableOpacity>
          </View>
        </View>
      </View>
    );
  }

  const StarRating = ({ rating }: { rating: number }) => (
    <View className="flex-row">
      {[1, 2, 3, 4, 5].map((star) => (
        <Ionicons
          key={star}
          name={star <= rating ? 'star' : 'star-outline'}
          size={16}
          color="#F59E0B"
        />
      ))}
    </View>
  );

  const ReviewItem = ({ review }: { review: Review }) => (
    <View className="bg-white rounded-2xl p-4 mb-3 mx-4" style={{
      shadowColor: '#122F26',
      shadowOffset: { width: 0, height: 4 },
      shadowOpacity: 0.08,
      shadowRadius: 8,
      elevation: 4,
    }}>
      {/* Property Info */}
      <Text className="text-base font-bold text-forest mb-2">
        {review.property?.title || 'Property'}
      </Text>

      {/* Reviewer Info */}
      <View className="flex-row items-center mb-3">
        <LinearGradient
          colors={['#D9B168', '#bea04f']}
          className="w-10 h-10 rounded-full items-center justify-center mr-3"
        >
          <Ionicons name="person" size={20} color="#122F26" />
        </LinearGradient>
        <View className="flex-1">
          <Text className="text-sm font-semibold text-forest">
            {review.reviewer?.first_name} {review.reviewer?.last_name}
          </Text>
          <Text className="text-xs text-moss">
            {format(new Date(review.created_at), 'MMM dd, yyyy')}
          </Text>
        </View>
        <StarRating rating={review.rating} />
      </View>

      {/* Review Text */}
      <Text className="text-sm text-moss leading-5 mb-3">
        {review.comment}
      </Text>

      {/* Review Categories */}
      <View className="flex-row flex-wrap gap-2">
        {review.cleanliness_rating && (
          <View className="bg-sand-100 px-3 py-1 rounded-full">
            <Text className="text-xs text-forest">
              🧹 Cleanliness: {review.cleanliness_rating}/5
            </Text>
          </View>
        )}
        {review.accuracy_rating && (
          <View className="bg-sand-100 px-3 py-1 rounded-full">
            <Text className="text-xs text-forest">
              ✓ Accuracy: {review.accuracy_rating}/5
            </Text>
          </View>
        )}
        {review.communication_rating && (
          <View className="bg-sand-100 px-3 py-1 rounded-full">
            <Text className="text-xs text-forest">
              💬 Communication: {review.communication_rating}/5
            </Text>
          </View>
        )}
      </View>

      {/* Response */}
      {review.host_response && (
        <View className="mt-3 pt-3 border-t border-sand-200">
          <Text className="text-xs font-semibold text-moss mb-1">Host Response:</Text>
          <Text className="text-sm text-forest">{review.host_response}</Text>
        </View>
      )}
    </View>
  );

  const ReviewSkeleton = () => (
    <View className="bg-white rounded-2xl p-4 mb-3 mx-4">
      <Skeleton height={18} width="70%" className="mb-2" />
      <View className="flex-row items-center mb-3">
        <Skeleton height={40} width={40} borderRadius={20} className="mr-3" />
        <View className="flex-1">
          <Skeleton height={14} width="50%" className="mb-2" />
          <Skeleton height={12} width="30%" />
        </View>
      </View>
      <Skeleton height={60} width="100%" className="mb-3" />
      <View className="flex-row gap-2">
        <Skeleton height={24} width={100} borderRadius={12} />
        <Skeleton height={24} width={90} borderRadius={12} />
      </View>
    </View>
  );

  return (
    <View className="flex-1 bg-sand-100">
      {/* Modern Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="px-4 pt-12 pb-6"
      >
        <TouchableOpacity onPress={() => router.back()} className="mb-3">
          <Ionicons name="arrow-back" size={24} color="#D9B168" />
        </TouchableOpacity>
        <Text className="text-3xl font-black text-white tracking-tight mb-2">
          Reviews
        </Text>
        <View className="flex-row items-center">
          <Ionicons name="star" size={16} color="#D9B168" />
          <Text className="text-sand-100 ml-2">
            {reviews.length} {reviews.length === 1 ? 'review' : 'reviews'}
          </Text>
        </View>
      </LinearGradient>

      {/* Summary Card */}
      <View className="mx-4 mt-4 mb-2 bg-white rounded-2xl p-5" style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 6 },
        shadowOpacity: 0.1,
        shadowRadius: 12,
        elevation: 6,
      }}>
        <View className="flex-row items-center justify-between">
          <View className="flex-1">
            <Text className="text-sm text-moss mb-2 font-semibold">Overall Rating</Text>
            <View className="flex-row items-center">
              <Ionicons name="star" size={32} color="#F59E0B" />
              <Text className="text-4xl font-black text-forest ml-2">N/A</Text>
            </View>
          </View>
          <View className="w-px h-16 bg-sand-200 mx-4" />
          <View className="flex-1 items-end">
            <Text className="text-sm text-moss mb-2 font-semibold">Total Reviews</Text>
            <Text className="text-4xl font-black text-forest">{reviews.length}</Text>
          </View>
        </View>
      </View>

      {/* Reviews List */}
      {isLoading ? (
        <View className="pt-2">
          {[1, 2, 3, 4].map((i) => (
            <ReviewSkeleton key={i} />
          ))}
        </View>
      ) : reviews.length === 0 ? (
        <View className="flex-1 justify-center items-center px-6">
          <View className="bg-white rounded-3xl p-8 items-center" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}>
            <View className="bg-sand-200 rounded-full p-8 mb-6">
              <Ionicons name="star-outline" size={72} color="#3A5C50" />
            </View>
            <Text className="text-2xl font-bold text-forest mb-3">No Reviews Yet</Text>
            <Text className="text-moss text-center px-4 leading-6">
              Reviews from guests will appear here once you start hosting
            </Text>
          </View>
        </View>
      ) : (
        <FlatList
          data={reviews}
          renderItem={({ item }) => <ReviewItem review={item} />}
          keyExtractor={(item) => item.id}
          contentContainerStyle={{ paddingVertical: 12, paddingBottom: 40 }}
        />
      )}
    </View>
  );
}

ReviewsScreen.displayName = 'ReviewsScreen';
