import { View, Text, FlatList, TouchableOpacity, ActivityIndicator } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { useAuth } from '@/context/auth-context';
import { useState } from 'react';
import { format } from 'date-fns';

export default function ReviewsScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const [reviews, setReviews] = useState<any[]>([]);
  const [isLoading, setIsLoading] = useState(false);

  if (!isAuthenticated) {
    return (
      <View className="flex-1 items-center justify-center bg-white px-6">
        <Ionicons name="star-outline" size={64} color="#ddd" />
        <Text className="text-xl font-bold text-gray-800 mt-4">Sign In Required</Text>
        <Text className="text-gray-600 text-center mt-2 mb-6">
          Please sign in to view your reviews
        </Text>
        <TouchableOpacity
          className="bg-primary-600 px-8 py-3 rounded-lg"
          onPress={() => router.push('/(auth)/login')}
        >
          <Text className="text-white font-semibold">Sign In</Text>
        </TouchableOpacity>
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

  const ReviewItem = ({ review }: any) => (
    <View className="bg-white rounded-xl p-4 mb-3 shadow-sm border border-gray-100">
      {/* Property Info */}
      <Text className="text-base font-bold text-gray-900 mb-2">
        {review.property?.title || 'Property'}
      </Text>

      {/* Reviewer Info */}
      <View className="flex-row items-center mb-3">
        <View className="w-10 h-10 rounded-full bg-secondary-100 items-center justify-center mr-3">
          <Ionicons name="person" size={20} color="#D9B168" />
        </View>
        <View className="flex-1">
          <Text className="text-sm font-semibold text-gray-900">
            {review.reviewer?.first_name} {review.reviewer?.last_name}
          </Text>
          <Text className="text-xs text-gray-600">
            {format(new Date(review.created_at), 'MMM dd, yyyy')}
          </Text>
        </View>
        <StarRating rating={review.rating} />
      </View>

      {/* Review Text */}
      <Text className="text-sm text-gray-700 leading-5 mb-3">
        {review.comment}
      </Text>

      {/* Review Categories */}
      <View className="flex-row flex-wrap gap-2">
        {review.cleanliness_rating && (
          <View className="bg-gray-50 px-3 py-1 rounded-full">
            <Text className="text-xs text-gray-700">
              🧹 Cleanliness: {review.cleanliness_rating}/5
            </Text>
          </View>
        )}
        {review.accuracy_rating && (
          <View className="bg-gray-50 px-3 py-1 rounded-full">
            <Text className="text-xs text-gray-700">
              ✓ Accuracy: {review.accuracy_rating}/5
            </Text>
          </View>
        )}
        {review.communication_rating && (
          <View className="bg-gray-50 px-3 py-1 rounded-full">
            <Text className="text-xs text-gray-700">
              💬 Communication: {review.communication_rating}/5
            </Text>
          </View>
        )}
      </View>

      {/* Response */}
      {review.host_response && (
        <View className="mt-3 pt-3 border-t border-gray-100">
          <Text className="text-xs font-semibold text-gray-600 mb-1">Host Response:</Text>
          <Text className="text-sm text-gray-700">{review.host_response}</Text>
        </View>
      )}
    </View>
  );

  return (
    <View className="flex-1 bg-gray-50">
      {/* Header */}
      <View className="bg-white px-4 pt-12 pb-4 border-b border-gray-100 shadow-sm">
        <TouchableOpacity onPress={() => router.back()} className="mb-2">
          <Ionicons name="arrow-back" size={24} color="#333" />
        </TouchableOpacity>
        <Text className="text-2xl font-extrabold text-primary-900">Reviews</Text>
        <Text className="text-sm text-gray-500 mt-1">
          {reviews.length} {reviews.length === 1 ? 'review' : 'reviews'}
        </Text>
      </View>

      {/* Summary Card */}
      <View className="mx-4 mt-4 mb-2 bg-white rounded-xl p-4 shadow-sm border border-gray-100">
        <View className="flex-row items-center justify-between">
          <View className="flex-1">
            <Text className="text-sm text-gray-600 mb-1">Overall Rating</Text>
            <View className="flex-row items-center">
              <Ionicons name="star" size={24} color="#F59E0B" />
              <Text className="text-3xl font-extrabold text-gray-900 ml-2">N/A</Text>
            </View>
          </View>
          <View className="w-px h-12 bg-gray-200 mx-4" />
          <View className="flex-1 items-end">
            <Text className="text-sm text-gray-600 mb-1">Total Reviews</Text>
            <Text className="text-3xl font-extrabold text-gray-900">{reviews.length}</Text>
          </View>
        </View>
      </View>

      {/* Reviews List */}
      {isLoading ? (
        <View className="flex-1 justify-center items-center">
          <ActivityIndicator size="large" color="#3A5C50" />
          <Text className="mt-2 text-gray-600">Loading reviews...</Text>
        </View>
      ) : reviews.length === 0 ? (
        <View className="flex-1 justify-center items-center px-6">
          <Ionicons name="star-outline" size={64} color="#ddd" />
          <Text className="text-xl font-bold text-gray-800 mt-4">No Reviews Yet</Text>
          <Text className="text-gray-600 text-center mt-2">
            Reviews from guests will appear here
          </Text>
        </View>
      ) : (
        <FlatList
          data={reviews}
          renderItem={({ item }) => <ReviewItem review={item} />}
          keyExtractor={(item) => item.id}
          contentContainerStyle={{ padding: 16 }}
        />
      )}
    </View>
  );
}
