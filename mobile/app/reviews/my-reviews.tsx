import { View, Text, ScrollView, TouchableOpacity, Platform } from 'react-native';
import { useState } from 'react';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useSafeAreaInsets } from 'react-native-safe-area-context';
import { useAuth } from '@/context/auth-context';
import { useHostReviews } from '@/hooks/api-hooks';
import { Skeleton } from '@/components/common/Skeletons';
import type { Review } from '@/types';

export default function MyReviewsScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const { user, isAuthenticated } = useAuth();
  const [activeTab, setActiveTab] = useState<'written' | 'received'>('written');
  const { data, isLoading } = useHostReviews();

  const allReviews = data?.results || [];
  const writtenReviews = allReviews.filter((r: Review) => r.guest === user?.id);
  const receivedReviews = allReviews.filter((r: Review) => r.host === user?.id);
  const filteredReviews = activeTab === 'written' ? writtenReviews : receivedReviews;

  const renderStars = (rating: number) => {
    return (
      <View className="flex-row">
        {[1, 2, 3, 4, 5].map((star) => (
          <Ionicons
            key={star}
            name={star <= rating ? 'star' : 'star-outline'}
            size={16}
            color="#D9B168"
          />
        ))}
      </View>
    );
  };

  if (!isAuthenticated) {
    return (
      <View className="flex-1 bg-sand-100">
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
        >
          <View className="flex-row items-center">
            <TouchableOpacity
              onPress={() => router.back()}
              className="w-10 h-10 rounded-xl items-center justify-center mr-3"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="arrow-back" size={24} color="#fff" />
            </TouchableOpacity>
            <Text className="text-xl font-bold text-white">My Reviews</Text>
          </View>
        </LinearGradient>

        <View className="flex-1 items-center justify-center px-6">
          <View className="bg-sand-200 rounded-full p-6 mb-4">
            <Ionicons name="star-outline" size={48} color="#94a3b8" />
          </View>
          <Text className="text-xl font-bold text-forest mb-2">Sign In Required</Text>
          <Text className="text-moss text-center mb-6">Sign in to view your reviews</Text>
          <TouchableOpacity onPress={() => router.push('/(auth)/login')}>
            <LinearGradient
              colors={['#D9B168', '#bea04f']}
              className="px-8 py-4 rounded-2xl"
            >
              <Text className="text-forest font-bold">Sign In</Text>
            </LinearGradient>
          </TouchableOpacity>
        </View>
      </View>
    );
  }

  return (
    <ScrollView className="flex-1 bg-sand-100" showsVerticalScrollIndicator={false} contentContainerStyle={{ paddingBottom: insets.bottom + 24 }}>
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f']}
        className="px-4 pb-6"
        style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
      >
        <View className="flex-row items-center mb-6">
          <TouchableOpacity
            onPress={() => router.back()}
            className="w-10 h-10 rounded-xl items-center justify-center mr-3"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </TouchableOpacity>
          <Text className="text-xl font-bold text-white">My Reviews</Text>
        </View>

        {/* Tab Switcher */}
        <View className="flex-row bg-white/10 rounded-xl p-1">
          <TouchableOpacity
            className={`flex-1 py-3 rounded-lg ${activeTab === 'written' ? 'bg-gold' : ''}`}
            onPress={() => setActiveTab('written')}
          >
            <Text className={`text-center font-semibold ${activeTab === 'written' ? 'text-forest' : 'text-white'}`}>
              Reviews I Wrote
            </Text>
          </TouchableOpacity>
          <TouchableOpacity
            className={`flex-1 py-3 rounded-lg ${activeTab === 'received' ? 'bg-gold' : ''}`}
            onPress={() => setActiveTab('received')}
          >
            <Text className={`text-center font-semibold ${activeTab === 'received' ? 'text-forest' : 'text-white'}`}>
              Reviews of Me
            </Text>
          </TouchableOpacity>
        </View>
      </LinearGradient>

      <View className="px-4 py-6">
        {isLoading ? (
          [1, 2, 3].map((i) => (
            <View key={i} className="bg-white rounded-2xl p-4 mb-3">
              <Skeleton height={16} width="60%" className="mb-3" />
              <Skeleton height={40} width="100%" className="mb-2" />
              <Skeleton height={12} width="30%" />
            </View>
          ))
        ) : filteredReviews.length > 0 ? (
          filteredReviews.map((review: Review) => (
            <View
              key={review.id}
              className="bg-white rounded-2xl p-4 mb-3"
              style={{
                shadowColor: '#122F26',
                shadowOffset: { width: 0, height: 2 },
                shadowOpacity: 0.05,
                shadowRadius: 4,
                elevation: 2,
              }}
            >
              <View className="flex-row items-center justify-between mb-2">
                <Text className="text-base font-semibold text-forest flex-1" numberOfLines={1}>
                  {review.property_title || review.property?.title || 'Property'}
                </Text>
                {renderStars(review.rating)}
              </View>
              <Text className="text-sm text-moss mb-2" numberOfLines={3}>
                {review.text || review.comment}
              </Text>
              <Text className="text-xs text-sand-400">
                {new Date(review.created_at).toLocaleDateString()}
              </Text>
              {review.host_response && (
                <View className="mt-3 pt-3 border-t border-sand-200">
                  <Text className="text-xs font-semibold text-moss mb-1">Host Response:</Text>
                  <Text className="text-sm text-forest">{review.host_response}</Text>
                </View>
              )}
            </View>
          ))
        ) : (
          <View className="items-center py-12">
            <View className="bg-sand-200 rounded-full p-6 mb-4">
              <Ionicons name="star-outline" size={48} color="#94a3b8" />
            </View>
            <Text className="text-xl font-bold text-forest mb-2">
              {activeTab === 'written' ? 'No Reviews Written' : 'No Reviews Yet'}
            </Text>
            <Text className="text-moss text-center px-8">
              {activeTab === 'written' 
                ? 'After your stays, share your experience by writing reviews'
                : 'Reviews from hosts will appear here after your stays'
              }
            </Text>
          </View>
        )}
      </View>
    </ScrollView>
  );
}
