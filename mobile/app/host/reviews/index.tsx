import { View, Text, ScrollView, TouchableOpacity, Platform, FlatList } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';

export default function HostReviewsScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();

  if (!isAuthenticated) {
    return (
      <View className="flex-1 bg-sand-100">
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
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
      </View>
    );
  }

  // Sample reviews data (empty state)
  const reviews: any[] = [];

  const RatingStats = () => (
    <View
      className="p-5 mx-4 -mt-4"
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
          <Text className="text-4xl font-black text-forest">0.0</Text>
          <View className="flex-row mt-1">
            {[1, 2, 3, 4, 5].map((star) => (
              <Ionicons key={star} name="star-outline" size={16} color="#D9B168" />
            ))}
          </View>
          <Text className="text-sm text-moss mt-1">0 reviews</Text>
        </View>

        <View className="flex-1 ml-6">
          {[5, 4, 3, 2, 1].map((rating) => (
            <View key={rating} className="flex-row items-center mb-1">
              <Text className="text-xs text-moss w-3">{rating}</Text>
              <Ionicons name="star" size={10} color="#D9B168" className="ml-1" />
              <View className="flex-1 h-2 bg-sand-200 rounded-full ml-2">
                <View className="h-2 bg-gold rounded-full" style={{ width: '0%' }} />
              </View>
              <Text className="text-xs text-moss w-6 text-right">0</Text>
            </View>
          ))}
        </View>
      </View>
    </View>
  );

  const ReviewItem = ({ review }: any) => (
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
            {review.guest?.first_name?.[0] || 'G'}
          </Text>
        </View>
        <View className="flex-1 ml-3">
          <Text className="font-semibold text-forest">
            {review.guest?.first_name} {review.guest?.last_name}
          </Text>
          <Text className="text-xs text-moss">{review.property?.title}</Text>
        </View>
        <View className="flex-row items-center bg-gold/20 px-2 py-1 rounded-full">
          <Ionicons name="star" size={14} color="#D9B168" />
          <Text className="text-sm font-bold text-forest ml-1">{review.rating}</Text>
        </View>
      </View>
      <Text className="text-moss">{review.text}</Text>
      <Text className="text-xs text-moss/70 mt-2">{review.date}</Text>
    </View>
  );

  return (
    <View className="flex-1 bg-sand-100">
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="px-4 pb-8"
        style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
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
        {/* Rating Stats */}
        <RatingStats />

        {/* Reviews List */}
        <View className="mt-6 mb-8">
          <Text className="text-lg font-bold text-forest mb-3 mx-4">All Reviews</Text>

          {reviews.length === 0 ? (
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
            reviews.map((review, index) => (
              <ReviewItem key={index} review={review} />
            ))
          )}
        </View>
      </ScrollView>
    </View>
  );
}
