import { View, Text, ScrollView, TouchableOpacity, Platform, ActivityIndicator, FlatList, Image } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { AnimatedCompassIcon } from '@/components/common/AnimatedCompassIcon';
import { LinearGradient } from 'expo-linear-gradient';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';

export default function ExperiencesScreen() {
  const router = useRouter();

  // Fetch experiences
  const { data: experiencesData, isLoading } = useQuery({
    queryKey: ['experiences'],
    queryFn: async () => {
      const response = await apiClient.get('/experiences/');
      return response.data;
    },
  });

  const experiences = experiencesData?.results || [];

  const ExperienceCard = ({ experience }: any) => (
    <TouchableOpacity
      onPress={() => router.push(`/experiences/${experience.id}`)}
      className="mb-4"
    >
      <View
        className="overflow-hidden"
        style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 4 },
          shadowOpacity: 0.08,
          shadowRadius: 8,
          elevation: 4,
        }}
      >
        {experience.images?.[0] && (
          <Image
            source={{ uri: experience.images[0] }}
            style={{ width: '100%', height: 200 }}
            resizeMode="cover"
          />
        )}
        <View className="p-4 bg-white rounded-2xl">
          <Text className="text-lg font-bold text-forest mb-1">
            {experience.title}
          </Text>
          <View className="flex-row items-center mb-2">
            <Ionicons name="location" size={14} color="#3A5C50" />
            <Text className="text-sm text-moss ml-1">
              {experience.location}
            </Text>
          </View>
          <Text className="text-sm text-moss mb-3 line-clamp-2">
            {experience.description}
          </Text>
          <View className="flex-row justify-between items-center">
            <View className="flex-row items-center">
              <Ionicons name="time" size={16} color="#D9B168" />
              <Text className="text-sm text-moss ml-1">{experience.duration}</Text>
            </View>
            <Text className="text-lg font-bold text-forest">
              ${experience.price}
            </Text>
          </View>
          {experience.rating && (
            <View className="flex-row items-center mt-2">
              <Ionicons name="star" size={14} color="#F59E0B" />
              <Text className="text-sm text-moss ml-1">
                {experience.rating} ({experience.review_count} reviews)
              </Text>
            </View>
          )}
        </View>
      </View>
    </TouchableOpacity>
  );

  return (
    <ScrollView 
      className="flex-1 bg-sand-100" 
      showsVerticalScrollIndicator={false}
      contentContainerStyle={{ paddingBottom: 40 }}
    >
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="px-4 pb-8"
        style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
      >
        <TouchableOpacity 
          onPress={() => router.back()} 
          className="mb-4 w-10 h-10 rounded-xl items-center justify-center"
          style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
        >
          <Ionicons name="arrow-back" size={24} color="#fff" />
        </TouchableOpacity>
        <Text className="text-4xl font-black text-white tracking-tight mb-2">
          Experiences
        </Text>
        <Text className="text-sand-200 text-base">
          Discover unique activities and adventures
        </Text>
      </LinearGradient>

      <View className="px-4 -mt-4">
        {/* Category Pills */}
        <View className="mb-4">
          <ScrollView horizontal showsHorizontalScrollIndicator={false} className="flex-row">
            {['All', 'Adventure', 'Culture', 'Food & Drink', 'Wellness', 'Nature'].map((category) => (
              <TouchableOpacity
                key={category}
                className="mr-2"
              >
                <View
                  className="px-4 py-2 rounded-full"
                  style={{
                    backgroundColor: '#ffffff',
                    shadowColor: '#122F26',
                    shadowOffset: { width: 0, height: 2 },
                    shadowOpacity: 0.05,
                    shadowRadius: 4,
                    elevation: 2,
                  }}
                >
                  <Text className="text-forest font-semibold">{category}</Text>
                </View>
              </TouchableOpacity>
            ))}
          </ScrollView>
        </View>

        {/* Experiences List */}
        {isLoading ? (
          <View className="py-12 items-center">
            <ActivityIndicator size="large" color="#D9B168" />
            <Text className="mt-4 text-moss">Loading experiences...</Text>
          </View>
        ) : experiences.length > 0 ? (
          <View>
            {experiences.map((experience: any) => (
              <ExperienceCard key={experience.id} experience={experience} />
            ))}
          </View>
        ) : (
          <View className="py-12 items-center">
            <View className="bg-sand-200 rounded-full p-8 mb-4">
              <AnimatedCompassIcon size={64} color="#94a3b8" />
            </View>
            <Text className="text-xl font-bold text-forest mb-2">No Experiences Yet</Text>
            <Text className="text-moss text-center px-6">
              Check back soon for exciting experiences in your area
            </Text>
          </View>
        )}
      </View>
    </ScrollView>
  );
}
