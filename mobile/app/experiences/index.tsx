import { View, Text, ScrollView, TouchableOpacity, Platform, ActivityIndicator, FlatList, Image } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { AnimatedCompassIcon } from '@/components/common/AnimatedCompassIcon';
import { LinearGradient } from 'expo-linear-gradient';
import { useState } from 'react';
import { useExperiences, useExperienceCategories } from '@/hooks/api-hooks';
import type { Experience } from '@/types';

const durationLabels: Record<string, string> = {
  half_day: 'Half Day',
  full_day: 'Full Day',
  multi_day: 'Multi-Day',
  hourly: 'Hourly',
};

export default function ExperiencesScreen() {
  const router = useRouter();
  const [selectedCategory, setSelectedCategory] = useState<string>('');
  const [searchParams, setSearchParams] = useState<any>({});

  const queryParams = {
    ...searchParams,
    ...(selectedCategory ? { category: selectedCategory } : {}),
  };

  const { data: experiencesData, isLoading } = useExperiences(
    Object.keys(queryParams).length > 0 ? queryParams : undefined
  );
  const { data: categoriesData } = useExperienceCategories();

  const experiences: Experience[] = experiencesData?.results || [];
  const categories = categoriesData || [];

  const ExperienceCard = ({ experience }: { experience: Experience }) => (
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
        {experience.cover_image && (
          <Image
            source={{ uri: experience.cover_image }}
            style={{ width: '100%', height: 200 }}
            resizeMode="cover"
          />
        )}
        <View className="p-4 bg-white rounded-2xl">
          {experience.category_name && (
            <View className="bg-gold/20 self-start px-3 py-1 rounded-full mb-2">
              <Text className="text-xs font-semibold text-forest">{experience.category_name}</Text>
            </View>
          )}
          <Text className="text-lg font-bold text-forest mb-1">
            {experience.title}
          </Text>
          <View className="flex-row items-center mb-2">
            <Ionicons name="location" size={14} color="#3A5C50" />
            <Text className="text-sm text-moss ml-1">
              {experience.city}, {experience.country}
            </Text>
          </View>
          <Text className="text-sm text-moss mb-3" numberOfLines={2}>
            {experience.description}
          </Text>
          <View className="flex-row justify-between items-center">
            <View className="flex-row items-center">
              <Ionicons name="time" size={16} color="#D9B168" />
              <Text className="text-sm text-moss ml-1">
                {experience.duration_hours}h · {durationLabels[experience.duration_type] || experience.duration_type}
              </Text>
            </View>
            <Text className="text-lg font-bold text-forest">
              {experience.currency} {experience.price_per_person}/person
            </Text>
          </View>
          <View className="flex-row items-center justify-between mt-2">
            {experience.average_rating > 0 && (
              <View className="flex-row items-center">
                <Ionicons name="star" size={14} color="#F59E0B" />
                <Text className="text-sm text-moss ml-1">
                  {experience.average_rating.toFixed(1)} ({experience.review_count} reviews)
                </Text>
              </View>
            )}
            <View className="flex-row items-center">
              <Ionicons name="people" size={14} color="#3A5C50" />
              <Text className="text-sm text-moss ml-1">
                {experience.min_participants}-{experience.max_participants} people
              </Text>
            </View>
          </View>
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
            <TouchableOpacity
              onPress={() => setSelectedCategory('')}
              className="mr-2"
            >
              <View
                className="px-4 py-2 rounded-full"
                style={{
                  backgroundColor: selectedCategory === '' ? '#D9B168' : '#ffffff',
                  shadowColor: '#122F26',
                  shadowOffset: { width: 0, height: 2 },
                  shadowOpacity: 0.05,
                  shadowRadius: 4,
                  elevation: 2,
                }}
              >
                <Text className={`font-semibold ${selectedCategory === '' ? 'text-white' : 'text-forest'}`}>All</Text>
              </View>
            </TouchableOpacity>
            {categories.map((cat) => (
              <TouchableOpacity
                key={cat.id}
                onPress={() => setSelectedCategory(String(cat.id))}
                className="mr-2"
              >
                <View
                  className="px-4 py-2 rounded-full"
                  style={{
                    backgroundColor: selectedCategory === String(cat.id) ? '#D9B168' : '#ffffff',
                    shadowColor: '#122F26',
                    shadowOffset: { width: 0, height: 2 },
                    shadowOpacity: 0.05,
                    shadowRadius: 4,
                    elevation: 2,
                  }}
                >
                  <Text className={`font-semibold ${selectedCategory === String(cat.id) ? 'text-white' : 'text-forest'}`}>
                    {cat.name}
                  </Text>
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
            {experiences.map((experience) => (
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
