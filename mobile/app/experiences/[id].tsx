import { View, Text, ScrollView, TouchableOpacity, Platform, ActivityIndicator, Image } from 'react-native';
import { useRouter, useLocalSearchParams } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useSafeAreaInsets } from 'react-native-safe-area-context';
import { useExperienceById } from '@/hooks/api-hooks';
import { useState } from 'react';

const durationLabels: Record<string, string> = {
  half_day: 'Half Day',
  full_day: 'Full Day',
  multi_day: 'Multi-Day',
  hourly: 'Hourly',
};

const difficultyColors: Record<string, string> = {
  easy: '#10B981',
  moderate: '#F59E0B',
  challenging: '#EF4444',
};

export default function ExperienceDetailScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const params = useLocalSearchParams();
  const experienceId = params.id as string;
  const [selectedDate, setSelectedDate] = useState('');

  const { data: experience, isLoading } = useExperienceById(experienceId);

  const handleBookNow = () => {
    // Navigate to booking page for experiences
    router.push({
      pathname: '/booking/confirm',
      params: {
        experienceId,
        date: selectedDate || new Date().toISOString().split('T')[0],
      },
    });
  };

  if (isLoading) {
    return (
      <View className="flex-1 bg-sand-100 items-center justify-center">
        <ActivityIndicator size="large" color="#D9B168" />
        <Text className="mt-4 text-moss">Loading experience...</Text>
      </View>
    );
  }

  if (!experience) {
    return (
      <View className="flex-1 bg-sand-100 items-center justify-center">
        <Text className="text-forest text-lg">Experience not found</Text>
        <TouchableOpacity onPress={() => router.back()} className="mt-4">
          <Text className="text-gold font-semibold">Go Back</Text>
        </TouchableOpacity>
      </View>
    );
  }

  return (
    <ScrollView 
      className="flex-1 bg-sand-100" 
      showsVerticalScrollIndicator={false}
      contentContainerStyle={{ paddingBottom: insets.bottom + 24 }}
    >
      {/* Header Image */}
      {experience.cover_image && (
        <View style={{ height: 300 }}>
          <Image
            source={{ uri: experience.cover_image }}
            style={{ width: '100%', height: 300 }}
            resizeMode="cover"
          />
          <LinearGradient
            colors={['rgba(0,0,0,0.6)', 'transparent']}
            style={{ position: 'absolute', top: 0, left: 0, right: 0, height: 100 }}
          >
            <View style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35, paddingHorizontal: 16 }}>
              <TouchableOpacity onPress={() => router.back()}>
                <View className="w-10 h-10 rounded-xl items-center justify-center" style={{ backgroundColor: 'rgba(255, 255, 255, 0.9)' }}>
                  <Ionicons name="arrow-back" size={24} color="#122F26" />
                </View>
              </TouchableOpacity>
            </View>
          </LinearGradient>
        </View>
      )}

      <View className="px-4 -mt-6">
        {/* Title Card */}
        <View className="bg-white rounded-2xl p-5 mb-4" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 8 },
          shadowOpacity: 0.1,
          shadowRadius: 16,
          elevation: 8,
        }}>
          <Text className="text-2xl font-black text-forest mb-2">
            {experience.title}
          </Text>
          {experience.category_name && (
            <View className="bg-gold/20 self-start px-3 py-1 rounded-full mb-3">
              <Text className="text-xs font-semibold text-forest">{experience.category_name}</Text>
            </View>
          )}
          <View className="flex-row items-center mb-3">
            <Ionicons name="location" size={16} color="#3A5C50" />
            <Text className="text-moss ml-1">{experience.city}, {experience.country}</Text>
          </View>
          {experience.average_rating > 0 && (
            <View className="flex-row items-center">
              <Ionicons name="star" size={16} color="#F59E0B" />
              <Text className="text-moss ml-1">
                {experience.average_rating.toFixed(1)} ({experience.review_count} reviews)
              </Text>
            </View>
          )}
        </View>

        {/* Quick Info */}
        <View className="bg-white rounded-2xl p-5 mb-4" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 4 },
          shadowOpacity: 0.08,
          shadowRadius: 8,
          elevation: 4,
        }}>
          <View className="flex-row justify-around">
            <View className="items-center">
              <View className="bg-gold/20 rounded-full p-3 mb-2">
                <Ionicons name="time" size={24} color="#D9B168" />
              </View>
              <Text className="text-xs text-moss">Duration</Text>
              <Text className="font-semibold text-forest">
                {experience.duration_hours}h · {durationLabels[experience.duration_type] || experience.duration_type}
              </Text>
            </View>
            <View className="items-center">
              <View className="bg-gold/20 rounded-full p-3 mb-2">
                <Ionicons name="people" size={24} color="#D9B168" />
              </View>
              <Text className="text-xs text-moss">Group Size</Text>
              <Text className="font-semibold text-forest">{experience.min_participants}-{experience.max_participants}</Text>
            </View>
            <View className="items-center">
              <View className="bg-gold/20 rounded-full p-3 mb-2">
                <Ionicons name="fitness" size={24} color={difficultyColors[experience.difficulty] || '#D9B168'} />
              </View>
              <Text className="text-xs text-moss">Difficulty</Text>
              <Text className="font-semibold text-forest capitalize">{experience.difficulty}</Text>
            </View>
          </View>
        </View>

        {/* Description */}
        <View className="bg-white rounded-2xl p-5 mb-4" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 4 },
          shadowOpacity: 0.08,
          shadowRadius: 8,
          elevation: 4,
        }}>
          <Text className="text-lg font-bold text-forest mb-3">About This Experience</Text>
          <Text className="text-moss leading-6">{experience.description}</Text>
        </View>

        {/* What's Included */}
        {experience.included_items && experience.included_items.length > 0 && (
          <View className="bg-white rounded-2xl p-5 mb-4" style={{
            shadowColor: '#122F26',
            shadowOffset: { width: 0, height: 4 },
            shadowOpacity: 0.08,
            shadowRadius: 8,
            elevation: 4,
          }}>
            <Text className="text-lg font-bold text-forest mb-3">What's Included</Text>
            {experience.included_items.map((item: string, index: number) => (
              <View key={index} className="flex-row items-center mb-2">
                <Ionicons name="checkmark-circle" size={20} color="#10B981" />
                <Text className="text-moss ml-2">{item}</Text>
              </View>
            ))}
          </View>
        )}

        {/* Requirements */}
        {experience.requirements && experience.requirements.length > 0 && (
          <View className="bg-white rounded-2xl p-5 mb-4" style={{
            shadowColor: '#122F26',
            shadowOffset: { width: 0, height: 4 },
            shadowOpacity: 0.08,
            shadowRadius: 8,
            elevation: 4,
          }}>
            <Text className="text-lg font-bold text-forest mb-3">Requirements</Text>
            {experience.requirements.map((item: string, index: number) => (
              <View key={index} className="flex-row items-center mb-2">
                <Ionicons name="information-circle" size={20} color="#D9B168" />
                <Text className="text-moss ml-2">{item}</Text>
              </View>
            ))}
          </View>
        )}

        {/* Cancellation Policy */}
        {experience.cancellation_policy && (
          <View className="bg-white rounded-2xl p-5 mb-4" style={{
            shadowColor: '#122F26',
            shadowOffset: { width: 0, height: 4 },
            shadowOpacity: 0.08,
            shadowRadius: 8,
            elevation: 4,
          }}>
            <Text className="text-lg font-bold text-forest mb-3">Cancellation Policy</Text>
            <Text className="text-moss leading-6">{experience.cancellation_policy}</Text>
          </View>
        )}

        {/* Price Card */}
        <View className="bg-white rounded-2xl p-5 mb-4" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 4 },
          shadowOpacity: 0.08,
          shadowRadius: 8,
          elevation: 4,
        }}>
          <View className="flex-row justify-between items-center mb-4">
            <View>
              <Text className="text-lg font-bold text-forest">Price Per Person</Text>
              <Text className="text-xs text-moss">Hosted by {experience.host_name}</Text>
            </View>
            <Text className="text-3xl font-black text-forest">
              {experience.currency} {experience.price_per_person}
            </Text>
          </View>
          <TouchableOpacity
            onPress={handleBookNow}
            className="rounded-2xl overflow-hidden"
          >
            <LinearGradient
              colors={['#D9B168', '#bea04f']}
              className="py-4 items-center"
              style={{
                shadowColor: '#D9B168',
                shadowOffset: { width: 0, height: 4 },
                shadowOpacity: 0.3,
                shadowRadius: 8,
                elevation: 5,
              }}
            >
              <View className="flex-row items-center">
                <Ionicons name="calendar" size={20} color="#122F26" />
                <Text className="text-forest font-bold text-base ml-2">Book This Experience</Text>
              </View>
            </LinearGradient>
          </TouchableOpacity>
        </View>
      </View>
    </ScrollView>
  );
}
