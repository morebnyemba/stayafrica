import { View, Text, ScrollView, TouchableOpacity, Platform, ActivityIndicator, Image } from 'react-native';
import { useRouter, useLocalSearchParams } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { useState } from 'react';

export default function ExperienceDetailScreen() {
  const router = useRouter();
  const params = useLocalSearchParams();
  const experienceId = params.id as string;
  const [selectedDate, setSelectedDate] = useState('');

  // Fetch experience details
  const { data: experience, isLoading } = useQuery({
    queryKey: ['experience', experienceId],
    queryFn: async () => {
      const response = await apiClient.get(`/experiences/${experienceId}/`);
      return response.data;
    },
    enabled: !!experienceId,
  });

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
      contentContainerStyle={{ paddingBottom: 40 }}
    >
      {/* Header Image */}
      {experience.images?.[0] && (
        <View style={{ height: 300 }}>
          <Image
            source={{ uri: experience.images[0] }}
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
          <View className="flex-row items-center mb-3">
            <Ionicons name="location" size={16} color="#3A5C50" />
            <Text className="text-moss ml-1">{experience.location}</Text>
          </View>
          {experience.rating && (
            <View className="flex-row items-center">
              <Ionicons name="star" size={16} color="#F59E0B" />
              <Text className="text-moss ml-1">
                {experience.rating} ({experience.review_count} reviews)
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
              <Text className="font-semibold text-forest">{experience.duration}</Text>
            </View>
            <View className="items-center">
              <View className="bg-gold/20 rounded-full p-3 mb-2">
                <Ionicons name="people" size={24} color="#D9B168" />
              </View>
              <Text className="text-xs text-moss">Group Size</Text>
              <Text className="font-semibold text-forest">{experience.max_participants || 'Varies'}</Text>
            </View>
            <View className="items-center">
              <View className="bg-gold/20 rounded-full p-3 mb-2">
                <Ionicons name="language" size={24} color="#D9B168" />
              </View>
              <Text className="text-xs text-moss">Language</Text>
              <Text className="font-semibold text-forest">{experience.language || 'English'}</Text>
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
        {experience.inclusions && experience.inclusions.length > 0 && (
          <View className="bg-white rounded-2xl p-5 mb-4" style={{
            shadowColor: '#122F26',
            shadowOffset: { width: 0, height: 4 },
            shadowOpacity: 0.08,
            shadowRadius: 8,
            elevation: 4,
          }}>
            <Text className="text-lg font-bold text-forest mb-3">What's Included</Text>
            {experience.inclusions.map((item: string, index: number) => (
              <View key={index} className="flex-row items-center mb-2">
                <Ionicons name="checkmark-circle" size={20} color="#10B981" />
                <Text className="text-moss ml-2">{item}</Text>
              </View>
            ))}
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
            <Text className="text-lg font-bold text-forest">Price</Text>
            <Text className="text-3xl font-black text-forest">${experience.price}</Text>
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

ExperienceDetailScreen.displayName = 'ExperienceDetailScreen';
