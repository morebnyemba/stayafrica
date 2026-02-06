import { useState, useRef } from 'react';
import {
  View,
  Text,
  Image,
  TouchableOpacity,
  Dimensions,
  FlatList,
  NativeScrollEvent,
  NativeSyntheticEvent,
} from 'react-native';
import { LinearGradient } from 'expo-linear-gradient';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { AnimatedCompassIcon } from '@/components/common/AnimatedCompassIcon';
import AsyncStorage from '@react-native-async-storage/async-storage';

const { width } = Dimensions.get('window');

const ONBOARDING_KEY = 'has_seen_onboarding';

interface OnboardingSlide {
  id: string;
  title: string;
  subtitle: string;
  description: string;
  icon: keyof typeof Ionicons.glyphMap;
  colors: string[];
}

const slides: OnboardingSlide[] = [
  {
    id: '1',
    title: 'Welcome to StayAfrica',
    subtitle: 'Your African Adventure Awaits',
    description: 'Discover unique accommodations across the African continent. From luxury lodges to authentic local stays.',
    icon: 'globe-outline',
    colors: ['#122F26', '#1d392f'],
  },
  {
    id: '2',
    title: 'Explore Africa',
    subtitle: 'Find Your Perfect Stay',
    description: 'Browse thousands of properties in stunning destinations. Beach houses, safari lodges, city apartments, and more.',
    icon: 'compass-outline',
    colors: ['#1d392f', '#2d4a40'],
  },
  {
    id: '3',
    title: 'Book with Confidence',
    subtitle: 'Secure & Easy Booking',
    description: 'Book instantly with secure payments. Communicate directly with hosts and enjoy hassle-free travel.',
    icon: 'shield-checkmark-outline',
    colors: ['#2d4a40', '#3a5c50'],
  },
  {
    id: '4',
    title: 'Become a Host',
    subtitle: 'Share Your Space',
    description: 'List your property and earn income by hosting travelers from around the world. Join our community of hosts.',
    icon: 'home-outline',
    colors: ['#3a5c50', '#4a6c60'],
  },
];

export default function WelcomeScreen() {
  const [currentIndex, setCurrentIndex] = useState(0);
  const flatListRef = useRef<FlatList>(null);
  const router = useRouter();

  const markOnboardingSeen = async () => {
    try {
      await AsyncStorage.setItem(ONBOARDING_KEY, 'true');
    } catch (error) {
      console.error('Error saving onboarding state:', error);
    }
  };

  const handleNext = () => {
    if (currentIndex < slides.length - 1) {
      flatListRef.current?.scrollToIndex({ index: currentIndex + 1 });
    } else {
      handleGetStarted();
    }
  };

  const handleSkip = () => {
    handleGetStarted();
  };

  const handleGetStarted = async () => {
    await markOnboardingSeen();
    router.replace('/(tabs)');
  };

  const handleSignIn = async () => {
    await markOnboardingSeen();
    router.replace('/(auth)/login');
  };

  const renderSlide = ({ item, index }: { item: OnboardingSlide; index: number }) => (
    <View className="flex-1" style={{ width }}>
      <LinearGradient
        colors={item.colors as [string, string]}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="flex-1"
      >
        {/* Content */}
        <View className="flex-1 justify-center items-center px-8">
          {/* Logo */}
          {index === 0 && (
            <View className="mb-8 bg-white/10 rounded-3xl p-6" style={{
              shadowColor: '#000',
              shadowOffset: { width: 0, height: 8 },
              shadowOpacity: 0.3,
              shadowRadius: 16,
              elevation: 8,
            }}>
              <Image
                source={require('@/../assets/logo.png')}
                style={{ width: 120, height: 120 }}
                resizeMode="contain"
              />
            </View>
          )}

          {/* Icon for other slides */}
          {index > 0 && (
            <View className="mb-8 bg-white/10 rounded-full p-8" style={{
              shadowColor: '#000',
              shadowOffset: { width: 0, height: 8 },
              shadowOpacity: 0.3,
              shadowRadius: 16,
              elevation: 8,
            }}>
              {item.icon === 'compass-outline' ? (
                <AnimatedCompassIcon size={80} color="#D9B168" />
              ) : (
                <Ionicons name={item.icon} size={80} color="#D9B168" />
              )}
            </View>
          )}

          {/* Title */}
          <Text className="text-4xl font-black text-white text-center mb-2 tracking-tight">
            {item.title}
          </Text>

          {/* Subtitle */}
          <View className="bg-gold/20 px-4 py-2 rounded-full mb-6">
            <Text className="text-gold font-semibold text-base">
              {item.subtitle}
            </Text>
          </View>

          {/* Description */}
          <Text className="text-white/80 text-center text-lg leading-7 px-4">
            {item.description}
          </Text>
        </View>
      </LinearGradient>
    </View>
  );

  const renderDots = () => (
    <View className="flex-row justify-center items-center mb-8">
      {slides.map((_, index) => (
        <View
          key={index}
          className={`mx-1.5 rounded-full ${
            index === currentIndex
              ? 'w-8 h-3 bg-gold'
              : 'w-3 h-3 bg-white/30'
          }`}
        />
      ))}
    </View>
  );

  return (
    <View className="flex-1 bg-forest">
      {/* Skip Button */}
      <TouchableOpacity
        className="absolute top-14 right-6 z-10 bg-white/10 px-4 py-2 rounded-full"
        onPress={handleSkip}
      >
        <Text className="text-white font-semibold">Skip</Text>
      </TouchableOpacity>

      {/* Slides */}
      <FlatList
        ref={flatListRef}
        data={slides}
        renderItem={renderSlide}
        horizontal
        pagingEnabled
        showsHorizontalScrollIndicator={false}
        onMomentumScrollEnd={(e: NativeSyntheticEvent<NativeScrollEvent>) => {
          const index = Math.round(e.nativeEvent.contentOffset.x / width);
          setCurrentIndex(index);
        }}
        keyExtractor={(item: OnboardingSlide) => item.id}
        bounces={false}
      />

      {/* Bottom Section */}
      <View className="absolute bottom-0 left-0 right-0 pb-12 px-6">
        {/* Dots */}
        {renderDots()}

        {/* Buttons */}
        {currentIndex === slides.length - 1 ? (
          <View className="gap-3">
            {/* Get Started Button */}
            <TouchableOpacity onPress={handleGetStarted} activeOpacity={0.9}>
              <LinearGradient
                colors={['#D9B168', '#bea04f']}
                start={{ x: 0, y: 0 }}
                end={{ x: 1, y: 0 }}
                className="py-4 rounded-2xl items-center"
                style={{
                  shadowColor: '#D9B168',
                  shadowOffset: { width: 0, height: 4 },
                  shadowOpacity: 0.3,
                  shadowRadius: 8,
                  elevation: 5,
                }}
              >
                <View className="flex-row items-center">
                  <Text className="text-forest font-bold text-lg mr-2">Get Started</Text>
                  <Ionicons name="arrow-forward" size={20} color="#122F26" />
                </View>
              </LinearGradient>
            </TouchableOpacity>

            {/* Sign In Button */}
            <TouchableOpacity
              className="py-4 rounded-2xl items-center border-2 border-white/30"
              onPress={handleSignIn}
              activeOpacity={0.8}
            >
              <Text className="text-white font-bold text-lg">I have an account</Text>
            </TouchableOpacity>
          </View>
        ) : (
          <TouchableOpacity onPress={handleNext} activeOpacity={0.9}>
            <LinearGradient
              colors={['#D9B168', '#bea04f']}
              start={{ x: 0, y: 0 }}
              end={{ x: 1, y: 0 }}
              className="py-4 rounded-2xl items-center"
              style={{
                shadowColor: '#D9B168',
                shadowOffset: { width: 0, height: 4 },
                shadowOpacity: 0.3,
                shadowRadius: 8,
                elevation: 5,
              }}
            >
              <View className="flex-row items-center">
                <Text className="text-forest font-bold text-lg mr-2">Next</Text>
                <Ionicons name="arrow-forward" size={20} color="#122F26" />
              </View>
            </LinearGradient>
          </TouchableOpacity>
        )}
      </View>
    </View>
  );
}

WelcomeScreen.displayName = 'WelcomeScreen';
