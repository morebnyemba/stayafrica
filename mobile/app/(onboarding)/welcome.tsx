import { useState, useRef, useCallback, useEffect } from 'react';
import {
  View,
  Text,
  Image,
  TouchableOpacity,
  Dimensions,
  FlatList,
  NativeScrollEvent,
  NativeSyntheticEvent,
  StyleSheet,
} from 'react-native';
import { LinearGradient } from 'expo-linear-gradient';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { useSafeAreaInsets } from 'react-native-safe-area-context';
import { AnimatedCompassIcon } from '@/components/common/AnimatedCompassIcon';
import AsyncStorage from '@react-native-async-storage/async-storage';
import Animated, {
  useSharedValue,
  useAnimatedStyle,
  withTiming,
  withDelay,
  withSpring,
  withRepeat,
  withSequence,
  Easing,
} from 'react-native-reanimated';

const { width, height } = Dimensions.get('window');

const ONBOARDING_KEY = 'has_seen_onboarding';

interface OnboardingSlide {
  id: string;
  title: string;
  subtitle: string;
  description: string;
  icon: keyof typeof Ionicons.glyphMap;
  features?: string[];
  gradientAngle: { start: { x: number; y: number }; end: { x: number; y: number } };
  colors: string[];
}

const slides: OnboardingSlide[] = [
  {
    id: '1',
    title: 'Welcome to\nStayAfrica',
    subtitle: 'Your African Adventure Awaits',
    description:
      'Discover unique accommodations across the African continent — from luxury lodges to authentic local stays.',
    icon: 'globe-outline',
    gradientAngle: { start: { x: 0, y: 0 }, end: { x: 0.6, y: 1 } },
    colors: ['#0e2a20', '#163830', '#1d4538'],
  },
  {
    id: '2',
    title: 'Explore\nAfrica',
    subtitle: 'Find Your Perfect Stay',
    description:
      'Browse thousands of properties in stunning destinations. Beach houses, safari lodges, city apartments, and more.',
    icon: 'compass-outline',
    features: ['Safari Lodges', 'Beach Villas', 'City Apartments'],
    gradientAngle: { start: { x: 0.3, y: 0 }, end: { x: 0.8, y: 1 } },
    colors: ['#122F26', '#1f4035', '#2a5245'],
  },
  {
    id: '3',
    title: 'Book with\nConfidence',
    subtitle: 'Secure & Easy Booking',
    description:
      'Instant booking with secure payments. Chat directly with hosts and enjoy hassle-free travel across the continent.',
    icon: 'shield-checkmark-outline',
    features: ['Instant Booking', 'Secure Payments', 'Direct Messaging'],
    gradientAngle: { start: { x: 0.5, y: 0 }, end: { x: 1, y: 1 } },
    colors: ['#1a3a30', '#254c40', '#305e52'],
  },
  {
    id: '4',
    title: 'Become\na Host',
    subtitle: 'Share Your Space',
    description:
      'List your property and earn income by hosting travelers from around the world. Join our community of African hosts.',
    icon: 'home-outline',
    features: ['Easy Listing', 'Earn Income', 'Host Community'],
    gradientAngle: { start: { x: 0, y: 0.2 }, end: { x: 1, y: 0.8 } },
    colors: ['#1f4035', '#2d5548', '#3a6a5c'],
  },
];

// Floating orb decoration
function FloatingOrb({
  size,
  color,
  top,
  left,
  delay = 0,
}: {
  size: number;
  color: string;
  top: number;
  left: number;
  delay?: number;
}) {
  const translateY = useSharedValue(0);

  useEffect(() => {
    translateY.value = withDelay(
      delay,
      withRepeat(
        withSequence(
          withTiming(-12, { duration: 2800, easing: Easing.inOut(Easing.sin) }),
          withTiming(12, { duration: 2800, easing: Easing.inOut(Easing.sin) }),
        ),
        -1,
        true,
      ),
    );
  }, []);

  const style = useAnimatedStyle(() => ({
    transform: [{ translateY: translateY.value }],
  }));

  return (
    <Animated.View
      style={[
        {
          position: 'absolute',
          top,
          left,
          width: size,
          height: size,
          borderRadius: size / 2,
          backgroundColor: color,
        },
        style,
      ]}
    />
  );
}

// Animated slide content that fades in when visible
function SlideContent({
  item,
  index,
  isActive,
}: {
  item: OnboardingSlide;
  index: number;
  isActive: boolean;
}) {
  const contentOpacity = useSharedValue(0);
  const contentTranslateY = useSharedValue(30);
  const iconScale = useSharedValue(0.5);

  useEffect(() => {
    if (isActive) {
      iconScale.value = withSpring(1, { damping: 12, stiffness: 100 });
      contentOpacity.value = withDelay(150, withTiming(1, { duration: 400 }));
      contentTranslateY.value = withDelay(150, withSpring(0, { damping: 16, stiffness: 90 }));
    } else {
      contentOpacity.value = withTiming(0, { duration: 200 });
      contentTranslateY.value = withTiming(20, { duration: 200 });
      iconScale.value = withTiming(0.7, { duration: 200 });
    }
  }, [isActive]);

  const contentStyle = useAnimatedStyle(() => ({
    opacity: contentOpacity.value,
    transform: [{ translateY: contentTranslateY.value }],
  }));

  const iconStyle = useAnimatedStyle(() => ({
    transform: [{ scale: iconScale.value }],
  }));

  const isFirst = index === 0;

  return (
    <View style={{ width, flex: 1 }}>
      <LinearGradient
        colors={item.colors as [string, string, string]}
        start={item.gradientAngle.start}
        end={item.gradientAngle.end}
        style={{ flex: 1 }}
      >
        {/* Decorative floating orbs */}
        <FloatingOrb size={140} color="rgba(217,177,104,0.04)" top={height * 0.08} left={width * 0.6} delay={0} />
        <FloatingOrb size={100} color="rgba(58,92,80,0.08)" top={height * 0.35} left={-30} delay={600} />
        <FloatingOrb size={80} color="rgba(217,177,104,0.03)" top={height * 0.55} left={width * 0.7} delay={1200} />

        {/* Content area */}
        <View style={localStyles.slideContent}>
          {/* Icon/Logo */}
          <Animated.View style={iconStyle}>
            {isFirst ? (
              <View style={localStyles.logoContainer}>
                <Image
                  source={require('@/../assets/logo.png')}
                  style={{ width: 110, height: 110 }}
                  resizeMode="contain"
                  accessibilityLabel="StayAfrica logo"
                />
              </View>
            ) : (
              <View style={localStyles.iconContainer}>
                {/* Decorative ring */}
                <View style={localStyles.iconRing} />
                {item.icon === 'compass-outline' ? (
                  <AnimatedCompassIcon size={64} color="#D9B168" />
                ) : (
                  <Ionicons name={item.icon} size={64} color="#D9B168" />
                )}
              </View>
            )}
          </Animated.View>

          {/* Text content */}
          <Animated.View style={[localStyles.textContent, contentStyle]}>
            {/* Title */}
            <Text style={localStyles.title}>{item.title}</Text>

            {/* Subtitle pill */}
            <View style={localStyles.subtitlePill}>
              <Text style={localStyles.subtitleText}>{item.subtitle}</Text>
            </View>

            {/* Description */}
            <Text style={localStyles.description}>{item.description}</Text>

            {/* Feature tags */}
            {item.features && (
              <View style={localStyles.featuresRow}>
                {item.features.map((feature, i) => (
                  <View key={i} style={localStyles.featureTag}>
                    <Ionicons name="checkmark-circle" size={14} color="#D9B168" />
                    <Text style={localStyles.featureText}>{feature}</Text>
                  </View>
                ))}
              </View>
            )}
          </Animated.View>
        </View>
      </LinearGradient>
    </View>
  );
}

export default function WelcomeScreen() {
  const [currentIndex, setCurrentIndex] = useState(0);
  const flatListRef = useRef<FlatList>(null);
  const router = useRouter();
  const insets = useSafeAreaInsets();

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

  const onScroll = useCallback((e: NativeSyntheticEvent<NativeScrollEvent>) => {
    const index = Math.round(e.nativeEvent.contentOffset.x / width);
    setCurrentIndex(index);
  }, []);

  const renderSlide = ({ item, index }: { item: OnboardingSlide; index: number }) => (
    <SlideContent item={item} index={index} isActive={currentIndex === index} />
  );

  const isLastSlide = currentIndex === slides.length - 1;
  const progress = (currentIndex + 1) / slides.length;

  return (
    <View style={{ flex: 1, backgroundColor: '#122F26' }}>
      {/* Skip Button */}
      <TouchableOpacity
        style={[localStyles.skipButton, { top: insets.top + 10 }]}
        onPress={handleSkip}
        activeOpacity={0.7}
        accessibilityRole="button"
        accessibilityLabel="Skip onboarding"
      >
        <Text style={localStyles.skipText}>Skip</Text>
        <Ionicons name="chevron-forward" size={14} color="rgba(255,255,255,0.7)" />
      </TouchableOpacity>

      {/* Slides */}
      <FlatList
        ref={flatListRef}
        data={slides}
        renderItem={renderSlide}
        horizontal
        pagingEnabled
        showsHorizontalScrollIndicator={false}
        onMomentumScrollEnd={onScroll}
        keyExtractor={(item: OnboardingSlide) => item.id}
        bounces={false}
        getItemLayout={(_, index) => ({ length: width, offset: width * index, index })}
      />

      {/* Bottom Section with gradient overlay */}
      <LinearGradient
        colors={['transparent', 'rgba(14,42,32,0.95)', 'rgba(14,42,32,1)']}
        style={[localStyles.bottomSection, { paddingBottom: insets.bottom + 16 }]}
        pointerEvents="box-none"
      >
        {/* Progress bar */}
        <View style={localStyles.progressContainer}>
          <View style={localStyles.progressTrack}>
            <Animated.View
              style={[
                localStyles.progressFill,
                { width: `${progress * 100}%` },
              ]}
            />
          </View>
          <Text style={localStyles.progressText}>
            {currentIndex + 1} of {slides.length}
          </Text>
        </View>

        {/* Buttons */}
        {isLastSlide ? (
          <View style={{ gap: 12 }}>
            {/* Get Started Button */}
            <TouchableOpacity
              onPress={handleGetStarted}
              activeOpacity={0.9}
              accessibilityRole="button"
              accessibilityLabel="Get started with StayAfrica"
            >
              <LinearGradient
                colors={['#D9B168', '#c5a050']}
                start={{ x: 0, y: 0 }}
                end={{ x: 1, y: 0 }}
                style={localStyles.primaryButton}
              >
                <Text style={localStyles.primaryButtonText}>Get Started</Text>
                <Ionicons name="arrow-forward" size={20} color="#122F26" />
              </LinearGradient>
            </TouchableOpacity>

            {/* Sign In Button */}
            <TouchableOpacity
              style={localStyles.secondaryButton}
              onPress={handleSignIn}
              activeOpacity={0.8}
              accessibilityRole="button"
              accessibilityLabel="Sign in to existing account"
            >
              <Text style={localStyles.secondaryButtonText}>I have an account</Text>
            </TouchableOpacity>
          </View>
        ) : (
          <TouchableOpacity
            onPress={handleNext}
            activeOpacity={0.9}
            accessibilityRole="button"
            accessibilityLabel={`Continue to step ${currentIndex + 2}`}
          >
            <LinearGradient
              colors={['#D9B168', '#c5a050']}
              start={{ x: 0, y: 0 }}
              end={{ x: 1, y: 0 }}
              style={localStyles.primaryButton}
            >
              <Text style={localStyles.primaryButtonText}>Continue</Text>
              <Ionicons name="arrow-forward" size={20} color="#122F26" />
            </LinearGradient>
          </TouchableOpacity>
        )}
      </LinearGradient>
    </View>
  );
}

const localStyles = StyleSheet.create({
  slideContent: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
    paddingHorizontal: 32,
    paddingBottom: 180,
  },
  logoContainer: {
    backgroundColor: 'rgba(255,255,255,0.08)',
    borderRadius: 28,
    padding: 24,
    marginBottom: 32,
    shadowColor: '#000',
    shadowOffset: { width: 0, height: 10 },
    shadowOpacity: 0.25,
    shadowRadius: 20,
    elevation: 10,
  },
  iconContainer: {
    backgroundColor: 'rgba(255,255,255,0.06)',
    borderRadius: 50,
    width: 120,
    height: 120,
    justifyContent: 'center',
    alignItems: 'center',
    marginBottom: 32,
  },
  iconRing: {
    position: 'absolute',
    width: 140,
    height: 140,
    borderRadius: 70,
    borderWidth: 1,
    borderColor: 'rgba(217,177,104,0.15)',
  },
  textContent: {
    alignItems: 'center',
  },
  title: {
    fontSize: 38,
    fontWeight: '900',
    color: '#fff',
    textAlign: 'center',
    marginBottom: 12,
    lineHeight: 44,
    letterSpacing: -0.5,
  },
  subtitlePill: {
    backgroundColor: 'rgba(217,177,104,0.15)',
    paddingHorizontal: 16,
    paddingVertical: 8,
    borderRadius: 20,
    marginBottom: 20,
    borderWidth: 1,
    borderColor: 'rgba(217,177,104,0.2)',
  },
  subtitleText: {
    color: '#D9B168',
    fontWeight: '600',
    fontSize: 14,
    letterSpacing: 0.3,
  },
  description: {
    color: 'rgba(255,255,255,0.75)',
    textAlign: 'center',
    fontSize: 16,
    lineHeight: 24,
    paddingHorizontal: 8,
    marginBottom: 20,
  },
  featuresRow: {
    flexDirection: 'row',
    flexWrap: 'wrap',
    justifyContent: 'center',
    gap: 8,
  },
  featureTag: {
    flexDirection: 'row',
    alignItems: 'center',
    backgroundColor: 'rgba(217,177,104,0.1)',
    paddingHorizontal: 12,
    paddingVertical: 6,
    borderRadius: 14,
    gap: 5,
  },
  featureText: {
    color: 'rgba(255,255,255,0.8)',
    fontSize: 12,
    fontWeight: '500',
  },
  skipButton: {
    position: 'absolute',
    right: 20,
    zIndex: 10,
    flexDirection: 'row',
    alignItems: 'center',
    backgroundColor: 'rgba(255,255,255,0.08)',
    paddingHorizontal: 14,
    paddingVertical: 8,
    borderRadius: 20,
    gap: 2,
  },
  skipText: {
    color: 'rgba(255,255,255,0.7)',
    fontWeight: '600',
    fontSize: 14,
  },
  bottomSection: {
    position: 'absolute',
    bottom: 0,
    left: 0,
    right: 0,
    paddingHorizontal: 24,
    paddingTop: 40,
  },
  progressContainer: {
    flexDirection: 'row',
    alignItems: 'center',
    marginBottom: 20,
    gap: 10,
  },
  progressTrack: {
    flex: 1,
    height: 3,
    backgroundColor: 'rgba(255,255,255,0.1)',
    borderRadius: 2,
    overflow: 'hidden',
  },
  progressFill: {
    height: '100%',
    backgroundColor: '#D9B168',
    borderRadius: 2,
  },
  progressText: {
    color: 'rgba(255,255,255,0.5)',
    fontSize: 12,
    fontWeight: '600',
    minWidth: 40,
    textAlign: 'right',
  },
  primaryButton: {
    flexDirection: 'row',
    alignItems: 'center',
    justifyContent: 'center',
    paddingVertical: 16,
    borderRadius: 16,
    gap: 8,
    shadowColor: '#D9B168',
    shadowOffset: { width: 0, height: 4 },
    shadowOpacity: 0.25,
    shadowRadius: 10,
    elevation: 6,
  },
  primaryButtonText: {
    color: '#122F26',
    fontWeight: '700',
    fontSize: 17,
  },
  secondaryButton: {
    paddingVertical: 16,
    borderRadius: 16,
    alignItems: 'center',
    borderWidth: 1.5,
    borderColor: 'rgba(255,255,255,0.2)',
  },
  secondaryButtonText: {
    color: '#fff',
    fontWeight: '700',
    fontSize: 17,
  },
});
