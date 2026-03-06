import { useEffect } from 'react';
import { StyleSheet, Dimensions, View, Text } from 'react-native';
import Animated, {
  useSharedValue,
  useAnimatedStyle,
  withTiming,
  withDelay,
  withSpring,
  Easing,
  runOnJS,
} from 'react-native-reanimated';

const { width } = Dimensions.get('window');

interface BrandedSplashProps {
  onFinish: () => void;
  duration?: number;
}

export function BrandedSplash({ onFinish, duration = 2200 }: BrandedSplashProps) {
  const containerOpacity = useSharedValue(1);
  const logoScale = useSharedValue(0.6);
  const logoOpacity = useSharedValue(0);
  const ringScale = useSharedValue(0.8);
  const ringOpacity = useSharedValue(0);
  const ring2Scale = useSharedValue(0.7);
  const ring2Opacity = useSharedValue(0);
  const taglineOpacity = useSharedValue(0);
  const taglineTranslateY = useSharedValue(12);
  const subtitleOpacity = useSharedValue(0);

  useEffect(() => {
    // Phase 1: Logo appears (0–400ms)
    logoOpacity.value = withTiming(1, { duration: 350, easing: Easing.out(Easing.cubic) });
    logoScale.value = withSpring(1, { damping: 14, stiffness: 120 });

    // Phase 2: Rings pulse outward (200–700ms)
    ringOpacity.value = withDelay(200, withTiming(0.25, { duration: 400 }));
    ringScale.value = withDelay(200, withTiming(1.6, { duration: 800, easing: Easing.out(Easing.cubic) }));
    ring2Opacity.value = withDelay(400, withTiming(0.15, { duration: 400 }));
    ring2Scale.value = withDelay(400, withTiming(2.2, { duration: 900, easing: Easing.out(Easing.cubic) }));

    // Phase 3: Tagline slides up (500–900ms)
    taglineOpacity.value = withDelay(500, withTiming(1, { duration: 400 }));
    taglineTranslateY.value = withDelay(500, withSpring(0, { damping: 16, stiffness: 100 }));

    // Phase 3b: Subtitle (700–1000ms)
    subtitleOpacity.value = withDelay(700, withTiming(0.6, { duration: 350 }));

    // Phase 4: Rings fade (after hold)
    const ringFadeDelay = duration - 600;
    ringOpacity.value = withDelay(ringFadeDelay, withTiming(0, { duration: 300 }));
    ring2Opacity.value = withDelay(ringFadeDelay, withTiming(0, { duration: 300 }));

    // Phase 5: Everything fades out
    const fadeOutDelay = duration - 400;
    containerOpacity.value = withDelay(fadeOutDelay, withTiming(0, { duration: 400 }, (finished) => {
      if (finished) runOnJS(onFinish)();
    }));
  }, [duration, onFinish]);

  const containerStyle = useAnimatedStyle(() => ({
    opacity: containerOpacity.value,
  }));

  const logoStyle = useAnimatedStyle(() => ({
    opacity: logoOpacity.value,
    transform: [{ scale: logoScale.value }],
  }));

  const ringStyle = useAnimatedStyle(() => ({
    opacity: ringOpacity.value,
    transform: [{ scale: ringScale.value }],
  }));

  const ring2Style = useAnimatedStyle(() => ({
    opacity: ring2Opacity.value,
    transform: [{ scale: ring2Scale.value }],
  }));

  const taglineStyle = useAnimatedStyle(() => ({
    opacity: taglineOpacity.value,
    transform: [{ translateY: taglineTranslateY.value }],
  }));

  const subtitleStyle = useAnimatedStyle(() => ({
    opacity: subtitleOpacity.value,
  }));

  return (
    <Animated.View style={[styles.container, containerStyle]}>
      {/* Decorative gradient orbs */}
      <View style={styles.orbTopRight} />
      <View style={styles.orbBottomLeft} />

      {/* Pulsing rings */}
      <Animated.View style={[styles.ring, ringStyle]} />
      <Animated.View style={[styles.ring, ring2Style]} />

      {/* Logo */}
      <Animated.Image
        source={require('../../../assets/logo.png')}
        style={[styles.logo, logoStyle]}
        resizeMode="contain"
      />

      {/* Tagline */}
      <Animated.View style={[styles.taglineContainer, taglineStyle]}>
        <Text style={styles.tagline}>Discover Africa's Finest Stays</Text>
      </Animated.View>

      {/* Subtle subtitle */}
      <Animated.View style={[styles.subtitleContainer, subtitleStyle]}>
        <View style={styles.dividerLine} />
        <Text style={styles.subtitle}>STAYAFRICA</Text>
        <View style={styles.dividerLine} />
      </Animated.View>
    </Animated.View>
  );
}

const LOGO_SIZE = width * 0.32;
const RING_SIZE = LOGO_SIZE * 1.4;

const styles = StyleSheet.create({
  container: {
    ...StyleSheet.absoluteFillObject,
    backgroundColor: '#122F26',
    justifyContent: 'center',
    alignItems: 'center',
    zIndex: 999,
  },
  orbTopRight: {
    position: 'absolute',
    top: -80,
    right: -60,
    width: 220,
    height: 220,
    borderRadius: 110,
    backgroundColor: 'rgba(217, 177, 104, 0.06)',
  },
  orbBottomLeft: {
    position: 'absolute',
    bottom: -60,
    left: -80,
    width: 200,
    height: 200,
    borderRadius: 100,
    backgroundColor: 'rgba(58, 92, 80, 0.15)',
  },
  ring: {
    position: 'absolute',
    width: RING_SIZE,
    height: RING_SIZE,
    borderRadius: RING_SIZE / 2,
    borderWidth: 1.5,
    borderColor: 'rgba(217, 177, 104, 0.3)',
  },
  logo: {
    width: LOGO_SIZE,
    height: LOGO_SIZE,
  },
  taglineContainer: {
    marginTop: 20,
  },
  tagline: {
    color: '#D9B168',
    fontSize: 17,
    fontWeight: '600',
    letterSpacing: 0.3,
    textAlign: 'center',
  },
  subtitleContainer: {
    flexDirection: 'row',
    alignItems: 'center',
    marginTop: 14,
    gap: 10,
  },
  dividerLine: {
    width: 24,
    height: 1,
    backgroundColor: 'rgba(217, 177, 104, 0.3)',
  },
  subtitle: {
    color: 'rgba(255, 255, 255, 0.4)',
    fontSize: 11,
    fontWeight: '700',
    letterSpacing: 4,
  },
});
