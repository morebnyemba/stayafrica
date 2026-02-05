import React, { useEffect, useRef } from 'react';
import { Animated, Easing, ViewStyle } from 'react-native';
import { Ionicons } from '@expo/vector-icons';

interface AnimatedCompassIconProps {
  size?: number;
  color?: string;
  style?: ViewStyle;
  durationMs?: number;
  filled?: boolean;
}

export function AnimatedCompassIcon({
  size = 24,
  color = '#D9B168',
  style,
  durationMs = 4000,
  filled = false,
}: AnimatedCompassIconProps) {
  const rotateAnim = useRef(new Animated.Value(0)).current;

  useEffect(() => {
    const animation = Animated.loop(
      Animated.timing(rotateAnim, {
        toValue: 1,
        duration: durationMs,
        easing: Easing.linear,
        useNativeDriver: true,
      })
    );

    animation.start();
    return () => animation.stop();
  }, [durationMs, rotateAnim]);

  const rotate = rotateAnim.interpolate({
    inputRange: [0, 1],
    outputRange: ['0deg', '360deg'],
  });

  return (
    <Animated.View style={[{ transform: [{ rotate }] }, style]}>
      <Ionicons name={filled ? 'compass' : 'compass-outline'} size={size} color={color} />
    </Animated.View>
  );
}
