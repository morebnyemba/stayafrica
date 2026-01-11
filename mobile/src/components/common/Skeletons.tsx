import { View } from 'react-native';
import Animated, { useSharedValue, useAnimatedStyle, withRepeat, withTiming, withSequence } from 'react-native-reanimated';
import { useEffect } from 'react';

interface SkeletonProps {
  width?: number | string;
  height?: number;
  borderRadius?: number;
  className?: string;
}

export function Skeleton({ width = '100%', height = 20, borderRadius = 8, className = '' }: SkeletonProps) {
  const opacity = useSharedValue(1);

  useEffect(() => {
    opacity.value = withRepeat(
      withSequence(
        withTiming(0.4, { duration: 800 }),
        withTiming(1, { duration: 800 })
      ),
      -1,
      false
    );
  }, []);

  const animatedStyle = useAnimatedStyle(() => ({
    opacity: opacity.value,
  }));

  return (
    <Animated.View
      className={`bg-sand-200 ${className}`}
      style={[
        {
          width,
          height,
          borderRadius,
        },
        animatedStyle,
      ]}
    />
  );
}

export function PropertyCardSkeleton() {
  return (
    <View className="mb-4 rounded-2xl overflow-hidden bg-white p-4 mx-4" style={{
      shadowColor: '#122F26',
      shadowOffset: { width: 0, height: 4 },
      shadowOpacity: 0.08,
      shadowRadius: 8,
      elevation: 4,
    }}>
      <Skeleton height={200} className="mb-4 rounded-2xl" />
      <Skeleton height={24} width="80%" className="mb-2" />
      <Skeleton height={16} width="60%" className="mb-3" />
      <View className="flex-row justify-between items-center mb-3">
        <Skeleton height={20} width={100} />
        <Skeleton height={20} width={60} />
      </View>
      <View className="flex-row justify-between">
        <Skeleton height={16} width={60} />
        <Skeleton height={16} width={60} />
        <Skeleton height={16} width={60} />
      </View>
    </View>
  );
}

export function BookingCardSkeleton() {
  return (
    <View className="bg-white rounded-2xl p-4 mb-3 mx-4" style={{
      shadowColor: '#122F26',
      shadowOffset: { width: 0, height: 4 },
      shadowOpacity: 0.08,
      shadowRadius: 8,
      elevation: 4,
    }}>
      <Skeleton height={20} width="70%" className="mb-2" />
      <Skeleton height={16} width="50%" className="mb-3" />
      <View className="flex-row items-center mb-2">
        <Skeleton height={16} width={150} />
      </View>
      <View className="flex-row items-center mb-3">
        <Skeleton height={16} width={100} />
      </View>
      <View className="flex-row justify-between">
        <Skeleton height={24} width={80} borderRadius={12} />
        <Skeleton height={24} width={60} />
      </View>
    </View>
  );
}

export function ProfileSkeleton() {
  return (
    <View className="p-4">
      <View className="items-center mb-6">
        <Skeleton height={112} width={112} borderRadius={56} className="mb-4" />
        <Skeleton height={24} width={150} className="mb-2" />
        <Skeleton height={16} width={200} />
      </View>
      
      <View className="space-y-4">
        {[1, 2, 3, 4].map((i) => (
          <View key={i} className="bg-white rounded-2xl p-4 mb-3" style={{
            shadowColor: '#122F26',
            shadowOffset: { width: 0, height: 2 },
            shadowOpacity: 0.05,
            shadowRadius: 4,
            elevation: 2,
          }}>
            <Skeleton height={20} width="40%" className="mb-2" />
            <Skeleton height={16} width="70%" />
          </View>
        ))}
      </View>
    </View>
  );
}

export function ListSkeleton({ count = 5 }: { count?: number }) {
  return (
    <View className="pt-4">
      {Array.from({ length: count }).map((_, index) => (
        <PropertyCardSkeleton key={index} />
      ))}
    </View>
  );
}
