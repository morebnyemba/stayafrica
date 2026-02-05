import { View, Text, TouchableOpacity } from 'react-native';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import Animated, { FadeInDown } from 'react-native-reanimated';

interface EmptyStateProps {
  icon: keyof typeof Ionicons.glyphMap;
  title: string;
  description: string;
  actionLabel?: string;
  onAction?: () => void;
  variant?: 'default' | 'compact';
}

export function EmptyState({
  icon,
  title,
  description,
  actionLabel,
  onAction,
  variant = 'default',
}: EmptyStateProps) {
  if (variant === 'compact') {
    return (
      <Animated.View
        entering={FadeInDown.duration(400).springify()}
        className="py-12 px-6 items-center"
      >
        <View className="bg-sand-200 rounded-full p-6 mb-4">
          <Ionicons name={icon} size={48} color="#3A5C50" />
        </View>
        <Text className="text-primary-900 text-lg font-bold mb-2 text-center">
          {title}
        </Text>
        <Text className="text-primary-600 text-center text-sm mb-6">
          {description}
        </Text>
        {actionLabel && onAction && (
          <TouchableOpacity onPress={onAction}>
            <LinearGradient
              colors={['#D9B168', '#bea04f']}
              className="px-6 py-3 rounded-xl"
              style={{
                shadowColor: '#D9B168',
                shadowOffset: { width: 0, height: 3 },
                shadowOpacity: 0.25,
                shadowRadius: 6,
                elevation: 4,
              }}
            >
              <Text className="text-forest font-bold text-sm">{actionLabel}</Text>
            </LinearGradient>
          </TouchableOpacity>
        )}
      </Animated.View>
    );
  }

  return (
    <Animated.View
      entering={FadeInDown.duration(500).springify()}
      className="flex-1 justify-center items-center px-6"
    >
      <View
        className="bg-white rounded-3xl p-8 items-center"
        style={{
          shadowColor: '#000',
          shadowOffset: { width: 0, height: 8 },
          shadowOpacity: 0.1,
          shadowRadius: 16,
          elevation: 8,
          maxWidth: 400,
        }}
      >
        {/* Icon with gradient background */}
        <LinearGradient
          colors={['rgba(217, 177, 104, 0.15)', 'rgba(217, 177, 104, 0.05)']}
          className="rounded-full p-8 mb-6"
        >
          <Ionicons name={icon} size={72} color="#D9B168" />
        </LinearGradient>

        {/* Title */}
        <Text className="text-2xl font-bold text-forest mb-3 text-center">
          {title}
        </Text>

        {/* Description */}
        <Text className="text-moss text-center mb-8 px-4 leading-6">
          {description}
        </Text>

        {/* Action Button */}
        {actionLabel && onAction && (
          <TouchableOpacity onPress={onAction} className="w-full">
            <LinearGradient
              colors={['#D9B168', '#bea04f']}
              className="px-8 py-4 rounded-2xl items-center"
              style={{
                shadowColor: '#D9B168',
                shadowOffset: { width: 0, height: 4 },
                shadowOpacity: 0.3,
                shadowRadius: 8,
                elevation: 5,
              }}
            >
              <Text className="text-forest font-bold text-base">{actionLabel}</Text>
            </LinearGradient>
          </TouchableOpacity>
        )}
      </View>
    </Animated.View>
  );
}
