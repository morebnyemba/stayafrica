import { View, Text, TouchableOpacity } from 'react-native';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import Animated, { FadeIn, FadeInUp } from 'react-native-reanimated';

interface ErrorStateProps {
  title?: string;
  message: string;
  onRetry?: () => void;
  retryLabel?: string;
  showIcon?: boolean;
  variant?: 'default' | 'inline';
}

export function ErrorState({
  title = 'Oops! Something went wrong',
  message,
  onRetry,
  retryLabel = 'Try Again',
  showIcon = true,
  variant = 'default',
}: ErrorStateProps) {
  if (variant === 'inline') {
    return (
      <Animated.View
        entering={FadeIn.duration(300)}
        className="bg-red-50 border-2 border-red-200 rounded-2xl p-4 mx-4 my-2"
      >
        <View className="flex-row items-start">
          <View className="bg-red-100 rounded-full p-2 mr-3">
            <Ionicons name="alert-circle" size={20} color="#EF4444" />
          </View>
          <View className="flex-1">
            <Text className="text-red-800 font-semibold text-sm mb-1">
              {title}
            </Text>
            <Text className="text-red-600 text-xs leading-5">
              {message}
            </Text>
            {onRetry && (
              <TouchableOpacity
                onPress={onRetry}
                className="mt-3 self-start"
              >
                <Text className="text-red-700 font-bold text-xs underline">
                  {retryLabel}
                </Text>
              </TouchableOpacity>
            )}
          </View>
        </View>
      </Animated.View>
    );
  }

  return (
    <Animated.View
      entering={FadeInUp.duration(500).springify()}
      className="flex-1 justify-center items-center px-6"
    >
      <View
        className="bg-white rounded-3xl p-8 items-center"
        style={{
          shadowColor: '#EF4444',
          shadowOffset: { width: 0, height: 8 },
          shadowOpacity: 0.15,
          shadowRadius: 16,
          elevation: 8,
          maxWidth: 400,
        }}
      >
        {/* Error Icon */}
        {showIcon && (
          <View className="bg-red-50 rounded-full p-8 mb-6">
            <Ionicons name="alert-circle-outline" size={72} color="#EF4444" />
          </View>
        )}

        {/* Title */}
        <Text className="text-2xl font-bold text-red-600 mb-3 text-center">
          {title}
        </Text>

        {/* Message */}
        <Text className="text-gray-600 text-center mb-8 px-4 leading-6">
          {message}
        </Text>

        {/* Retry Button */}
        {onRetry && (
          <TouchableOpacity onPress={onRetry} className="w-full">
            <LinearGradient
              colors={['#EF4444', '#DC2626']}
              className="px-8 py-4 rounded-2xl items-center flex-row justify-center"
              style={{
                shadowColor: '#EF4444',
                shadowOffset: { width: 0, height: 4 },
                shadowOpacity: 0.3,
                shadowRadius: 8,
                elevation: 5,
              }}
            >
              <Ionicons name="refresh" size={20} color="#fff" style={{ marginRight: 8 }} />
              <Text className="text-white font-bold text-base">{retryLabel}</Text>
            </LinearGradient>
          </TouchableOpacity>
        )}

        {/* Helper text */}
        <Text className="text-gray-400 text-xs text-center mt-4">
          If the problem persists, please contact support
        </Text>
      </View>
    </Animated.View>
  );
}
