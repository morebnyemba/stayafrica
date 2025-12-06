import { View, Text, ActivityIndicator } from 'react-native';

export function LoadingSpinner() {
  return (
    <View className="flex-1 justify-center items-center">
      <ActivityIndicator size="large" color="#0ea5e9" />
    </View>
  );
}

export function EmptyState({ title, description }: { title: string; description?: string }) {
  return (
    <View className="flex-1 justify-center items-center px-6">
      <Text className="text-xl font-bold text-gray-800 mb-2">{title}</Text>
      {description && <Text className="text-gray-600 text-center">{description}</Text>}
    </View>
  );
}

export function ErrorBoundary({
  error,
  onRetry,
}: {
  error: string;
  onRetry: () => void;
}) {
  return (
    <View className="flex-1 justify-center items-center px-6">
      <Text className="text-red-600 text-center mb-4">{error}</Text>
      <TouchableOpacity className="bg-primary-600 px-6 py-2 rounded-lg" onPress={onRetry}>
        <Text className="text-white font-semibold">Retry</Text>
      </TouchableOpacity>
    </View>
  );
}

import { TouchableOpacity } from 'react-native';
