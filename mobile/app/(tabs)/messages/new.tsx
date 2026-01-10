import { View, Text, TouchableOpacity } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';

export default function NewMessageScreen() {
  const router = useRouter();

  return (
    <View className="flex-1 bg-white items-center justify-center px-6">
      <Ionicons name="chatbox-ellipses" size={64} color="#D4A574" />
      <Text className="text-2xl font-bold text-primary-900 mt-4">Start a conversation</Text>
      <Text className="text-gray-600 text-center mt-2 mb-6">
        New conversation creation will be available soon. In the meantime, open an existing chat.
      </Text>
      <TouchableOpacity
        className="bg-secondary-600 px-6 py-3 rounded-full"
        onPress={() => router.back()}
      >
        <Text className="text-white font-semibold">Go back</Text>
      </TouchableOpacity>
    </View>
  );
}
