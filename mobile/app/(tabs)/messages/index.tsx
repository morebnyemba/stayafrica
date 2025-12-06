import { View, Text, FlatList, TouchableOpacity } from 'react-native';
import { useConversations } from '@/hooks/api-hooks';
import { useAuth } from '@/context/auth-context';

export default function MessagesScreen() {
  const { isAuthenticated } = useAuth();
  const { data: conversations, isLoading } = useConversations();

  if (!isAuthenticated) {
    return (
      <View className="flex-1 items-center justify-center bg-white">
        <Text className="text-gray-600">Please login to view messages</Text>
      </View>
    );
  }

  return (
    <View className="flex-1 bg-white">
      <View className="px-4 py-4 border-b border-gray-200">
        <Text className="text-2xl font-bold">Messages</Text>
      </View>

      <FlatList
        data={conversations?.results}
        keyExtractor={(item) => item.participant_id}
        renderItem={({ item }) => (
          <TouchableOpacity className="px-4 py-4 border-b border-gray-200 flex-row items-center">
            {item.participant_picture && (
              <View className="w-12 h-12 rounded-full bg-gray-300 mr-3" />
            )}
            <View className="flex-1">
              <Text className="font-semibold text-lg">{item.participant_name}</Text>
              <Text className="text-gray-600 text-sm" numberOfLines={1}>
                {item.last_message}
              </Text>
            </View>
            {item.unread_count > 0 && (
              <View className="bg-primary-600 rounded-full w-6 h-6 items-center justify-center">
                <Text className="text-white text-xs font-bold">{item.unread_count}</Text>
              </View>
            )}
          </TouchableOpacity>
        )}
        ListEmptyComponent={
          !isLoading && (
            <View className="items-center justify-center py-8">
              <Text className="text-gray-600">No conversations yet</Text>
            </View>
          )
        }
      />
    </View>
  );
}
