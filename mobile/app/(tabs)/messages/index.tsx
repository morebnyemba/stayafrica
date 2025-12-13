import { View, Text, FlatList, TouchableOpacity, ActivityIndicator } from 'react-native';
import { useRouter } from 'expo-router';
import { useConversations } from '@/hooks/api-hooks';
import { useAuth } from '@/context/auth-context';
import { Ionicons } from '@expo/vector-icons';

interface Conversation {
  id: string;
  participant_id: string;
  participant_name: string;
  last_message: string;
  unread_count: number;
  last_message_time?: string;
  participant_picture?: string;
}

export default function MessagesScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const { data: conversationsData, isLoading } = useConversations();
  const conversations = conversationsData?.results || [];

  if (!isAuthenticated) {
    return (
      <View className="flex-1 items-center justify-center bg-white">
        <Ionicons name="chatbubbles" size={48} color="#999" />
        <Text className="text-gray-600 mb-4 mt-4">Please login to view messages</Text>
        <TouchableOpacity
          className="bg-secondary-600 px-8 py-3 rounded-lg"
          onPress={() => router.replace('/(auth)/login')}
        >
          <Text className="text-white font-semibold">Go to Login</Text>
        </TouchableOpacity>
      </View>
    );
  }

  const ConversationCard = ({ conversation }: { conversation: Conversation }) => (
    <TouchableOpacity
      className="px-4 py-3 border-b border-gray-100 flex-row items-center"
      onPress={() => router.push(`/(tabs)/messages/${conversation.id}`)}
    >
      {/* Avatar */}
      <View className="w-12 h-12 rounded-full bg-gradient-to-br from-secondary-400 to-secondary-600 items-center justify-center mr-3">
        <Text className="text-white font-bold">
          {conversation.participant_name.charAt(0).toUpperCase()}
        </Text>
      </View>

      {/* Content */}
      <View className="flex-1">
        <View className="flex-row justify-between items-start mb-1">
          <Text className="font-semibold text-primary-900">{conversation.participant_name}</Text>
          {conversation.last_message_time && (
            <Text className="text-gray-500 text-xs">{conversation.last_message_time}</Text>
          )}
        </View>
        <Text
          className={`text-sm ${
            conversation.unread_count > 0 ? 'text-primary-900 font-semibold' : 'text-gray-600'
          }`}
          numberOfLines={1}
        >
          {conversation.last_message}
        </Text>
      </View>

      {/* Unread Badge */}
      {conversation.unread_count > 0 && (
        <View className="bg-secondary-600 rounded-full w-6 h-6 items-center justify-center ml-2">
          <Text className="text-white text-xs font-bold">{conversation.unread_count}</Text>
        </View>
      )}
    </TouchableOpacity>
  );

  return (
    <View className="flex-1 bg-gray-50">
      {/* Header */}
      <View className="bg-white px-4 py-4 border-b border-gray-200 flex-row justify-between items-center">
        <Text className="text-3xl font-bold text-primary-900">Messages</Text>
        <TouchableOpacity
          className="bg-secondary-600 p-2 rounded-full"
          onPress={() => router.push('/messages/new')}
        >
          <Ionicons name="send" size={20} color="white" />
        </TouchableOpacity>
      </View>

      {/* Conversations List */}
      {isLoading ? (
        <View className="flex-1 justify-center items-center">
          <ActivityIndicator size="large" color="#D4A574" />
        </View>
      ) : (
        <FlatList
          data={conversations}
          renderItem={({ item }) => <ConversationCard conversation={item} />}
          keyExtractor={(item) => item.id}
          contentContainerStyle={{ backgroundColor: 'white' }}
          ListEmptyComponent={
            <View className="flex-1 justify-center items-center bg-white py-12">
              <Ionicons name="chatbubbles" size={48} color="#DDD" />
              <Text className="text-gray-600 text-base mt-4 mb-2">No conversations yet</Text>
              <Text className="text-gray-500 text-sm">Start a conversation with a host</Text>
            </View>
          }
        />
      )}
    </View>
  );
}
