import { useState } from 'react';
import { View, Text, FlatList, TextInput, TouchableOpacity, ActivityIndicator } from 'react-native';
import { useLocalSearchParams, useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { useConversationMessages, useSendMessage } from '@/hooks/api-hooks';

interface ConversationMessage {
  id: string;
  content: string;
  sender_name?: string;
  created_at?: string;
}

export default function ConversationDetailScreen() {
  const router = useRouter();
  const { id } = useLocalSearchParams();
  const conversationId = Array.isArray(id) ? id[0] : id;
  const [message, setMessage] = useState('');

  const {
    data: messagesData,
    isLoading,
    isRefetching,
    refetch,
  } = useConversationMessages(conversationId || '');
  const { mutateAsync: sendMessage, isPending } = useSendMessage();

  const messages = (messagesData as any)?.results ?? [];

  const handleSend = async () => {
    const trimmed = message.trim();
    if (!conversationId || !trimmed) return;

    await sendMessage({ conversationId, message: trimmed });
    setMessage('');
    refetch();
  };

  if (!conversationId) {
    return (
      <View className="flex-1 items-center justify-center bg-white p-6">
        <Text className="text-lg text-gray-600">Conversation not found.</Text>
        <TouchableOpacity onPress={() => router.back()} className="mt-4">
          <Text className="text-secondary-600 font-semibold">Go back</Text>
        </TouchableOpacity>
      </View>
    );
  }

  if (isLoading) {
    return (
      <View className="flex-1 items-center justify-center bg-white">
        <ActivityIndicator size="large" color="#D4A574" />
      </View>
    );
  }

  return (
    <View className="flex-1 bg-gray-50">
      <View className="flex-row items-center justify-between px-4 py-3 bg-white border-b border-gray-200">
        <TouchableOpacity onPress={() => router.back()}>
          <Ionicons name="arrow-back" size={24} color="#333" />
        </TouchableOpacity>
        <Text className="text-lg font-semibold text-primary-900">Conversation</Text>
        <TouchableOpacity onPress={() => refetch()}>
          {isRefetching ? (
            <ActivityIndicator size="small" color="#D4A574" />
          ) : (
            <Ionicons name="refresh" size={22} color="#3A5C50" />
          )}
        </TouchableOpacity>
      </View>

      <FlatList
        data={messages as ConversationMessage[]}
        keyExtractor={(item) => item.id}
        contentContainerStyle={{ padding: 16, paddingBottom: 120 }}
        renderItem={({ item }) => (
          <View className="mb-3">
            <View className="flex-row items-center mb-1">
              <Ionicons name="person-circle" size={18} color="#3A5C50" />
              <Text className="ml-2 text-sm font-semibold text-primary-900">
                {item.sender_name || 'Guest'}
              </Text>
              {item.created_at && (
                <Text className="ml-2 text-xs text-gray-500">{item.created_at}</Text>
              )}
            </View>
            <View className="bg-white rounded-2xl px-4 py-3 border border-gray-100" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 6, elevation: 2 }}>
              <Text className="text-gray-800">{item.content}</Text>
            </View>
          </View>
        )}
        ListEmptyComponent={
          <View className="flex-1 items-center justify-center py-20">
            <Ionicons name="chatbox-ellipses-outline" size={48} color="#d1d5db" />
            <Text className="text-gray-600 mt-4">No messages yet.</Text>
            <Text className="text-gray-500 text-sm">Send the first message to start chatting.</Text>
          </View>
        }
      />

      <View className="absolute bottom-0 left-0 right-0 bg-white border-t border-gray-200 px-4 py-3">
        <View className="flex-row items-center bg-gray-100 rounded-full px-3 py-2">
          <TextInput
            className="flex-1 text-sm text-primary-900 px-2"
            placeholder="Type a message..."
            placeholderTextColor="#9ca3af"
            value={message}
            onChangeText={setMessage}
            editable={!isPending}
          />
          <TouchableOpacity
            className={`ml-2 rounded-full px-3 py-2 ${message.trim() ? 'bg-secondary-600' : 'bg-gray-300'}`}
            disabled={!message.trim() || isPending}
            onPress={handleSend}
          >
            {isPending ? (
              <ActivityIndicator size="small" color="#fff" />
            ) : (
              <Ionicons name="send" size={18} color="#fff" />
            )}
          </TouchableOpacity>
        </View>
      </View>
    </View>
  );
}
