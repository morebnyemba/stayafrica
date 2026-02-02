import { useState, useEffect, useRef } from 'react';
import { View, Text, FlatList, TextInput, TouchableOpacity, ActivityIndicator, KeyboardAvoidingView, Platform, Alert } from 'react-native';
import { useLocalSearchParams, useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { useConversationMessages, useSendMessage, useEditMessage, useDeleteMessage, useMarkConversationAsRead, useArchiveConversation } from '@/hooks/api-hooks';
import { SafeAreaView } from 'react-native-safe-area-context';

interface ConversationMessage {
  id: string;
  content: string;
  text?: string;
  sender_name?: string;
  created_at?: string;
  is_own_message?: boolean;
  edited_at?: string;
}

export default function ConversationDetailScreen() {
  const router = useRouter();
  const { id, participantId } = useLocalSearchParams();
  const conversationId = Array.isArray(id) ? id[0] : id;
  const receiverId = Array.isArray(participantId) ? participantId[0] : participantId;
  const [message, setMessage] = useState('');
  const [editingMessageId, setEditingMessageId] = useState<string | null>(null);
  const [editText, setEditText] = useState('');
  const flatListRef = useRef<FlatList>(null);

  const {
    data: messagesData,
    isLoading,
    isRefetching,
    refetch,
  } = useConversationMessages(conversationId || '');
  const { mutateAsync: sendMessage, isPending } = useSendMessage();
  const editMessageMutation = useEditMessage();
  const { mutate: deleteMessage } = useDeleteMessage();
  const { mutate: markAsRead } = useMarkConversationAsRead();
  const { mutate: archiveConversation } = useArchiveConversation();

  const messages = (messagesData as any)?.results ?? [];

  // Mark conversation as read when entering
  useEffect(() => {
    if (conversationId) {
      markAsRead(conversationId);
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [conversationId]); // markAsRead is stable from useMutation

  const handleSend = async () => {
    const trimmed = message.trim();
    if (!conversationId || !trimmed || !receiverId) return;

    await sendMessage({ conversationId, receiverId, message: trimmed });
    setMessage('');
    refetch();
  };

  const handleEditMessage = (messageId: string, currentText: string) => {
    setEditingMessageId(messageId);
    setEditText(currentText);
  };

  const handleSaveEdit = (messageId: string) => {
    if (!editText.trim()) return;
    editMessageMutation.mutate({ messageId, text: editText }, {
      onSuccess: () => {
        setEditingMessageId(null);
        setEditText('');
        refetch();
      },
    });
  };

  const handleCancelEdit = () => {
    setEditingMessageId(null);
    setEditText('');
  };

  const handleDeleteMessage = (messageId: string) => {
    Alert.alert(
      'Delete Message',
      'Are you sure you want to delete this message?',
      [
        { text: 'Cancel', style: 'cancel' },
        {
          text: 'Delete',
          style: 'destructive',
          onPress: () => {
            deleteMessage(messageId, {
              onSuccess: () => refetch(),
            });
          },
        },
      ]
    );
  };

  const handleArchive = () => {
    if (!conversationId) return;
    Alert.alert(
      'Archive Conversation',
      'Are you sure you want to archive this conversation?',
      [
        { text: 'Cancel', style: 'cancel' },
        {
          text: 'Archive',
          style: 'destructive',
          onPress: () => {
            archiveConversation(conversationId, {
              onSuccess: () => router.back(),
            });
          },
        },
      ]
    );
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
    <SafeAreaView className="flex-1 bg-gray-50" edges={['top', 'left', 'right']}>
      <KeyboardAvoidingView 
        className="flex-1" 
        behavior={Platform.OS === 'ios' ? 'padding' : 'height'}
        keyboardVerticalOffset={Platform.OS === 'ios' ? 0 : 20}
      >
        <View className="flex-row items-center justify-between px-4 py-3 bg-white border-b border-gray-200">
          <View className="flex-row items-center flex-1">
            <TouchableOpacity onPress={() => router.back()}>
              <Ionicons name="arrow-back" size={24} color="#333" />
            </TouchableOpacity>
            <Text className="text-lg font-semibold text-primary-900 ml-3">Conversation</Text>
          </View>
          <View className="flex-row items-center">
            <TouchableOpacity onPress={() => refetch()} className="mr-3">
              {isRefetching ? (
                <ActivityIndicator size="small" color="#D4A574" />
              ) : (
                <Ionicons name="refresh" size={22} color="#3A5C50" />
              )}
            </TouchableOpacity>
            <TouchableOpacity onPress={handleArchive}>
              <Ionicons name="archive" size={22} color="#3A5C50" />
            </TouchableOpacity>
          </View>
        </View>

        <FlatList
          ref={flatListRef}
          data={messages as ConversationMessage[]}
          keyExtractor={(item) => item.id}
          contentContainerStyle={{ padding: 16, paddingBottom: 120 }}
          keyboardShouldPersistTaps="handled"
          renderItem={({ item, index }) => {
            const messageText = item.text || item.content || '';
            const isOwnMessage = item.is_own_message;
            const isEditing = editingMessageId === item.id;
            
            // Check if we should show a date separator
            const messageTime = new Date(item.created_at || '');
            const prevMessage = index > 0 ? messages[index - 1] : null;
            const prevTime = prevMessage ? new Date(prevMessage.created_at || '') : null;
            const isNewDay = !prevTime || messageTime.toDateString() !== prevTime.toDateString();

            return (
              <View>
                {/* Date Separator */}
                {isNewDay && (
                  <View className="flex-row justify-center my-3">
                    <View className="bg-gray-200 px-3 py-1 rounded-full">
                      <Text className="text-xs text-gray-600">
                        {messageTime.toLocaleDateString([], { 
                          weekday: 'short', 
                          month: 'short', 
                          day: 'numeric' 
                        })}
                      </Text>
                    </View>
                  </View>
                )}

                {/* Message */}
                <View className={`mb-3 ${isOwnMessage ? 'items-end' : 'items-start'}`}>
                  {!isOwnMessage && (
                    <View className="flex-row items-center mb-1">
                      <Ionicons name="person-circle" size={18} color="#3A5C50" />
                      <Text className="ml-2 text-sm font-semibold text-primary-900">
                        {item.sender_name || 'Guest'}
                      </Text>
                    </View>
                  )}
                  
                  {isEditing ? (
                    <View className="bg-white rounded-2xl px-4 py-3 border-2 border-gold w-4/5" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 6, elevation: 2 }}>
                      <TextInput
                        value={editText}
                        onChangeText={setEditText}
                        multiline
                        className="text-gray-800 mb-2"
                        autoFocus
                      />
                      <View className="flex-row justify-end gap-2">
                        <TouchableOpacity onPress={handleCancelEdit} className="px-3 py-1 bg-gray-200 rounded-lg">
                          <Text className="text-gray-700">Cancel</Text>
                        </TouchableOpacity>
                        <TouchableOpacity 
                          onPress={() => handleSaveEdit(item.id)} 
                          disabled={editMessageMutation.isPending}
                          className="px-3 py-1 bg-gold rounded-lg"
                        >
                          <Text className="text-forest font-semibold">Save</Text>
                        </TouchableOpacity>
                      </View>
                    </View>
                  ) : (
                    <View className={`rounded-2xl px-4 py-3 max-w-[80%] ${
                      isOwnMessage ? 'bg-gold' : 'bg-white border border-gray-100'
                    }`} style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 6, elevation: 2 }}>
                      <Text className={isOwnMessage ? 'text-forest' : 'text-gray-800'}>
                        {messageText}
                      </Text>
                      {item.edited_at && (
                        <Text className={`text-xs mt-1 ${isOwnMessage ? 'text-forest/70' : 'text-gray-500'}`}>
                          (edited)
                        </Text>
                      )}
                      <View className="flex-row items-center justify-between mt-1">
                        <Text className={`text-xs ${isOwnMessage ? 'text-forest/70' : 'text-gray-500'}`}>
                          {messageTime.toLocaleTimeString([], { 
                            hour: '2-digit', 
                            minute: '2-digit' 
                          })}
                        </Text>
                        {isOwnMessage && (
                          <View className="flex-row ml-2">
                            <TouchableOpacity 
                              onPress={() => handleEditMessage(item.id, messageText)}
                              className="mr-2"
                            >
                              <Ionicons name="create-outline" size={16} color="#122F26" />
                            </TouchableOpacity>
                            <TouchableOpacity onPress={() => handleDeleteMessage(item.id)}>
                              <Ionicons name="trash-outline" size={16} color="#122F26" />
                            </TouchableOpacity>
                          </View>
                        )}
                      </View>
                    </View>
                  )}
                </View>
              </View>
            );
          }}
          ListEmptyComponent={
            <View className="flex-1 items-center justify-center py-20">
              <Ionicons name="chatbox-ellipses-outline" size={48} color="#d1d5db" />
              <Text className="text-gray-600 mt-4">No messages yet.</Text>
              <Text className="text-gray-500 text-sm">Send the first message to start chatting.</Text>
            </View>
          }
        />

        <View className="bg-white border-t border-gray-200 px-4 py-3">
          <View className="flex-row items-center bg-gray-100 rounded-full px-3 py-2">
            <TextInput
              className="flex-1 text-sm text-primary-900 px-2"
              placeholder="Type a message..."
              placeholderTextColor="#9ca3af"
              value={message}
              onChangeText={setMessage}
              editable={!isPending}
              multiline
              blurOnSubmit={false}
              returnKeyType="send"
              onSubmitEditing={handleSend}
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
      </KeyboardAvoidingView>
    </SafeAreaView>
  );
}
