import { useState, useEffect, useRef } from 'react';
import { View, Text, FlatList, TextInput, TouchableOpacity, ActivityIndicator, KeyboardAvoidingView, Platform, Alert } from 'react-native';
import { useLocalSearchParams, useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useConversationMessages, useSendMessage, useEditMessage, useDeleteMessage, useMarkConversationAsRead, useArchiveConversation } from '@/hooks/api-hooks';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';

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
  const insets = useSafeAreaInsets();
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
    if (!conversationId || !trimmed) {
      Alert.alert('Error', 'Conversation ID or message is missing');
      return;
    }

    try {
      await sendMessage({ conversationId, receiverId: receiverId || '', message: trimmed });
      setMessage('');
      refetch();
    } catch (error: any) {
      console.error('Send message error:', error?.response?.data || error?.message);
      Alert.alert('Error', `Failed to send message: ${error?.response?.data?.detail || error?.message || 'Unknown error'}`);
    }
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
      <SafeAreaView className="flex-1 bg-sand-100">
        <View className="flex-1 items-center justify-center px-6">
          <View className="bg-white rounded-3xl p-8 items-center" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}>
            <View className="bg-sand-200 rounded-full p-8 mb-6">
              <Ionicons name="chatbubbles-outline" size={72} color="#3A5C50" />
            </View>
            <Text className="text-2xl font-bold text-forest mb-3">Conversation Not Found</Text>
            <Text className="text-moss text-center mb-8 px-4 leading-6">
              This conversation could not be loaded
            </Text>
            <TouchableOpacity onPress={() => router.back()}>
              <LinearGradient
                colors={['#122F26', '#1d392f']}
                className="px-8 py-4 rounded-2xl"
                style={{
                  shadowColor: '#122F26',
                  shadowOffset: { width: 0, height: 4 },
                  shadowOpacity: 0.3,
                  shadowRadius: 8,
                  elevation: 5,
                }}
              >
                <Text className="text-gold font-bold text-base">Go Back</Text>
              </LinearGradient>
            </TouchableOpacity>
          </View>
        </View>
      </SafeAreaView>
    );
  }

  if (isLoading) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100">
        <View className="flex-1 items-center justify-center">
          <ActivityIndicator size="large" color="#D9B168" />
          <Text className="text-moss mt-4">Loading conversation...</Text>
        </View>
      </SafeAreaView>
    );
  }

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      <KeyboardAvoidingView 
        className="flex-1" 
        behavior={Platform.OS === 'ios' ? 'padding' : 'height'}
        keyboardVerticalOffset={Platform.OS === 'ios' ? 0 : 20}
      >
        {/* Header */}
        <LinearGradient
          colors={['#122F26', '#1d392f', '#2d4a40']}
          start={{ x: 0, y: 0 }}
          end={{ x: 1, y: 1 }}
          className="px-4 pb-4"
          style={{ paddingTop: insets.top + 12 }}
        >
          <View className="flex-row items-center justify-between">
            <View className="flex-row items-center flex-1">
              <TouchableOpacity 
                onPress={() => router.back()}
                className="w-10 h-10 rounded-xl items-center justify-center mr-3"
                style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
              >
                <Ionicons name="arrow-back" size={24} color="#fff" />
              </TouchableOpacity>
              <Text className="text-xl font-black text-white tracking-tight">Conversation</Text>
            </View>
            <View className="flex-row items-center">
              <TouchableOpacity 
                onPress={() => refetch()} 
                className="mr-3 w-10 h-10 rounded-xl items-center justify-center"
                style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
              >
                {isRefetching ? (
                  <ActivityIndicator size="small" color="#D9B168" />
                ) : (
                  <Ionicons name="refresh" size={22} color="#D9B168" />
                )}
              </TouchableOpacity>
              <TouchableOpacity 
                onPress={handleArchive}
                className="w-10 h-10 rounded-xl items-center justify-center"
                style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
              >
                <Ionicons name="archive" size={22} color="#D9B168" />
              </TouchableOpacity>
            </View>
          </View>
        </LinearGradient>

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
