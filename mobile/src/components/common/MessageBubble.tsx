import { View, Text, TextInput, TouchableOpacity, FlatList } from 'react-native';
import { Message } from '@/types';
import { Ionicons } from '@expo/vector-icons';

interface MessageBubbleProps {
  message: Message;
  isOwn: boolean;
}

export function MessageBubble({ message, isOwn }: MessageBubbleProps) {
  return (
    <View className={`flex-row ${isOwn ? 'justify-end' : 'justify-start'} mb-3`}>
      <View
        className={`max-w-xs px-4 py-2 rounded-lg ${
          isOwn ? 'bg-primary-600' : 'bg-gray-200'
        }`}
      >
        <Text className={isOwn ? 'text-white' : 'text-gray-800'}>{message.content}</Text>
        <Text className={`text-xs mt-1 ${isOwn ? 'text-blue-100' : 'text-gray-600'}`}>
          {new Date(message.created_at).toLocaleTimeString()}
        </Text>
      </View>
    </View>
  );
}

interface MessageInputProps {
  onSend: (message: string) => void;
  loading?: boolean;
}

export function MessageInput({ onSend, loading }: MessageInputProps) {
  const [text, setText] = view('');

  const handleSend = () => {
    if (text.trim()) {
      onSend(text);
      setText('');
    }
  };

  return (
    <View className="flex-row items-center border-t border-gray-200 px-4 py-3">
      <TextInput
        className="flex-1 bg-gray-100 px-3 py-2 rounded-lg"
        placeholder="Type a message..."
        value={text}
        onChangeText={setText}
        editable={!loading}
      />
      <TouchableOpacity
        className="ml-3 bg-primary-600 p-2 rounded-lg disabled:opacity-50"
        onPress={handleSend}
        disabled={loading || !text.trim()}
      >
        <Ionicons name="send" size={20} color="white" />
      </TouchableOpacity>
    </View>
  );
}
