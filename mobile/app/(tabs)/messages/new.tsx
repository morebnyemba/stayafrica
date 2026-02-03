import { View, Text, TouchableOpacity, ScrollView, TextInput, ActivityIndicator } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useSafeAreaInsets } from 'react-native-safe-area-context';
import { useState } from 'react';
import { useAuth } from '@/context/auth-context';

export default function NewMessageScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const { isAuthenticated } = useAuth();
  const [searchQuery, setSearchQuery] = useState('');
  const [selectedRecipient, setSelectedRecipient] = useState<any>(null);
  const [message, setMessage] = useState('');
  const [loading, setLoading] = useState(false);

  // TODO: Fetch users/hosts from API based on search
  const recentContacts = [
    {
      id: '1',
      name: 'John Safari',
      role: 'Host',
      property: 'Luxury Safari Lodge',
      avatar: null,
    },
    {
      id: '2',
      name: 'Sarah Beach',
      role: 'Host',
      property: 'Coastal Paradise Villa',
      avatar: null,
    },
  ];

  const handleSendMessage = async () => {
    if (!selectedRecipient || !message.trim()) return;
    
    setLoading(true);
    // TODO: Send message via API
    setTimeout(() => {
      setLoading(false);
      router.back();
    }, 1000);
  };

  const ContactCard = ({ contact }: any) => (
    <TouchableOpacity
      onPress={() => setSelectedRecipient(contact)}
      className={`bg-white rounded-2xl p-4 mb-3 flex-row items-center ${
        selectedRecipient?.id === contact.id ? 'border-2 border-gold' : ''
      }`}
      style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 2 },
        shadowOpacity: 0.05,
        shadowRadius: 4,
        elevation: 2,
      }}
    >
      <View className="bg-sand-200 rounded-full w-12 h-12 items-center justify-center mr-3">
        <Text className="text-forest font-bold text-lg">
          {contact.name.charAt(0)}
        </Text>
      </View>
      <View className="flex-1">
        <Text className="text-base font-bold text-forest">{contact.name}</Text>
        <Text className="text-xs text-moss">{contact.role}</Text>
        {contact.property && (
          <Text className="text-xs text-moss/70">{contact.property}</Text>
        )}
      </View>
      {selectedRecipient?.id === contact.id && (
        <Ionicons name="checkmark-circle" size={24} color="#D9B168" />
      )}
    </TouchableOpacity>
  );

  if (!isAuthenticated) {
    return (
      <View className="flex-1 bg-sand-100">
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: insets.top + 12 }}
        >
          <View className="flex-row items-center justify-between">
            <TouchableOpacity
              onPress={() => router.back()}
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="arrow-back" size={24} color="#fff" />
            </TouchableOpacity>
            <Text className="text-white font-bold text-lg flex-1 text-center mr-10">
              New Message
            </Text>
          </View>
        </LinearGradient>

        <View className="flex-1 items-center justify-center px-6">
          <Text className="text-moss text-center">Please sign in to send messages</Text>
        </View>
      </View>
    );
  }

  return (
    <View className="flex-1 bg-sand-100">
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f']}
        className="px-4 pb-6"
        style={{ paddingTop: insets.top + 12 }}
      >
        <View className="flex-row items-center justify-between mb-4">
          <TouchableOpacity
            onPress={() => router.back()}
            className="w-10 h-10 rounded-xl items-center justify-center"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </TouchableOpacity>
          <Text className="text-white font-bold text-lg flex-1 text-center">
            New Message
          </Text>
          <View className="w-10" />
        </View>
        <Text className="text-sand-200 text-sm text-center">
          Start a new conversation
        </Text>
      </LinearGradient>

      <ScrollView className="flex-1" showsVerticalScrollIndicator={false}>
        {/* Search */}
        <View className="px-4 pt-6 pb-4">
          <View className="bg-white rounded-2xl px-4 py-3 flex-row items-center" style={{
            shadowColor: '#122F26',
            shadowOffset: { width: 0, height: 2 },
            shadowOpacity: 0.05,
            shadowRadius: 4,
            elevation: 2,
          }}>
            <Ionicons name="search" size={20} color="#3A5C50" />
            <TextInput
              className="flex-1 ml-3 text-forest"
              placeholder="Search hosts or guests..."
              placeholderTextColor="#6B8E7F"
              value={searchQuery}
              onChangeText={setSearchQuery}
            />
          </View>
        </View>

        {/* Recent Contacts */}
        <View className="px-4 pb-4">
          <Text className="text-base font-bold text-forest mb-3">Recent Contacts</Text>
          {recentContacts.map((contact) => (
            <ContactCard key={contact.id} contact={contact} />
          ))}
        </View>

        {/* Message Input */}
        {selectedRecipient && (
          <View className="px-4 pb-8">
            <Text className="text-base font-bold text-forest mb-3">Your Message</Text>
            <View
              className="bg-white rounded-2xl p-4"
              style={{
                shadowColor: '#122F26',
                shadowOffset: { width: 0, height: 4 },
                shadowOpacity: 0.08,
                shadowRadius: 8,
                elevation: 4,
              }}
            >
              <TextInput
                className="text-forest min-h-[120px]"
                placeholder="Type your message here..."
                placeholderTextColor="#6B8E7F"
                multiline
                numberOfLines={6}
                textAlignVertical="top"
                value={message}
                onChangeText={setMessage}
              />
            </View>

            <TouchableOpacity
              onPress={handleSendMessage}
              disabled={!message.trim() || loading}
              className="mt-4"
            >
              <LinearGradient
                colors={message.trim() && !loading ? ['#D9B168', '#bea04f'] : ['#D9D9D9', '#CCCCCC']}
                className="rounded-2xl p-5 flex-row items-center justify-center"
                style={{
                  shadowColor: message.trim() && !loading ? '#D9B168' : '#999',
                  shadowOffset: { width: 0, height: 4 },
                  shadowOpacity: 0.3,
                  shadowRadius: 8,
                  elevation: 5,
                }}
              >
                {loading ? (
                  <ActivityIndicator color="#122F26" />
                ) : (
                  <>
                    <Ionicons name="send" size={20} color={message.trim() ? "#122F26" : "#666"} />
                    <Text className={`font-bold text-base ml-2 ${message.trim() ? 'text-forest' : 'text-gray-600'}`}>
                      Send Message
                    </Text>
                  </>
                )}
              </LinearGradient>
            </TouchableOpacity>
          </View>
        )}
      </ScrollView>
    </View>
  );
}
