import { View, Text, FlatList, TouchableOpacity, TextInput } from 'react-native';
import { useState } from 'react';
import { useRouter } from 'expo-router';
import { useConversations, useUnreadCount } from '@/hooks/api-hooks';
import { useAuth } from '@/context/auth-context';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { Skeleton } from '@/components/common/Skeletons';
import { Avatar } from '@/components/common/Avatar';
import { Sidebar } from '@/components/common/Sidebar';
import { EmptyState } from '@/components/common/EmptyState';
import { useSafeAreaInsets } from 'react-native-safe-area-context';

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
  const insets = useSafeAreaInsets();
  const { user, isAuthenticated } = useAuth();
  const { data: conversationsData, isLoading } = useConversations();
  const { data: unreadData } = useUnreadCount();
  const conversations = conversationsData?.results || [];
  const [sidebarVisible, setSidebarVisible] = useState(false);
  const [searchQuery, setSearchQuery] = useState('');

  const handleAvatarPress = () => {
    if (isAuthenticated) {
      router.push('/(tabs)/profile');
    } else {
      router.push('/(auth)/login');
    }
  };

  // Filter conversations by search query
  const filteredConversations = conversations.filter((conv: any) => {
    if (!searchQuery) return true;
    const participantName = conv.participant_name?.toLowerCase() || '';
    const lastMessage = conv.last_message?.toLowerCase() || '';
    return participantName.includes(searchQuery.toLowerCase()) ||
           lastMessage.includes(searchQuery.toLowerCase());
  });

  if (!isAuthenticated) {
    return (
      <View className="flex-1 bg-sand-100">
        {/* Sidebar */}
        <Sidebar
          isVisible={sidebarVisible}
          onClose={() => setSidebarVisible(false)}
        />

        {/* Header */}
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: insets.top + 12 }}
        >
          {/* Top Navigation Bar with Menu */}
          <View className="flex-row items-center justify-between mb-4">
            {/* Hamburger Menu with Glassmorphism */}
            <View
              borderRadius={12}
              style={{ width: 40, height: 40 }}
            >
              <TouchableOpacity
                onPress={() => setSidebarVisible(true)}
                className="w-10 h-10 rounded-xl items-center justify-center"
              >
                <Ionicons name="menu" size={24} color="#fff" />
              </TouchableOpacity>
            </View>

            {/* Avatar */}
            <Avatar
              uri={null}
              firstName={undefined}
              lastName={undefined}
              size="small"
              onPress={handleAvatarPress}
            />
          </View>

          <Text className="text-3xl font-black text-white tracking-tight">
            Messages
          </Text>
          <Text className="text-sand-200 text-sm mt-1">
            Chat with hosts and guests
          </Text>
        </LinearGradient>

        <EmptyState
          icon="chatbubbles-outline"
          title="Sign In to Chat"
          description="Sign in to start conversations with hosts and guests"
          actionLabel="Sign In Now"
          onAction={() => router.replace('/(auth)/login')}
        />
      </View>
    );
  }

  const ConversationCard = ({ conversation }: { conversation: Conversation }) => {
    const displayName = conversation.participant_name?.trim() || 'Guest';
    const initial = displayName.charAt(0).toUpperCase();

    return (
    <TouchableOpacity
      className="mx-4 mb-3 rounded-2xl overflow-hidden bg-white"
      onPress={() =>
        router.push(
          `/(tabs)/messages/${conversation.id}?participantId=${conversation.participant_id}`
        )
      }
      style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 4 },
        shadowOpacity: 0.08,
        shadowRadius: 8,
        elevation: 4,
      }}
    >
      <View className="px-4 py-4 flex-row items-center">
        {/* Avatar */}
        <LinearGradient
          colors={['#D9B168', '#bea04f']}
          className="w-14 h-14 rounded-full items-center justify-center mr-4"
        >
          <Text className="text-forest font-bold text-xl">
            {initial}
          </Text>
        </LinearGradient>

        {/* Content */}
        <View className="flex-1">
          <View className="flex-row justify-between items-start mb-1">
            <Text className="font-bold text-forest text-base">{displayName}</Text>
            {conversation.last_message_time && (
              <Text className="text-moss text-xs">{conversation.last_message_time}</Text>
            )}
          </View>
          <Text
            className={`text-sm ${
              conversation.unread_count > 0 ? 'text-forest font-semibold' : 'text-moss'
            }`}
            numberOfLines={1}
          >
            {conversation.last_message || 'No messages yet'}
          </Text>
        </View>

        {/* Unread Badge */}
        {conversation.unread_count > 0 && (
          <View className="bg-gold rounded-full min-w-[24px] h-6 items-center justify-center ml-2 px-2">
            <Text className="text-forest text-xs font-bold">{conversation.unread_count}</Text>
          </View>
        )}
      </View>
    </TouchableOpacity>
    );
  };

  const ConversationSkeleton = () => (
    <View className="mx-4 mb-3 rounded-2xl bg-white p-4 bg-white rounded-2xl">
      <View className="flex-row items-center">
        <Skeleton height={56} width={56} borderRadius={28} className="mr-4" />
        <View className="flex-1">
          <Skeleton height={18} width="60%" className="mb-2" />
          <Skeleton height={14} width="80%" />
        </View>
      </View>
    </View>
  );

  return (
    <View className="flex-1 bg-sand-100">
      {/* Sidebar */}
      <Sidebar
        isVisible={sidebarVisible}
        onClose={() => setSidebarVisible(false)}
      />

      {/* Modern Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="px-4 pb-6"
        style={{ paddingTop: insets.top + 12 }}
      >
        {/* Top Navigation Bar with Menu and Avatar */}
        <View className="flex-row items-center justify-between mb-4">
          {/* Hamburger Menu with Glassmorphism */}
          <View
            borderRadius={12}
            style={{ width: 40, height: 40 }}
          >
            <TouchableOpacity
              onPress={() => setSidebarVisible(true)}
              className="w-10 h-10 rounded-xl items-center justify-center"
            >
              <Ionicons name="menu" size={24} color="#fff" />
            </TouchableOpacity>
          </View>

          {/* Avatar */}
          <Avatar
            uri={null}
            firstName={user?.first_name}
            lastName={user?.last_name}
            size="small"
            onPress={handleAvatarPress}
            showBadge={isAuthenticated}
          />
        </View>

        <View className="flex-row justify-between items-center">
          <View className="flex-1">
            <Text className="text-3xl font-black text-white tracking-tight mb-2">
              Messages
            </Text>
            <View className="flex-row items-center">
              <Ionicons name="chatbubbles" size={16} color="#D9B168" />
              <Text className="text-sand-100 ml-2">
                {filteredConversations.length} {filteredConversations.length === 1 ? 'conversation' : 'conversations'}
              </Text>
              {unreadData?.unread_count > 0 && (
                <>
                  <Text className="text-sand-100 mx-1">•</Text>
                  <Text className="text-gold font-semibold">
                    {unreadData.unread_count} unread
                  </Text>
                </>
              )}
            </View>
          </View>
          <TouchableOpacity
            onPress={() => router.push('/(tabs)/messages/new')}
            style={{
              shadowColor: '#D9B168',
              shadowOffset: { width: 0, height: 4 },
              shadowOpacity: 0.3,
              shadowRadius: 8,
              elevation: 5,
            }}
          >
            <LinearGradient
              colors={['#D9B168', '#bea04f']}
              className="w-12 h-12 rounded-full items-center justify-center"
            >
              <Ionicons name="create-outline" size={24} color="#122F26" />
            </LinearGradient>
          </TouchableOpacity>
        </View>

        {/* Search Bar with Glassmorphism */}
        <View
          borderRadius={12}
          style={{ marginTop: 16 }}
        >
          <View className="px-4 py-2 flex-row items-center">
            <Ionicons name="search" size={20} color="#D9B168" />
            <TextInput
              placeholder="Search conversations..."
              placeholderTextColor="rgba(255, 255, 255, 0.5)"
              value={searchQuery}
              onChangeText={setSearchQuery}
              className="flex-1 ml-2 text-white"
            />
            {searchQuery ? (
              <TouchableOpacity onPress={() => setSearchQuery('')}>
                <Ionicons name="close-circle" size={20} color="rgba(255, 255, 255, 0.5)" />
              </TouchableOpacity>
            ) : null}
          </View>
        </View>
      </LinearGradient>

      {/* Conversations List */}
      {isLoading ? (
        <View className="pt-4">
          {[1, 2, 3, 4, 5].map((i) => (
            <ConversationSkeleton key={i} />
          ))}
        </View>
      ) : (
        <FlatList
          data={filteredConversations}
          renderItem={({ item }) => <ConversationCard conversation={item} />}
          keyExtractor={(item) => item.id}
          contentContainerStyle={{ paddingVertical: 12, paddingBottom: 40 }}
          ListEmptyComponent={
            <View className="flex-1 justify-center items-center py-20 px-6">
              <View className="bg-white rounded-3xl p-8 items-center" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}>
                <View className="bg-sand-200 rounded-full p-8 mb-6">
                  <Ionicons name="chatbubbles-outline" size={72} color="#3A5C50" />
                </View>
                <Text className="text-2xl font-bold text-forest mb-3">
                  {searchQuery ? 'No Results Found' : 'No Conversations Yet'}
                </Text>
                <Text className="text-moss text-center mb-8 px-4 leading-6">
                  {searchQuery 
                    ? 'Try adjusting your search terms' 
                    : 'Start a conversation with a host or guest to begin messaging'}
                </Text>
                {!searchQuery && (
                  <TouchableOpacity
                    onPress={() => router.push('/(tabs)/explore')}
                  >
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
                      <Text className="text-gold font-bold text-base">Explore Properties</Text>
                    </LinearGradient>
                  </TouchableOpacity>
                )}
              </View>
            </View>
          }
        />
      )}
    </View>
  );
}
