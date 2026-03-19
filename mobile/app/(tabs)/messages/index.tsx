import { View, Text, FlatList, TouchableOpacity, TextInput, RefreshControl } from 'react-native';
import { useState, useCallback, useMemo } from 'react';
import { useRouter } from 'expo-router';
import { useConversations, useUnreadCount } from '@/hooks/api-hooks';
import { useAuth } from '@/context/auth-context';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { Skeleton } from '@/components/common/Skeletons';
import { Avatar } from '@/components/common/Avatar';
import { Sidebar } from '@/components/common/Sidebar';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import Animated, { FadeInDown } from 'react-native-reanimated';

interface Conversation {
  id: string;
  participant_id: string;
  participant_name: string;
  last_message: string;
  unread_count: number;
  last_message_time?: string;
  participant_picture?: string;
}

function timeAgo(dateStr?: string): string {
  if (!dateStr) return '';
  const now = new Date();
  const date = new Date(dateStr);
  if (Number.isNaN(date.getTime())) return '';
  const diffMs = now.getTime() - date.getTime();
  if (Number.isNaN(diffMs) || diffMs < 0) return '';
  const diffMins = Math.floor(diffMs / 60000);
  if (diffMins < 1) return 'Now';
  if (diffMins < 60) return `${diffMins}m`;
  const diffHrs = Math.floor(diffMins / 60);
  if (diffHrs < 24) return `${diffHrs}h`;
  const diffDays = Math.floor(diffHrs / 24);
  if (diffDays < 7) return `${diffDays}d`;
  const diffWeeks = Math.floor(diffDays / 7);
  if (diffWeeks < 4) return `${diffWeeks}w`;
  // Keep formatting locale-agnostic to avoid Intl/locale runtime issues on some Android builds.
  const month = date.getMonth() + 1;
  const day = date.getDate();
  return `${month}/${day}`;
}

export default function MessagesScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const { user, isAuthenticated } = useAuth();
  const { data: conversationsData, isLoading, refetch } = useConversations();
  const { data: unreadData } = useUnreadCount();
  const conversations: Conversation[] = useMemo(() => {
    const payload = Array.isArray((conversationsData as any)?.results)
      ? (conversationsData as any).results
      : Array.isArray(conversationsData)
        ? conversationsData
        : [];

    return payload.filter((item: any) => item && typeof item === 'object');
  }, [conversationsData]);
  const [sidebarVisible, setSidebarVisible] = useState(false);
  const [searchQuery, setSearchQuery] = useState('');
  const [refreshing, setRefreshing] = useState(false);

  const totalUnread = Number((unreadData as { unread_count?: number } | undefined)?.unread_count ?? 0);

  const handleAvatarPress = useCallback(() => {
    router.push(isAuthenticated ? '/(tabs)/profile' : '/(auth)/login');
  }, [isAuthenticated, router]);

  const onRefresh = useCallback(async () => {
    setRefreshing(true);
    await refetch();
    setRefreshing(false);
  }, [refetch]);

  const filteredConversations = useMemo(() =>
    conversations.filter((conv) => {
      if (!searchQuery) return true;
      const q = searchQuery.toLowerCase();
      const participantName = String((conv as any)?.participant_name ?? '').toLowerCase();
      const lastMessage = String((conv as any)?.last_message ?? '').toLowerCase();
      return participantName.includes(q) || lastMessage.includes(q);
    }),
    [conversations, searchQuery]
  );

  if (!isAuthenticated) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100">
        <Sidebar isVisible={sidebarVisible} onClose={() => setSidebarVisible(false)} />
        <LinearGradient colors={['#122F26', '#1d392f']} className="px-4 pb-6" style={{ paddingTop: insets.top + 12 }}>
          <View className="flex-row items-center justify-between mb-4">
            <TouchableOpacity
              onPress={() => setSidebarVisible(true)}
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="menu" size={24} color="#fff" />
            </TouchableOpacity>
            <Avatar uri={null} firstName={undefined} lastName={undefined} size="small" onPress={handleAvatarPress} />
          </View>
          <Text className="text-3xl font-black text-white tracking-tight">Messages</Text>
          <Text className="text-sand-200 text-sm mt-1">Chat with hosts and guests</Text>
        </LinearGradient>
        <View className="flex-1 items-center justify-center px-6">
          <View className="bg-white rounded-3xl p-8 items-center" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}>
            <View className="bg-sand-200 rounded-full p-8 mb-6">
              <Ionicons name="chatbubbles-outline" size={72} color="#D9B168" />
            </View>
            <Text className="text-2xl font-bold text-forest mb-3">Sign In to Chat</Text>
            <Text className="text-moss text-center mb-8 px-4 leading-6">Sign in to start conversations with hosts and guests</Text>
            <View className="flex-row gap-3">
              <TouchableOpacity className="flex-1" onPress={() => router.push('/(auth)/login')}>
                <LinearGradient colors={['#D9B168', '#bea04f']} className="px-6 py-4 rounded-2xl items-center" style={{ shadowColor: '#D9B168', shadowOffset: { width: 0, height: 4 }, shadowOpacity: 0.3, shadowRadius: 8, elevation: 5 }}>
                  <Text className="text-forest font-bold text-base">Log In</Text>
                </LinearGradient>
              </TouchableOpacity>
              <TouchableOpacity className="flex-1" onPress={() => router.push('/(auth)/register')}>
                <LinearGradient colors={['#122F26', '#1d392f']} className="px-6 py-4 rounded-2xl items-center" style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 4 }, shadowOpacity: 0.3, shadowRadius: 8, elevation: 5 }}>
                  <Text className="text-gold font-bold text-base">Sign Up</Text>
                </LinearGradient>
              </TouchableOpacity>
            </View>
          </View>
        </View>
      </SafeAreaView>
    );
  }

  const ConversationCard = ({ conversation, index }: { conversation: Conversation; index: number }) => {
    const displayName = String((conversation as any)?.participant_name ?? '').trim() || 'Guest';
    const initial = displayName.charAt(0).toUpperCase();
    const unreadCount = Number((conversation as any)?.unread_count ?? 0);
    const isUnread = unreadCount > 0;
    const relativeTime = timeAgo(conversation.last_message_time);

    return (
      <Animated.View entering={FadeInDown.delay(index * 50).duration(300)}>
        <TouchableOpacity
          className="mx-4 mb-2.5 rounded-2xl overflow-hidden bg-white"
          onPress={() =>
            router.push(`/(tabs)/messages/${String((conversation as any)?.id ?? '')}?participantId=${String((conversation as any)?.participant_id ?? '')}`)
          }
          activeOpacity={0.7}
          accessibilityRole="button"
          accessibilityLabel={`Conversation with ${displayName}${isUnread ? `, ${unreadCount} unread` : ''}`}
          style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.06, shadowRadius: 6, elevation: 3 }}
        >
          <View className="px-4 py-3.5 flex-row items-center">
            {/* Avatar with online-style ring for unread */}
            <View className="relative mr-3.5">
              <LinearGradient
                colors={isUnread ? ['#D9B168', '#bea04f'] : ['#3a5c50', '#2d4a40']}
                className="w-12 h-12 rounded-full items-center justify-center"
              >
                <Text className={`font-bold text-lg ${isUnread ? 'text-forest' : 'text-sand-200'}`}>
                  {initial}
                </Text>
              </LinearGradient>
              {isUnread && (
                <View className="absolute -top-0.5 -right-0.5 w-3.5 h-3.5 rounded-full bg-gold border-2 border-white" />
              )}
            </View>

            {/* Content */}
            <View className="flex-1">
              <View className="flex-row justify-between items-center mb-1">
                <Text className={`text-base ${isUnread ? 'font-bold text-forest' : 'font-semibold text-forest'}`} numberOfLines={1}>
                  {displayName}
                </Text>
                <Text className={`text-xs ml-2 ${isUnread ? 'text-gold font-semibold' : 'text-moss'}`}>
                  {relativeTime}
                </Text>
              </View>
              <View className="flex-row items-center">
                <Text
                  className={`flex-1 text-sm ${isUnread ? 'text-forest font-medium' : 'text-moss'}`}
                  numberOfLines={1}
                >
                  {conversation.last_message || 'No messages yet'}
                </Text>
                {isUnread && (
                  <View className="bg-gold rounded-full min-w-[20px] h-5 items-center justify-center ml-2 px-1.5">
                    <Text className="text-forest text-[10px] font-bold">{unreadCount}</Text>
                  </View>
                )}
              </View>
            </View>
          </View>
        </TouchableOpacity>
      </Animated.View>
    );
  };

  const ConversationSkeleton = () => (
    <View className="mx-4 mb-2.5 rounded-2xl bg-white p-4">
      <View className="flex-row items-center">
        <Skeleton height={48} width={48} borderRadius={24} className="mr-3.5" />
        <View className="flex-1">
          <View className="flex-row justify-between mb-2">
            <Skeleton height={16} width="50%" />
            <Skeleton height={12} width={32} />
          </View>
          <Skeleton height={14} width="75%" />
        </View>
      </View>
    </View>
  );

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      <Sidebar isVisible={sidebarVisible} onClose={() => setSidebarVisible(false)} />

      <View className="flex-1 bg-sand-100">
        {/* Header */}
        <LinearGradient
          colors={['#122F26', '#1d392f', '#2d4a40']}
          start={{ x: 0, y: 0 }}
          end={{ x: 1, y: 1 }}
          className="px-4 pb-5"
          style={{ paddingTop: insets.top + 12 }}
        >
          <View className="flex-row items-center justify-between mb-3">
            <TouchableOpacity
              onPress={() => setSidebarVisible(true)}
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
              accessibilityRole="button"
              accessibilityLabel="Open menu"
            >
              <Ionicons name="menu" size={24} color="#fff" />
            </TouchableOpacity>
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
              <Text className="text-2xl font-black text-white tracking-tight mb-1">Messages</Text>
              <View className="flex-row items-center">
                <Ionicons name="chatbubbles" size={14} color="#D9B168" />
                <Text className="text-sand-200 text-sm ml-1.5">
                  {filteredConversations.length} {filteredConversations.length === 1 ? 'conversation' : 'conversations'}
                </Text>
                {totalUnread > 0 && (
                  <>
                    <Text className="text-sand-300 mx-1.5">·</Text>
                    <Text className="text-gold font-semibold text-sm">{totalUnread} unread</Text>
                  </>
                )}
              </View>
            </View>
            <TouchableOpacity
              onPress={() => router.push('/(tabs)/messages/new')}
              accessibilityRole="button"
              accessibilityLabel="New message"
            >
              <LinearGradient
                colors={['#D9B168', '#bea04f']}
                className="w-11 h-11 rounded-full items-center justify-center"
                style={{ shadowColor: '#D9B168', shadowOffset: { width: 0, height: 4 }, shadowOpacity: 0.3, shadowRadius: 8, elevation: 5 }}
              >
                <Ionicons name="create-outline" size={22} color="#122F26" />
              </LinearGradient>
            </TouchableOpacity>
          </View>

          {/* Search Bar */}
          <View
            className="flex-row items-center mt-4 px-3.5 py-2.5"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.1)', borderRadius: 12 }}
          >
            <Ionicons name="search" size={18} color="#D9B168" />
            <TextInput
              placeholder="Search conversations..."
              placeholderTextColor="rgba(255, 255, 255, 0.4)"
              value={searchQuery}
              onChangeText={setSearchQuery}
              className="flex-1 ml-2 text-white text-sm"
              accessibilityLabel="Search conversations"
            />
            {searchQuery ? (
              <TouchableOpacity onPress={() => setSearchQuery('')} accessibilityLabel="Clear search">
                <Ionicons name="close-circle" size={18} color="rgba(255, 255, 255, 0.5)" />
              </TouchableOpacity>
            ) : null}
          </View>
        </LinearGradient>

        {/* Conversations List */}
        {isLoading ? (
          <View className="pt-3">
            {[1, 2, 3, 4, 5].map((i) => <ConversationSkeleton key={i} />)}
          </View>
        ) : (
          <FlatList
            data={filteredConversations}
            renderItem={({ item, index }) => <ConversationCard conversation={item} index={index} />}
            keyExtractor={(item, index) => String((item as any)?.id ?? `conversation-${index}`)}
            contentContainerStyle={{ paddingTop: 8, paddingBottom: 40 }}
            refreshControl={
              <RefreshControl refreshing={refreshing} onRefresh={onRefresh} tintColor="#D9B168" colors={['#D9B168']} />
            }
            ListEmptyComponent={
              <View className="flex-1 justify-center items-center py-20 px-6">
                <Animated.View
                  entering={FadeInDown.duration(400)}
                  className="bg-white rounded-3xl p-8 items-center"
                  style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}
                >
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
                    <TouchableOpacity onPress={() => router.push('/(tabs)/explore')}>
                      <LinearGradient
                        colors={['#122F26', '#1d392f']}
                        className="px-8 py-4 rounded-2xl"
                        style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 4 }, shadowOpacity: 0.3, shadowRadius: 8, elevation: 5 }}
                      >
                        <Text className="text-gold font-bold text-base">Explore Properties</Text>
                      </LinearGradient>
                    </TouchableOpacity>
                  )}
                </Animated.View>
              </View>
            }
          />
        )}
      </View>
    </SafeAreaView>
  );
}
