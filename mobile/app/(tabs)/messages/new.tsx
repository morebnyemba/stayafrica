import { View, Text, TouchableOpacity, FlatList, ActivityIndicator, Alert } from 'react-native';
import { useState } from 'react';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { useBookings, useCreateConversation } from '@/hooks/api-hooks';

export default function NewMessageScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const { data: bookingsData, isLoading } = useBookings();
  const createConversation = useCreateConversation();
  const [creating, setCreating] = useState<string | null>(null);

  const bookings = (bookingsData?.results || []).filter(
    (b: any) => b.status !== 'cancelled'
  );

  const handleStartConversation = async (booking: any) => {
    const propertyId = booking.rental_property || booking.property;
    if (!propertyId) {
      Alert.alert('Error', 'Property information not available');
      return;
    }
    try {
      setCreating(booking.id);
      const conversation = await createConversation.mutateAsync(propertyId);
      if (conversation?.id) {
        router.replace(
          `/(tabs)/messages/${conversation.id}?participantId=${conversation.participant_id || ''}`
        );
      }
    } catch (error: any) {
      Alert.alert('Error', error?.response?.data?.error || 'Failed to start conversation');
    } finally {
      setCreating(null);
    }
  };

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="px-4 pb-5"
        style={{ paddingTop: insets.top + 12 }}
      >
        <View className="flex-row items-center">
          <TouchableOpacity
            onPress={() => router.back()}
            className="w-10 h-10 rounded-xl items-center justify-center mr-3"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </TouchableOpacity>
          <View>
            <Text className="text-xl font-black text-white tracking-tight">
              New Conversation
            </Text>
            <Text className="text-sand-200 text-xs mt-0.5">
              Select a booking to message the host
            </Text>
          </View>
        </View>
      </LinearGradient>

      {isLoading ? (
        <View className="flex-1 items-center justify-center">
          <ActivityIndicator size="large" color="#D9B168" />
          <Text className="text-moss mt-4">Loading your bookings...</Text>
        </View>
      ) : (
        <FlatList
          data={bookings}
          keyExtractor={(item: any) => item.id}
          contentContainerStyle={{ padding: 16, paddingBottom: 40 }}
          ListEmptyComponent={
            <View className="items-center py-20 px-6">
              <View className="bg-white rounded-3xl p-8 items-center" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}>
                <View className="bg-sand-200 rounded-full p-6 mb-4">
                  <Ionicons name="home-outline" size={48} color="#3A5C50" />
                </View>
                <Text className="text-xl font-bold text-forest mb-2">No Bookings Yet</Text>
                <Text className="text-moss text-center mb-6 leading-5">
                  Book a property to start chatting with hosts
                </Text>
                <TouchableOpacity onPress={() => router.push('/(tabs)/explore')}>
                  <LinearGradient
                    colors={['#122F26', '#1d392f']}
                    className="px-6 py-3 rounded-2xl"
                  >
                    <Text className="text-gold font-bold">Explore Properties</Text>
                  </LinearGradient>
                </TouchableOpacity>
              </View>
            </View>
          }
          renderItem={({ item: booking }: { item: any }) => (
            <TouchableOpacity
              className="mb-3 rounded-2xl overflow-hidden bg-white"
              onPress={() => handleStartConversation(booking)}
              disabled={creating === booking.id}
              style={{
                shadowColor: '#122F26',
                shadowOffset: { width: 0, height: 4 },
                shadowOpacity: 0.08,
                shadowRadius: 8,
                elevation: 4,
              }}
            >
              <View className="px-4 py-4 flex-row items-center">
                <LinearGradient
                  colors={['#3A5C50', '#2d4a40']}
                  className="w-12 h-12 rounded-full items-center justify-center mr-4"
                >
                  <Ionicons name="home" size={22} color="#D9B168" />
                </LinearGradient>
                <View className="flex-1">
                  <Text className="font-bold text-forest text-base" numberOfLines={1}>
                    {booking.property_title || 'Property'}
                  </Text>
                  <Text className="text-moss text-xs mt-0.5">
                    {booking.check_in} → {booking.check_out}
                  </Text>
                  <Text className="text-xs text-moss mt-0.5">
                    Ref: {booking.booking_ref || booking.id}
                  </Text>
                </View>
                {creating === booking.id ? (
                  <ActivityIndicator size="small" color="#D9B168" />
                ) : (
                  <View className="bg-gold/20 rounded-full p-2">
                    <Ionicons name="chatbubble-outline" size={20} color="#D9B168" />
                  </View>
                )}
              </View>
            </TouchableOpacity>
          )}
        />
      )}
    </SafeAreaView>
  );
}
