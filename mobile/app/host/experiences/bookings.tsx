import { View, Text, FlatList, TouchableOpacity, Alert, RefreshControl } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { useState, useMemo } from 'react';
import {
  useHostExperienceBookings,
  useConfirmExperienceBooking,
  useCompleteExperienceBooking,
  useCancelExperienceBooking,
} from '@/hooks/api-hooks';
import type { ExperienceBooking, ExperienceBookingStatus } from '@/types';

const statusColors: Record<ExperienceBookingStatus, { bg: string; text: string }> = {
  pending: { bg: '#FEF3C7', text: '#D97706' },
  confirmed: { bg: '#DBEAFE', text: '#2563EB' },
  completed: { bg: '#D1FAE5', text: '#059669' },
  cancelled: { bg: '#FEE2E2', text: '#DC2626' },
};

const filterTabs: { label: string; value: ExperienceBookingStatus | 'all' }[] = [
  { label: 'All', value: 'all' },
  { label: 'Pending', value: 'pending' },
  { label: 'Confirmed', value: 'confirmed' },
  { label: 'Completed', value: 'completed' },
  { label: 'Cancelled', value: 'cancelled' },
];

export default function HostExperienceBookingsScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const [activeFilter, setActiveFilter] = useState<ExperienceBookingStatus | 'all'>('all');

  const { data: bookings, isLoading, refetch } = useHostExperienceBookings();
  const confirmMutation = useConfirmExperienceBooking();
  const completeMutation = useCompleteExperienceBooking();
  const cancelMutation = useCancelExperienceBooking();

  const filtered = useMemo(() => {
    const list = (bookings as ExperienceBooking[]) || [];
    if (activeFilter === 'all') return list;
    return list.filter((b) => b.status === activeFilter);
  }, [bookings, activeFilter]);

  const stats = useMemo(() => {
    const list = (bookings as ExperienceBooking[]) || [];
    return {
      pending: list.filter((b) => b.status === 'pending').length,
      confirmed: list.filter((b) => b.status === 'confirmed').length,
      completed: list.filter((b) => b.status === 'completed').length,
    };
  }, [bookings]);

  const handleConfirm = (id: number) => {
    Alert.alert('Confirm Booking', 'Confirm this experience booking?', [
      { text: 'Cancel', style: 'cancel' },
      {
        text: 'Confirm',
        onPress: () =>
          confirmMutation.mutate(id, {
            onSuccess: () => refetch(),
            onError: () => Alert.alert('Error', 'Failed to confirm booking'),
          }),
      },
    ]);
  };

  const handleComplete = (id: number) => {
    Alert.alert('Complete Booking', 'Mark this booking as completed?', [
      { text: 'Cancel', style: 'cancel' },
      {
        text: 'Complete',
        onPress: () =>
          completeMutation.mutate(id, {
            onSuccess: () => refetch(),
            onError: () => Alert.alert('Error', 'Failed to complete booking'),
          }),
      },
    ]);
  };

  const handleCancel = (id: number) => {
    Alert.alert('Cancel Booking', 'Are you sure you want to cancel this booking?', [
      { text: 'No', style: 'cancel' },
      {
        text: 'Yes, Cancel',
        style: 'destructive',
        onPress: () =>
          cancelMutation.mutate(id, {
            onSuccess: () => refetch(),
            onError: () => Alert.alert('Error', 'Failed to cancel booking'),
          }),
      },
    ]);
  };

  const renderBooking = ({ item }: { item: ExperienceBooking }) => {
    const colors = statusColors[item.status] || statusColors.pending;
    return (
      <View
        className="bg-white rounded-2xl mx-4 mb-3 p-4"
        style={{
          shadowColor: '#000',
          shadowOffset: { width: 0, height: 2 },
          shadowOpacity: 0.06,
          shadowRadius: 8,
          elevation: 3,
        }}
      >
        {/* Header */}
        <View className="flex-row items-start justify-between mb-2">
          <View className="flex-1 mr-3">
            <Text className="text-base font-bold text-forest" numberOfLines={1}>
              {item.experience_title || `Booking #${item.id}`}
            </Text>
            <Text className="text-xs text-moss mt-0.5">
              {item.guest_name || 'Guest'}
            </Text>
          </View>
          <View
            className="px-3 py-1 rounded-full"
            style={{ backgroundColor: colors.bg }}
          >
            <Text className="text-xs font-bold capitalize" style={{ color: colors.text }}>
              {item.status}
            </Text>
          </View>
        </View>

        {/* Details */}
        <View className="flex-row flex-wrap gap-y-2 mt-2">
          <View className="flex-row items-center w-1/2">
            <Ionicons name="calendar-outline" size={14} color="#3A5C50" />
            <Text className="text-xs text-moss ml-1.5">
              {item.booking_date || 'No date'}
            </Text>
          </View>
          <View className="flex-row items-center w-1/2">
            <Ionicons name="people-outline" size={14} color="#3A5C50" />
            <Text className="text-xs text-moss ml-1.5">
              {item.number_of_participants} participant{item.number_of_participants !== 1 ? 's' : ''}
            </Text>
          </View>
          <View className="flex-row items-center w-1/2">
            <Ionicons name="cash-outline" size={14} color="#3A5C50" />
            <Text className="text-xs text-moss ml-1.5">
              {item.currency || 'USD'} {item.total_price}
            </Text>
          </View>
          {item.created_at && (
            <View className="flex-row items-center w-1/2">
              <Ionicons name="time-outline" size={14} color="#3A5C50" />
              <Text className="text-xs text-moss ml-1.5">
                Booked {new Date(item.created_at).toLocaleDateString()}
              </Text>
            </View>
          )}
        </View>

        {/* Actions */}
        {item.status === 'pending' && (
          <View className="flex-row gap-2 mt-3 pt-3 border-t border-sand-200">
            <TouchableOpacity
              onPress={() => handleConfirm(item.id)}
              className="flex-1 flex-row items-center justify-center py-2.5 rounded-xl bg-emerald-500"
            >
              <Ionicons name="checkmark-circle" size={16} color="#fff" />
              <Text className="text-white font-semibold text-xs ml-1.5">Confirm</Text>
            </TouchableOpacity>
            <TouchableOpacity
              onPress={() => handleCancel(item.id)}
              className="flex-1 flex-row items-center justify-center py-2.5 rounded-xl bg-red-50"
            >
              <Ionicons name="close-circle" size={16} color="#dc2626" />
              <Text className="text-red-600 font-semibold text-xs ml-1.5">Cancel</Text>
            </TouchableOpacity>
          </View>
        )}

        {item.status === 'confirmed' && (
          <View className="flex-row gap-2 mt-3 pt-3 border-t border-sand-200">
            <TouchableOpacity
              onPress={() => handleComplete(item.id)}
              className="flex-1 flex-row items-center justify-center py-2.5 rounded-xl bg-blue-500"
            >
              <Ionicons name="trophy" size={16} color="#fff" />
              <Text className="text-white font-semibold text-xs ml-1.5">Mark Complete</Text>
            </TouchableOpacity>
            <TouchableOpacity
              onPress={() => handleCancel(item.id)}
              className="flex-1 flex-row items-center justify-center py-2.5 rounded-xl bg-red-50"
            >
              <Ionicons name="close-circle" size={16} color="#dc2626" />
              <Text className="text-red-600 font-semibold text-xs ml-1.5">Cancel</Text>
            </TouchableOpacity>
          </View>
        )}
      </View>
    );
  };

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f']}
        className="px-4 pb-5"
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
          <Text className="text-xl font-bold text-white">Experience Bookings</Text>
          <View className="w-10" />
        </View>

        {/* Quick Stats */}
        <View className="flex-row mt-4 gap-3">
          {[
            { label: 'Pending', value: stats.pending, icon: 'hourglass' as const, color: '#F59E0B' },
            { label: 'Confirmed', value: stats.confirmed, icon: 'checkmark-circle' as const, color: '#3B82F6' },
            { label: 'Completed', value: stats.completed, icon: 'trophy' as const, color: '#10B981' },
          ].map((s) => (
            <View
              key={s.label}
              className="flex-1 rounded-xl p-3 items-center"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.1)' }}
            >
              <Ionicons name={s.icon} size={18} color={s.color} />
              <Text className="text-white font-bold text-lg mt-1">{s.value}</Text>
              <Text className="text-sand-300 text-xs">{s.label}</Text>
            </View>
          ))}
        </View>
      </LinearGradient>

      {/* Filter Tabs */}
      <View className="px-4 pt-3 pb-1">
        <FlatList
          data={filterTabs}
          horizontal
          showsHorizontalScrollIndicator={false}
          keyExtractor={(item) => item.value}
          renderItem={({ item }) => (
            <TouchableOpacity
              onPress={() => setActiveFilter(item.value)}
              className="mr-2 px-4 py-2 rounded-xl"
              style={{
                backgroundColor: activeFilter === item.value ? '#122F26' : '#fff',
              }}
            >
              <Text
                className={`text-sm font-semibold ${
                  activeFilter === item.value ? 'text-gold' : 'text-forest'
                }`}
              >
                {item.label}
              </Text>
            </TouchableOpacity>
          )}
        />
      </View>

      {/* Bookings List */}
      <FlatList
        data={filtered}
        keyExtractor={(item) => String(item.id)}
        renderItem={renderBooking}
        contentContainerStyle={{ paddingTop: 12, paddingBottom: 24 }}
        refreshControl={<RefreshControl refreshing={isLoading} onRefresh={refetch} />}
        ListEmptyComponent={
          <View className="items-center justify-center py-20 px-8">
            <Ionicons name="receipt-outline" size={48} color="#D9B168" />
            <Text className="text-lg font-bold text-forest mt-4 text-center">
              No bookings found
            </Text>
            <Text className="text-sm text-moss mt-1 text-center">
              {activeFilter === 'all'
                ? 'When guests book your experiences, they will appear here.'
                : `No ${activeFilter} bookings at the moment.`}
            </Text>
          </View>
        }
      />
    </SafeAreaView>
  );
}
