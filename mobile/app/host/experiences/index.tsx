import { View, Text, FlatList, TouchableOpacity, Image, ActivityIndicator, Alert } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useHostExperiences, useDeleteExperience, useUpdateExperience } from '@/hooks/api-hooks';
import type { Experience, ExperienceStatus } from '@/types';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { useState } from 'react';

const statusConfig: Record<ExperienceStatus, { label: string; bg: string; text: string }> = {
  active: { label: 'Active', bg: 'bg-green-100', text: 'text-green-800' },
  inactive: { label: 'Inactive', bg: 'bg-gray-100', text: 'text-gray-800' },
  pending_approval: { label: 'Pending', bg: 'bg-yellow-100', text: 'text-yellow-800' },
};

const durationLabels: Record<string, string> = {
  half_day: 'Half Day',
  full_day: 'Full Day',
  multi_day: 'Multi-Day',
  hourly: 'Hourly',
};

export default function HostExperiencesScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const [statusFilter, setStatusFilter] = useState<string>('');

  const params = statusFilter ? { status: statusFilter } : undefined;
  const { data, isLoading, refetch } = useHostExperiences(params);
  const deleteMutation = useDeleteExperience();
  const updateMutation = useUpdateExperience();

  const experiences: Experience[] = data?.results || [];

  const stats = {
    total: experiences.length,
    active: experiences.filter((e) => e.status === 'active').length,
    inactive: experiences.filter((e) => e.status === 'inactive').length,
  };

  const handleDelete = (id: string, title: string) => {
    Alert.alert(
      'Delete Experience',
      `Are you sure you want to delete "${title}"?`,
      [
        { text: 'Cancel', style: 'cancel' },
        {
          text: 'Delete',
          style: 'destructive',
          onPress: () => deleteMutation.mutate(id),
        },
      ]
    );
  };

  const handleToggleStatus = (exp: Experience) => {
    const newStatus = exp.status === 'active' ? 'inactive' : 'active';
    updateMutation.mutate({
      id: exp.id,
      data: { status: newStatus },
    });
  };

  const renderExperience = ({ item: exp }: { item: Experience }) => {
    const config = statusConfig[exp.status] || statusConfig.inactive;
    return (
      <TouchableOpacity
        className="mx-4 mb-4"
        onPress={() => router.push(`/experiences/${exp.id}`)}
      >
        <View
          className="bg-white rounded-2xl overflow-hidden"
          style={{
            shadowColor: '#122F26',
            shadowOffset: { width: 0, height: 4 },
            shadowOpacity: 0.08,
            shadowRadius: 8,
            elevation: 4,
          }}
        >
          {exp.cover_image && (
            <Image
              source={{ uri: exp.cover_image }}
              style={{ width: '100%', height: 160 }}
              resizeMode="cover"
            />
          )}
          <View className="p-4">
            <View className="flex-row items-center justify-between mb-2">
              <Text className="text-lg font-bold text-forest flex-1 mr-2" numberOfLines={1}>
                {exp.title}
              </Text>
              <View className={`${config.bg} px-3 py-1 rounded-full`}>
                <Text className={`${config.text} text-xs font-semibold`}>{config.label}</Text>
              </View>
            </View>

            <View className="flex-row items-center mb-2">
              <Ionicons name="location" size={14} color="#3A5C50" />
              <Text className="text-sm text-moss ml-1">{exp.city}, {exp.country}</Text>
            </View>

            <View className="flex-row items-center justify-between mb-3">
              <View className="flex-row items-center">
                <Ionicons name="time" size={14} color="#D9B168" />
                <Text className="text-sm text-moss ml-1">
                  {exp.duration_hours}h · {durationLabels[exp.duration_type] || exp.duration_type}
                </Text>
              </View>
              <Text className="text-base font-bold text-forest">
                {exp.currency} {exp.price_per_person}/person
              </Text>
            </View>

            {/* Action Buttons */}
            <View className="flex-row gap-2 pt-3 border-t border-sand-200">
              <TouchableOpacity
                className="flex-1 flex-row items-center justify-center py-2.5 bg-gold/10 rounded-xl"
                onPress={() => router.push({
                  pathname: '/host/experiences/edit',
                  params: { id: exp.id },
                })}
              >
                <Ionicons name="create" size={16} color="#D9B168" />
                <Text className="text-sm font-semibold text-forest ml-1">Edit</Text>
              </TouchableOpacity>

              <TouchableOpacity
                className="flex-1 flex-row items-center justify-center py-2.5 bg-sand-100 rounded-xl"
                onPress={() => handleToggleStatus(exp)}
              >
                <Ionicons
                  name={exp.status === 'active' ? 'eye-off' : 'eye'}
                  size={16}
                  color="#3A5C50"
                />
                <Text className="text-sm font-semibold text-forest ml-1">
                  {exp.status === 'active' ? 'Deactivate' : 'Activate'}
                </Text>
              </TouchableOpacity>

              <TouchableOpacity
                className="py-2.5 px-3 bg-red-50 rounded-xl"
                onPress={() => handleDelete(exp.id, exp.title)}
              >
                <Ionicons name="trash" size={16} color="#EF4444" />
              </TouchableOpacity>
            </View>
          </View>
        </View>
      </TouchableOpacity>
    );
  };

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
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
          <TouchableOpacity
            onPress={() => router.push('/host/experiences/new')}
            className="flex-row items-center px-4 py-2 rounded-xl"
            style={{ backgroundColor: 'rgba(217, 177, 104, 0.9)' }}
          >
            <Ionicons name="add" size={20} color="#122F26" />
            <Text className="text-forest font-bold ml-1">New</Text>
          </TouchableOpacity>
        </View>
        <Text className="text-3xl font-black text-white tracking-tight">
          My Experiences
        </Text>
        <Text className="text-sand-200 text-sm mt-1">
          {stats.total} total · {stats.active} active
        </Text>
      </LinearGradient>

      {/* Status Filter */}
      <View className="px-4 py-3">
        <View className="flex-row gap-2">
          {[
            { value: '', label: 'All' },
            { value: 'active', label: 'Active' },
            { value: 'inactive', label: 'Inactive' },
            { value: 'pending_approval', label: 'Pending' },
          ].map((f) => (
            <TouchableOpacity
              key={f.value}
              onPress={() => setStatusFilter(f.value)}
              className="px-4 py-2 rounded-full"
              style={{
                backgroundColor: statusFilter === f.value ? '#D9B168' : '#fff',
              }}
            >
              <Text
                className={`text-sm font-semibold ${
                  statusFilter === f.value ? 'text-white' : 'text-forest'
                }`}
              >
                {f.label}
              </Text>
            </TouchableOpacity>
          ))}
        </View>
      </View>

      {/* Bookings Link */}
      <TouchableOpacity
        className="mx-4 mb-3"
        onPress={() => router.push('/host/experiences/bookings')}
      >
        <View className="bg-white rounded-2xl p-4 flex-row items-center" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 2 },
          shadowOpacity: 0.05,
          shadowRadius: 4,
          elevation: 2,
        }}>
          <LinearGradient
            colors={['#D9B16820', '#D9B16810']}
            className="w-10 h-10 rounded-full items-center justify-center"
          >
            <Ionicons name="calendar" size={20} color="#D9B168" />
          </LinearGradient>
          <View className="flex-1 ml-3">
            <Text className="text-base font-semibold text-forest">Experience Bookings</Text>
            <Text className="text-sm text-moss">View & manage guest bookings</Text>
          </View>
          <Ionicons name="chevron-forward" size={20} color="#3A5C50" />
        </View>
      </TouchableOpacity>

      {/* List */}
      {isLoading ? (
        <View className="flex-1 items-center justify-center">
          <ActivityIndicator size="large" color="#D9B168" />
          <Text className="mt-4 text-moss">Loading experiences...</Text>
        </View>
      ) : (
        <FlatList
          data={experiences}
          renderItem={renderExperience}
          keyExtractor={(item) => item.id}
          contentContainerStyle={{ paddingBottom: 40 }}
          showsVerticalScrollIndicator={false}
          onRefresh={refetch}
          refreshing={false}
          ListEmptyComponent={
            <View className="py-16 items-center px-6">
              <View className="bg-sand-200 rounded-full p-8 mb-4">
                <Ionicons name="compass-outline" size={64} color="#94a3b8" />
              </View>
              <Text className="text-xl font-bold text-forest mb-2">No Experiences Yet</Text>
              <Text className="text-moss text-center mb-6">
                Create your first experience to start hosting unique activities
              </Text>
              <TouchableOpacity
                onPress={() => router.push('/host/experiences/new')}
              >
                <LinearGradient
                  colors={['#D9B168', '#bea04f']}
                  className="px-8 py-4 rounded-2xl"
                >
                  <Text className="text-forest font-bold text-base">Create Experience</Text>
                </LinearGradient>
              </TouchableOpacity>
            </View>
          }
        />
      )}
    </SafeAreaView>
  );
}
