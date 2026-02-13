import { View, Text, ScrollView, TouchableOpacity, TextInput, Alert, KeyboardAvoidingView, Platform } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { useState } from 'react';
import { useCreateExperience, useExperienceCategories } from '@/hooks/api-hooks';
import type { ExperienceDuration, ExperienceDifficulty } from '@/types';

export default function NewExperienceScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const createMutation = useCreateExperience();
  const { data: categories } = useExperienceCategories();

  const [form, setForm] = useState({
    title: '',
    description: '',
    category: '',
    country: '',
    city: '',
    address: '',
    latitude: '',
    longitude: '',
    price_per_person: '',
    currency: 'USD',
    duration_type: 'half_day' as ExperienceDuration,
    duration_hours: '',
    difficulty: 'easy' as ExperienceDifficulty,
    min_participants: '1',
    max_participants: '10',
    cancellation_policy: '',
  });

  const [errors, setErrors] = useState<Record<string, string>>({});

  const updateField = (key: string, value: string) => {
    setForm((prev) => ({ ...prev, [key]: value }));
    if (errors[key]) setErrors((prev) => ({ ...prev, [key]: '' }));
  };

  const validate = () => {
    const newErrors: Record<string, string> = {};
    if (!form.title.trim()) newErrors.title = 'Title is required';
    if (!form.description.trim()) newErrors.description = 'Description is required';
    if (!form.country.trim()) newErrors.country = 'Country is required';
    if (!form.city.trim()) newErrors.city = 'City is required';
    if (!form.price_per_person) newErrors.price_per_person = 'Price is required';
    if (!form.latitude || !form.longitude) newErrors.latitude = 'Location coordinates are required';
    if (!form.duration_hours) newErrors.duration_hours = 'Duration is required';
    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleSubmit = () => {
    if (!validate()) return;

    const data = {
      title: form.title.trim(),
      description: form.description.trim(),
      category: form.category ? Number(form.category) : undefined,
      country: form.country.trim(),
      city: form.city.trim(),
      address: form.address.trim(),
      latitude: parseFloat(form.latitude),
      longitude: parseFloat(form.longitude),
      price_per_person: parseFloat(form.price_per_person),
      currency: form.currency,
      duration_type: form.duration_type,
      duration_hours: parseFloat(form.duration_hours),
      difficulty: form.difficulty,
      min_participants: parseInt(form.min_participants),
      max_participants: parseInt(form.max_participants),
      cancellation_policy: form.cancellation_policy.trim(),
    };

    createMutation.mutate(data, {
      onSuccess: () => {
        Alert.alert('Success', 'Experience created successfully!', [
          { text: 'OK', onPress: () => router.back() },
        ]);
      },
      onError: (error: any) => {
        Alert.alert('Error', error?.response?.data?.detail || 'Failed to create experience');
      },
    });
  };

  const InputField = ({
    label,
    value,
    onChangeText,
    error,
    placeholder,
    multiline,
    keyboardType,
  }: {
    label: string;
    value: string;
    onChangeText: (text: string) => void;
    error?: string;
    placeholder?: string;
    multiline?: boolean;
    keyboardType?: 'default' | 'numeric' | 'decimal-pad';
  }) => (
    <View className="mb-4">
      <Text className="text-sm font-semibold text-forest mb-1.5">{label}</Text>
      <TextInput
        value={value}
        onChangeText={onChangeText}
        placeholder={placeholder}
        placeholderTextColor="#94a3b8"
        keyboardType={keyboardType || 'default'}
        multiline={multiline}
        className={`px-4 py-3 bg-white rounded-xl border ${
          error ? 'border-red-400' : 'border-sand-300'
        } text-forest ${multiline ? 'min-h-[100px] text-top' : ''}`}
        style={multiline ? { textAlignVertical: 'top' } : undefined}
      />
      {error && <Text className="text-red-500 text-xs mt-1">{error}</Text>}
    </View>
  );

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      <KeyboardAvoidingView
        behavior={Platform.OS === 'ios' ? 'padding' : undefined}
        className="flex-1"
      >
        {/* Header */}
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
            <Text className="text-xl font-bold text-white">Create Experience</Text>
            <View className="w-10" />
          </View>
        </LinearGradient>

        <ScrollView
          className="flex-1 px-4 pt-4"
          showsVerticalScrollIndicator={false}
          contentContainerStyle={{ paddingBottom: 40 }}
        >
          {/* Basic Info */}
          <Text className="text-lg font-bold text-forest mb-3">Basic Information</Text>

          <InputField
            label="Title"
            value={form.title}
            onChangeText={(v) => updateField('title', v)}
            error={errors.title}
            placeholder="e.g. Sunset Safari Tour"
          />

          <InputField
            label="Description"
            value={form.description}
            onChangeText={(v) => updateField('description', v)}
            error={errors.description}
            placeholder="Describe your experience..."
            multiline
          />

          {/* Category */}
          <View className="mb-4">
            <Text className="text-sm font-semibold text-forest mb-1.5">Category</Text>
            <ScrollView horizontal showsHorizontalScrollIndicator={false}>
              {(categories || []).map((cat) => (
                <TouchableOpacity
                  key={cat.id}
                  onPress={() => updateField('category', String(cat.id))}
                  className="mr-2 px-4 py-2.5 rounded-xl"
                  style={{
                    backgroundColor: form.category === String(cat.id) ? '#D9B168' : '#fff',
                  }}
                >
                  <Text
                    className={`text-sm font-semibold ${
                      form.category === String(cat.id) ? 'text-white' : 'text-forest'
                    }`}
                  >
                    {cat.name}
                  </Text>
                </TouchableOpacity>
              ))}
            </ScrollView>
          </View>

          {/* Location */}
          <Text className="text-lg font-bold text-forest mb-3 mt-4">Location</Text>

          <View className="flex-row gap-3">
            <View className="flex-1">
              <InputField
                label="Country"
                value={form.country}
                onChangeText={(v) => updateField('country', v)}
                error={errors.country}
                placeholder="Zimbabwe"
              />
            </View>
            <View className="flex-1">
              <InputField
                label="City"
                value={form.city}
                onChangeText={(v) => updateField('city', v)}
                error={errors.city}
                placeholder="Victoria Falls"
              />
            </View>
          </View>

          <InputField
            label="Address"
            value={form.address}
            onChangeText={(v) => updateField('address', v)}
            placeholder="Street address"
          />

          <View className="flex-row gap-3">
            <View className="flex-1">
              <InputField
                label="Latitude"
                value={form.latitude}
                onChangeText={(v) => updateField('latitude', v)}
                error={errors.latitude}
                placeholder="-17.9243"
                keyboardType="decimal-pad"
              />
            </View>
            <View className="flex-1">
              <InputField
                label="Longitude"
                value={form.longitude}
                onChangeText={(v) => updateField('longitude', v)}
                placeholder="25.8572"
                keyboardType="decimal-pad"
              />
            </View>
          </View>

          {/* Pricing & Duration */}
          <Text className="text-lg font-bold text-forest mb-3 mt-4">Pricing & Duration</Text>

          <View className="flex-row gap-3">
            <View className="flex-1">
              <InputField
                label="Price per Person"
                value={form.price_per_person}
                onChangeText={(v) => updateField('price_per_person', v)}
                error={errors.price_per_person}
                placeholder="50.00"
                keyboardType="decimal-pad"
              />
            </View>
            <View className="flex-1">
              <InputField
                label="Duration (hours)"
                value={form.duration_hours}
                onChangeText={(v) => updateField('duration_hours', v)}
                error={errors.duration_hours}
                placeholder="3"
                keyboardType="decimal-pad"
              />
            </View>
          </View>

          {/* Duration Type */}
          <View className="mb-4">
            <Text className="text-sm font-semibold text-forest mb-1.5">Duration Type</Text>
            <View className="flex-row flex-wrap gap-2">
              {(['hourly', 'half_day', 'full_day', 'multi_day'] as ExperienceDuration[]).map((d) => (
                <TouchableOpacity
                  key={d}
                  onPress={() => updateField('duration_type', d)}
                  className="px-4 py-2.5 rounded-xl"
                  style={{
                    backgroundColor: form.duration_type === d ? '#D9B168' : '#fff',
                  }}
                >
                  <Text
                    className={`text-sm font-semibold ${
                      form.duration_type === d ? 'text-white' : 'text-forest'
                    }`}
                  >
                    {d === 'half_day' ? 'Half Day' : d === 'full_day' ? 'Full Day' : d === 'multi_day' ? 'Multi-Day' : 'Hourly'}
                  </Text>
                </TouchableOpacity>
              ))}
            </View>
          </View>

          {/* Difficulty */}
          <View className="mb-4">
            <Text className="text-sm font-semibold text-forest mb-1.5">Difficulty</Text>
            <View className="flex-row gap-2">
              {(['easy', 'moderate', 'challenging'] as ExperienceDifficulty[]).map((d) => (
                <TouchableOpacity
                  key={d}
                  onPress={() => updateField('difficulty', d)}
                  className="flex-1 py-3 rounded-xl items-center"
                  style={{
                    backgroundColor: form.difficulty === d
                      ? d === 'easy' ? '#10B981' : d === 'moderate' ? '#F59E0B' : '#EF4444'
                      : '#fff',
                  }}
                >
                  <Text
                    className={`text-sm font-semibold capitalize ${
                      form.difficulty === d ? 'text-white' : 'text-forest'
                    }`}
                  >
                    {d}
                  </Text>
                </TouchableOpacity>
              ))}
            </View>
          </View>

          {/* Capacity */}
          <Text className="text-lg font-bold text-forest mb-3 mt-4">Capacity</Text>
          <View className="flex-row gap-3">
            <View className="flex-1">
              <InputField
                label="Min Participants"
                value={form.min_participants}
                onChangeText={(v) => updateField('min_participants', v)}
                placeholder="1"
                keyboardType="numeric"
              />
            </View>
            <View className="flex-1">
              <InputField
                label="Max Participants"
                value={form.max_participants}
                onChangeText={(v) => updateField('max_participants', v)}
                placeholder="10"
                keyboardType="numeric"
              />
            </View>
          </View>

          {/* Cancellation Policy */}
          <InputField
            label="Cancellation Policy"
            value={form.cancellation_policy}
            onChangeText={(v) => updateField('cancellation_policy', v)}
            placeholder="Free cancellation up to 24 hours before..."
            multiline
          />

          {/* Submit */}
          <TouchableOpacity
            onPress={handleSubmit}
            disabled={createMutation.isPending}
            className="mt-4 rounded-2xl overflow-hidden"
          >
            <LinearGradient
              colors={createMutation.isPending ? ['#94a3b8', '#94a3b8'] : ['#D9B168', '#bea04f']}
              className="py-4 items-center flex-row justify-center"
              style={{
                shadowColor: '#D9B168',
                shadowOffset: { width: 0, height: 4 },
                shadowOpacity: 0.3,
                shadowRadius: 8,
                elevation: 5,
              }}
            >
              {createMutation.isPending ? (
                <Text className="text-white font-bold text-base">Creating...</Text>
              ) : (
                <>
                  <Ionicons name="add-circle" size={20} color="#122F26" />
                  <Text className="text-forest font-bold text-base ml-2">Create Experience</Text>
                </>
              )}
            </LinearGradient>
          </TouchableOpacity>
        </ScrollView>
      </KeyboardAvoidingView>
    </SafeAreaView>
  );
}
