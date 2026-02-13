import {
  View, Text, ScrollView, TouchableOpacity, TextInput, Alert,
  KeyboardAvoidingView, Platform, Image, Modal, FlatList,
} from 'react-native';
import { useState, useCallback } from 'react';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { useCreateExperience, useExperienceCategories } from '@/hooks/api-hooks';
import * as ImagePicker from 'expo-image-picker';
import * as Location from 'expo-location';
import type { ExperienceDuration, ExperienceDifficulty, ExperienceCategory } from '@/types';

// ── Constants ──────────────────────────────────────────
type FormStep = 'basic' | 'location' | 'details' | 'images';

const STEPS: { id: FormStep; label: string; desc: string; icon: keyof typeof Ionicons.glyphMap }[] = [
  { id: 'basic', label: 'Basic Info', desc: 'Title & category', icon: 'compass-outline' },
  { id: 'location', label: 'Location', desc: 'Where is it?', icon: 'location-outline' },
  { id: 'details', label: 'Details', desc: 'Pricing & capacity', icon: 'pricetag-outline' },
  { id: 'images', label: 'Image', desc: 'Cover photo', icon: 'images-outline' },
];

const COUNTRIES = [
  'Zimbabwe', 'South Africa', 'Botswana', 'Namibia', 'Zambia', 'Mozambique',
  'Malawi', 'Lesotho', 'Eswatini', 'Tanzania', 'Kenya', 'Uganda', 'Rwanda',
];

const CURRENCIES = ['USD', 'ZAR', 'ZWL', 'EUR', 'GBP', 'KES', 'NGN'];

const DURATION_LABELS: Record<ExperienceDuration, string> = {
  hourly: 'Hourly',
  half_day: 'Half Day',
  full_day: 'Full Day',
  multi_day: 'Multi-Day',
};

const DIFFICULTY_META: { value: ExperienceDifficulty; label: string; color: string }[] = [
  { value: 'easy', label: 'Easy', color: '#10B981' },
  { value: 'moderate', label: 'Moderate', color: '#F59E0B' },
  { value: 'challenging', label: 'Challenging', color: '#EF4444' },
];

// ── InputField ─────────────────────────────────────────
function InputField({
  label, value, onChangeText, placeholder, required, multiline, keyboardType, error,
  type = 'text', onPress,
}: {
  label: string; value: string; onChangeText: (t: string) => void; placeholder: string;
  required?: boolean; multiline?: boolean; keyboardType?: any; error?: string;
  type?: 'text' | 'select'; onPress?: () => void;
}) {
  return (
    <View className="mb-4">
      <Text className="text-base font-semibold text-forest mb-2">
        {label} {required && <Text className="text-red-500">*</Text>}
      </Text>
      {type === 'select' ? (
        <TouchableOpacity
          onPress={onPress}
          className="bg-white rounded-2xl p-4 flex-row justify-between items-center"
          style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 4, elevation: 2 }}
        >
          <Text className={`text-base ${value ? 'text-forest' : 'text-[#94a3b8]'}`}>
            {value || placeholder}
          </Text>
          <Ionicons name="chevron-down" size={20} color="#3A5C50" />
        </TouchableOpacity>
      ) : (
        <View
          className="bg-white rounded-2xl p-4"
          style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 4, elevation: 2 }}
        >
          <TextInput
            className="text-base text-forest"
            placeholder={placeholder}
            placeholderTextColor="#94a3b8"
            keyboardType={keyboardType || 'default'}
            multiline={multiline}
            numberOfLines={multiline ? 4 : 1}
            textAlignVertical={multiline ? 'top' : 'center'}
            value={value}
            onChangeText={onChangeText}
          />
        </View>
      )}
      {error && <Text className="text-red-500 text-xs mt-1">{error}</Text>}
    </View>
  );
}

// ── Screen ─────────────────────────────────────────────
export default function NewExperienceScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const createMutation = useCreateExperience();
  const { data: rawCategories } = useExperienceCategories();
  const categories: ExperienceCategory[] = Array.isArray(rawCategories)
    ? rawCategories
    : (rawCategories as any)?.results ?? [];

  const [step, setStep] = useState<FormStep>('basic');
  const [coverImage, setCoverImage] = useState<string | null>(null);
  const [showCountryPicker, setShowCountryPicker] = useState(false);
  const [showCategoryPicker, setShowCategoryPicker] = useState(false);
  const [loading, setLoading] = useState(false);

  const [form, setForm] = useState({
    title: '',
    description: '',
    category: '',
    categoryName: '',
    country: '',
    city: '',
    address: '',
    latitude: null as number | null,
    longitude: null as number | null,
    price_per_person: '',
    currency: 'USD',
    duration_type: 'half_day' as ExperienceDuration,
    duration_hours: '',
    difficulty: 'easy' as ExperienceDifficulty,
    min_participants: '1',
    max_participants: '10',
    included_items: '',
    requirements: '',
    cancellation_policy: '',
  });

  const update = (key: string, value: any) =>
    setForm((p) => ({ ...p, [key]: value }));

  // ── Location helpers ───────────────────────────────
  const getCurrentLocation = useCallback(async () => {
    const { status } = await Location.requestForegroundPermissionsAsync();
    if (status !== 'granted') {
      Alert.alert('Permission Denied', 'Location permission is required');
      return;
    }
    const loc = await Location.getCurrentPositionAsync({});
    setForm((p) => ({ ...p, latitude: loc.coords.latitude, longitude: loc.coords.longitude }));
    Alert.alert('Success', 'GPS coordinates captured!');
  }, []);

  const handleGeocode = useCallback(async () => {
    if (!form.address || !form.city || !form.country) {
      Alert.alert('Missing Info', 'Enter address, city and country first');
      return;
    }
    try {
      setLoading(true);
      const addr = `${form.address}, ${form.city}, ${form.country}`;
      const result = await Location.geocodeAsync(addr);
      if (result.length > 0) {
        setForm((p) => ({ ...p, latitude: result[0].latitude, longitude: result[0].longitude }));
        Alert.alert('Location Found', 'Coordinates updated!');
      } else {
        Alert.alert('Not Found', 'Could not geocode this address');
      }
    } catch {
      Alert.alert('Error', 'Failed to find coordinates');
    } finally {
      setLoading(false);
    }
  }, [form.address, form.city, form.country]);

  // ── Image picker ───────────────────────────────────
  const pickCoverImage = useCallback(async () => {
    const { status } = await ImagePicker.requestMediaLibraryPermissionsAsync();
    if (status !== 'granted') {
      Alert.alert('Permission Denied', 'Camera roll permission is required');
      return;
    }
    const result = await ImagePicker.launchImageLibraryAsync({
      mediaTypes: ImagePicker.MediaTypeOptions.Images,
      quality: 0.8,
      allowsEditing: true,
      aspect: [16, 9],
    });
    if (!result.canceled && result.assets[0]) {
      setCoverImage(result.assets[0].uri);
    }
  }, []);

  // ── Validation ─────────────────────────────────────
  const validateStep = (): boolean => {
    switch (step) {
      case 'basic':
        if (!form.title.trim() || !form.description.trim()) {
          Alert.alert('Error', 'Title and description are required');
          return false;
        }
        return true;
      case 'location':
        if (!form.country || !form.city) {
          Alert.alert('Error', 'Country and city are required');
          return false;
        }
        if (form.latitude === null || form.longitude === null) {
          Alert.alert('Error', 'Please set the location coordinates');
          return false;
        }
        return true;
      case 'details':
        if (!form.price_per_person || parseFloat(form.price_per_person) <= 0) {
          Alert.alert('Error', 'Please set a valid price');
          return false;
        }
        if (!form.duration_hours || parseFloat(form.duration_hours) <= 0) {
          Alert.alert('Error', 'Please set the duration');
          return false;
        }
        return true;
      case 'images':
        return true; // cover image is optional
    }
  };

  const handleNext = () => {
    if (!validateStep()) return;
    const idx = STEPS.findIndex((s) => s.id === step);
    if (idx < STEPS.length - 1) setStep(STEPS[idx + 1].id);
  };

  const handleBack = () => {
    const idx = STEPS.findIndex((s) => s.id === step);
    if (idx > 0) setStep(STEPS[idx - 1].id);
    else router.back();
  };

  // ── Submit ─────────────────────────────────────────
  const handleCreate = async () => {
    if (!validateStep()) return;
    setLoading(true);
    try {
      const payload = new FormData();
      payload.append('title', form.title.trim());
      payload.append('description', form.description.trim());
      if (form.category) payload.append('category', form.category);
      payload.append('country', form.country);
      payload.append('city', form.city);
      payload.append('address', form.address);
      payload.append('latitude', String(form.latitude));
      payload.append('longitude', String(form.longitude));
      payload.append('price_per_person', form.price_per_person);
      payload.append('currency', form.currency);
      payload.append('duration', form.duration_type);
      payload.append('duration_hours', form.duration_hours);
      payload.append('difficulty', form.difficulty);
      payload.append('min_participants', form.min_participants);
      payload.append('max_participants', form.max_participants);
      if (form.included_items.trim()) payload.append('included_items', form.included_items.trim());
      if (form.requirements.trim()) payload.append('requirements', form.requirements.trim());
      if (form.cancellation_policy.trim()) payload.append('cancellation_policy', form.cancellation_policy.trim());

      if (coverImage) {
        payload.append('main_image', {
          uri: coverImage,
          type: 'image/jpeg',
          name: 'cover.jpg',
        } as any);
      }

      await createMutation.mutateAsync(payload);
      Alert.alert('Success', 'Experience created!', [
        { text: 'OK', onPress: () => router.back() },
      ]);
    } catch (err: any) {
      Alert.alert('Error', err?.response?.data?.detail || 'Failed to create experience');
    } finally {
      setLoading(false);
    }
  };

  // ── Render ─────────────────────────────────────────
  const stepIdx = STEPS.findIndex((s) => s.id === step);
  const isLast = stepIdx === STEPS.length - 1;

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      <KeyboardAvoidingView
        behavior={Platform.OS === 'ios' ? 'padding' : 'height'}
        className="flex-1"
      >
        <ScrollView className="flex-1" showsVerticalScrollIndicator={false} keyboardShouldPersistTaps="handled">
          {/* Header */}
          <LinearGradient
            colors={['#122F26', '#1d392f']}
            className="px-4 pb-6"
            style={{ paddingTop: insets.top + 12 }}
          >
            <View className="flex-row items-center justify-between">
              <View className="flex-row items-center flex-1">
                <TouchableOpacity
                  onPress={handleBack}
                  className="w-10 h-10 rounded-xl items-center justify-center mr-3"
                  style={{ backgroundColor: 'rgba(255,255,255,0.15)' }}
                >
                  <Ionicons name="arrow-back" size={24} color="#fff" />
                </TouchableOpacity>
                <View>
                  <Text className="text-xl font-bold text-white">New Experience</Text>
                  <Text className="text-xs text-sand-300 mt-1">
                    Step {stepIdx + 1} of {STEPS.length}
                  </Text>
                </View>
              </View>
            </View>

            {/* Progress bar */}
            <View className="flex-row mt-4 gap-2">
              {STEPS.map((s, i) => (
                <View
                  key={s.id}
                  className="flex-1 h-1 rounded-full"
                  style={{ backgroundColor: i <= stepIdx ? '#D9B168' : 'rgba(255,255,255,0.3)' }}
                />
              ))}
            </View>
          </LinearGradient>

          <View className="px-4 py-6">
            {/* Step label */}
            <View className="flex-row items-center mb-6">
              <View className="w-12 h-12 rounded-2xl items-center justify-center mr-3" style={{ backgroundColor: '#D9B168' }}>
                <Ionicons name={STEPS[stepIdx].icon} size={24} color="#122F26" />
              </View>
              <View>
                <Text className="text-xl font-bold text-forest">{STEPS[stepIdx].label}</Text>
                <Text className="text-sm text-moss">{STEPS[stepIdx].desc}</Text>
              </View>
            </View>

            {/* ─── STEP: Basic ─────────────────────── */}
            {step === 'basic' && (
              <>
                <InputField
                  label="Experience Title"
                  value={form.title}
                  onChangeText={(t) => update('title', t)}
                  placeholder="e.g. Sunset Safari Tour"
                  required
                />

                {/* Category selector */}
                <InputField
                  label="Category"
                  value={form.categoryName}
                  onChangeText={() => {}}
                  placeholder="Select a category"
                  type="select"
                  onPress={() => setShowCategoryPicker(true)}
                />

                <InputField
                  label="Description"
                  value={form.description}
                  onChangeText={(t) => update('description', t)}
                  placeholder="Describe what guests will experience..."
                  required
                  multiline
                />

                {/* Duration type chips */}
                <View className="mb-4">
                  <Text className="text-base font-semibold text-forest mb-2">Duration Type</Text>
                  <View className="flex-row flex-wrap gap-2">
                    {(Object.keys(DURATION_LABELS) as ExperienceDuration[]).map((d) => (
                      <TouchableOpacity
                        key={d}
                        onPress={() => update('duration_type', d)}
                        className={`px-4 py-3 rounded-xl ${form.duration_type === d ? 'bg-gold' : 'bg-white'}`}
                        style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 4, elevation: 2 }}
                      >
                        <Text className={`text-sm font-semibold ${form.duration_type === d ? 'text-forest' : 'text-moss'}`}>
                          {DURATION_LABELS[d]}
                        </Text>
                      </TouchableOpacity>
                    ))}
                  </View>
                </View>

                {/* Difficulty chips */}
                <View className="mb-4">
                  <Text className="text-base font-semibold text-forest mb-2">Difficulty</Text>
                  <View className="flex-row gap-2">
                    {DIFFICULTY_META.map((d) => (
                      <TouchableOpacity
                        key={d.value}
                        onPress={() => update('difficulty', d.value)}
                        className="flex-1 py-3 rounded-xl items-center"
                        style={{
                          backgroundColor: form.difficulty === d.value ? d.color : '#fff',
                          shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 4, elevation: 2,
                        }}
                      >
                        <Text className={`text-sm font-semibold ${form.difficulty === d.value ? 'text-white' : 'text-moss'}`}>
                          {d.label}
                        </Text>
                      </TouchableOpacity>
                    ))}
                  </View>
                </View>
              </>
            )}

            {/* ─── STEP: Location ──────────────────── */}
            {step === 'location' && (
              <>
                <InputField
                  label="Country"
                  value={form.country}
                  onChangeText={() => {}}
                  placeholder="Select Country"
                  required
                  type="select"
                  onPress={() => setShowCountryPicker(true)}
                />

                <InputField
                  label="City"
                  value={form.city}
                  onChangeText={(t) => update('city', t)}
                  placeholder="e.g. Victoria Falls"
                  required
                />

                <InputField
                  label="Address"
                  value={form.address}
                  onChangeText={(t) => update('address', t)}
                  placeholder="Street address (optional)"
                />

                {/* Geocode / GPS buttons */}
                <View className="mb-4">
                  <Text className="text-base font-semibold text-forest mb-2">
                    Pin Location <Text className="text-red-500">*</Text>
                  </Text>
                  <View className="flex-row gap-3 mb-3">
                    <TouchableOpacity
                      onPress={handleGeocode}
                      className="flex-1 bg-white rounded-2xl p-4 flex-row items-center justify-center"
                      style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 4, elevation: 2 }}
                    >
                      <Ionicons name="search" size={18} color="#4a5f4b" />
                      <Text className="text-moss ml-2 font-medium">Find Coords</Text>
                    </TouchableOpacity>
                    <TouchableOpacity
                      onPress={getCurrentLocation}
                      className="flex-1 bg-white rounded-2xl p-4 flex-row items-center justify-center"
                      style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 4, elevation: 2 }}
                    >
                      <Ionicons name="locate" size={18} color="#4a5f4b" />
                      <Text className="text-moss ml-2 font-medium">Use GPS</Text>
                    </TouchableOpacity>
                  </View>

                  {form.latitude !== null && form.longitude !== null && (
                    <View
                      className="bg-white rounded-2xl p-4 flex-row items-center"
                      style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 4, elevation: 2 }}
                    >
                      <Ionicons name="checkmark-circle" size={24} color="#10B981" />
                      <View className="ml-3">
                        <Text className="text-forest font-semibold">Location Set</Text>
                        <Text className="text-moss text-xs">
                          {form.latitude.toFixed(5)}, {form.longitude.toFixed(5)}
                        </Text>
                      </View>
                    </View>
                  )}
                </View>
              </>
            )}

            {/* ─── STEP: Details ───────────────────── */}
            {step === 'details' && (
              <>
                {/* Currency chips */}
                <View className="mb-4">
                  <Text className="text-base font-semibold text-forest mb-2">Currency <Text className="text-red-500">*</Text></Text>
                  <View className="flex-row flex-wrap gap-2">
                    {CURRENCIES.map((c) => (
                      <TouchableOpacity
                        key={c}
                        onPress={() => update('currency', c)}
                        className={`px-4 py-3 rounded-xl ${form.currency === c ? 'bg-gold' : 'bg-white'}`}
                        style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 4, elevation: 2 }}
                      >
                        <Text className={`text-sm font-semibold ${form.currency === c ? 'text-forest' : 'text-moss'}`}>{c}</Text>
                      </TouchableOpacity>
                    ))}
                  </View>
                </View>

                <View className="flex-row gap-3">
                  <View className="flex-1">
                    <InputField
                      label="Price per Person"
                      value={form.price_per_person}
                      onChangeText={(t) => update('price_per_person', t)}
                      placeholder="0.00"
                      keyboardType="decimal-pad"
                      required
                    />
                  </View>
                  <View className="flex-1">
                    <InputField
                      label="Duration (hours)"
                      value={form.duration_hours}
                      onChangeText={(t) => update('duration_hours', t)}
                      placeholder="3"
                      keyboardType="decimal-pad"
                      required
                    />
                  </View>
                </View>

                <View className="flex-row gap-3">
                  <View className="flex-1">
                    <InputField
                      label="Min Participants"
                      value={form.min_participants}
                      onChangeText={(t) => update('min_participants', t)}
                      placeholder="1"
                      keyboardType="number-pad"
                    />
                  </View>
                  <View className="flex-1">
                    <InputField
                      label="Max Participants"
                      value={form.max_participants}
                      onChangeText={(t) => update('max_participants', t)}
                      placeholder="10"
                      keyboardType="number-pad"
                    />
                  </View>
                </View>

                <InputField
                  label="What's Included"
                  value={form.included_items}
                  onChangeText={(t) => update('included_items', t)}
                  placeholder="Transport, lunch, equipment..."
                  multiline
                />

                <InputField
                  label="Requirements"
                  value={form.requirements}
                  onChangeText={(t) => update('requirements', t)}
                  placeholder="Min age 12, comfortable shoes..."
                  multiline
                />

                <InputField
                  label="Cancellation Policy"
                  value={form.cancellation_policy}
                  onChangeText={(t) => update('cancellation_policy', t)}
                  placeholder="Free cancellation up to 24h before..."
                  multiline
                />
              </>
            )}

            {/* ─── STEP: Images ────────────────────── */}
            {step === 'images' && (
              <>
                <View className="mb-4">
                  <Text className="text-base font-semibold text-forest mb-2">Cover Image</Text>
                  <Text className="text-sm text-moss mb-3">
                    Add a cover photo for your experience
                  </Text>

                  {coverImage ? (
                    <View className="relative rounded-2xl overflow-hidden">
                      <Image source={{ uri: coverImage }} className="w-full h-52 rounded-2xl" resizeMode="cover" />
                      <TouchableOpacity
                        onPress={() => setCoverImage(null)}
                        className="absolute top-3 right-3 w-8 h-8 rounded-full bg-black/50 items-center justify-center"
                      >
                        <Ionicons name="close" size={18} color="#fff" />
                      </TouchableOpacity>
                      <TouchableOpacity
                        onPress={pickCoverImage}
                        className="absolute bottom-3 right-3 px-4 py-2 rounded-xl bg-black/50 flex-row items-center"
                      >
                        <Ionicons name="camera" size={16} color="#fff" />
                        <Text className="text-white text-xs ml-1.5 font-medium">Change</Text>
                      </TouchableOpacity>
                    </View>
                  ) : (
                    <TouchableOpacity
                      onPress={pickCoverImage}
                      className="bg-white rounded-2xl p-8 items-center border-2 border-dashed border-gold"
                      style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 4, elevation: 2 }}
                    >
                      <Ionicons name="cloud-upload-outline" size={48} color="#D9B168" />
                      <Text className="text-forest font-semibold mt-2">Upload Cover Image</Text>
                      <Text className="text-moss text-sm">Tap to select a photo</Text>
                    </TouchableOpacity>
                  )}
                </View>

                {/* Summary */}
                <View className="bg-sand-200 rounded-2xl p-4 mt-2">
                  <Text className="text-sm font-semibold text-forest mb-3">Summary</Text>
                  {[
                    { l: 'Experience', v: form.title || 'N/A' },
                    { l: 'Category', v: form.categoryName || 'None' },
                    { l: 'Location', v: `${form.city || 'N/A'}, ${form.country || 'N/A'}` },
                    { l: 'Duration', v: `${form.duration_hours || '0'} hrs (${DURATION_LABELS[form.duration_type]})` },
                    { l: 'Difficulty', v: form.difficulty.charAt(0).toUpperCase() + form.difficulty.slice(1) },
                    { l: 'Capacity', v: `${form.min_participants} – ${form.max_participants} people` },
                    { l: 'Cover Image', v: coverImage ? '1 photo' : 'None' },
                  ].map((row) => (
                    <View key={row.l} className="flex-row justify-between py-1">
                      <Text className="text-moss">{row.l}</Text>
                      <Text className="text-forest font-semibold max-w-[60%] text-right" numberOfLines={1}>
                        {row.v}
                      </Text>
                    </View>
                  ))}
                  <View className="h-px bg-moss opacity-20 my-2" />
                  <View className="flex-row justify-between">
                    <Text className="text-forest font-bold">Price per Person</Text>
                    <Text className="text-forest font-bold text-lg">
                      {form.currency} {form.price_per_person || '0.00'}
                    </Text>
                  </View>
                </View>
              </>
            )}

            {/* ─── Navigation ──────────────────────── */}
            <View className="flex-row gap-3 mt-6 mb-4">
              {stepIdx > 0 && (
                <TouchableOpacity
                  onPress={handleBack}
                  className="flex-1 py-4 rounded-2xl items-center border-2 border-gold"
                >
                  <Text className="text-forest font-bold text-base">Back</Text>
                </TouchableOpacity>
              )}
              <TouchableOpacity
                onPress={isLast ? handleCreate : handleNext}
                disabled={loading || createMutation.isPending}
                className="flex-1"
              >
                <LinearGradient
                  colors={loading || createMutation.isPending ? ['#94a3b8', '#94a3b8'] : ['#D9B168', '#bea04f']}
                  className="py-4 rounded-2xl items-center"
                >
                  <Text className="text-forest font-bold text-base">
                    {loading || createMutation.isPending
                      ? 'Creating...'
                      : isLast
                        ? 'Create Experience'
                        : 'Next'}
                  </Text>
                </LinearGradient>
              </TouchableOpacity>
            </View>
          </View>
        </ScrollView>

        {/* ─── Country Picker Modal ──────────────── */}
        <Modal visible={showCountryPicker} animationType="slide" transparent onRequestClose={() => setShowCountryPicker(false)}>
          <View className="flex-1 bg-black/50 justify-end">
            <View className="bg-white rounded-t-3xl h-[70%]">
              <View className="p-4 border-b border-gray-100 flex-row justify-between items-center">
                <Text className="text-lg font-bold text-forest">Select Country</Text>
                <TouchableOpacity onPress={() => setShowCountryPicker(false)}>
                  <Ionicons name="close" size={24} color="#122F26" />
                </TouchableOpacity>
              </View>
              <FlatList
                data={COUNTRIES}
                keyExtractor={(item) => item}
                renderItem={({ item }) => (
                  <TouchableOpacity
                    className="p-4 border-b border-gray-50"
                    onPress={() => { update('country', item); setShowCountryPicker(false); }}
                  >
                    <Text className={`text-base ${form.country === item ? 'text-gold font-bold' : 'text-forest'}`}>
                      {item}
                    </Text>
                  </TouchableOpacity>
                )}
              />
            </View>
          </View>
        </Modal>

        {/* ─── Category Picker Modal ─────────────── */}
        <Modal visible={showCategoryPicker} animationType="slide" transparent onRequestClose={() => setShowCategoryPicker(false)}>
          <View className="flex-1 bg-black/50 justify-end">
            <View className="bg-white rounded-t-3xl h-[60%]">
              <View className="p-4 border-b border-gray-100 flex-row justify-between items-center">
                <Text className="text-lg font-bold text-forest">Select Category</Text>
                <TouchableOpacity onPress={() => setShowCategoryPicker(false)}>
                  <Ionicons name="close" size={24} color="#122F26" />
                </TouchableOpacity>
              </View>
              {categories.length === 0 ? (
                <View className="flex-1 items-center justify-center px-8">
                  <Ionicons name="folder-open-outline" size={40} color="#94a3b8" />
                  <Text className="text-moss mt-2 text-center">No categories available</Text>
                </View>
              ) : (
                <FlatList
                  data={categories}
                  keyExtractor={(item) => String(item.id)}
                  renderItem={({ item }) => (
                    <TouchableOpacity
                      className="p-4 border-b border-gray-50"
                      onPress={() => {
                        update('category', String(item.id));
                        update('categoryName', item.name);
                        setShowCategoryPicker(false);
                      }}
                    >
                      <Text className={`text-base ${form.category === String(item.id) ? 'text-gold font-bold' : 'text-forest'}`}>
                        {item.name}
                      </Text>
                      {item.description ? (
                        <Text className="text-xs text-moss mt-0.5">{item.description}</Text>
                      ) : null}
                    </TouchableOpacity>
                  )}
                />
              )}
            </View>
          </View>
        </Modal>
      </KeyboardAvoidingView>
    </SafeAreaView>
  );
}
