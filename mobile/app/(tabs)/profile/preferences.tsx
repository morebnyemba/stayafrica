import { View, Text, ScrollView, TouchableOpacity, TextInput, Alert, ActivityIndicator } from 'react-native';
import { useState, useEffect } from 'react';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { useSafeAreaInsets } from 'react-native-safe-area-context';
import { useUserPreferences, useUpdateUserPreferences } from '@/hooks/api-hooks';

const PROPERTY_TYPES = ['apartment', 'house', 'villa', 'cottage', 'lodge', 'guesthouse', 'hotel', 'hostel'];
const COMMON_AMENITIES = ['wifi', 'pool', 'parking', 'kitchen', 'ac', 'tv', 'washer', 'gym', 'security', 'garden', 'hot_water', 'electricity'];

export default function PreferencesScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const { data: prefs, isLoading } = useUserPreferences();
  const { mutate: updatePreferences, isPending: saving } = useUpdateUserPreferences();

  const [form, setForm] = useState({
    preferred_property_types: [] as string[],
    preferred_min_price: '',
    preferred_max_price: '',
    preferred_cities: [] as string[],
    usual_guest_count: '',
    preferred_amenities: [] as string[],
  });
  const [cityInput, setCityInput] = useState('');

  useEffect(() => {
    if (prefs) {
      setForm({
        preferred_property_types: prefs.preferred_property_types || [],
        preferred_min_price: prefs.preferred_min_price?.toString() || '',
        preferred_max_price: prefs.preferred_max_price?.toString() || '',
        preferred_cities: prefs.preferred_cities || [],
        usual_guest_count: prefs.usual_guest_count?.toString() || '',
        preferred_amenities: prefs.preferred_amenities || [],
      });
    }
  }, [prefs]);

  const togglePropertyType = (type: string) => {
    setForm(f => ({
      ...f,
      preferred_property_types: f.preferred_property_types.includes(type)
        ? f.preferred_property_types.filter(t => t !== type)
        : [...f.preferred_property_types, type],
    }));
  };

  const toggleAmenity = (amenity: string) => {
    setForm(f => ({
      ...f,
      preferred_amenities: f.preferred_amenities.includes(amenity)
        ? f.preferred_amenities.filter(a => a !== amenity)
        : [...f.preferred_amenities, amenity],
    }));
  };

  const addCity = () => {
    const city = cityInput.trim();
    if (city && !form.preferred_cities.includes(city)) {
      setForm(f => ({ ...f, preferred_cities: [...f.preferred_cities, city] }));
      setCityInput('');
    }
  };

  const removeCity = (city: string) => {
    setForm(f => ({ ...f, preferred_cities: f.preferred_cities.filter(c => c !== city) }));
  };

  const handleSave = () => {
    updatePreferences({
      preferred_property_types: form.preferred_property_types,
      preferred_min_price: form.preferred_min_price ? parseFloat(form.preferred_min_price) : null,
      preferred_max_price: form.preferred_max_price ? parseFloat(form.preferred_max_price) : null,
      preferred_cities: form.preferred_cities,
      usual_guest_count: form.usual_guest_count ? parseInt(form.usual_guest_count) : null,
      preferred_amenities: form.preferred_amenities,
    }, {
      onSuccess: () => Alert.alert('Saved', 'Your travel preferences have been updated.'),
      onError: () => Alert.alert('Error', 'Failed to save preferences.'),
    });
  };

  if (isLoading) {
    return (
      <View className="flex-1 bg-sand-100 items-center justify-center">
        <ActivityIndicator size="large" color="#D9B168" />
      </View>
    );
  }

  return (
    <View className="flex-1 bg-sand-100">
      {/* Header */}
      <View
        className="bg-white px-4 py-3 border-b border-sand-200 flex-row items-center"
        style={{ paddingTop: insets.top + 8 }}
      >
        <TouchableOpacity onPress={() => router.back()}>
          <Ionicons name="arrow-back" size={24} color="#122F26" />
        </TouchableOpacity>
        <Text className="text-lg font-semibold text-forest ml-3">Travel Preferences</Text>
      </View>

      <ScrollView className="flex-1 p-4" showsVerticalScrollIndicator={false} contentContainerStyle={{ paddingBottom: 32 }}>
        {/* Property Types */}
        <View className="mb-6">
          <Text className="text-base font-bold text-forest mb-3">Preferred Property Types</Text>
          <View className="flex-row flex-wrap">
            {PROPERTY_TYPES.map(type => (
              <TouchableOpacity
                key={type}
                onPress={() => togglePropertyType(type)}
                className={`px-4 py-2 rounded-xl mr-2 mb-2 ${
                  form.preferred_property_types.includes(type)
                    ? 'bg-forest'
                    : 'bg-white border border-sand-300'
                }`}
              >
                <Text className={`text-sm font-semibold capitalize ${
                  form.preferred_property_types.includes(type) ? 'text-white' : 'text-forest'
                }`}>
                  {type}
                </Text>
              </TouchableOpacity>
            ))}
          </View>
        </View>

        {/* Price Range */}
        <View className="mb-6">
          <Text className="text-base font-bold text-forest mb-3">Price Range (per night)</Text>
          <View className="flex-row gap-3">
            <View className="flex-1">
              <TextInput
                value={form.preferred_min_price}
                onChangeText={v => setForm(f => ({ ...f, preferred_min_price: v }))}
                placeholder="Min"
                keyboardType="numeric"
                className="bg-white border border-sand-300 rounded-xl px-4 py-3 text-forest"
              />
            </View>
            <Text className="self-center text-moss">—</Text>
            <View className="flex-1">
              <TextInput
                value={form.preferred_max_price}
                onChangeText={v => setForm(f => ({ ...f, preferred_max_price: v }))}
                placeholder="Max"
                keyboardType="numeric"
                className="bg-white border border-sand-300 rounded-xl px-4 py-3 text-forest"
              />
            </View>
          </View>
        </View>

        {/* Preferred Cities */}
        <View className="mb-6">
          <Text className="text-base font-bold text-forest mb-3">Preferred Cities</Text>
          <View className="flex-row mb-2">
            <TextInput
              value={cityInput}
              onChangeText={setCityInput}
              placeholder="Add a city"
              onSubmitEditing={addCity}
              className="flex-1 bg-white border border-sand-300 rounded-xl px-4 py-3 text-forest mr-2"
            />
            <TouchableOpacity
              onPress={addCity}
              className="bg-forest px-4 rounded-xl items-center justify-center"
            >
              <Ionicons name="add" size={24} color="#fff" />
            </TouchableOpacity>
          </View>
          <View className="flex-row flex-wrap">
            {form.preferred_cities.map(city => (
              <View key={city} className="flex-row items-center bg-gold/20 px-3 py-2 rounded-xl mr-2 mb-2">
                <Text className="text-sm text-forest font-semibold">{city}</Text>
                <TouchableOpacity onPress={() => removeCity(city)} className="ml-2">
                  <Ionicons name="close-circle" size={18} color="#122F26" />
                </TouchableOpacity>
              </View>
            ))}
          </View>
        </View>

        {/* Guest Count */}
        <View className="mb-6">
          <Text className="text-base font-bold text-forest mb-3">Usual Guest Count</Text>
          <TextInput
            value={form.usual_guest_count}
            onChangeText={v => setForm(f => ({ ...f, usual_guest_count: v }))}
            placeholder="Number of guests"
            keyboardType="numeric"
            className="bg-white border border-sand-300 rounded-xl px-4 py-3 text-forest"
          />
        </View>

        {/* Preferred Amenities */}
        <View className="mb-6">
          <Text className="text-base font-bold text-forest mb-3">Preferred Amenities</Text>
          <View className="flex-row flex-wrap">
            {COMMON_AMENITIES.map(amenity => (
              <TouchableOpacity
                key={amenity}
                onPress={() => toggleAmenity(amenity)}
                className={`px-4 py-2 rounded-xl mr-2 mb-2 ${
                  form.preferred_amenities.includes(amenity)
                    ? 'bg-forest'
                    : 'bg-white border border-sand-300'
                }`}
              >
                <Text className={`text-sm font-semibold capitalize ${
                  form.preferred_amenities.includes(amenity) ? 'text-white' : 'text-forest'
                }`}>
                  {amenity.replace(/_/g, ' ')}
                </Text>
              </TouchableOpacity>
            ))}
          </View>
        </View>

        {/* Save Button */}
        <TouchableOpacity
          onPress={handleSave}
          disabled={saving}
          className="bg-forest py-4 rounded-2xl items-center"
        >
          {saving ? (
            <ActivityIndicator color="#D9B168" />
          ) : (
            <Text className="text-gold font-bold text-base">Save Preferences</Text>
          )}
        </TouchableOpacity>
      </ScrollView>
    </View>
  );
}
