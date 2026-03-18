import { useState } from 'react';
import {
  View,
  Text,
  Modal,
  Pressable,
  TouchableOpacity,
  ScrollView,
  TextInput,
} from 'react-native';
import { Ionicons } from '@expo/vector-icons';
import Animated, { SlideInDown } from 'react-native-reanimated';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';

export interface FilterSheetState {
  priceMin?: number;
  priceMax?: number;
  minRating?: number;
  guests?: number;
  amenities: string[];
  checkIn?: string;
  checkOut?: string;
}

interface FilterSheetProps {
  visible: boolean;
  onClose: () => void;
  filters: FilterSheetState;
  onFiltersChange: (filters: FilterSheetState) => void;
}

const PRICE_PRESETS = [
  { min: 0, max: 50, label: 'Under $50' },
  { min: 50, max: 100, label: '$50 - $100' },
  { min: 100, max: 200, label: '$100 - $200' },
  { min: 200, max: Infinity, label: '$200+' },
];

const RATING_OPTIONS = [
  { value: 1, label: '1+ stars' },
  { value: 2, label: '2+ stars' },
  { value: 3, label: '3+ stars' },
  { value: 4, label: '4+ stars' },
  { value: 5, label: '5 stars' },
];

const GUEST_OPTIONS = [
  { value: 1, label: '1 guest' },
  { value: 2, label: '2 guests' },
  { value: 3, label: '3 guests' },
  { value: 4, label: '4 guests' },
  { value: 6, label: '6+ guests' },
  { value: 8, label: '8+ guests' },
];

export function FilterSheet({ visible, onClose, filters, onFiltersChange }: FilterSheetProps) {
  const [localFilters, setLocalFilters] = useState<FilterSheetState>(filters);

  // Fetch amenities for the amenities list
  const { data: amenitiesData } = useQuery({
    queryKey: ['amenities'],
    queryFn: async () => {
      try {
        return await apiClient.getAmenities?.() || [];
      } catch {
        return [];
      }
    },
    enabled: visible,
  });

  const amenities = amenitiesData || [];

  const handlePricePreset = (preset: (typeof PRICE_PRESETS)[0]) => {
    const newFilters = { ...localFilters };
    newFilters.priceMin = preset.min === 0 ? undefined : preset.min;
    newFilters.priceMax = preset.max === Infinity ? undefined : preset.max;
    setLocalFilters(newFilters);
  };

  const handleAmenityToggle = (amenityId: string) => {
    setLocalFilters((prev) => ({
      ...prev,
      amenities: prev.amenities.includes(amenityId)
        ? prev.amenities.filter((id) => id !== amenityId)
        : [...prev.amenities, amenityId],
    }));
  };

  const handleApply = () => {
    onFiltersChange(localFilters);
    onClose();
  };

  const handleReset = () => {
    const resetFilters: FilterSheetState = { amenities: [] };
    setLocalFilters(resetFilters);
    onFiltersChange(resetFilters);
  };

  return (
    <Modal
      visible={visible}
      transparent
      animationType="none"
      statusBarTranslucent
      onRequestClose={onClose}
    >
      <Pressable
        className="flex-1 justify-end"
        style={{ backgroundColor: 'rgba(0,0,0,0.4)' }}
        onPress={onClose}
      >
        <Pressable onPress={(e) => e.stopPropagation()}>
          <Animated.View
            entering={SlideInDown.springify().damping(18)}
            className="bg-white rounded-t-3xl px-5 pb-8 max-h-[90%]"
            style={{ paddingTop: 16 }}
          >
            {/* Handle */}
            <View className="items-center mb-4">
              <View className="w-10 h-1 rounded-full bg-sand-300" />
            </View>

            <ScrollView showsVerticalScrollIndicator={false}>
              <View className="flex-row items-center justify-between mb-5">
                <Text className="text-lg font-bold text-forest">Filters</Text>
                <TouchableOpacity onPress={handleReset}>
                  <Text className="text-sm font-semibold text-secondary-600">Reset</Text>
                </TouchableOpacity>
              </View>

              {/* Price Range */}
              <View className="mb-6">
                <Text className="text-base font-bold text-forest mb-3">Price Range</Text>
                <View className="space-y-2">
                  {PRICE_PRESETS.map((preset, idx) => {
                    const isActive =
                      (localFilters.priceMin || 0) === (preset.min || 0) &&
                      (localFilters.priceMax || Infinity) === (preset.max || Infinity);
                    return (
                      <TouchableOpacity
                        key={idx}
                        className={`py-3 px-4 rounded-2xl flex-row items-center justify-between ${
                          isActive ? 'bg-primary-800' : 'bg-sand-50'
                        }`}
                        onPress={() => handlePricePreset(preset)}
                      >
                        <Text className={`font-semibold ${isActive ? 'text-gold' : 'text-forest'}`}>
                          {preset.label}
                        </Text>
                        {isActive && (
                          <Ionicons name="checkmark" size={20} color="#D9B168" />
                        )}
                      </TouchableOpacity>
                    );
                  })}
                </View>
              </View>

              {/* Rating */}
              <View className="mb-6">
                <Text className="text-base font-bold text-forest mb-3">Minimum Rating</Text>
                <View className="space-y-2">
                  {RATING_OPTIONS.map((opt) => {
                    const isActive = localFilters.minRating === opt.value;
                    return (
                      <TouchableOpacity
                        key={opt.value}
                        className={`py-3 px-4 rounded-2xl flex-row items-center justify-between ${
                          isActive ? 'bg-primary-800' : 'bg-sand-50'
                        }`}
                        onPress={() =>
                          setLocalFilters({
                            ...localFilters,
                            minRating: isActive ? undefined : opt.value,
                          })
                        }
                      >
                        <View className="flex-row items-center gap-2">
                          <Ionicons
                            name="star"
                            size={16}
                            color={isActive ? '#D9B168' : '#3A5C50'}
                          />
                          <Text className={`font-semibold ${isActive ? 'text-gold' : 'text-forest'}`}>
                            {opt.label}
                          </Text>
                        </View>
                        {isActive && (
                          <Ionicons name="checkmark" size={20} color="#D9B168" />
                        )}
                      </TouchableOpacity>
                    );
                  })}
                </View>
              </View>

              {/* Guests */}
              <View className="mb-6">
                <Text className="text-base font-bold text-forest mb-3">Number of Guests</Text>
                <View className="space-y-2">
                  {GUEST_OPTIONS.map((opt) => {
                    const isActive = localFilters.guests === opt.value;
                    return (
                      <TouchableOpacity
                        key={opt.value}
                        className={`py-3 px-4 rounded-2xl flex-row items-center justify-between ${
                          isActive ? 'bg-primary-800' : 'bg-sand-50'
                        }`}
                        onPress={() =>
                          setLocalFilters({
                            ...localFilters,
                            guests: isActive ? undefined : opt.value,
                          })
                        }
                      >
                        <View className="flex-row items-center gap-2">
                          <Ionicons
                            name="people"
                            size={16}
                            color={isActive ? '#D9B168' : '#3A5C50'}
                          />
                          <Text className={`font-semibold ${isActive ? 'text-gold' : 'text-forest'}`}>
                            {opt.label}
                          </Text>
                        </View>
                        {isActive && (
                          <Ionicons name="checkmark" size={20} color="#D9B168" />
                        )}
                      </TouchableOpacity>
                    );
                  })}
                </View>
              </View>

              {/* Amenities */}
              {amenities.length > 0 && (
                <View className="mb-6">
                  <Text className="text-base font-bold text-forest mb-3">Amenities</Text>
                  <View className="flex-row flex-wrap gap-2">
                    {amenities.map((amenity: any) => {
                      const isSelected = localFilters.amenities.includes(amenity.id);
                      return (
                        <TouchableOpacity
                          key={amenity.id}
                          className={`px-4 py-2 rounded-full border-2 ${
                            isSelected
                              ? 'border-secondary-600 bg-secondary-100'
                              : 'border-sand-200 bg-sand-50'
                          }`}
                          onPress={() => handleAmenityToggle(amenity.id)}
                        >
                          <Text
                            className={`text-sm font-semibold ${
                              isSelected ? 'text-secondary-700' : 'text-forest'
                            }`}
                          >
                            {amenity.name}
                          </Text>
                        </TouchableOpacity>
                      );
                    })}
                  </View>
                </View>
              )}

              {/* Date Pickers */}
              <View className="mb-6">
                <Text className="text-base font-bold text-forest mb-3">Dates</Text>
                <View className="space-y-3">
                  <View className="bg-sand-50 rounded-2xl px-4 py-3">
                    <Text className="text-xs text-moss mb-1 font-semibold">Check-in (YYYY-MM-DD)</Text>
                    <TextInput
                      placeholder="YYYY-MM-DD"
                      placeholderTextColor="rgba(58, 92, 80, 0.4)"
                      value={localFilters.checkIn || ''}
                      onChangeText={(text) =>
                        setLocalFilters({ ...localFilters, checkIn: text || undefined })
                      }
                      className="text-forest text-base font-semibold"
                    />
                  </View>
                  <View className="bg-sand-50 rounded-2xl px-4 py-3">
                    <Text className="text-xs text-moss mb-1 font-semibold">Check-out (YYYY-MM-DD)</Text>
                    <TextInput
                      placeholder="YYYY-MM-DD"
                      placeholderTextColor="rgba(58, 92, 80, 0.4)"
                      value={localFilters.checkOut || ''}
                      onChangeText={(text) =>
                        setLocalFilters({ ...localFilters, checkOut: text || undefined })
                      }
                      className="text-forest text-base font-semibold"
                    />
                  </View>
                </View>
              </View>

              {/* Apply Button */}
              <TouchableOpacity
                className="bg-primary-800 rounded-2xl py-4 items-center mb-4"
                onPress={handleApply}
              >
                <Text className="text-white font-bold text-base">Apply Filters</Text>
              </TouchableOpacity>
            </ScrollView>
          </Animated.View>
        </Pressable>
      </Pressable>
    </Modal>
  );
}
