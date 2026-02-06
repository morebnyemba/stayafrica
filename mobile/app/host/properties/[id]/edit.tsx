import { View, Text, ScrollView, TouchableOpacity, TextInput, Platform, ActivityIndicator, Alert, KeyboardAvoidingView, TouchableWithoutFeedback, Keyboard } from 'react-native';
import { useState, useEffect } from 'react';
import { useRouter, useLocalSearchParams } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';

export default function PropertyEditScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const insets = useSafeAreaInsets();
  const params = useLocalSearchParams();
  const queryClient = useQueryClient();
  
  const propertyId = params.id as string;

  const [formData, setFormData] = useState({
    title: '',
    description: '',
    price_per_night: '',
    bedrooms: '',
    bathrooms: '',
    max_guests: '',
    cleaning_fee: '',
  });

  // Fetch property details
  const { data: property, isLoading } = useQuery({
    queryKey: ['property', propertyId],
    queryFn: async () => {
      const response = await apiClient.get(`/properties/${propertyId}/`);
      return response.data;
    },
    enabled: !!propertyId,
  });

  // Initialize form data when property loads
  useEffect(() => {
    if (property) {
      setFormData({
        title: property.title || '',
        description: property.description || '',
        price_per_night: property.price_per_night?.toString() || '',
        bedrooms: property.bedrooms?.toString() || '',
        bathrooms: property.bathrooms?.toString() || '',
        max_guests: property.max_guests?.toString() || '',
        cleaning_fee: property.cleaning_fee?.toString() || '',
      });
    }
  }, [property]);

  // Update mutation
  const updateMutation = useMutation({
    mutationFn: async (data: any) => {
      return await apiClient.patch(`/properties/${propertyId}/`, data);
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['property', propertyId] });
      queryClient.invalidateQueries({ queryKey: ['host', 'properties'] });
      Alert.alert('Success', 'Property updated successfully');
      router.back();
    },
    onError: (error: any) => {
      Alert.alert('Error', error.response?.data?.message || 'Failed to update property');
    },
  });

  const handleSave = () => {
    const updateData = {
      title: formData.title,
      description: formData.description,
      price_per_night: parseFloat(formData.price_per_night) || 0,
      bedrooms: parseInt(formData.bedrooms) || 0,
      bathrooms: parseInt(formData.bathrooms) || 0,
      max_guests: parseInt(formData.max_guests) || 1,
      cleaning_fee: parseFloat(formData.cleaning_fee) || 0,
    };

    if (!updateData.title || updateData.price_per_night <= 0) {
      Alert.alert('Error', 'Please fill in all required fields');
      return;
    }

    updateMutation.mutate(updateData);
  };

  if (!isAuthenticated) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100 items-center justify-center">
        <Text className="text-forest text-lg">Please sign in to edit properties</Text>
      </SafeAreaView>
    );
  }

  if (isLoading) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100 items-center justify-center">
        <ActivityIndicator size="large" color="#D9B168" />
        <Text className="mt-4 text-moss">Loading property...</Text>
      </SafeAreaView>
    );
  }

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      <KeyboardAvoidingView 
        behavior={Platform.OS === 'ios' ? 'padding' : 'height'}
        className="flex-1"
      >
        <TouchableWithoutFeedback onPress={Keyboard.dismiss}>
          <ScrollView 
            className="flex-1 bg-sand-100" 
            showsVerticalScrollIndicator={false}
            contentContainerStyle={{ paddingBottom: 40 }}
            keyboardShouldPersistTaps="handled"
          >
      <LinearGradient
        colors={['#122F26', '#1d392f']}
        className="px-4 pb-6"
        style={{ paddingTop: insets.top + 12 }}
      >
        <TouchableOpacity onPress={() => router.back()} className="mb-4">
          <View className="w-10 h-10 rounded-xl items-center justify-center" style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}>
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </View>
        </TouchableOpacity>
        <Text className="text-3xl font-black text-white tracking-tight">
          Edit Property
        </Text>
        <Text className="text-sand-200 text-sm mt-1">
          Update your property details
        </Text>
      </LinearGradient>

      <View className="px-4 -mt-4">
        <View className="bg-white rounded-2xl p-5 mb-4" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 4 },
          shadowOpacity: 0.08,
          shadowRadius: 8,
          elevation: 4,
        }}>
          {/* Title */}
          <View className="mb-4">
            <Text className="text-sm font-semibold text-forest mb-2">Property Title *</Text>
            <TextInput
              value={formData.title}
              onChangeText={(text) => setFormData({ ...formData, title: text })}
              placeholder="Enter property title"
              className="bg-sand-100 rounded-xl px-4 py-3 text-forest"
              placeholderTextColor="#94a3b8"
            />
          </View>

          {/* Description */}
          <View className="mb-4">
            <Text className="text-sm font-semibold text-forest mb-2">Description</Text>
            <TextInput
              value={formData.description}
              onChangeText={(text) => setFormData({ ...formData, description: text })}
              placeholder="Describe your property"
              className="bg-sand-100 rounded-xl px-4 py-3 text-forest"
              placeholderTextColor="#94a3b8"
              multiline
              numberOfLines={4}
              textAlignVertical="top"
            />
          </View>

          {/* Price Per Night */}
          <View className="mb-4">
            <Text className="text-sm font-semibold text-forest mb-2">Price Per Night ($) *</Text>
            <TextInput
              value={formData.price_per_night}
              onChangeText={(text) => setFormData({ ...formData, price_per_night: text })}
              placeholder="0.00"
              keyboardType="decimal-pad"
              className="bg-sand-100 rounded-xl px-4 py-3 text-forest"
              placeholderTextColor="#94a3b8"
            />
          </View>

          {/* Cleaning Fee */}
          <View className="mb-4">
            <Text className="text-sm font-semibold text-forest mb-2">Cleaning Fee ($)</Text>
            <TextInput
              value={formData.cleaning_fee}
              onChangeText={(text) => setFormData({ ...formData, cleaning_fee: text })}
              placeholder="0.00"
              keyboardType="decimal-pad"
              className="bg-sand-100 rounded-xl px-4 py-3 text-forest"
              placeholderTextColor="#94a3b8"
            />
          </View>

          {/* Bedrooms */}
          <View className="mb-4">
            <Text className="text-sm font-semibold text-forest mb-2">Bedrooms</Text>
            <TextInput
              value={formData.bedrooms}
              onChangeText={(text) => setFormData({ ...formData, bedrooms: text })}
              placeholder="0"
              keyboardType="number-pad"
              className="bg-sand-100 rounded-xl px-4 py-3 text-forest"
              placeholderTextColor="#94a3b8"
            />
          </View>

          {/* Bathrooms */}
          <View className="mb-4">
            <Text className="text-sm font-semibold text-forest mb-2">Bathrooms</Text>
            <TextInput
              value={formData.bathrooms}
              onChangeText={(text) => setFormData({ ...formData, bathrooms: text })}
              placeholder="0"
              keyboardType="number-pad"
              className="bg-sand-100 rounded-xl px-4 py-3 text-forest"
              placeholderTextColor="#94a3b8"
            />
          </View>

          {/* Max Guests */}
          <View className="mb-4">
            <Text className="text-sm font-semibold text-forest mb-2">Max Guests</Text>
            <TextInput
              value={formData.max_guests}
              onChangeText={(text) => setFormData({ ...formData, max_guests: text })}
              placeholder="1"
              keyboardType="number-pad"
              className="bg-sand-100 rounded-xl px-4 py-3 text-forest"
              placeholderTextColor="#94a3b8"
            />
          </View>
        </View>

        {/* Save Button */}
        <TouchableOpacity
          onPress={handleSave}
          disabled={updateMutation.isPending}
          className={`rounded-2xl overflow-hidden ${updateMutation.isPending ? 'opacity-50' : ''}`}
        >
          <LinearGradient
            colors={['#D9B168', '#bea04f']}
            className="py-4 items-center"
            style={{
              shadowColor: '#D9B168',
              shadowOffset: { width: 0, height: 4 },
              shadowOpacity: 0.3,
              shadowRadius: 8,
              elevation: 5,
            }}
          >
            {updateMutation.isPending ? (
              <ActivityIndicator color="#122F26" />
            ) : (
              <View className="flex-row items-center">
                <Ionicons name="save" size={20} color="#122F26" />
                <Text className="text-forest font-bold text-base ml-2">Save Changes</Text>
              </View>
            )}
          </LinearGradient>
        </TouchableOpacity>
      </View>
        </ScrollView>
      </TouchableWithoutFeedback>
    </KeyboardAvoidingView>
    </SafeAreaView>
  );
}

PropertyEditScreen.displayName = 'PropertyEditScreen';
