import { View, Text, ScrollView, TouchableOpacity, Platform, TextInput, Alert } from 'react-native';
import { useState } from 'react';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';

export default function NewPropertyScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const [loading, setLoading] = useState(false);
  const [formData, setFormData] = useState({
    title: '',
    description: '',
    address: '',
    city: '',
    country: '',
    price: '',
    bedrooms: '',
    bathrooms: '',
    maxGuests: '',
  });

  const handleChange = (field: string, value: string) => {
    setFormData(prev => ({ ...prev, [field]: value }));
  };

  const handleCreate = async () => {
    if (!formData.title || !formData.price || !formData.city) {
      Alert.alert('Error', 'Please fill in all required fields');
      return;
    }

    setLoading(true);
    try {
      // TODO: Implement property creation API call
      await new Promise(resolve => setTimeout(resolve, 1500));
      Alert.alert('Success', 'Property created successfully!', [
        { text: 'OK', onPress: () => router.replace('/host/properties') }
      ]);
    } catch (error) {
      Alert.alert('Error', 'Failed to create property');
    } finally {
      setLoading(false);
    }
  };

  if (!isAuthenticated) {
    return (
      <View className="flex-1 bg-sand-100">
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
        >
          <View className="flex-row items-center">
            <TouchableOpacity
              onPress={() => router.back()}
              className="w-10 h-10 rounded-xl items-center justify-center mr-3"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="arrow-back" size={24} color="#fff" />
            </TouchableOpacity>
            <Text className="text-xl font-bold text-white">Add Property</Text>
          </View>
        </LinearGradient>

        <View className="flex-1 items-center justify-center px-6">
          <Text className="text-moss text-center">Please sign in to add a property</Text>
          <TouchableOpacity 
            onPress={() => router.push('/(auth)/login')}
            className="mt-4"
          >
            <LinearGradient
              colors={['#D9B168', '#bea04f']}
              className="px-8 py-4 rounded-2xl"
            >
              <Text className="text-forest font-bold">Sign In</Text>
            </LinearGradient>
          </TouchableOpacity>
        </View>
      </View>
    );
  }

  const InputField = ({ label, field, placeholder, keyboardType = 'default', required = false }: any) => (
    <View className="mb-4">
      <Text className="text-base font-semibold text-forest mb-2">
        {label} {required && <Text className="text-red-500">*</Text>}
      </Text>
      <View className="bg-white rounded-2xl p-4" style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 2 },
        shadowOpacity: 0.05,
        shadowRadius: 4,
        elevation: 2,
      }}>
        <TextInput
          className="text-base text-forest"
          placeholder={placeholder}
          placeholderTextColor="#94a3b8"
          keyboardType={keyboardType}
          value={formData[field as keyof typeof formData]}
          onChangeText={(value) => handleChange(field, value)}
        />
      </View>
    </View>
  );

  return (
    <ScrollView className="flex-1 bg-sand-100" showsVerticalScrollIndicator={false}>
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f']}
        className="px-4 pb-6"
        style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
      >
        <View className="flex-row items-center">
          <TouchableOpacity
            onPress={() => router.back()}
            className="w-10 h-10 rounded-xl items-center justify-center mr-3"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </TouchableOpacity>
          <Text className="text-xl font-bold text-white">Add New Property</Text>
        </View>
      </LinearGradient>

      <View className="px-4 py-6">
        {/* Basic Info */}
        <Text className="text-lg font-bold text-forest mb-4">Basic Information</Text>
        
        <InputField 
          label="Property Title" 
          field="title" 
          placeholder="e.g., Cozy Safari Lodge"
          required
        />
        
        <View className="mb-4">
          <Text className="text-base font-semibold text-forest mb-2">Description</Text>
          <View className="bg-white rounded-2xl p-4" style={{
            shadowColor: '#122F26',
            shadowOffset: { width: 0, height: 2 },
            shadowOpacity: 0.05,
            shadowRadius: 4,
            elevation: 2,
          }}>
            <TextInput
              className="text-base text-forest"
              placeholder="Describe your property..."
              placeholderTextColor="#94a3b8"
              multiline
              numberOfLines={4}
              textAlignVertical="top"
              value={formData.description}
              onChangeText={(value) => handleChange('description', value)}
            />
          </View>
        </View>

        {/* Location */}
        <Text className="text-lg font-bold text-forest mb-4 mt-4">Location</Text>
        
        <InputField 
          label="Address" 
          field="address" 
          placeholder="Street address"
        />
        
        <InputField 
          label="City" 
          field="city" 
          placeholder="City"
          required
        />
        
        <InputField 
          label="Country" 
          field="country" 
          placeholder="Country"
        />

        {/* Details */}
        <Text className="text-lg font-bold text-forest mb-4 mt-4">Property Details</Text>
        
        <InputField 
          label="Price per Night ($)" 
          field="price" 
          placeholder="0.00"
          keyboardType="decimal-pad"
          required
        />
        
        <View className="flex-row gap-3">
          <View className="flex-1">
            <InputField 
              label="Bedrooms" 
              field="bedrooms" 
              placeholder="0"
              keyboardType="number-pad"
            />
          </View>
          <View className="flex-1">
            <InputField 
              label="Bathrooms" 
              field="bathrooms" 
              placeholder="0"
              keyboardType="number-pad"
            />
          </View>
        </View>
        
        <InputField 
          label="Max Guests" 
          field="maxGuests" 
          placeholder="0"
          keyboardType="number-pad"
        />

        {/* Submit Button */}
        <TouchableOpacity
          onPress={handleCreate}
          disabled={loading}
          className="mt-6"
        >
          <LinearGradient
            colors={loading ? ['#94a3b8', '#94a3b8'] : ['#D9B168', '#bea04f']}
            className="py-4 rounded-2xl items-center"
          >
            <Text className="text-forest font-bold text-base">
              {loading ? 'Creating...' : 'Create Property'}
            </Text>
          </LinearGradient>
        </TouchableOpacity>
      </View>
    </ScrollView>
  );
}
