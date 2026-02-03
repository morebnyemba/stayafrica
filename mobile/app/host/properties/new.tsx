import { View, Text, ScrollView, TouchableOpacity, Platform, TextInput, Alert, KeyboardTypeOptions, Modal } from 'react-native';
import { useState } from 'react';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';

interface InputFieldProps {
  label: string;
  field: keyof FormData;
  placeholder: string;
  keyboardType?: KeyboardTypeOptions;
  required?: boolean;
}

interface FormData {
  title: string;
  description: string;
  propertyType: string;
  address: string;
  city: string;
  suburb: string;
  country: string;
  latitude: string;
  longitude: string;
  price: string;
  currency: string;
  bedrooms: string;
  bathrooms: string;
  maxGuests: string;
}

export default function NewPropertyScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const [loading, setLoading] = useState(false);
  const [showPropertyTypeModal, setShowPropertyTypeModal] = useState(false);
  const [showCountryModal, setShowCountryModal] = useState(false);
  const [showCurrencyModal, setShowCurrencyModal] = useState(false);
  
  const [formData, setFormData] = useState<FormData>({
    title: '',
    description: '',
    propertyType: 'house',
    address: '',
    city: '',
    suburb: '',
    country: '',
    latitude: '',
    longitude: '',
    price: '',
    currency: 'USD',
    bedrooms: '',
    bathrooms: '',
    maxGuests: '',
  });

  const propertyTypes = [
    { label: 'House', value: 'house' },
    { label: 'Apartment', value: 'apartment' },
    { label: 'Villa', value: 'villa' },
    { label: 'Cottage', value: 'cottage' },
    { label: 'Lodge', value: 'lodge' },
    { label: 'Room', value: 'room' },
  ];

  const countries = [
    { label: 'Select Country', value: '' },
    { label: 'Zimbabwe', value: 'Zimbabwe' },
    { label: 'South Africa', value: 'South Africa' },
    { label: 'Botswana', value: 'Botswana' },
    { label: 'Namibia', value: 'Namibia' },
    { label: 'Zambia', value: 'Zambia' },
    { label: 'Kenya', value: 'Kenya' },
    { label: 'Tanzania', value: 'Tanzania' },
  ];

  const currencies = [
    { label: 'USD ($)', value: 'USD' },
    { label: 'ZWL (Z$)', value: 'ZWL' },
    { label: 'ZAR (R)', value: 'ZAR' },
    { label: 'BWP (P)', value: 'BWP' },
  ];

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

  const InputField = ({ label, field, placeholder, keyboardType = 'default', required = false }: InputFieldProps) => (
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
          value={formData[field]}
          onChangeText={(value) => handleChange(field, value)}
        />
      </View>
    </View>
  );

  const SelectField = ({ 
    label, 
    value, 
    onPress, 
    required = false 
  }: { 
    label: string; 
    value: string; 
    onPress: () => void; 
    required?: boolean;
  }) => (
    <View className="mb-4">
      <Text className="text-base font-semibold text-forest mb-2">
        {label} {required && <Text className="text-red-500">*</Text>}
      </Text>
      <TouchableOpacity
        onPress={onPress}
        className="bg-white rounded-2xl p-4 flex-row items-center justify-between"
        style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 2 },
          shadowOpacity: 0.05,
          shadowRadius: 4,
          elevation: 2,
        }}
      >
        <Text className="text-base text-forest">{value}</Text>
        <Ionicons name="chevron-down" size={20} color="#3A5C50" />
      </TouchableOpacity>
    </View>
  );

  const SelectModal = ({
    visible,
    onClose,
    title,
    options,
    selectedValue,
    onSelect,
  }: {
    visible: boolean;
    onClose: () => void;
    title: string;
    options: { label: string; value: string }[];
    selectedValue: string;
    onSelect: (value: string) => void;
  }) => (
    <Modal
      visible={visible}
      transparent
      animationType="slide"
      onRequestClose={onClose}
    >
      <View className="flex-1 bg-black/50 justify-end">
        <View className="bg-white rounded-t-3xl" style={{ maxHeight: '70%' }}>
          <View className="flex-row items-center justify-between p-4 border-b border-sand-200">
            <Text className="text-lg font-bold text-forest">{title}</Text>
            <TouchableOpacity onPress={onClose}>
              <Ionicons name="close" size={24} color="#3A5C50" />
            </TouchableOpacity>
          </View>
          <ScrollView>
            {options.map((option) => (
              <TouchableOpacity
                key={option.value}
                onPress={() => {
                  onSelect(option.value);
                  onClose();
                }}
                className={`p-4 border-b border-sand-100 ${
                  selectedValue === option.value ? 'bg-sand-100' : ''
                }`}
              >
                <View className="flex-row items-center justify-between">
                  <Text className={`text-base ${
                    selectedValue === option.value ? 'font-bold text-forest' : 'text-moss'
                  }`}>
                    {option.label}
                  </Text>
                  {selectedValue === option.value && (
                    <Ionicons name="checkmark-circle" size={24} color="#D9B168" />
                  )}
                </View>
              </TouchableOpacity>
            ))}
          </ScrollView>
        </View>
      </View>
    </Modal>
  );

  return (
    <ScrollView 
      className="flex-1 bg-sand-100" 
      showsVerticalScrollIndicator={false}
      contentContainerStyle={{ paddingBottom: 40 }}
      keyboardShouldPersistTaps="handled"
    >
      {/* Modals */}
      <SelectModal
        visible={showPropertyTypeModal}
        onClose={() => setShowPropertyTypeModal(false)}
        title="Select Property Type"
        options={propertyTypes}
        selectedValue={formData.propertyType}
        onSelect={(value) => handleChange('propertyType', value)}
      />
      <SelectModal
        visible={showCountryModal}
        onClose={() => setShowCountryModal(false)}
        title="Select Country"
        options={countries}
        selectedValue={formData.country}
        onSelect={(value) => handleChange('country', value)}
      />
      <SelectModal
        visible={showCurrencyModal}
        onClose={() => setShowCurrencyModal(false)}
        title="Select Currency"
        options={currencies}
        selectedValue={formData.currency}
        onSelect={(value) => handleChange('currency', value)}
      />

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

        <SelectField
          label="Property Type"
          value={propertyTypes.find(pt => pt.value === formData.propertyType)?.label || 'House'}
          onPress={() => setShowPropertyTypeModal(true)}
          required
        />

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
          label="Suburb" 
          field="suburb" 
          placeholder="Suburb (optional)"
        />
        
        <SelectField
          label="Country"
          value={countries.find(c => c.value === formData.country)?.label || 'Select Country'}
          onPress={() => setShowCountryModal(true)}
        />

        <View className="flex-row gap-3 mb-4">
          <View className="flex-1">
            <InputField 
              label="Latitude" 
              field="latitude" 
              placeholder="-17.8252"
              keyboardType="decimal-pad"
            />
          </View>
          <View className="flex-1">
            <InputField 
              label="Longitude" 
              field="longitude" 
              placeholder="31.0335"
              keyboardType="decimal-pad"
            />
          </View>
        </View>

        <View className="bg-blue-50 rounded-2xl p-3 mb-4">
          <View className="flex-row items-start">
            <Ionicons name="information-circle" size={20} color="#3B82F6" />
            <Text className="text-xs text-blue-700 ml-2 flex-1">
              You can get coordinates by dropping a pin on Google Maps or using a GPS app
            </Text>
          </View>
        </View>

        {/* Details */}
        <Text className="text-lg font-bold text-forest mb-4 mt-4">Property Details</Text>
        
        <View className="flex-row gap-3">
          <View className="flex-1">
            <InputField 
              label="Price/Night" 
              field="price" 
              placeholder="0.00"
              keyboardType="decimal-pad"
              required
            />
          </View>
          <View className="flex-1">
            <SelectField
              label="Currency"
              value={currencies.find(c => c.value === formData.currency)?.label || 'USD ($)'}
              onPress={() => setShowCurrencyModal(true)}
              required
            />
          </View>
        </View>
        
        <View className="flex-row gap-3">
          <View className="flex-1">
            <InputField 
              label="Bedrooms" 
              field="bedrooms" 
              placeholder="1"
              keyboardType="number-pad"
            />
          </View>
          <View className="flex-1">
            <InputField 
              label="Bathrooms" 
              field="bathrooms" 
              placeholder="1"
              keyboardType="number-pad"
            />
          </View>
        </View>
        
        <InputField 
          label="Max Guests" 
          field="maxGuests" 
          placeholder="2"
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
