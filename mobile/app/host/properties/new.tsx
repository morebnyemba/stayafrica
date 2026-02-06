import { View, Text, ScrollView, TouchableOpacity, Platform, TextInput, Alert, KeyboardTypeOptions, KeyboardAvoidingView, TouchableWithoutFeedback, Keyboard, Image, Modal, FlatList } from 'react-native';
import { useState } from 'react';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { apiClient } from '@/services/api-client';
import * as ImagePicker from 'expo-image-picker';
import * as Location from 'expo-location';
import MapView, { Marker, PROVIDER_GOOGLE } from 'react-native-maps';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';

interface InputFieldProps {
  label: string;
  field: keyof FormData;
  placeholder: string;
  keyboardType?: KeyboardTypeOptions;
  required?: boolean;
  multiline?: boolean;
  value: string;
  onChangeText: (text: string) => void;
  type?: 'text' | 'select';
  onPress?: () => void;
}

interface FormData {
  title: string;
  description: string;
  address: string;
  city: string;
  suburb: string;
  country: string;
  price: string;
  bedrooms: string;
  bathrooms: string;
  maxGuests: string;
  latitude: number | null;
  longitude: number | null;
  propertyType: string;
  currency: string;
}

type FormStep = 'basic' | 'location' | 'images' | 'pricing';

interface StepConfig {
  id: FormStep;
  label: string;
  description: string;
  icon: keyof typeof Ionicons.glyphMap;
}

const STEPS: StepConfig[] = [
  { id: 'basic', label: 'Basic Info', description: 'Property details', icon: 'home-outline' },
  { id: 'location', label: 'Location', description: 'Pin your property', icon: 'location-outline' },
  { id: 'images', label: 'Images', description: 'Add photos', icon: 'images-outline' },
  { id: 'pricing', label: 'Pricing', description: 'Set rates', icon: 'pricetag-outline' },
];

const COUNTRIES = [
  'Zimbabwe', 'South Africa', 'Botswana', 'Namibia', 'Zambia', 'Mozambique', 
  'Malawi', 'Lesotho', 'Eswatini', 'Tanzania', 'Kenya', 'Uganda', 'Rwanda'
];

const BASIC_FIELDS = [
  { label: 'Property Title', field: 'title', placeholder: 'e.g., Cozy Safari Lodge', required: true },
  { label: 'Description', field: 'description', placeholder: 'Describe your property...', required: true, multiline: true },
  { label: 'Max Guests', field: 'maxGuests', placeholder: '0', keyboardType: 'number-pad' as KeyboardTypeOptions },
];

const LOCATION_TEXT_FIELDS = [
  { label: 'Address', field: 'address', placeholder: 'Street address', required: true },
  { label: 'Suburb', field: 'suburb', placeholder: 'Suburb (Optional)' },
  { label: 'City', field: 'city', placeholder: 'City', required: true },
];

const InputField = ({ 
  label, 
  value, 
  onChangeText, 
  placeholder, 
  keyboardType = 'default', 
  required = false, 
  multiline = false,
  type = 'text',
  onPress
}: InputFieldProps) => (
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
        <Text className={`text-base ${value ? 'text-forest' : 'text-[#94a3b8]'}`}>{value || placeholder}</Text>
        <Ionicons name="chevron-down" size={20} color="#3A5C50" />
      </TouchableOpacity>
    ) : (
      <View className="bg-white rounded-2xl p-4" style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 4, elevation: 2 }}>
        <TextInput
          className="text-base text-forest"
          placeholder={placeholder}
          placeholderTextColor="#94a3b8"
          keyboardType={keyboardType}
          multiline={multiline}
          numberOfLines={multiline ? 4 : 1}
          textAlignVertical={multiline ? 'top' : 'center'}
          value={value}
          onChangeText={onChangeText}
        />
      </View>
    )}
  </View>
);

export default function NewPropertyScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const insets = useSafeAreaInsets();
  const [loading, setLoading] = useState(false);
  const [currentStep, setCurrentStep] = useState<FormStep>('basic');
  const [images, setImages] = useState<string[]>([]);
  const [showCountryPicker, setShowCountryPicker] = useState(false);
  const [formData, setFormData] = useState<FormData>({
    title: '',
    description: '',
    address: '',
    city: '',
    suburb: '',
    country: '',
    price: '',
    bedrooms: '1',
    bathrooms: '1',
    maxGuests: '2',
    latitude: null,
    longitude: null,
    propertyType: 'house',
    currency: 'USD',
  });

  const handleChange = (field: keyof FormData, value: string) => {
    setFormData(prev => ({ ...prev, [field]: value }));
  };

  const pickImages = async () => {
    const { status } = await ImagePicker.requestMediaLibraryPermissionsAsync();
    if (status !== 'granted') {
      Alert.alert('Permission Denied', 'Camera roll permission is required');
      return;
    }

    const result = await ImagePicker.launchImageLibraryAsync({
      mediaTypes: ImagePicker.MediaTypeOptions.Images,
      allowsMultipleSelection: true,
      quality: 0.8,
      selectionLimit: 10 - images.length,
    });

    if (!result.canceled && result.assets) {
      setImages(prev => [...prev, ...result.assets.map(asset => asset.uri)]);
    }
  };

  const removeImage = (index: number) => {
    setImages(prev => prev.filter((_, i) => i !== index));
  };

  const getCurrentLocation = async () => {
    const { status } = await Location.requestForegroundPermissionsAsync();
    if (status !== 'granted') {
      Alert.alert('Permission Denied', 'Location permission is required');
      return;
    }

    const location = await Location.getCurrentPositionAsync({});
    setFormData(prev => ({
      ...prev,
      latitude: location.coords.latitude,
      longitude: location.coords.longitude,
    }));
  };

  const handleGeocode = async () => {
    if (!formData.address || !formData.city || !formData.country) {
      Alert.alert('Missing Info', 'Please enter address, city and country first');
      return;
    }

    try {
      setLoading(true);
      const address = `${formData.address}, ${formData.suburb ? formData.suburb + ', ' : ''}${formData.city}, ${formData.country}`;
      const result = await Location.geocodeAsync(address);
      
      if (result.length > 0) {
        const { latitude, longitude } = result[0];
        setFormData(prev => ({ ...prev, latitude, longitude }));
        Alert.alert('Location Found', 'Coordinates updated successfully!');
      } else {
        Alert.alert('Not Found', 'Could not find coordinates for this address');
      }
    } catch (error) {
      Alert.alert('Error', 'Failed to find location coordinates');
    } finally {
      setLoading(false);
    }
  };

  const validateStep = (): boolean => {
    switch (currentStep) {
      case 'basic':
        if (!formData.title || !formData.description || !formData.propertyType) {
          Alert.alert('Error', 'Please fill in all required fields');
          return false;
        }
        break;
      case 'location':
        if (!formData.address || !formData.city || !formData.latitude || !formData.longitude) {
          Alert.alert('Error', 'Please provide complete location information');
          return false;
        }
        break;
      case 'images':
        if (images.length === 0) {
          Alert.alert('Error', 'Please add at least one image');
          return false;
        }
        break;
      case 'pricing':
        if (!formData.price || parseFloat(formData.price) <= 0) {
          Alert.alert('Error', 'Please set a valid price');
          return false;
        }
        break;
    }
    return true;
  };

  const handleNext = () => {
    if (!validateStep()) return;

    const stepIndex = STEPS.findIndex(s => s.id === currentStep);
    if (stepIndex < STEPS.length - 1) {
      setCurrentStep(STEPS[stepIndex + 1].id);
    }
  };

  const handleBack = () => {
    const stepIndex = STEPS.findIndex(s => s.id === currentStep);
    if (stepIndex > 0) {
      setCurrentStep(STEPS[stepIndex - 1].id);
    }
  };

  const handleCreate = async () => {
    if (!formData.title || !formData.price || !formData.city) {
      Alert.alert('Error', 'Please fill in all required fields');
      return;
    }

    setLoading(true);
    try {
      const payload = new FormData();
      payload.append('title', formData.title);
      payload.append('description', formData.description);
      payload.append('address', formData.address);
      payload.append('city', formData.city);
      payload.append('suburb', formData.suburb);
      payload.append('country', formData.country);
      payload.append('price_per_night', formData.price);
      payload.append('bedrooms', formData.bedrooms);
      payload.append('bathrooms', formData.bathrooms);
      payload.append('max_guests', formData.maxGuests);
      payload.append('property_type', formData.propertyType);
      payload.append('currency', formData.currency);
      payload.append('location', JSON.stringify({
        type: 'Point',
        coordinates: [formData.longitude, formData.latitude]
      }));

      // Add images
      images.forEach((uri, index) => {
        payload.append('images', {
          uri,
          type: 'image/jpeg',
          name: `property-${index}.jpg`,
        } as any);
      });

      await apiClient.post('/properties/', payload, {
        headers: {
          'Content-Type': 'multipart/form-data',
        },
      });

      Alert.alert('Success', 'Property created successfully!', [
        { text: 'OK', onPress: () => router.replace('/host/properties') }
      ]);
    } catch (error) {
      console.error('Create error:', error);
      Alert.alert('Error', 'Failed to create property');
    } finally {
      setLoading(false);
    }
  };

  if (!isAuthenticated) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100">
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
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
      </SafeAreaView>
    );
  }

  const currentStepIndex = STEPS.findIndex(s => s.id === currentStep);
  const isLastStep = currentStepIndex === STEPS.length - 1;

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
          keyboardShouldPersistTaps="handled"
        >
        {/* Header */}
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: insets.top + 12 }}
        >
          <View className="flex-row items-center justify-between">
            <View className="flex-row items-center flex-1">
              <TouchableOpacity
                onPress={() => currentStepIndex > 0 ? handleBack() : router.back()}
                className="w-10 h-10 rounded-xl items-center justify-center mr-3"
                style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
              >
                <Ionicons name="arrow-back" size={24} color="#fff" />
              </TouchableOpacity>
              <View>
                <Text className="text-xl font-bold text-white">Add New Property</Text>
                <Text className="text-xs text-sand-300 mt-1">
                  Step {currentStepIndex + 1} of {STEPS.length}
                </Text>
              </View>
            </View>
          </View>

          {/* Step Indicator */}
          <View className="flex-row mt-4 gap-2">
            {STEPS.map((step, index) => (
              <View 
                key={step.id}
                className="flex-1 h-1 rounded-full"
                style={{
                  backgroundColor: index <= currentStepIndex ? '#D9B168' : 'rgba(255, 255, 255, 0.3)',
                }}
              />
            ))}
          </View>
        </LinearGradient>

      <View className="px-4 py-6">
        {/* Step Label */}
        <View className="flex-row items-center mb-6">
          <View 
            className="w-12 h-12 rounded-2xl items-center justify-center mr-3"
            style={{ backgroundColor: '#D9B168' }}
          >
            <Ionicons name={STEPS[currentStepIndex].icon} size={24} color="#122F26" />
          </View>
          <View>
            <Text className="text-xl font-bold text-forest">{STEPS[currentStepIndex].label}</Text>
            <Text className="text-sm text-moss">{STEPS[currentStepIndex].description}</Text>
          </View>
        </View>

        {/* Basic Info Step */}
        {currentStep === 'basic' && (
          <>
            <InputField
              label="Property Title"
              value={formData.title}
              onChangeText={(text) => handleChange('title', text)}
              placeholder="e.g., Cozy Safari Lodge"
              required
              field="title"
            />
            
            <View className="mb-4">
              <Text className="text-base font-semibold text-forest mb-2">
                Property Type <Text className="text-red-500">*</Text>
              </Text>
              <View className="flex-row gap-2 mb-3">
                {['house', 'apartment', 'villa', 'guesthouse'].map((type) => (
                  <TouchableOpacity
                    key={type}
                    onPress={() => handleChange('propertyType', type)}
                    className={`flex-1 py-3 rounded-xl ${
                      formData.propertyType === type ? 'bg-gold' : 'bg-white'
                    }`}
                    style={{
                      shadowColor: '#122F26',
                      shadowOffset: { width: 0, height: 2 },
                      shadowOpacity: 0.05,
                      shadowRadius: 4,
                      elevation: 2,
                    }}
                  >
                    <Text 
                      className={`text-center text-sm font-semibold ${
                        formData.propertyType === type ? 'text-forest' : 'text-moss'
                      }`}
                    >
                      {type.charAt(0).toUpperCase() + type.slice(1)}
                    </Text>
                  </TouchableOpacity>
                ))}
              </View>
            </View>

            <InputField
              label="Description"
              value={formData.description}
              onChangeText={(text) => handleChange('description', text)}
              placeholder="Describe your property..."
              required
              multiline
              field="description"
            />

            <View className="flex-row gap-3">
              <View className="flex-1">
                <InputField 
                  label="Bedrooms" 
                  value={formData.bedrooms}
                  onChangeText={(text) => handleChange('bedrooms', text)}
                  placeholder="0"
                  keyboardType="number-pad"
                  field="bedrooms"
                />
              </View>
              <View className="flex-1">
                <InputField 
                  label="Bathrooms" 
                  value={formData.bathrooms}
                  onChangeText={(text) => handleChange('bathrooms', text)}
                  placeholder="0"
                  keyboardType="number-pad"
                  field="bathrooms"
                />
              </View>
            </View>

            <InputField 
              label="Max Guests" 
              value={formData.maxGuests}
              onChangeText={(text) => handleChange('maxGuests', text)}
              placeholder="0"
              keyboardType="number-pad"
              field="maxGuests"
            />
          </>
        )}

        {/* Location Step */}
        {currentStep === 'location' && (
          <>
            {LOCATION_TEXT_FIELDS.map((field) => (
              <InputField
                key={field.field}
                label={field.label}
                value={formData[field.field as keyof FormData] as string}
                onChangeText={(text) => handleChange(field.field as keyof FormData, text)}
                placeholder={field.placeholder}
                required={field.required}
                field={field.field as keyof FormData}
              />
            ))}
            
            <InputField 
              label="Country" 
              value={formData.country}
              onChangeText={() => {}}
              placeholder="Select Country"
              required
              type="select"
              onPress={() => setShowCountryPicker(true)}
              field="country"
            />

            <View className="mb-4">
              <Text className="text-base font-semibold text-forest mb-2">
                Pin Location <Text className="text-red-500">*</Text>
              </Text>
              
              <View className="flex-row gap-3 mb-3">
                <TouchableOpacity
                  onPress={handleGeocode}
                  className="flex-1 bg-white rounded-2xl p-4 flex-row items-center justify-center"
                  style={{
                    shadowColor: '#122F26',
                    shadowOffset: { width: 0, height: 2 },
                    shadowOpacity: 0.05,
                    shadowRadius: 4,
                    elevation: 2,
                  }}
                >
                  <Ionicons name="search" size={18} color="#4a5f4b" />
                  <Text className="text-moss ml-2 font-medium">Find Coords</Text>
                </TouchableOpacity>

                <TouchableOpacity
                  onPress={getCurrentLocation}
                  className="flex-1 bg-white rounded-2xl p-4 flex-row items-center justify-center"
                  style={{
                    shadowColor: '#122F26',
                    shadowOffset: { width: 0, height: 2 },
                    shadowOpacity: 0.05,
                    shadowRadius: 4,
                    elevation: 2,
                  }}
                >
                  <Ionicons name="locate" size={18} color="#4a5f4b" />
                  <Text className="text-moss ml-2 font-medium">Use GPS</Text>
                </TouchableOpacity>
              </View>

              {formData.latitude !== null && formData.longitude !== null && (
                <View 
                  className="rounded-2xl overflow-hidden bg-gray-200 mt-2"
                  style={{
                    height: 300,
                    shadowColor: '#122F26',
                    shadowOffset: { width: 0, height: 2 },
                    shadowOpacity: 0.1,
                    shadowRadius: 4,
                    elevation: 3,
                  }}
                >
                  <MapView
                    provider={PROVIDER_GOOGLE}
                    style={{ flex: 1 }}
                    region={{
                      latitude: formData.latitude!,
                      longitude: formData.longitude!,
                      latitudeDelta: 0.01,
                      longitudeDelta: 0.01,
                    }}
                  >
                    <Marker
                      coordinate={{
                        latitude: formData.latitude!,
                        longitude: formData.longitude!,
                      }}
                      title="Property Location"
                      pinColor="#D9B168"
                    />
                  </MapView>
                  <Text className="absolute bottom-2 left-2 right-2 bg-white px-3 py-2 rounded-xl text-xs text-moss text-center">
                    Location set successfully
                  </Text>
                </View>
              )}
            </View>
          </>
        )}

        {/* Images Step */}
        {currentStep === 'images' && (
          <>
            <View className="mb-4">
              <Text className="text-base font-semibold text-forest mb-2">
                Property Images <Text className="text-red-500">*</Text>
              </Text>
              <Text className="text-sm text-moss mb-3">
                Add at least one image (max 10)
              </Text>

              <TouchableOpacity
                onPress={pickImages}
                className="bg-white rounded-2xl p-6 items-center border-2 border-dashed border-gold"
                style={{
                  shadowColor: '#122F26',
                  shadowOffset: { width: 0, height: 2 },
                  shadowOpacity: 0.05,
                  shadowRadius: 4,
                  elevation: 2,
                }}
              >
                <Ionicons name="cloud-upload-outline" size={48} color="#D9B168" />
                <Text className="text-forest font-semibold mt-2">Upload Images</Text>
                <Text className="text-moss text-sm">Tap to select photos</Text>
              </TouchableOpacity>
            </View>

            {images.length > 0 && (
              <View className="flex-row flex-wrap gap-2">
                {images.map((uri, index) => (
                  <View key={index} className="relative">
                    <Image 
                      source={{ uri }} 
                      className="w-24 h-24 rounded-xl"
                    />
                    <TouchableOpacity
                      onPress={() => removeImage(index)}
                      className="absolute -top-2 -right-2 w-6 h-6 rounded-full bg-red-500 items-center justify-center"
                      style={{
                        shadowColor: '#000',
                        shadowOffset: { width: 0, height: 2 },
                        shadowOpacity: 0.2,
                        shadowRadius: 3,
                        elevation: 3,
                      }}
                    >
                      <Ionicons name="close" size={16} color="#fff" />
                    </TouchableOpacity>
                  </View>
                ))}
              </View>
            )}
          </>
        )}

        {/* Pricing Step */}
        {currentStep === 'pricing' && (
          <>
            <View className="mb-4">
              <Text className="text-base font-semibold text-forest mb-2">
                Currency <Text className="text-red-500">*</Text>
              </Text>
              <View className="flex-row gap-2 mb-3">
                {['USD', 'ZAR', 'ZWL'].map((curr) => (
                  <TouchableOpacity
                    key={curr}
                    onPress={() => handleChange('currency', curr)}
                    className={`flex-1 py-3 rounded-xl ${
                      formData.currency === curr ? 'bg-gold' : 'bg-white'
                    }`}
                    style={{
                      shadowColor: '#122F26',
                      shadowOffset: { width: 0, height: 2 },
                      shadowOpacity: 0.05,
                      shadowRadius: 4,
                      elevation: 2,
                    }}
                  >
                    <Text 
                      className={`text-center text-sm font-semibold ${
                        formData.currency === curr ? 'text-forest' : 'text-moss'
                      }`}
                    >
                      {curr}
                    </Text>
                  </TouchableOpacity>
                ))}
              </View>
            </View>

            <InputField 
              label="Price per Night" 
              value={formData.price}
              onChangeText={(text) => handleChange('price', text)}
              placeholder="0.00"
              keyboardType="decimal-pad"
              required
              field="price"
            />

            <View className="bg-sand-200 rounded-2xl p-4 mt-6">
              <Text className="text-sm font-semibold text-forest mb-3">Summary</Text>
              <View className="space-y-2">
                <View className="flex-row justify-between">
                  <Text className="text-moss">Property</Text>
                  <Text className="text-forest font-semibold">{formData.title || 'N/A'}</Text>
                </View>
                <View className="flex-row justify-between">
                  <Text className="text-moss">Type</Text>
                  <Text className="text-forest font-semibold capitalize">{formData.propertyType}</Text>
                </View>
                <View className="flex-row justify-between">
                  <Text className="text-moss">Address</Text>
                  <Text className="text-forest font-semibold max-w-[60%] text-right" numberOfLines={1}>{formData.address}</Text>
                </View>
                <View className="flex-row justify-between">
                  <Text className="text-moss">Location</Text>
                  <Text className="text-forest font-semibold">{formData.city || 'N/A'}</Text>
                </View>
                <View className="flex-row justify-between">
                  <Text className="text-moss">Capacity</Text>
                  <Text className="text-forest font-semibold">
                    {formData.bedrooms} bed • {formData.bathrooms} bath • {formData.maxGuests} guests
                  </Text>
                </View>
                <View className="flex-row justify-between">
                  <Text className="text-moss">Images</Text>
                  <Text className="text-forest font-semibold">{images.length} photos</Text>
                </View>
                <View className="h-px bg-moss opacity-20 my-2" />
                <View className="flex-row justify-between">
                  <Text className="text-forest font-bold">Price per Night</Text>
                  <Text className="text-forest font-bold text-lg">
                    {formData.currency} {formData.price || '0.00'}
                  </Text>
                </View>
              </View>
            </View>
          </>
        )}

        {/* Navigation Buttons */}
        <View className="flex-row gap-3 mt-6">
          {currentStepIndex > 0 && (
            <TouchableOpacity
              onPress={handleBack}
              className="flex-1 py-4 rounded-2xl items-center border-2 border-gold"
            >
              <Text className="text-forest font-bold text-base">Back</Text>
            </TouchableOpacity>
          )}

          <TouchableOpacity
            onPress={isLastStep ? handleCreate : handleNext}
            disabled={loading}
            className="flex-1"
          >
            <LinearGradient
              colors={loading ? ['#94a3b8', '#94a3b8'] : ['#D9B168', '#bea04f']}
              className="py-4 rounded-2xl items-center"
            >
              <Text className="text-forest font-bold text-base">
                {loading ? 'Creating...' : isLastStep ? 'Create Property' : 'Next'}
              </Text>
            </LinearGradient>
          </TouchableOpacity>
        </View>
      </View>

      {/* Country Picker Modal */}
      <Modal
        visible={showCountryPicker}
        animationType="slide"
        transparent={true}
        onRequestClose={() => setShowCountryPicker(false)}
      >
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
                  onPress={() => {
                    handleChange('country', item);
                    setShowCountryPicker(false);
                  }}
                >
                  <Text className={`text-base ${formData.country === item ? 'text-gold font-bold' : 'text-forest'}`}>{item}</Text>
                </TouchableOpacity>
              )}
            />
          </View>
        </View>
      </Modal>
        </ScrollView>
        </TouchableWithoutFeedback>
      </KeyboardAvoidingView>
    </SafeAreaView>
  );
}
