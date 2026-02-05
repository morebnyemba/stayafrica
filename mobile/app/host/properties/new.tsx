import { View, Text, ScrollView, TouchableOpacity, Platform, TextInput, Alert, KeyboardTypeOptions, KeyboardAvoidingView, TouchableWithoutFeedback, Keyboard, Image } from 'react-native';
import { useState } from 'react';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import * as ImagePicker from 'expo-image-picker';
import * as Location from 'expo-location';

interface InputFieldProps {
  label: string;
  field: keyof FormData;
  placeholder: string;
  keyboardType?: KeyboardTypeOptions;
  required?: boolean;
  multiline?: boolean;
}

interface FormData {
  title: string;
  description: string;
  address: string;
  city: string;
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

export default function NewPropertyScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const [loading, setLoading] = useState(false);
  const [currentStep, setCurrentStep] = useState<FormStep>('basic');
  const [images, setImages] = useState<string[]>([]);
  const [formData, setFormData] = useState<FormData>({
    title: '',
    description: '',
    address: '',
    city: '',
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

  const validateStep = (): boolean => {
    switch (currentStep) {
      case 'basic':
        if (!formData.title || !formData.description) {
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

  const InputField = ({ label, field, placeholder, keyboardType = 'default', required = false, multiline = false }: InputFieldProps) => (
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
          multiline={multiline}
          numberOfLines={multiline ? 4 : 1}
          textAlignVertical={multiline ? 'top' : 'center'}
          value={formData[field] as string}
          onChangeText={(value) => handleChange(field, value)}
        />
      </View>
    </View>
  );

  const currentStepIndex = STEPS.findIndex(s => s.id === currentStep);
  const isLastStep = currentStepIndex === STEPS.length - 1;

  return (
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
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f']}
        className="px-4 pb-6"
        style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
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
              field="title" 
              placeholder="e.g., Cozy Safari Lodge"
              required
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

            <View className="mb-4">
              <Text className="text-base font-semibold text-forest mb-2">
                Description <Text className="text-red-500">*</Text>
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
          </>
        )}

        {/* Location Step */}
        {currentStep === 'location' && (
          <>
            <InputField 
              label="Address" 
              field="address" 
              placeholder="Street address"
              required
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
              required
            />

            <View className="mb-4">
              <Text className="text-base font-semibold text-forest mb-2">
                Pin Location <Text className="text-red-500">*</Text>
              </Text>
              <TouchableOpacity
                onPress={getCurrentLocation}
                className="bg-white rounded-2xl p-4 mb-3 flex-row items-center justify-between"
                style={{
                  shadowColor: '#122F26',
                  shadowOffset: { width: 0, height: 2 },
                  shadowOpacity: 0.05,
                  shadowRadius: 4,
                  elevation: 2,
                }}
              >
                <Text className="text-moss">Use current location</Text>
                <Ionicons name="locate" size={20} color="#4a5f4b" />
              </TouchableOpacity>

              {formData.latitude && formData.longitude && (
                <View 
                  className="rounded-2xl overflow-hidden bg-gray-200"
                  style={{
                    height: 300,
                    shadowColor: '#122F26',
                    shadowOffset: { width: 0, height: 2 },
                    shadowOpacity: 0.1,
                    shadowRadius: 4,
                    elevation: 3,
                  }}
                >
                  <View className="flex-1 items-center justify-center">
                    <Ionicons name="map-outline" size={48} color="#4a5f4b" />
                    <Text className="text-sm text-moss mt-2 text-center px-4">
                      Map will be displayed here
                    </Text>
                    <Text className="text-xs text-gray-500 mt-1 text-center px-4">
                      Install @rnmapbox/maps for interactive map
                    </Text>
                    <Text className="text-xs text-gray-600 mt-3 font-semibold">
                      📍 {formData.latitude.toFixed(6)}, {formData.longitude.toFixed(6)}
                    </Text>
                  </View>
                  <Text className="absolute bottom-2 left-2 right-2 bg-white px-3 py-2 rounded-xl text-xs text-moss text-center">
                    Use GPS button above to set location
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
              field="price" 
              placeholder="0.00"
              keyboardType="decimal-pad"
              required
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
    </ScrollView>
      </TouchableWithoutFeedback>
    </KeyboardAvoidingView>
  );
}
