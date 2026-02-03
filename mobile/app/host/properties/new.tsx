import { View, Text, ScrollView, TouchableOpacity, Platform, TextInput, Alert, KeyboardTypeOptions, Modal, Image } from 'react-native';
import { useState } from 'react';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import * as ImagePicker from 'expo-image-picker';
import MapView, { Marker } from 'react-native-maps';

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

type FormStep = 'basic' | 'location' | 'images' | 'pricing';

const STEPS: { id: FormStep; label: string; description: string; icon: keyof typeof Ionicons.glyphMap }[] = [
  { id: 'basic', label: 'Basic Info', description: 'Property details', icon: 'home' },
  { id: 'location', label: 'Location', description: 'Pin on map', icon: 'location' },
  { id: 'images', label: 'Images', description: 'Add photos', icon: 'images' },
  { id: 'pricing', label: 'Pricing', description: 'Set rates', icon: 'cash' },
];

export default function NewPropertyScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const [loading, setLoading] = useState(false);
  const [currentStep, setCurrentStep] = useState<FormStep>('basic');
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
    bedrooms: '1',
    bathrooms: '1',
    maxGuests: '2',
  });

  const [imageUris, setImageUris] = useState<string[]>([]);
  const [mapRegion, setMapRegion] = useState({
    latitude: -17.8252,
    longitude: 31.0335,
    latitudeDelta: 0.0922,
    longitudeDelta: 0.0421,
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

  const validateStep = (step: FormStep): boolean => {
    switch (step) {
      case 'basic':
        if (!formData.title || !formData.description || !formData.propertyType) {
          Alert.alert('Error', 'Please fill in title, description, and property type');
          return false;
        }
        return true;
      case 'location':
        if (!formData.address || !formData.city || !formData.country) {
          Alert.alert('Error', 'Please fill in address, city, and country');
          return false;
        }
        if (!formData.latitude || !formData.longitude) {
          Alert.alert('Error', 'Please pin the location on the map');
          return false;
        }
        return true;
      case 'images':
        // Images are optional
        return true;
      case 'pricing':
        if (!formData.price || !formData.currency) {
          Alert.alert('Error', 'Please fill in price and currency');
          return false;
        }
        return true;
      default:
        return true;
    }
  };

  const goToNextStep = () => {
    if (!validateStep(currentStep)) return;

    const stepIndex = STEPS.findIndex(s => s.id === currentStep);
    if (stepIndex < STEPS.length - 1) {
      setCurrentStep(STEPS[stepIndex + 1].id);
    }
  };

  const goToPreviousStep = () => {
    const stepIndex = STEPS.findIndex(s => s.id === currentStep);
    if (stepIndex > 0) {
      setCurrentStep(STEPS[stepIndex - 1].id);
    }
  };

  const pickImages = async () => {
    const { status } = await ImagePicker.requestMediaLibraryPermissionsAsync();
    if (status !== 'granted') {
      Alert.alert('Permission Required', 'We need camera roll permissions to add images.');
      return;
    }

    const result = await ImagePicker.launchImageLibraryAsync({
      mediaTypes: ImagePicker.MediaTypeOptions.Images,
      allowsMultipleSelection: true,
      quality: 0.8,
      selectionLimit: 10 - imageUris.length,
    });

    if (!result.canceled && result.assets) {
      const newUris = result.assets.map(asset => asset.uri);
      setImageUris([...imageUris, ...newUris].slice(0, 10));
    }
  };

  const removeImage = (index: number) => {
    setImageUris(imageUris.filter((_, i) => i !== index));
  };

  const handleMapPress = (event: any) => {
    const { latitude, longitude } = event.nativeEvent.coordinate;
    setFormData(prev => ({
      ...prev,
      latitude: latitude.toString(),
      longitude: longitude.toString(),
    }));
    setMapRegion({
      ...mapRegion,
      latitude,
      longitude,
    });
  };

  const handleCreate = async () => {
    if (!validateStep('pricing')) return;

    setLoading(true);
    try {
      const propertyData = {
        title: formData.title,
        description: formData.description,
        property_type: formData.propertyType,
        address: formData.address,
        city: formData.city,
        suburb: formData.suburb,
        country: formData.country,
        latitude: parseFloat(formData.latitude),
        longitude: parseFloat(formData.longitude),
        price_per_night: parseFloat(formData.price),
        currency: formData.currency,
        bedrooms: parseInt(formData.bedrooms) || 1,
        bathrooms: parseInt(formData.bathrooms) || 1,
        max_guests: parseInt(formData.maxGuests) || 2,
      };
      
      console.log('Property data to submit:', propertyData);
      console.log('Images to upload:', imageUris.length);
      
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

  const currentStepIndex = STEPS.findIndex(s => s.id === currentStep);
  const isLastStep = currentStepIndex === STEPS.length - 1;

  return (
    <View className="flex-1 bg-sand-100">
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
        className="px-4 pb-4"
        style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
      >
        <View className="flex-row items-center mb-4">
          <TouchableOpacity
            onPress={() => router.back()}
            className="w-10 h-10 rounded-xl items-center justify-center mr-3"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </TouchableOpacity>
          <Text className="text-xl font-bold text-white">Add New Property</Text>
        </View>

        {/* Step Indicator */}
        <View className="flex-row justify-between">
          {STEPS.map((step, index) => (
            <View key={step.id} className="items-center flex-1">
              <View className={`w-10 h-10 rounded-full items-center justify-center mb-1 ${
                index < currentStepIndex ? 'bg-green-500' :
                index === currentStepIndex ? 'bg-gold' :
                'bg-sand-200'
              }`}>
                {index < currentStepIndex ? (
                  <Ionicons name="checkmark" size={20} color="#fff" />
                ) : (
                  <Ionicons 
                    name={step.icon} 
                    size={20} 
                    color={index === currentStepIndex ? '#122F26' : '#6B8E7F'} 
                  />
                )}
              </View>
              <Text className={`text-xs ${
                index === currentStepIndex ? 'text-white font-bold' : 'text-sand-300'
              }`}>
                {step.label}
              </Text>
            </View>
          ))}
        </View>
      </LinearGradient>

      <ScrollView 
        className="flex-1" 
        showsVerticalScrollIndicator={false}
        contentContainerStyle={{ paddingBottom: 100 }}
        keyboardShouldPersistTaps="handled"
      >
        <View className="px-4 py-6">
          {/* Step 1: Basic Info */}
          {currentStep === 'basic' && (
            <View>
              <View className="flex-row items-center mb-4">
                <Ionicons name="home" size={24} color="#3A5C50" />
                <Text className="text-xl font-bold text-forest ml-2">Basic Information</Text>
              </View>
              
              <InputField 
                label="Property Title" 
                field="title" 
                placeholder="e.g., Cozy Safari Lodge"
                required
              />
              
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

              <SelectField
                label="Property Type"
                value={propertyTypes.find(pt => pt.value === formData.propertyType)?.label || propertyTypes[0].label}
                onPress={() => setShowPropertyTypeModal(true)}
                required
              />

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
            </View>
          )}

          {/* Step 2: Location */}
          {currentStep === 'location' && (
            <View>
              <View className="flex-row items-center mb-4">
                <Ionicons name="location" size={24} color="#3A5C50" />
                <Text className="text-xl font-bold text-forest ml-2">Location</Text>
              </View>

              <InputField 
                label="Full Address" 
                field="address" 
                placeholder="123 Main Street"
                required
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
                value={countries.find(c => c.value === formData.country)?.label || 'Tap to select'}
                onPress={() => setShowCountryModal(true)}
                required
              />

              <Text className="text-base font-semibold text-forest mb-2">
                Pin Your Property on Map <Text className="text-red-500">*</Text>
              </Text>
              <View className="bg-white rounded-2xl overflow-hidden mb-4" style={{
                shadowColor: '#122F26',
                shadowOffset: { width: 0, height: 4 },
                shadowOpacity: 0.08,
                shadowRadius: 8,
                elevation: 4,
                height: 300,
              }}>
                <MapView
                  style={{ flex: 1 }}
                  region={mapRegion}
                  onPress={handleMapPress}
                >
                  {formData.latitude && formData.longitude && (
                    <Marker
                      coordinate={{
                        latitude: parseFloat(formData.latitude),
                        longitude: parseFloat(formData.longitude),
                      }}
                      title="Property Location"
                    />
                  )}
                </MapView>
              </View>

              <View className="bg-blue-50 rounded-2xl p-3 mb-4">
                <View className="flex-row items-start">
                  <Ionicons name="information-circle" size={20} color="#3B82F6" />
                  <Text className="text-xs text-blue-700 ml-2 flex-1">
                    Tap on the map to pin your property location. The coordinates will be saved automatically.
                  </Text>
                </View>
              </View>

              <View className="flex-row gap-3">
                <View className="flex-1">
                  <Text className="text-sm font-semibold text-forest mb-2">Latitude</Text>
                  <View className="bg-sand-200 rounded-2xl p-3">
                    <Text className="text-sm text-moss">
                      {formData.latitude || 'Tap map to set'}
                    </Text>
                  </View>
                </View>
                <View className="flex-1">
                  <Text className="text-sm font-semibold text-forest mb-2">Longitude</Text>
                  <View className="bg-sand-200 rounded-2xl p-3">
                    <Text className="text-sm text-moss">
                      {formData.longitude || 'Tap map to set'}
                    </Text>
                  </View>
                </View>
              </View>
            </View>
          )}

          {/* Step 3: Images */}
          {currentStep === 'images' && (
            <View>
              <View className="flex-row items-center mb-4">
                <Ionicons name="images" size={24} color="#3A5C50" />
                <Text className="text-xl font-bold text-forest ml-2">Property Images</Text>
              </View>

              <View className="bg-blue-50 rounded-2xl p-3 mb-4">
                <View className="flex-row items-start">
                  <Ionicons name="information-circle" size={20} color="#3B82F6" />
                  <Text className="text-xs text-blue-700 ml-2 flex-1">
                    Add up to 10 images of your property. The first image will be used as the main thumbnail.
                  </Text>
                </View>
              </View>

              <TouchableOpacity
                onPress={pickImages}
                disabled={imageUris.length >= 10}
                className="mb-4"
              >
                <View className="border-2 border-dashed border-sand-300 rounded-2xl p-8 items-center" style={{
                  backgroundColor: imageUris.length >= 10 ? '#f3f4f6' : '#fff',
                }}>
                  <Ionicons 
                    name="cloud-upload-outline" 
                    size={48} 
                    color={imageUris.length >= 10 ? '#9ca3af' : '#3A5C50'} 
                  />
                  <Text className="text-base font-semibold text-forest mt-3">
                    {imageUris.length >= 10 ? 'Maximum images reached' : 'Tap to select images'}
                  </Text>
                  <Text className="text-sm text-moss mt-1">
                    {imageUris.length}/10 images added
                  </Text>
                </View>
              </TouchableOpacity>

              {imageUris.length > 0 && (
                <View>
                  <Text className="text-base font-semibold text-forest mb-3">
                    Selected Images ({imageUris.length})
                  </Text>
                  <View className="flex-row flex-wrap gap-2">
                    {imageUris.map((uri, index) => (
                      <View key={index} className="relative" style={{ width: '48%' }}>
                        <Image
                          source={{ uri }}
                          style={{ 
                            width: '100%', 
                            height: 120, 
                            borderRadius: 12,
                          }}
                          resizeMode="cover"
                        />
                        {index === 0 && (
                          <View className="absolute top-2 left-2 bg-gold px-2 py-1 rounded-lg">
                            <Text className="text-xs font-bold text-forest">Main</Text>
                          </View>
                        )}
                        <TouchableOpacity
                          onPress={() => removeImage(index)}
                          className="absolute top-2 right-2 bg-red-600 rounded-full p-1"
                        >
                          <Ionicons name="close" size={16} color="#fff" />
                        </TouchableOpacity>
                      </View>
                    ))}
                  </View>
                </View>
              )}
            </View>
          )}

          {/* Step 4: Pricing */}
          {currentStep === 'pricing' && (
            <View>
              <View className="flex-row items-center mb-4">
                <Ionicons name="cash" size={24} color="#3A5C50" />
                <Text className="text-xl font-bold text-forest ml-2">Pricing</Text>
              </View>

              <View className="flex-row gap-3">
                <View className="flex-1">
                  <InputField 
                    label="Price/Night" 
                    field="price" 
                    placeholder="50.00"
                    keyboardType="decimal-pad"
                    required
                  />
                </View>
                <View className="flex-1">
                  <SelectField
                    label="Currency"
                    value={currencies.find(c => c.value === formData.currency)?.label || currencies[0].label}
                    onPress={() => setShowCurrencyModal(true)}
                    required
                  />
                </View>
              </View>

              <View className="bg-green-50 rounded-2xl p-4 mt-4">
                <View className="flex-row items-start">
                  <Ionicons name="checkmark-circle" size={20} color="#10B981" />
                  <View className="flex-1 ml-2">
                    <Text className="text-sm font-semibold text-green-800 mb-1">Ready to submit!</Text>
                    <Text className="text-xs text-green-700">
                      Review your information and tap "Create Property" to complete the listing.
                    </Text>
                  </View>
                </View>
              </View>
            </View>
          )}
        </View>
      </ScrollView>

      {/* Navigation Footer */}
      <View className="bg-white px-4 py-4 border-t border-sand-200" style={{
        shadowColor: '#000',
        shadowOffset: { width: 0, height: -4 },
        shadowOpacity: 0.1,
        shadowRadius: 8,
        elevation: 8,
      }}>
        <View className="flex-row gap-3">
          {currentStepIndex > 0 && (
            <TouchableOpacity
              onPress={goToPreviousStep}
              className="flex-1"
            >
              <View className="bg-sand-200 py-4 rounded-2xl flex-row items-center justify-center">
                <Ionicons name="chevron-back" size={20} color="#3A5C50" />
                <Text className="text-forest font-bold text-base ml-1">Previous</Text>
              </View>
            </TouchableOpacity>
          )}
          
          {!isLastStep ? (
            <TouchableOpacity
              onPress={goToNextStep}
              className={currentStepIndex === 0 ? 'flex-1' : 'flex-1'}
            >
              <LinearGradient
                colors={['#D9B168', '#bea04f']}
                className="py-4 rounded-2xl flex-row items-center justify-center"
              >
                <Text className="text-forest font-bold text-base mr-1">Next</Text>
                <Ionicons name="chevron-forward" size={20} color="#122F26" />
              </LinearGradient>
            </TouchableOpacity>
          ) : (
            <TouchableOpacity
              onPress={handleCreate}
              disabled={loading}
              className={currentStepIndex === 0 ? 'flex-1' : 'flex-1'}
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
          )}
        </View>
      </View>
    </View>
  );
}
