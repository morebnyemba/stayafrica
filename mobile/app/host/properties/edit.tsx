import { View, Text, ScrollView, TouchableOpacity, Platform, TextInput, KeyboardAvoidingView, TouchableWithoutFeedback, Keyboard, ActivityIndicator, Image } from 'react-native';
import { useState, useEffect } from 'react';
import { useRouter, useLocalSearchParams } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { apiClient } from '@/services/api-client';
import * as Location from 'expo-location';
import MapView, { Marker, PROVIDER_GOOGLE } from 'react-native-maps';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import * as ImagePicker from 'expo-image-picker';
import { AppDialog, AppDialogAction } from '@/components/common/AppDialog';

interface InputFieldProps {
  label: string;
  value: string;
  onChangeText: (text: string) => void;
  placeholder: string;
  keyboardType?: 'default' | 'number-pad' | 'decimal-pad' | 'numeric' | 'email-address' | 'phone-pad';
  required?: boolean;
  multiline?: boolean;
}

const InputField = ({ 
  label, 
  value, 
  onChangeText, 
  placeholder, 
  keyboardType = 'default', 
  required = false, 
  multiline = false 
}: InputFieldProps) => (
  <View className="mb-4">
    <Text className="text-base font-semibold text-forest mb-2">
      {label} {required && <Text className="text-red-500">*</Text>}
    </Text>
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
  </View>
);

export default function EditPropertyScreen() {
  const router = useRouter();
  const { id } = useLocalSearchParams<{ id: string }>();
  const { isAuthenticated } = useAuth();
  const insets = useSafeAreaInsets();
  const [loading, setLoading] = useState(false);
  const [initialLoading, setInitialLoading] = useState(true);
  const [images, setImages] = useState<string[]>([]);
  const [existingImages, setExistingImages] = useState<{id: number, image_url?: string, image?: string}[]>([]);
  const [amenities, setAmenities] = useState<any[]>([]);
  const [selectedAmenityIds, setSelectedAmenityIds] = useState<number[]>([]);
  const [dialog, setDialog] = useState<{ visible: boolean; title: string; message: string; primaryAction: AppDialogAction; secondaryAction?: AppDialogAction }>({
    visible: false,
    title: '',
    message: '',
    primaryAction: { label: 'OK' },
  });

  const openDialog = (
    title: string,
    message: string,
    primaryAction: AppDialogAction = { label: 'OK' },
    secondaryAction?: AppDialogAction
  ) => {
    setDialog({ visible: true, title, message, primaryAction, secondaryAction });
  };
  
  const [formData, setFormData] = useState({
    title: '',
    description: '',
    address: '',
    city: '',
    suburb: '',
    country: '',
    price: '',
    bedrooms: '',
    bathrooms: '',
    maxGuests: '',
    propertyType: '',
    currency: 'USD',
    latitude: 0,
    longitude: 0,
  });

  useEffect(() => {
    if (id) {
      fetchProperty();
    }
  }, [id]);

  useEffect(() => {
    const loadAmenities = async () => {
      try {
        const data = await apiClient.getAmenities();
        setAmenities(data);
      } catch (error) {
        console.error('Failed to load amenities:', error);
      }
    };

    loadAmenities();
  }, []);

  const fetchProperty = async () => {
    try {
      const data: any = await apiClient.getHostPropertyById(String(id));
      
      setFormData({
        title: data.title || '',
        description: data.description || '',
        address: data.address || '',
        city: data.city || '',
        suburb: data.suburb || '',
        country: data.country || '',
        price: data.price_per_night?.toString() || '',
        bedrooms: data.bedrooms?.toString() || '',
        bathrooms: data.bathrooms?.toString() || '',
        maxGuests: data.max_guests?.toString() || '',
        propertyType: data.property_type || 'house',
        currency: data.currency || 'USD',
        latitude: data.location?.coordinates?.[1] || data.location?.latitude || 0,
        longitude: data.location?.coordinates?.[0] || data.location?.longitude || 0,
      });

      if (Array.isArray(data.amenities)) {
        setSelectedAmenityIds(
          data.amenities
            .map((amenity: any) => Number(amenity?.id))
            .filter((amenityId: number) => Number.isFinite(amenityId))
        );
      }

      if (data.images && Array.isArray(data.images)) {
        setExistingImages(data.images);
      }
    } catch (error) {
      console.error('Error fetching property:', error);
      openDialog('Error', 'Failed to load property details', {
        label: 'Back',
        onPress: () => router.back(),
      });
    } finally {
      setInitialLoading(false);
    }
  };

  const handleChange = (field: string, value: string) => {
    setFormData(prev => ({ ...prev, [field]: value }));
  };

  const toggleAmenity = (amenityId: number) => {
    setSelectedAmenityIds((prev) =>
      prev.includes(amenityId) ? prev.filter((id) => id !== amenityId) : [...prev, amenityId]
    );
  };

  const removeExistingImage = async (imageId: number) => {
    try {
      await apiClient.deletePropertyImage(String(id), imageId);
      setExistingImages(prev => prev.filter((img) => img.id !== imageId));
    } catch (error) {
      console.error('Error deleting image:', error);
      openDialog('Error', 'Failed to delete image');
    }
  };

  const handleGeocode = async () => {
    if (!formData.address || !formData.city || !formData.country) {
      openDialog('Missing Info', 'Please enter address, city and country first');
      return;
    }

    try {
      setLoading(true);
      const address = `${formData.address}, ${formData.suburb ? formData.suburb + ', ' : ''}${formData.city}, ${formData.country}`;
      const result = await Location.geocodeAsync(address);
      
      if (result.length > 0) {
        const { latitude, longitude } = result[0];
        setFormData(prev => ({ ...prev, latitude, longitude }));
        openDialog('Location Found', 'Coordinates updated successfully!');
      } else {
        openDialog('Not Found', 'Could not find coordinates for this address');
      }
    } catch (error) {
      openDialog('Error', 'Failed to find location coordinates');
    } finally {
      setLoading(false);
    }
  };

  const pickImages = async () => {
    const { status } = await ImagePicker.requestMediaLibraryPermissionsAsync();
    if (status !== 'granted') {
      openDialog('Permission Denied', 'Camera roll permission is required');
      return;
    }

    const result = await ImagePicker.launchImageLibraryAsync({
      mediaTypes: ImagePicker.MediaTypeOptions.Images,
      allowsMultipleSelection: true,
      quality: 0.8,
      selectionLimit: 10 - (existingImages.length + images.length),
    });

    if (!result.canceled && result.assets) {
      setImages(prev => [...prev, ...result.assets.map(asset => asset.uri)]);
    }
  };

  const handleUpdate = async () => {
    if (!formData.title || !formData.price || !formData.city) {
      openDialog('Error', 'Please fill in all required fields');
      return;
    }

    setLoading(true);
    try {
      const payload = {
        title: formData.title,
        description: formData.description,
        address: formData.address,
        city: formData.city,
        suburb: formData.suburb,
        country: formData.country,
        price_per_night: parseFloat(formData.price),
        bedrooms: parseInt(formData.bedrooms) || 1,
        bathrooms: parseInt(formData.bathrooms) || 1,
        max_guests: parseInt(formData.maxGuests) || 1,
        property_type: formData.propertyType,
        currency: formData.currency,
        amenities_ids: selectedAmenityIds,
        location: {
          type: 'Point',
          coordinates: [formData.longitude, formData.latitude]
        }
      };

      await apiClient.patch(`/properties/${id}/`, payload);

      if (images.length > 0) {
        const imageFormData = new FormData();
        images.forEach((uri, index) => {
          imageFormData.append('images', {
            uri,
            type: 'image/jpeg',
            name: `property-update-${index}.jpg`,
          } as any);
        });
        await apiClient.uploadPropertyImages(String(id), imageFormData);
      }
      
      openDialog('Success', 'Property updated successfully!', {
        label: 'OK',
        onPress: () => router.replace('/host/properties'),
      });
    } catch (error) {
      console.error('Update error:', error);
      openDialog('Error', 'Failed to update property');
    } finally {
      setLoading(false);
    }
  };

  if (!isAuthenticated) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100 items-center justify-center">
        <Text className="text-moss">Please sign in to edit properties</Text>
      </SafeAreaView>
    );
  }

  if (initialLoading) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100 items-center justify-center">
        <ActivityIndicator size="large" color="#D9B168" />
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
          >
            {/* Header */}
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
                <Text className="text-xl font-bold text-white">Edit Property</Text>
              </View>
            </LinearGradient>

            <View className="px-4 py-6">
            <InputField
              label="Property Title"
              value={formData.title}
              onChangeText={(text) => handleChange('title', text)}
              placeholder="e.g., Cozy Safari Lodge"
              required
            />

            <InputField
              label="Description"
              value={formData.description}
              onChangeText={(text) => handleChange('description', text)}
              placeholder="Describe your property..."
              required
              multiline
            />

            <View className="mb-4">
              <Text className="text-base font-semibold text-forest mb-2">
                Property Type
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
                    <Text className={`text-center text-sm font-semibold ${formData.propertyType === type ? 'text-forest' : 'text-moss'}`}>
                      {type.charAt(0).toUpperCase() + type.slice(1)}
                    </Text>
                  </TouchableOpacity>
                ))}
              </View>
            </View>

            <View className="flex-row gap-3">
              <View className="flex-1">
                <InputField 
                  label="Bedrooms" 
                  value={formData.bedrooms}
                  onChangeText={(text) => handleChange('bedrooms', text)}
                  placeholder="0"
                  keyboardType="number-pad"
                />
              </View>
              <View className="flex-1">
                <InputField 
                  label="Bathrooms" 
                  value={formData.bathrooms}
                  onChangeText={(text) => handleChange('bathrooms', text)}
                  placeholder="0"
                  keyboardType="number-pad"
                />
              </View>
            </View>

            <InputField 
              label="Max Guests" 
              value={formData.maxGuests}
              onChangeText={(text) => handleChange('maxGuests', text)}
              placeholder="0"
              keyboardType="number-pad"
            />

            <View className="mb-4">
              <Text className="text-base font-semibold text-forest mb-2">Amenities</Text>
              <Text className="text-xs text-moss mb-3">Select available amenities ({selectedAmenityIds.length} selected)</Text>
              <View className="flex-row flex-wrap gap-2">
                {amenities.map((amenity) => {
                  const amenityId = Number(amenity.id);
                  const isSelected = selectedAmenityIds.includes(amenityId);
                  return (
                    <TouchableOpacity
                      key={amenity.id}
                      onPress={() => toggleAmenity(amenityId)}
                      className="px-3 py-2 rounded-full border"
                      style={{
                        backgroundColor: isSelected ? '#D9B168' : '#FFFFFF',
                        borderColor: isSelected ? '#D9B168' : '#D8D2C4',
                      }}
                    >
                      <Text style={{ color: isSelected ? '#122F26' : '#3A5C50', fontWeight: '600', fontSize: 12 }}>
                        {amenity.name}
                      </Text>
                    </TouchableOpacity>
                  );
                })}
              </View>
            </View>

            <View className="h-px bg-sand-300 my-4" />
            <Text className="text-lg font-bold text-forest mb-4">Location</Text>

            <InputField 
              label="Address" 
              value={formData.address}
              onChangeText={(text) => handleChange('address', text)}
              placeholder="Street address"
              required
            />
            
            <InputField 
              label="Suburb" 
              value={formData.suburb}
              onChangeText={(text) => handleChange('suburb', text)}
              placeholder="Suburb (Optional)"
            />

            <InputField 
              label="City" 
              value={formData.city}
              onChangeText={(text) => handleChange('city', text)}
              placeholder="City"
              required
            />
            
            <InputField 
              label="Country" 
              value={formData.country}
              onChangeText={(text) => handleChange('country', text)}
              placeholder="Country"
              required
            />

            <View className="mb-4">
              <Text className="text-base font-semibold text-forest mb-2">
                Location Map
              </Text>
              
              <TouchableOpacity
                onPress={handleGeocode}
                className="bg-white rounded-2xl p-4 mb-3 flex-row items-center justify-center"
                style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 4, elevation: 2 }}
              >
                <Ionicons name="search" size={18} color="#4a5f4b" />
                <Text className="text-moss ml-2 font-medium">Update Coordinates from Address</Text>
              </TouchableOpacity>

              {formData.latitude !== 0 && formData.longitude !== 0 && (
                <View 
                  className="rounded-2xl overflow-hidden bg-gray-200"
                  style={{
                    height: 250,
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
                      latitude: formData.latitude,
                      longitude: formData.longitude,
                      latitudeDelta: 0.01,
                      longitudeDelta: 0.01,
                    }}
                  >
                    <Marker
                      coordinate={{
                        latitude: formData.latitude,
                        longitude: formData.longitude,
                      }}
                      title="Property Location"
                      pinColor="#D9B168"
                    />
                  </MapView>
                </View>
              )}
            </View>

            <View className="mb-4">
              <Text className="text-base font-semibold text-forest mb-2">Images</Text>
              <ScrollView horizontal showsHorizontalScrollIndicator={false} className="flex-row">
                {existingImages.map((img) => (
                  <View key={img.id} className="relative mr-2">
                    <Image source={{ uri: img.image_url || img.image }} className="w-24 h-24 rounded-xl" />
                    <TouchableOpacity
                      onPress={() => removeExistingImage(img.id)}
                      className="absolute -top-2 -right-2 bg-red-500 rounded-full w-6 h-6 items-center justify-center"
                    >
                      <Ionicons name="close" size={14} color="white" />
                    </TouchableOpacity>
                  </View>
                ))}
                {images.map((uri, index) => (
                   <View key={index} className="relative mr-2">
                    <Image source={{ uri }} className="w-24 h-24 rounded-xl" />
                    <TouchableOpacity 
                      onPress={() => setImages(prev => prev.filter((_, i) => i !== index))}
                      className="absolute -top-2 -right-2 bg-red-500 rounded-full w-6 h-6 items-center justify-center"
                    >
                      <Ionicons name="close" size={14} color="white" />
                    </TouchableOpacity>
                  </View>
                ))}
                <TouchableOpacity 
                  onPress={pickImages}
                  className="w-24 h-24 bg-white rounded-xl items-center justify-center border-2 border-dashed border-gold"
                >
                  <Ionicons name="add" size={32} color="#D9B168" />
                </TouchableOpacity>
              </ScrollView>
            </View>

            <View className="h-px bg-sand-300 my-4" />
            <Text className="text-lg font-bold text-forest mb-4">Pricing</Text>

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
            />

            <TouchableOpacity
              onPress={handleUpdate}
              disabled={loading}
              className="mt-6"
            >
              <LinearGradient
                colors={loading ? ['#94a3b8', '#94a3b8'] : ['#D9B168', '#bea04f']}
                className="py-4 rounded-2xl items-center"
              >
                <Text className="text-forest font-bold text-base">
                  {loading ? 'Updating...' : 'Update Property'}
                </Text>
              </LinearGradient>
            </TouchableOpacity>
          </View>
        </ScrollView>
      </TouchableWithoutFeedback>
    </KeyboardAvoidingView>
    <AppDialog
      visible={dialog.visible}
      title={dialog.title}
      message={dialog.message}
      primaryAction={dialog.primaryAction}
      secondaryAction={dialog.secondaryAction}
      onRequestClose={() => setDialog((prev) => ({ ...prev, visible: false }))}
    />
  </SafeAreaView>
  );
}