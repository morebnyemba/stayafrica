import { View, Text, TouchableOpacity, ScrollView, TextInput, Alert, Platform, ActivityIndicator, KeyboardAvoidingView, TouchableWithoutFeedback, Keyboard } from 'react-native';
import { useState, useEffect } from 'react';
import { useAuth } from '@/context/auth-context';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';

// List of countries for the picker
const COUNTRIES = [
  'Afghanistan', 'Albania', 'Algeria', 'Angola', 'Argentina', 'Australia', 'Austria',
  'Bangladesh', 'Belgium', 'Benin', 'Botswana', 'Brazil', 'Burkina Faso', 'Burundi',
  'Cameroon', 'Canada', 'Central African Republic', 'Chad', 'Chile', 'China', 'Colombia',
  'Congo', 'Croatia', 'Czech Republic', 'Denmark', 'Egypt', 'Ethiopia', 'Finland', 'France',
  'Gabon', 'Gambia', 'Germany', 'Ghana', 'Greece', 'Guinea', 'Hungary', 'India', 'Indonesia',
  'Ireland', 'Israel', 'Italy', 'Ivory Coast', 'Japan', 'Kenya', 'Lesotho', 'Liberia',
  'Madagascar', 'Malawi', 'Malaysia', 'Mali', 'Mauritius', 'Mexico', 'Morocco', 'Mozambique',
  'Namibia', 'Netherlands', 'New Zealand', 'Niger', 'Nigeria', 'Norway', 'Pakistan', 'Peru',
  'Philippines', 'Poland', 'Portugal', 'Romania', 'Russia', 'Rwanda', 'Saudi Arabia', 'Senegal',
  'Sierra Leone', 'Singapore', 'South Africa', 'South Korea', 'Spain', 'Sudan', 'Swaziland',
  'Sweden', 'Switzerland', 'Tanzania', 'Thailand', 'Togo', 'Tunisia', 'Turkey', 'Uganda',
  'Ukraine', 'United Arab Emirates', 'United Kingdom', 'United States', 'Vietnam', 'Zambia', 'Zimbabwe'
];

export default function EditProfileScreen() {
  const { user, updateProfile } = useAuth();
  const router = useRouter();
  const [isLoading, setIsLoading] = useState(false);
  const [showCountryPicker, setShowCountryPicker] = useState(false);

  const [formData, setFormData] = useState({
    first_name: '',
    last_name: '',
    email: '',
    phone_number: '',
    country_of_residence: '',
  });

  // Sync form data when user data loads
  useEffect(() => {
    if (user) {
      setFormData({
        first_name: user.first_name || '',
        last_name: user.last_name || '',
        email: user.email || '',
        phone_number: user.phone_number || '',
        country_of_residence: user.country_of_residence || '',
      });
    }
  }, [user]);

  const handleUpdateProfile = async () => {
    setIsLoading(true);
    
    try {
      const payload = {
        first_name: formData.first_name.trim(),
        last_name: formData.last_name.trim(),
        phone_number: formData.phone_number.trim(),
        country_of_residence: formData.country_of_residence.trim(),
      };
      if (payload.phone_number) {
        const phoneRegex = /^\+?\d{9,15}$/;
        if (!phoneRegex.test(payload.phone_number)) {
          Alert.alert('Invalid phone number', 'Use 9-15 digits, optionally starting with +.');
          return;
        }
      }
      const cleanedPayload = Object.fromEntries(
        Object.entries(payload).filter(([, value]) => value !== '')
      );
      if (Object.keys(cleanedPayload).length === 0) {
        Alert.alert('No changes', 'Update at least one field before saving.');
        return;
      }
      await updateProfile(cleanedPayload);
      Alert.alert('Success', 'Profile updated successfully!', [
        { text: 'OK', onPress: () => router.back() }
      ]);
    } catch (error: any) {
      const serverMessage =
        error?.response?.data?.detail ||
        error?.response?.data?.error ||
        error?.message ||
        'Failed to update profile';
      Alert.alert('Error', serverMessage);
    } finally {
      setIsLoading(false);
    }
  };

  const InputField = ({ 
    icon, 
    label, 
    value, 
    onChangeText, 
    placeholder,
    keyboardType = 'default',
    editable = true,
  }: any) => (
    <View className="mb-4">
      <Text className="text-sm font-medium text-forest mb-2">{label}</Text>
      <View className="flex-row items-center bg-white rounded-2xl border border-sand-300">
        <View className="pl-4">
          <Ionicons name={icon} size={20} color="#3A5C50" />
        </View>
        <TextInput
          value={value}
          onChangeText={onChangeText}
          placeholder={placeholder}
          keyboardType={keyboardType}
          editable={editable}
          className={`flex-1 px-4 py-4 text-forest ${!editable ? 'opacity-50' : ''}`}
          placeholderTextColor="#94a3b8"
        />
      </View>
    </View>
  );

  return (
    <KeyboardAvoidingView 
      behavior={Platform.OS === 'ios' ? 'padding' : 'height'}
      className="flex-1"
    >
      <TouchableWithoutFeedback onPress={Keyboard.dismiss}>
        <ScrollView 
          className="flex-1 bg-sand-100" 
          showsVerticalScrollIndicator={false}
          keyboardShouldPersistTaps="handled"
          contentContainerStyle={{ paddingBottom: 40 }}
        >
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="px-4 pb-8"
        style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
      >
        {/* Top Navigation */}
        <View className="flex-row items-center justify-between mb-6">
          <TouchableOpacity
            onPress={() => router.back()}
            className="w-10 h-10 rounded-xl items-center justify-center"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </TouchableOpacity>

          <Text className="text-xl font-bold text-white">Edit Profile</Text>

          <View className="w-10" />
        </View>

        {/* Avatar */}
        <View className="items-center">
          <View className="w-24 h-24 rounded-full bg-gold items-center justify-center">
            <Text className="text-4xl font-bold text-forest">
              {user?.first_name?.[0]?.toUpperCase() || 'U'}
              {user?.last_name?.[0]?.toUpperCase() || ''}
            </Text>
          </View>
          <TouchableOpacity className="mt-2 flex-row items-center">
            <Ionicons name="camera-outline" size={16} color="#D9B168" />
            <Text className="text-gold ml-1 text-sm">Change Photo</Text>
          </TouchableOpacity>
        </View>
      </LinearGradient>

      {/* Form */}
      <View className="px-4 pt-6 -mt-4">
        <View className="bg-white rounded-3xl p-5" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 4 },
          shadowOpacity: 0.08,
          shadowRadius: 12,
          elevation: 4,
        }}>
          <Text className="text-lg font-bold text-forest mb-4">Personal Information</Text>

          <InputField
            icon="person-outline"
            label="First Name"
            value={formData.first_name}
            onChangeText={(text: string) => setFormData({ ...formData, first_name: text })}
            placeholder="Enter your first name"
          />

          <InputField
            icon="person-outline"
            label="Last Name"
            value={formData.last_name}
            onChangeText={(text: string) => setFormData({ ...formData, last_name: text })}
            placeholder="Enter your last name"
          />

          <InputField
            icon="mail-outline"
            label="Email Address"
            value={formData.email}
            placeholder="Enter your email"
            keyboardType="email-address"
            editable={false}
          />

          <InputField
            icon="call-outline"
            label="Phone Number"
            value={formData.phone_number}
            onChangeText={(text: string) => setFormData({ ...formData, phone_number: text })}
            placeholder="Enter your phone number"
            keyboardType="phone-pad"
          />

          {/* Country Picker */}
          <View className="mb-4">
            <Text className="text-sm font-medium text-forest mb-2">Country of Residence</Text>
            <TouchableOpacity 
              onPress={() => setShowCountryPicker(!showCountryPicker)}
              className="flex-row items-center bg-white rounded-2xl border border-sand-300 p-4"
            >
              <Ionicons name="location-outline" size={20} color="#3A5C50" />
              <Text className={`flex-1 ml-4 ${formData.country_of_residence ? 'text-forest' : 'text-gray-400'}`}>
                {formData.country_of_residence || 'Select your country'}
              </Text>
              <Ionicons name={showCountryPicker ? "chevron-up" : "chevron-down"} size={20} color="#3A5C50" />
            </TouchableOpacity>

            {showCountryPicker && (
              <View 
                className="bg-white rounded-2xl border border-sand-300 mt-2 overflow-hidden"
                style={{ maxHeight: 240 }}
              >
                <ScrollView nestedScrollEnabled>
                  {COUNTRIES.map((country) => (
                    <TouchableOpacity
                      key={country}
                      onPress={() => {
                        setFormData({ ...formData, country_of_residence: country });
                        setShowCountryPicker(false);
                      }}
                      className={`p-4 border-b border-sand-100 ${
                        formData.country_of_residence === country ? 'bg-sand-100' : ''
                      }`}
                    >
                      <Text className={`${
                        formData.country_of_residence === country ? 'text-forest font-semibold' : 'text-moss'
                      }`}>
                        {country}
                      </Text>
                    </TouchableOpacity>
                  ))}
                </ScrollView>
              </View>
            )}
          </View>
        </View>

        {/* Save Button */}
        <TouchableOpacity
          onPress={handleUpdateProfile}
          disabled={isLoading}
          className="mt-6 mb-8"
        >
          <LinearGradient
            colors={isLoading ? ['#94a3b8', '#94a3b8'] : ['#D9B168', '#bea04f']}
            className="py-4 rounded-2xl flex-row items-center justify-center"
            style={{
              shadowColor: '#D9B168',
              shadowOffset: { width: 0, height: 4 },
              shadowOpacity: 0.3,
              shadowRadius: 8,
              elevation: 5,
            }}
          >
            {isLoading ? (
              <ActivityIndicator color="#fff" />
            ) : (
              <>
                <Ionicons name="checkmark-circle-outline" size={22} color="#122F26" />
                <Text className="text-forest font-bold ml-2 text-base">Save Changes</Text>
              </>
            )}
          </LinearGradient>
        </TouchableOpacity>

        {/* Cancel Button */}
        <TouchableOpacity
          onPress={() => router.back()}
          className="mb-8"
        >
          <View className="py-4 rounded-2xl border-2 border-forest items-center justify-center">
            <Text className="text-forest font-bold text-base">Cancel</Text>
          </View>
        </TouchableOpacity>
      </View>
        </ScrollView>
      </TouchableWithoutFeedback>
    </KeyboardAvoidingView>
  );
}

EditProfileScreen.displayName = 'EditProfileScreen';
