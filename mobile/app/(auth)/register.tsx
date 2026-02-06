import { useState, useRef, useEffect } from 'react';
import { 
  Text, 
  View, 
  ScrollView, 
  TextInput, 
  TouchableOpacity, 
  Image,
  Modal,
  Animated,
  Dimensions,
  KeyboardAvoidingView,
  Platform,
  Alert,
  ActivityIndicator
} from 'react-native';
import { LinearGradient } from 'expo-linear-gradient';
import { useSafeAreaInsets } from 'react-native-safe-area-context';
import { useRouter } from 'expo-router';
import { useAuth } from '@/context/auth-context';
import { Ionicons, MaterialIcons, FontAwesome5 } from '@expo/vector-icons';

const { width, height } = Dimensions.get('window');

type Step = 1 | 2 | 3;

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

// Animated Step Indicator with StayAfrica colors
const AnimatedStepIndicator = ({ currentStep, steps }: { currentStep: Step; steps: number }) => {
  const progressAnim = useRef(new Animated.Value(0)).current;

  useEffect(() => {
    Animated.spring(progressAnim, {
      toValue: (currentStep - 1) / (steps - 1),
      useNativeDriver: false,
      tension: 100,
      friction: 15,
    }).start();
  }, [currentStep]);

  const progressWidth = progressAnim.interpolate({
    inputRange: [0, 1],
    outputRange: ['0%', '100%'],
  });

  return (
    <View className="relative mb-12 mt-4">
      {/* Background track */}
      <View className="h-1.5 bg-sand-200 rounded-full overflow-hidden mx-4">
        {/* Progress fill */}
        <Animated.View 
          style={{ 
            width: progressWidth, 
            height: '100%', 
            backgroundColor: '#D9B168',
            borderRadius: 999,
          }}
        />
      </View>
      
      {/* Step circles */}
      <View className="absolute w-full flex-row justify-between -top-4 px-0">
        {[1, 2, 3].map((step) => (
          <View key={step} className="items-center">
            <View 
              className={`w-10 h-10 rounded-full items-center justify-center border-4 ${
                step <= currentStep
                  ? 'bg-gold border-gold'
                  : 'bg-white border-sand-200'
              }`}
              style={{
                shadowColor: step <= currentStep ? '#D9B168' : '#9ca3af',
                shadowOpacity: 0.3,
                shadowRadius: 8,
                shadowOffset: { width: 0, height: 4 },
                elevation: 8,
              }}
            >
              {step < currentStep ? (
                <Ionicons name="checkmark" size={20} color="#122F26" />
              ) : (
                <Text className={`font-bold text-base ${step === currentStep ? 'text-forest' : 'text-sand-400'}`}>
                  {step}
                </Text>
              )}
            </View>
            <Text className={`mt-2 text-xs font-semibold ${step <= currentStep ? 'text-forest' : 'text-sand-400'}`}>
              {step === 1 ? 'Account' : step === 2 ? 'Personal' : 'Complete'}
            </Text>
          </View>
        ))}
      </View>
    </View>
  );
};

export default function RegisterScreen() {
  const [currentStep, setCurrentStep] = useState<Step>(1);
  const [formData, setFormData] = useState({
    email: '',
    password: '',
    confirmPassword: '',
    firstName: '',
    lastName: '',
    phone: '',
    country: 'South Africa',
    role: 'guest' as 'guest' | 'host',
  });
  const [errors, setErrors] = useState<Record<string, string>>({});
  const [loading, setLoading] = useState(false);
  const [focusedInput, setFocusedInput] = useState<string | null>(null);
  const [showPassword, setShowPassword] = useState(false);
  const [showConfirmPassword, setShowConfirmPassword] = useState(false);
  const [showCountryPicker, setShowCountryPicker] = useState(false);
  const { register } = useAuth();
  const router = useRouter();
  const insets = useSafeAreaInsets();

  const validateStep = (step: Step): boolean => {
    const newErrors: Record<string, string> = {};

    if (step === 1) {
      if (!formData.email) newErrors.email = 'Email is required';
      else if (!/\S+@\S+\.\S+/.test(formData.email)) newErrors.email = 'Invalid email format';
      
      if (!formData.password) newErrors.password = 'Password is required';
      else if (formData.password.length < 8) newErrors.password = 'Password must be at least 8 characters';
      
      if (formData.password !== formData.confirmPassword) {
        newErrors.confirmPassword = 'Passwords do not match';
      }
    } else if (step === 2) {
      if (!formData.firstName) newErrors.firstName = 'First name is required';
      if (!formData.lastName) newErrors.lastName = 'Last name is required';
      if (!formData.phone) newErrors.phone = 'Phone number is required';
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleNext = () => {
    if (validateStep(currentStep)) {
      setCurrentStep((prev) => (prev + 1) as Step);
    }
  };

  const handleBack = () => {
    setCurrentStep((prev) => (prev - 1) as Step);
  };

  const handleRegister = async () => {
    if (!validateStep(currentStep)) return;

    try {
      setLoading(true);
      await register({
        email: formData.email.trim().toLowerCase(),
        password: formData.password,
        first_name: formData.firstName.trim(),
        last_name: formData.lastName.trim(),
        phone_number: formData.phone,
        country_of_residence: formData.country,
        role: formData.role,
      });
      // Redirect based on selected role
      if (formData.role === 'host') {
        router.replace('/(tabs)/host');
      } else {
        router.replace('/(tabs)/dashboard');
      }
    } catch (error: any) {
      console.error('Registration failed:', error);
      const errorMessage = error?.response?.data?.email?.[0] || 
                          error?.response?.data?.detail ||
                          'Please check your details and try again.';
      Alert.alert('Registration Failed', errorMessage);
    } finally {
      setLoading(false);
    }
  };

  const getStepTitle = () => {
    switch (currentStep) {
      case 1: return 'Create Account';
      case 2: return 'Personal Info';
      case 3: return 'Almost Done!';
    }
  };

  const getStepSubtitle = () => {
    switch (currentStep) {
      case 1: return 'Enter your email and create a password';
      case 2: return 'Tell us a bit about yourself';
      case 3: return 'Choose how you want to use StayAfrica';
    }
  };

  // Enhanced Input Component
  const InputField = ({ 
    label, 
    icon, 
    field, 
    placeholder, 
    keyboardType = 'default',
    secureTextEntry = false,
    showToggle = false,
    toggleState = false,
    onToggle = () => {},
  }: {
    label: string;
    icon: string;
    field: string;
    placeholder: string;
    keyboardType?: any;
    secureTextEntry?: boolean;
    showToggle?: boolean;
    toggleState?: boolean;
    onToggle?: () => void;
  }) => (
    <View className="mb-5">
      <View className="flex-row items-center mb-2">
        <Ionicons name={icon as any} size={18} color="#3A5C50" />
        <Text className="ml-2 text-sm font-semibold text-forest">{label}</Text>
      </View>
      <View 
        className={`flex-row items-center rounded-xl bg-white px-4 border-2 ${
          errors[field] ? 'border-red-400' : focusedInput === field ? 'border-gold' : 'border-sand-200'
        }`}
      >
        <TextInput
          className="flex-1 py-4 text-base text-forest"
          placeholder={placeholder}
          placeholderTextColor="#94a3b8"
          value={(formData as any)[field]}
          onChangeText={(value) => {
            setFormData((prev) => ({ ...prev, [field]: value }));
            if (errors[field]) setErrors((prev) => ({ ...prev, [field]: '' }));
          }}
          onFocus={() => setFocusedInput(field)}
          onBlur={() => setFocusedInput(null)}
          editable={!loading}
          keyboardType={keyboardType}
          autoCapitalize={field === 'email' ? 'none' : 'words'}
          secureTextEntry={secureTextEntry && !toggleState}
          blurOnSubmit={false}
          returnKeyType="next"
        />
        {showToggle && (
          <TouchableOpacity onPress={onToggle}>
            <Ionicons 
              name={toggleState ? "eye-outline" : "eye-off-outline"} 
              size={22} 
              color="#94a3b8" 
            />
          </TouchableOpacity>
        )}
      </View>
      {errors[field] && (
        <View className="flex-row items-center mt-2">
          <Ionicons name="alert-circle" size={16} color="#ef4444" />
          <Text className="ml-1 text-red-500 text-sm">{errors[field]}</Text>
        </View>
      )}
    </View>
  );

  const renderStepContent = () => {
    switch (currentStep) {
      case 1:
        return (
          <>
            <InputField 
              label="Email Address" 
              icon="mail-outline" 
              field="email" 
              placeholder="you@example.com"
              keyboardType="email-address"
            />
            <InputField 
              label="Password" 
              icon="lock-closed-outline" 
              field="password" 
              placeholder="Create a strong password"
              secureTextEntry
              showToggle
              toggleState={showPassword}
              onToggle={() => setShowPassword(!showPassword)}
            />
            <InputField 
              label="Confirm Password" 
              icon="lock-closed-outline" 
              field="confirmPassword" 
              placeholder="Confirm your password"
              secureTextEntry
              showToggle
              toggleState={showConfirmPassword}
              onToggle={() => setShowConfirmPassword(!showConfirmPassword)}
            />
          </>
        );

      case 2:
        return (
          <>
            <InputField 
              label="First Name" 
              icon="person-outline" 
              field="firstName" 
              placeholder="Enter your first name"
            />
            <InputField 
              label="Last Name" 
              icon="person-outline" 
              field="lastName" 
              placeholder="Enter your last name"
            />
            <InputField 
              label="Phone Number" 
              icon="call-outline" 
              field="phone" 
              placeholder="+27 12 345 6789"
              keyboardType="phone-pad"
            />
          </>
        );

      case 3:
        return (
          <>
            {/* Country */}
            <View className="mb-6">
              <View className="flex-row items-center mb-2">
                <Ionicons name="location-outline" size={18} color="#3A5C50" />
                <Text className="ml-2 text-sm font-semibold text-forest">Country</Text>
              </View>
              <TouchableOpacity
                onPress={() => setShowCountryPicker(true)}
                disabled={loading}
                className="flex-row items-center rounded-xl bg-white px-4 border-2 border-sand-200"
                activeOpacity={0.8}
              >
                <Text className="flex-1 py-4 text-base text-forest">
                  {formData.country || 'Select your country'}
                </Text>
                <Ionicons name="chevron-down" size={22} color="#94a3b8" />
              </TouchableOpacity>
            </View>

            {/* Role Selection */}
            <View className="mb-4">
              <View className="flex-row items-center mb-3">
                <Ionicons name="people-outline" size={18} color="#3A5C50" />
                <Text className="ml-2 text-sm font-semibold text-forest">How will you use StayAfrica?</Text>
              </View>
              <View className="flex-row gap-3">
                <TouchableOpacity
                  className={`flex-1 rounded-2xl overflow-hidden border-2 ${
                    formData.role === 'guest' ? 'border-gold' : 'border-sand-200'
                  }`}
                  onPress={() => setFormData({ ...formData, role: 'guest' })}
                  activeOpacity={0.8}
                >
                  <View 
                    className={`p-5 items-center ${formData.role === 'guest' ? 'bg-gold/10' : 'bg-white'}`}
                  >
                    <View className={`w-14 h-14 rounded-full items-center justify-center mb-3 ${
                      formData.role === 'guest' ? 'bg-gold/20' : 'bg-sand-100'
                    }`}>
                      <Ionicons 
                        name="bed-outline" 
                        size={26} 
                        color={formData.role === 'guest' ? '#D9B168' : '#94a3b8'} 
                      />
                    </View>
                    <Text className={`font-bold text-base mb-1 ${
                      formData.role === 'guest' ? 'text-forest' : 'text-sand-500'
                    }`}>
                      Book Stays
                    </Text>
                    <Text className={`text-xs text-center ${
                      formData.role === 'guest' ? 'text-forest/70' : 'text-sand-400'
                    }`}>
                      Find amazing places
                    </Text>
                  </View>
                </TouchableOpacity>

                <TouchableOpacity
                  className={`flex-1 rounded-2xl overflow-hidden border-2 ${
                    formData.role === 'host' ? 'border-gold' : 'border-sand-200'
                  }`}
                  onPress={() => setFormData({ ...formData, role: 'host' })}
                  activeOpacity={0.8}
                >
                  <View 
                    className={`p-5 items-center ${formData.role === 'host' ? 'bg-gold/10' : 'bg-white'}`}
                  >
                    <View className={`w-14 h-14 rounded-full items-center justify-center mb-3 ${
                      formData.role === 'host' ? 'bg-gold/20' : 'bg-sand-100'
                    }`}>
                      <Ionicons 
                        name="home-outline" 
                        size={26} 
                        color={formData.role === 'host' ? '#D9B168' : '#94a3b8'} 
                      />
                    </View>
                    <Text className={`font-bold text-base mb-1 ${
                      formData.role === 'host' ? 'text-forest' : 'text-sand-500'
                    }`}>
                      Host Properties
                    </Text>
                    <Text className={`text-xs text-center ${
                      formData.role === 'host' ? 'text-forest/70' : 'text-sand-400'
                    }`}>
                      Share your space
                    </Text>
                  </View>
                </TouchableOpacity>
              </View>
            </View>
          </>
        );
    }
  };

  return (
    <View className="flex-1 bg-sand-100">
      {/* Background gradient */}
      <LinearGradient
        colors={['#122F26', '#1d4a3d', '#F4F1EA']}
        locations={[0, 0.25, 0.5]}
        start={{ x: 0, y: 0 }}
        end={{ x: 0, y: 1 }}
        style={{ position: 'absolute', top: 0, left: 0, right: 0, bottom: 0 }}
      />

      <KeyboardAvoidingView 
        behavior={Platform.OS === 'ios' ? 'padding' : 'height'}
        className="flex-1"
      >
        <ScrollView 
          className="flex-1"
          contentContainerStyle={{ 
            flexGrow: 1,
            paddingTop: insets.top + 10,
            paddingBottom: insets.bottom + 20,
          }}
          showsVerticalScrollIndicator={false}
          keyboardShouldPersistTaps="always"
          keyboardDismissMode="none"
        >
          <View className="flex-1 px-6">
            {/* Header */}
            <View className="flex-row items-center mb-6">
              <TouchableOpacity 
                onPress={() => currentStep > 1 ? handleBack() : router.back()}
                className="w-10 h-10 rounded-full bg-white/20 items-center justify-center"
              >
                <Ionicons name="chevron-back" size={24} color="#fff" />
              </TouchableOpacity>
              <View className="flex-1 items-center mr-10">
                <Image
                  source={require('@/../assets/logo.png')}
                  style={{ width: 50, height: 50 }}
                  resizeMode="contain"
                />
              </View>
            </View>

            {/* Title */}
            <View className="items-center mb-2">
              <Text className="text-3xl font-black text-white text-center tracking-tight">
                {getStepTitle()}
              </Text>
              <Text className="text-base text-white/80 text-center mt-1">
                {getStepSubtitle()}
              </Text>
            </View>

            {/* Step Indicator */}
            <AnimatedStepIndicator currentStep={currentStep} steps={3} />

            {/* Form Container */}
            <View 
              className="rounded-3xl overflow-hidden bg-sand-50 p-6 mb-6"
              style={{
                shadowColor: '#000',
                shadowOffset: { width: 0, height: 10 },
                shadowOpacity: 0.1,
                shadowRadius: 20,
                elevation: 8,
              }}
            >
              {renderStepContent()}

              {/* Navigation Buttons */}
              <View className="flex-row gap-3 mt-6">
                {currentStep > 1 && (
                  <TouchableOpacity
                    onPress={handleBack}
                    disabled={loading}
                    className="flex-1 py-4 rounded-xl border-2 border-sand-300 bg-white"
                    activeOpacity={0.8}
                  >
                    <Text className="text-forest font-semibold text-center text-base">Back</Text>
                  </TouchableOpacity>
                )}
                
                <TouchableOpacity
                  onPress={currentStep === 3 ? handleRegister : handleNext}
                  disabled={loading}
                  activeOpacity={0.9}
                  className={currentStep === 1 ? 'w-full' : 'flex-1'}
                >
                  <LinearGradient
                    colors={loading ? ['#e5d9c3', '#d4c4a8'] : ['#D9B168', '#c9a158']}
                    start={{ x: 0, y: 0 }}
                    end={{ x: 1, y: 0 }}
                    style={{ paddingVertical: 16, borderRadius: 12 }}
                  >
                    <View className="flex-row justify-center items-center">
                      {loading ? (
                        <ActivityIndicator size="small" color="#122F26" />
                      ) : (
                        <>
                          <Text className="font-bold text-center text-base text-forest">
                            {currentStep === 3 ? 'Create Account' : 'Continue'}
                          </Text>
                          {currentStep < 3 && (
                            <Ionicons name="arrow-forward" size={20} color="#122F26" style={{ marginLeft: 8 }} />
                          )}
                        </>
                      )}
                    </View>
                  </LinearGradient>
                </TouchableOpacity>
              </View>
            </View>

            {/* Already have account */}
            <View className="items-center">
              <Text className="text-forest/70 text-sm mb-2">
                Already have an account?
              </Text>
              <TouchableOpacity 
                onPress={() => router.back()}
                activeOpacity={0.7}
                className="px-6 py-2 rounded-full bg-forest/10"
              >
                <Text className="text-forest font-bold text-base">
                  Sign in instead
                </Text>
              </TouchableOpacity>
            </View>
          </View>
        </ScrollView>
      </KeyboardAvoidingView>

      <Modal
        visible={showCountryPicker}
        animationType="slide"
        transparent={true}
        onRequestClose={() => setShowCountryPicker(false)}
      >
        <View className="flex-1 bg-black/40 justify-end">
          <View className="bg-white rounded-t-3xl pt-6 pb-8" style={{ paddingBottom: insets.bottom + 32 }}>
            <View className="px-6 pb-4 border-b border-sand-100 flex-row items-center justify-between">
              <Text className="text-xl font-bold text-forest">Select Country</Text>
              <TouchableOpacity onPress={() => setShowCountryPicker(false)}>
                <Ionicons name="close" size={26} color="#3A5C50" />
              </TouchableOpacity>
            </View>

            <ScrollView className="px-6 pt-4" showsVerticalScrollIndicator={false} keyboardShouldPersistTaps="handled">
              {COUNTRIES.map((country) => (
                <TouchableOpacity
                  key={country}
                  onPress={() => {
                    setFormData({ ...formData, country });
                    setShowCountryPicker(false);
                  }}
                  className={`py-3 border-b border-sand-100 ${formData.country === country ? 'bg-gold/10' : ''}`}
                >
                  <Text className={`text-base ${formData.country === country ? 'text-forest font-semibold' : 'text-moss'}`}>
                    {country}
                  </Text>
                </TouchableOpacity>
              ))}
            </ScrollView>
          </View>
        </View>
      </Modal>
    </View>
  );
}