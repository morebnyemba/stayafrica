import { useState, useRef, useEffect } from 'react';
import { 
  Text, 
  View, 
  ScrollView, 
  TextInput, 
  TouchableOpacity, 
  Image,
  Animated,
  Dimensions,
  KeyboardAvoidingView,
  Platform,
  Alert
} from 'react-native';
import { LinearGradient } from 'expo-linear-gradient';
import { useRouter } from 'expo-router';
import { useAuth } from '@/context/auth-context';
import { Ionicons, MaterialIcons, FontAwesome5 } from '@expo/vector-icons';
 

const { width, height } = Dimensions.get('window');

type Step = 1 | 2 | 3;

// Animated Step Indicator
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
    <View className="relative mb-8">
      {/* Background track */}
      <View className="h-2 bg-gray-200/50 rounded-full overflow-hidden">
        {/* Progress fill */}
        <Animated.View 
          className="absolute h-full bg-gradient-to-r from-primary-500 to-secondary-500 rounded-full"
          style={{ width: progressWidth }}
        />
      </View>
      
      {/* Step circles */}
      <View className="absolute w-full flex-row justify-between -top-2">
        {[1, 2, 3].map((step) => (
          <View key={step} className="items-center">
            <Animated.View 
              className={`w-10 h-10 rounded-full items-center justify-center shadow-lg border-4 ${
                step <= currentStep
                  ? 'bg-white border-primary-500'
                  : 'bg-white/80 border-gray-200'
              }`}
              style={{
                shadowColor: step <= currentStep ? '#8b5cf6' : '#9ca3af',
                shadowOpacity: 0.3,
                shadowRadius: 8,
                elevation: 8,
              }}
            >
              {step < currentStep ? (
                <Ionicons name="checkmark" size={20} color="#8b5cf6" />
              ) : (
                <Text className={`font-bold text-lg ${step === currentStep ? 'text-primary-600' : 'text-gray-400'}`}>
                  {step}
                </Text>
              )}
            </Animated.View>
            <Text className={`mt-2 text-xs font-medium ${step <= currentStep ? 'text-primary-700' : 'text-gray-500'}`}>
              {step === 1 ? 'Account' : step === 2 ? 'Personal' : 'Details'}
            </Text>
          </View>
        ))}
      </View>
    </View>
  );
};

// Floating Particles Background
const FloatingParticles = ({ count = 20 }) => {
  const particles = Array.from({ length: count });
  
  return (
    <View className="absolute inset-0 overflow-hidden pointer-events-none">
      {particles.map((_, index) => {
        const size = Math.random() * 8 + 2;
        const left = Math.random() * width;
        const top = Math.random() * height;
        const delay = Math.random() * 2000;
        
        const animatedValue = new Animated.Value(0);
        
        useEffect(() => {
          const animate = () => {
            Animated.loop(
              Animated.sequence([
                Animated.timing(animatedValue, {
                  toValue: 1,
                  duration: Math.random() * 15000 + 10000,
                  delay,
                  useNativeDriver: true,
                }),
                Animated.timing(animatedValue, {
                  toValue: 0,
                  duration: Math.random() * 15000 + 10000,
                  useNativeDriver: true,
                }),
              ])
            ).start();
          };
          
          animate();
        }, []);

        const translateY = animatedValue.interpolate({
          inputRange: [0, 1],
          outputRange: [0, -100],
        });

        const opacity = animatedValue.interpolate({
          inputRange: [0, 1],
          outputRange: [0.1, 0.3],
        });

        return (
          <Animated.View
            key={index}
            className="absolute rounded-full"
            style={{
              width: size,
              height: size,
              left,
              top,
              backgroundColor: index % 3 === 0 ? '#8b5cf6' : index % 3 === 1 ? '#ec4899' : '#3b82f6',
              transform: [{ translateY }],
              opacity,
            }}
          />
        );
      })}
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
  const { register } = useAuth();
  const router = useRouter();

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
        email: formData.email,
        password: formData.password,
        first_name: formData.firstName,
        last_name: formData.lastName,
        phone_number: formData.phone,
        country_of_residence: formData.country,
      });
      router.replace('/(tabs)/explore');
    } catch (error) {
      console.error('Registration failed:', error);
      Alert.alert('Registration Failed', 'Please check your details and try again.');
    } finally {
      setLoading(false);
    }
  };

  const getStepTitle = () => {
    switch (currentStep) {
      case 1: return 'Create Your Account';
      case 2: return 'Personal Information';
      case 3: return 'Complete Your Profile';
    }
  };

  const renderStepContent = () => {
    switch (currentStep) {
      case 1:
        return (
          <>
            <View className="mb-6">
              <View className="flex-row items-center mb-2">
                <Ionicons name="mail-outline" size={20} color="#8b5cf6" />
                <Text className="ml-2 text-sm font-semibold text-primary-700">Email Address</Text>
              </View>
              <View className={`relative ${errors.email ? 'border-red-300' : focusedInput === 'email' ? 'border-primary-600' : 'border-primary-200'} border-2 rounded-2xl overflow-hidden bg-sand-50`}>
                <TextInput
                  className="w-full px-4 py-4 text-lg"
                  placeholder="you@example.com"
                  placeholderTextColor="#94a3b8"
                  value={formData.email}
                  onChangeText={(value) => {
                    setFormData({ ...formData, email: value });
                    if (errors.email) setErrors({ ...errors, email: '' });
                  }}
                  onFocus={() => setFocusedInput('email')}
                  onBlur={() => setFocusedInput(null)}
                  editable={!loading}
                  keyboardType="email-address"
                  autoCapitalize="none"
                />
              </View>
              {errors.email && (
                <View className="flex-row items-center mt-2">
                  <Ionicons name="alert-circle" size={16} color="#ef4444" />
                  <Text className="ml-1 text-red-600 text-sm">{errors.email}</Text>
                </View>
              )}
            </View>

            <View className="mb-6">
              <View className="flex-row items-center mb-2">
                <Ionicons name="lock-closed-outline" size={20} color="#8b5cf6" />
                <Text className="ml-2 text-sm font-semibold text-primary-700">Password</Text>
              </View>
              <View className={`relative ${errors.password ? 'border-red-300' : focusedInput === 'password' ? 'border-primary-600' : 'border-primary-200'} border-2 rounded-2xl overflow-hidden bg-sand-50`}>
                <TextInput
                  className="w-full px-4 py-4 text-lg"
                  placeholder="Create a password"
                  placeholderTextColor="#94a3b8"
                  value={formData.password}
                  onChangeText={(value) => {
                    setFormData({ ...formData, password: value });
                    if (errors.password) setErrors({ ...errors, password: '' });
                  }}
                  onFocus={() => setFocusedInput('password')}
                  onBlur={() => setFocusedInput(null)}
                  secureTextEntry
                  editable={!loading}
                />
              </View>
              {errors.password && (
                <View className="flex-row items-center mt-2">
                  <Ionicons name="alert-circle" size={16} color="#ef4444" />
                  <Text className="ml-1 text-red-600 text-sm">{errors.password}</Text>
                </View>
              )}
            </View>

            <View className="mb-6">
              <View className="flex-row items-center mb-2">
                <Ionicons name="lock-closed-outline" size={20} color="#8b5cf6" />
                <Text className="ml-2 text-sm font-semibold text-primary-700">Confirm Password</Text>
              </View>
              <View className={`relative ${errors.confirmPassword ? 'border-red-300' : focusedInput === 'confirmPassword' ? 'border-primary-600' : 'border-primary-200'} border-2 rounded-2xl overflow-hidden bg-sand-50`}>
                <TextInput
                  className="w-full px-4 py-4 text-lg"
                  placeholder="Confirm your password"
                  placeholderTextColor="#94a3b8"
                  value={formData.confirmPassword}
                  onChangeText={(value) => {
                    setFormData({ ...formData, confirmPassword: value });
                    if (errors.confirmPassword) setErrors({ ...errors, confirmPassword: '' });
                  }}
                  onFocus={() => setFocusedInput('confirmPassword')}
                  onBlur={() => setFocusedInput(null)}
                  secureTextEntry
                  editable={!loading}
                />
              </View>
              {errors.confirmPassword && (
                <View className="flex-row items-center mt-2">
                  <Ionicons name="alert-circle" size={16} color="#ef4444" />
                  <Text className="ml-1 text-red-600 text-sm">{errors.confirmPassword}</Text>
                </View>
              )}
            </View>
          </>
        );

      case 2:
        return (
          <>
            <View className="mb-6">
              <View className="flex-row items-center mb-2">
                <FontAwesome5 name="user" size={16} color="#8b5cf6" />
                <Text className="ml-2 text-sm font-semibold text-primary-700">First Name</Text>
              </View>
              <View className={`relative ${errors.firstName ? 'border-red-300' : focusedInput === 'firstName' ? 'border-primary-600' : 'border-primary-200'} border-2 rounded-2xl overflow-hidden bg-sand-50`}>
                <TextInput
                  className="w-full px-4 py-4 text-lg"
                  placeholder="Enter your first name"
                  placeholderTextColor="#94a3b8"
                  value={formData.firstName}
                  onChangeText={(value) => {
                    setFormData({ ...formData, firstName: value });
                    if (errors.firstName) setErrors({ ...errors, firstName: '' });
                  }}
                  onFocus={() => setFocusedInput('firstName')}
                  onBlur={() => setFocusedInput(null)}
                  editable={!loading}
                />
              </View>
              {errors.firstName && (
                <View className="flex-row items-center mt-2">
                  <Ionicons name="alert-circle" size={16} color="#ef4444" />
                  <Text className="ml-1 text-red-600 text-sm">{errors.firstName}</Text>
                </View>
              )}
            </View>

            <View className="mb-6">
              <View className="flex-row items-center mb-2">
                <FontAwesome5 name="user" size={16} color="#8b5cf6" />
                <Text className="ml-2 text-sm font-semibold text-primary-700">Last Name</Text>
              </View>
              <View className={`relative ${errors.lastName ? 'border-red-300' : focusedInput === 'lastName' ? 'border-primary-600' : 'border-primary-200'} border-2 rounded-2xl overflow-hidden bg-sand-50`}>
                <TextInput
                  className="w-full px-4 py-4 text-lg"
                  placeholder="Enter your last name"
                  placeholderTextColor="#94a3b8"
                  value={formData.lastName}
                  onChangeText={(value) => {
                    setFormData({ ...formData, lastName: value });
                    if (errors.lastName) setErrors({ ...errors, lastName: '' });
                  }}
                  onFocus={() => setFocusedInput('lastName')}
                  onBlur={() => setFocusedInput(null)}
                  editable={!loading}
                />
              </View>
              {errors.lastName && (
                <View className="flex-row items-center mt-2">
                  <Ionicons name="alert-circle" size={16} color="#ef4444" />
                  <Text className="ml-1 text-red-600 text-sm">{errors.lastName}</Text>
                </View>
              )}
            </View>

            <View className="mb-6">
              <View className="flex-row items-center mb-2">
                <Ionicons name="call-outline" size={20} color="#8b5cf6" />
                <Text className="ml-2 text-sm font-semibold text-primary-700">Phone Number</Text>
              </View>
              <View className={`relative ${errors.phone ? 'border-red-300' : focusedInput === 'phone' ? 'border-primary-600' : 'border-primary-200'} border-2 rounded-2xl overflow-hidden bg-sand-50`}>
                <TextInput
                  className="w-full px-4 py-4 text-lg"
                  placeholder="+27 12 345 6789"
                  placeholderTextColor="#94a3b8"
                  value={formData.phone}
                  onChangeText={(value) => {
                    setFormData({ ...formData, phone: value });
                    if (errors.phone) setErrors({ ...errors, phone: '' });
                  }}
                  onFocus={() => setFocusedInput('phone')}
                  onBlur={() => setFocusedInput(null)}
                  keyboardType="phone-pad"
                  editable={!loading}
                />
              </View>
              {errors.phone && (
                <View className="flex-row items-center mt-2">
                  <Ionicons name="alert-circle" size={16} color="#ef4444" />
                  <Text className="ml-1 text-red-600 text-sm">{errors.phone}</Text>
                </View>
              )}
            </View>
          </>
        );

      case 3:
        return (
          <>
            <View className="mb-8">
              <View className="flex-row items-center mb-2">
                <MaterialIcons name="location-on" size={20} color="#8b5cf6" />
                <Text className="ml-2 text-sm font-semibold text-primary-700">Country of Residence</Text>
              </View>
              <View className="relative border-2 border-primary-200 rounded-2xl overflow-hidden bg-sand-50">
                <TextInput
                  className="w-full px-4 py-4 text-lg"
                  placeholder="Select your country"
                  placeholderTextColor="#94a3b8"
                  value={formData.country}
                  onChangeText={(value) => setFormData({ ...formData, country: value })}
                  editable={!loading}
                />
                <TouchableOpacity className="absolute right-4 top-4">
                  <Ionicons name="chevron-down" size={24} color="#8b5cf6" />
                </TouchableOpacity>
              </View>
            </View>

            <View className="mb-8">
              <View className="flex-row items-center mb-4">
                <Ionicons name="person-outline" size={20} color="#8b5cf6" />
                <Text className="ml-2 text-sm font-semibold text-primary-700">What would you like to do?</Text>
              </View>
              <View className="flex-row gap-4">
                <TouchableOpacity
                  className={`flex-1 rounded-2xl overflow-hidden border-2 ${formData.role === 'guest' ? 'border-primary-500' : 'border-gray-200'}`}
                  onPress={() => setFormData({ ...formData, role: 'guest' })}
                  activeOpacity={0.8}
                >
                  <LinearGradient
                    colors={formData.role === 'guest' 
                      ? ['#e3ece7', '#f2f5f4'] /* primary-100 -> primary-50 */
                      : ['#ffffff', '#f4f1ea'] /* white -> sand-100 */
                    }
                    style={{ paddingHorizontal: 24, paddingVertical: 24, alignItems: 'center' }}
                  >
                    <View className={`w-16 h-16 rounded-full items-center justify-center mb-3 ${formData.role === 'guest' ? 'bg-primary-100' : 'bg-gray-100'}`}>
                      <Ionicons name="bed-outline" size={28} color={formData.role === 'guest' ? '#3a5c50' : '#6b7280'} />
                    </View>
                    <Text className={`font-bold text-lg mb-1 ${formData.role === 'guest' ? 'text-primary-700' : 'text-gray-700'}`}>
                      Book Stays
                    </Text>
                    <Text className={`text-sm text-center ${formData.role === 'guest' ? 'text-primary-600' : 'text-gray-600'}`}>
                      Discover amazing accommodations
                    </Text>
                  </LinearGradient>
                </TouchableOpacity>

                <TouchableOpacity
                  className={`flex-1 rounded-2xl overflow-hidden border-2 ${formData.role === 'host' ? 'border-secondary-500' : 'border-gray-200'}`}
                  onPress={() => setFormData({ ...formData, role: 'host' })}
                  activeOpacity={0.8}
                >
                  <LinearGradient
                    colors={formData.role === 'host' 
                      ? ['#f8e8c4', '#f3d99c'] /* secondary-100 -> secondary-200 */
                      : ['#ffffff', '#f4f1ea'] /* white -> sand-100 */
                    }
                    style={{ paddingHorizontal: 24, paddingVertical: 24, alignItems: 'center' }}
                  >
                    <View className={`w-16 h-16 rounded-full items-center justify-center mb-3 ${formData.role === 'host' ? 'bg-secondary-100' : 'bg-gray-100'}`}>
                      <Ionicons name="home-outline" size={28} color={formData.role === 'host' ? '#d9b168' : '#6b7280'} />
                    </View>
                    <Text className={`font-bold text-lg mb-1 ${formData.role === 'host' ? 'text-secondary-700' : 'text-gray-700'}`}>
                      Host Properties
                    </Text>
                    <Text className={`text-sm text-center ${formData.role === 'host' ? 'text-secondary-600' : 'text-gray-600'}`}>
                      Share your space with guests
                    </Text>
                  </LinearGradient>
                </TouchableOpacity>
              </View>
            </View>
          </>
        );
    }
  };

  return (
    <KeyboardAvoidingView 
      behavior={Platform.OS === 'ios' ? 'padding' : 'height'}
      className="flex-1 bg-gradient-to-b from-primary-50 to-white"
    >
      {/* Background Elements */}
      <FloatingParticles />
      
      <LinearGradient
        colors={['#F4F1EA', '#FFFFFF']} /* sand-100 -> white */
        start={{ x: 0, y: 0 }}
        end={{ x: 0, y: 1 }}
        style={{
          position: 'absolute',
          top: 0,
          left: 0,
          right: 0,
          height: height * 0.4,
        }}
      />

      <ScrollView 
        className="flex-1"
        contentContainerStyle={{ flexGrow: 1 }}
        showsVerticalScrollIndicator={false}
      >
        <View className="flex-1 px-6 py-8">
          {/* Header */}
          <View className="items-center mb-8">
            <View className="relative mb-6">
              <View className="absolute -inset-4 bg-primary-500/10 rounded-full blur-xl" />
              <TouchableOpacity 
                onPress={() => router.back()}
                className="absolute -left-12 top-2 w-10 h-10 rounded-full bg-white/80 items-center justify-center shadow-lg z-10"
              >
                <Ionicons name="chevron-back" size={24} color="#8b5cf6" />
              </TouchableOpacity>
              <View className="relative bg-white/80 rounded-3xl p-4 shadow-2xl shadow-primary-500/20">
                <Image
                  source={require('@/../assets/logo.png')}
                  style={{ width: 80, height: 80 }}
                  resizeMode="contain"
                />
              </View>
            </View>

            <Text className="text-3xl font-black text-primary-900 mb-2 text-center tracking-tight">
              {getStepTitle()}
            </Text>
            <Text className="text-base text-primary-600/80 text-center max-w-xs">
              {currentStep === 1 && 'Create your StayAfrica account to get started'}
              {currentStep === 2 && 'Tell us a bit about yourself'}
              {currentStep === 3 && 'Complete your profile setup'}
            </Text>
          </View>

          {/* Step Indicator */}
          <AnimatedStepIndicator currentStep={currentStep} steps={3} />

          {/* Form Container */}
            <LinearGradient
              colors={['#ffffff', '#ffffff']}
                style={{ padding: 24, borderRadius: 24, marginBottom: 24 }}
            >
              {renderStepContent()}

              {/* Navigation Buttons */}
              <View className="flex-row gap-3 mt-8">
                {currentStep > 1 && (
                  <TouchableOpacity
                    onPress={handleBack}
                    disabled={loading}
                    className="flex-1 py-4 rounded-2xl border-2 border-gray-300 bg-white/80"
                    activeOpacity={0.8}
                  >
                    <Text className="text-gray-700 font-semibold text-center text-base">Back</Text>
                  </TouchableOpacity>
                )}
                
                <TouchableOpacity
                  onPress={currentStep === 3 ? handleRegister : handleNext}
                  disabled={loading}
                  activeOpacity={0.9}
                  className={`${currentStep === 1 ? 'w-full' : 'flex-1'}`}
                >
                  <LinearGradient
                    colors={['#d9b168', '#bea04f']} // Use gold for all steps
                    start={{ x: 0, y: 0 }}
                    end={{ x: 1, y: 0 }}
                    style={{ paddingVertical: 16, borderRadius: 12 }}
                  >
                    <View className="flex-row justify-center items-center">
                      {loading && (
                        <Animated.View 
                          className="w-5 h-5 border-2 border-forest border-t-transparent rounded-full mr-3"
                          style={{
                            transform: [{ rotate: loading ? '0deg' : '360deg' }]
                          }}
                        />
                      )}
                      <Text className={`font-bold text-center text-base text-forest`}>
                        {loading 
                          ? 'Please wait...' 
                          : currentStep === 3 
                          ? 'Complete Registration' 
                          : 'Continue'
                        }
                      </Text>
                      {!loading && currentStep < 3 && (
                        <Ionicons name="arrow-forward" size={20} color="#122f26" className="ml-2" />
                      )}
                    </View>
                  </LinearGradient>
                </TouchableOpacity>
              </View>
            </LinearGradient>

          {/* Already have account */}
          <View className="items-center mt-6">
            <Text className="text-gray-600 text-sm mb-1">
              Already have an account?
            </Text>
            <TouchableOpacity 
              onPress={() => router.back()}
              activeOpacity={0.7}
            >
              <Text className="text-primary-700 font-bold text-base">
                Sign in instead
              </Text>
            </TouchableOpacity>
          </View>

          {/* Bottom decorative element (simplified to avoid SVG gradient conflict) */}
          <View className="mt-10 items-center">
            <View className="h-1 w-14 bg-primary-200 rounded-full" />
          </View>
        </View>
      </ScrollView>
    </KeyboardAvoidingView>
  );
}