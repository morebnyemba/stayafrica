import { View, Text, ScrollView, TouchableOpacity, Platform } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';

interface VerificationStepProps {
  number: number;
  title: string;
  description: string;
  isComplete: boolean;
  icon: keyof typeof Ionicons.glyphMap;
}

export default function ProfileVerificationScreen() {
  const router = useRouter();
  const { user } = useAuth();

  const VerificationStep = ({ number, title, description, isComplete, icon }: VerificationStepProps) => (
    <View className="flex-row items-start mb-4">
      <View className={`w-10 h-10 rounded-full items-center justify-center mr-4 ${isComplete ? 'bg-green-500' : 'bg-sand-200'}`}>
        {isComplete ? (
          <Ionicons name="checkmark" size={20} color="#fff" />
        ) : (
          <Text className="text-forest font-bold">{number}</Text>
        )}
      </View>
      <View className="flex-1 bg-white rounded-2xl p-4" style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 2 },
        shadowOpacity: 0.05,
        shadowRadius: 4,
        elevation: 2,
      }}>
        <View className="flex-row items-center mb-2">
          <Ionicons name={icon} size={20} color="#3A5C50" />
          <Text className="text-base font-semibold text-forest ml-2">{title}</Text>
        </View>
        <Text className="text-sm text-moss">{description}</Text>
        {!isComplete && (
          <TouchableOpacity className="mt-3 bg-gold/10 py-2 px-4 rounded-xl self-start">
            <Text className="text-gold font-semibold text-sm">Complete</Text>
          </TouchableOpacity>
        )}
      </View>
    </View>
  );

  // Use is_verified from user context - individual verification steps are placeholders
  // that can be updated when API provides more detailed verification status
  const verificationSteps = [
    {
      title: 'Email Verification',
      description: 'Verify your email address to secure your account',
      isComplete: user?.is_verified || false,
      icon: 'mail' as keyof typeof Ionicons.glyphMap,
    },
    {
      title: 'Phone Verification',
      description: 'Add and verify your phone number for account security',
      isComplete: false, // Placeholder - update when API provides this
      icon: 'call' as keyof typeof Ionicons.glyphMap,
    },
    {
      title: 'Identity Document',
      description: 'Upload a government-issued ID for identity verification',
      isComplete: false, // Placeholder - update when API provides this
      icon: 'card' as keyof typeof Ionicons.glyphMap,
    },
    {
      title: 'Profile Photo',
      description: 'Add a clear profile photo showing your face',
      isComplete: false, // Placeholder - update when API provides this
      icon: 'camera' as keyof typeof Ionicons.glyphMap,
    },
  ];

  const completedSteps = verificationSteps.filter(s => s.isComplete).length;
  const progressPercent = (completedSteps / verificationSteps.length) * 100;

  return (
    <ScrollView className="flex-1 bg-sand-100" showsVerticalScrollIndicator={false}>
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="px-4 pb-8"
        style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
      >
        <View className="flex-row items-center mb-6">
          <TouchableOpacity
            onPress={() => router.back()}
            className="w-10 h-10 rounded-xl items-center justify-center mr-3"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </TouchableOpacity>
          <Text className="text-xl font-bold text-white">Identity Verification</Text>
        </View>

        {/* Progress */}
        <View className="bg-white/10 rounded-2xl p-4">
          <View className="flex-row items-center justify-between mb-3">
            <Text className="text-white font-semibold">Verification Progress</Text>
            <Text className="text-gold font-bold">{completedSteps}/{verificationSteps.length}</Text>
          </View>
          <View className="bg-white/20 h-2 rounded-full overflow-hidden">
            <View 
              className="bg-gold h-full rounded-full" 
              style={{ width: `${progressPercent}%` }} 
            />
          </View>
          {user?.is_verified && (
            <View className="flex-row items-center mt-3">
              <Ionicons name="shield-checkmark" size={18} color="#10B981" />
              <Text className="text-green-400 font-semibold ml-2">Fully Verified</Text>
            </View>
          )}
        </View>
      </LinearGradient>

      {/* Benefits */}
      <View className="px-4 py-6">
        <Text className="text-lg font-bold text-forest mb-4">Why Verify?</Text>
        <View className="bg-blue-50 rounded-2xl p-4 mb-6">
          <View className="flex-row items-center mb-2">
            <Ionicons name="shield-checkmark" size={24} color="#3B82F6" />
            <Text className="text-blue-800 font-semibold ml-2">Trust & Safety</Text>
          </View>
          <Text className="text-blue-700 text-sm">
            Verified users get priority support, access to premium properties, and build trust with hosts.
          </Text>
        </View>

        {/* Verification Steps */}
        <Text className="text-lg font-bold text-forest mb-4">Verification Steps</Text>
        {verificationSteps.map((step, index) => (
          <VerificationStep
            key={index}
            number={index + 1}
            title={step.title}
            description={step.description}
            isComplete={step.isComplete}
            icon={step.icon}
          />
        ))}
      </View>
    </ScrollView>
  );
}
