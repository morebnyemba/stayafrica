import { View, Text, TouchableOpacity, Platform } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { VerificationWizard } from '@/components/verification/VerificationWizard';

export default function HostVerificationScreen() {
  const router = useRouter();
  const { user, isAuthenticated } = useAuth();

  if (!isAuthenticated) {
    return (
      <View className="flex-1 bg-sand-100">
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
        >
          <TouchableOpacity onPress={() => router.back()} className="mb-4">
            <View className="w-10 h-10 rounded-xl items-center justify-center" style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}>
              <Ionicons name="arrow-back" size={24} color="#fff" />
            </View>
          </TouchableOpacity>
          <Text className="text-3xl font-black text-white tracking-tight">
            Verification
          </Text>
        </LinearGradient>
        <View className="flex-1 items-center justify-center px-6">
          <Text className="text-forest text-lg">Please sign in to continue</Text>
        </View>
      </View>
    );
  }

  return <VerificationWizard />;
}
