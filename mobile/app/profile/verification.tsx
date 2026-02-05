import { View, Text, TouchableOpacity, Platform } from 'react-native';
import { useRouter } from 'expo-router';
import { VerificationWizard } from '@/components/verification/VerificationWizard';

export default function ProfileVerificationScreen() {
  const router = useRouter();

  return <VerificationWizard />;
}
