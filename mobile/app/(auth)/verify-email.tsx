import { useState } from 'react';
import {
  View,
  Text,
  TouchableOpacity,
  KeyboardAvoidingView,
  ScrollView,
  Platform,
  ActivityIndicator,
  Alert,
} from 'react-native';
import { SafeAreaView } from 'react-native-safe-area-context';
import { router, useLocalSearchParams } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { apiClient } from '@/services/api-client';

export default function VerifyEmailScreen() {
  const params = useLocalSearchParams();
  const email = params.email as string || '';
  
  const [resending, setResending] = useState(false);

  const handleResendCode = async () => {
    if (!email) {
      Alert.alert('Error', 'Email address is required');
      return;
    }

    setResending(true);
    try {
      // resend_verification requires auth, try unauthenticated-friendly approach
      await apiClient.client.post('/users/resend_verification/', { email });
      Alert.alert('Success', 'Verification email has been resent. Please check your inbox and spam folder.');
    } catch (error: any) {
      console.error('Resend error:', error);
      // If it fails due to auth, show a helpful message
      if (error.response?.status === 401) {
        Alert.alert(
          'Sign In Required',
          'Please sign in first, then request a new verification email from your profile settings.',
          [{ text: 'Sign In', onPress: () => router.replace('/(auth)/login') }]
        );
      } else {
        Alert.alert(
          'Error',
          error.response?.data?.detail ||
          error.response?.data?.error ||
          'Failed to resend email. Please try again.'
        );
      }
    } finally {
      setResending(false);
    }
  };

  return (
    <SafeAreaView className="flex-1 bg-white">
      <KeyboardAvoidingView
        behavior={Platform.OS === 'ios' ? 'padding' : 'height'}
        className="flex-1"
      >
        <ScrollView
          className="flex-1"
          keyboardShouldPersistTaps="handled"
          contentContainerStyle={{ flexGrow: 1 }}
        >
          {/* Header */}
          <View className="px-6 pt-4 pb-8">
            <TouchableOpacity
              onPress={() => router.back()}
              className="w-10 h-10 items-center justify-center"
            >
              <Ionicons name="arrow-back" size={24} color="#000" />
            </TouchableOpacity>
          </View>

          {/* Content */}
          <View className="flex-1 px-6">
            <View className="mb-8">
              <View className="w-16 h-16 bg-emerald-100 rounded-full items-center justify-center mb-6">
                <Ionicons name="mail-outline" size={32} color="#059669" />
              </View>
              
              <Text className="text-3xl font-bold text-gray-900 mb-3">
                Verify Your Email
              </Text>
              
              <Text className="text-base text-gray-600">
                We've sent a verification link to{' '}
                <Text className="font-semibold text-gray-900">{email || 'your email'}</Text>.
                {'\n\n'}Please check your inbox and click the link to verify your account.
              </Text>
            </View>

            {/* Instructions */}
            <View className="bg-emerald-50 rounded-xl p-4 mb-8">
              <View className="flex-row items-start mb-3">
                <Ionicons name="information-circle" size={20} color="#059669" />
                <Text className="flex-1 text-emerald-800 text-sm ml-2 font-semibold">
                  How to verify:
                </Text>
              </View>
              <View className="ml-7 space-y-2">
                <Text className="text-emerald-700 text-sm">1. Open your email inbox</Text>
                <Text className="text-emerald-700 text-sm">2. Find the email from StayAfrica</Text>
                <Text className="text-emerald-700 text-sm">3. Click the verification link</Text>
                <Text className="text-emerald-700 text-sm">4. Return here and sign in</Text>
              </View>
            </View>

            {/* Continue to Login */}
            <TouchableOpacity
              onPress={() => router.replace('/(auth)/login')}
              className="bg-emerald-600 py-4 rounded-xl mb-6"
            >
              <Text className="text-white text-center font-semibold text-base">
                Continue to Sign In
              </Text>
            </TouchableOpacity>

            {/* Resend Email */}
            <View className="flex-row justify-center items-center mb-4">
              <Text className="text-gray-600">Didn't receive the email? </Text>
              <TouchableOpacity
                onPress={handleResendCode}
                disabled={resending}
              >
                {resending ? (
                  <ActivityIndicator size="small" color="#059669" />
                ) : (
                  <Text className="text-emerald-600 font-semibold">
                    Resend
                  </Text>
                )}
              </TouchableOpacity>
            </View>

            {/* Check spam note */}
            <Text className="text-xs text-gray-400 text-center mb-4">
              Please also check your spam or junk folder
            </Text>

            {/* Change Email */}
            <TouchableOpacity
              onPress={() => router.back()}
              className="py-3"
            >
              <Text className="text-gray-600 text-center">
                Wrong email address? Go back
              </Text>
            </TouchableOpacity>
          </View>
        </ScrollView>
      </KeyboardAvoidingView>
    </SafeAreaView>
  );
}
