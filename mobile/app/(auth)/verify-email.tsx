import { useEffect, useState } from 'react';
import {
  View,
  Text,
  TouchableOpacity,
  KeyboardAvoidingView,
  ScrollView,
  Platform,
  ActivityIndicator,
} from 'react-native';
import { SafeAreaView } from 'react-native-safe-area-context';
import { router, useLocalSearchParams } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { apiClient } from '@/services/api-client';
import { useAuth } from '@/context/auth-context';
import { AppDialog, AppDialogAction } from '@/components/common/AppDialog';

type DialogState = {
  visible: boolean;
  title: string;
  message: string;
  primaryAction: AppDialogAction;
  secondaryAction?: AppDialogAction;
};

export default function VerifyEmailScreen() {
  const params = useLocalSearchParams();
  const email = params.email as string || '';
  const { user, isAuthenticated, refreshUser } = useAuth();
  
  const [resending, setResending] = useState(false);
  const [checkingVerification, setCheckingVerification] = useState(false);
  const [dialog, setDialog] = useState<DialogState>({
    visible: false,
    title: '',
    message: '',
    primaryAction: { label: 'OK' },
  });

  const openDialog = (next: Omit<DialogState, 'visible'>) => {
    setDialog({ visible: true, ...next });
  };

  const checkVerificationStatus = async () => {
    if (!isAuthenticated) {
      return;
    }

    setCheckingVerification(true);
    try {
      const profile = await apiClient.getUserProfile();
      if (profile?.is_verified) {
        await refreshUser();
        openDialog({
          title: 'Email Verified',
          message: 'Your email is verified. Redirecting to dashboard...',
          primaryAction: {
            label: 'Continue',
            onPress: () => router.replace('/(tabs)/dashboard'),
          },
        });
      }
    } catch (error: any) {
      // Ignore transient polling failures to avoid noisy alerts while user is on this screen.
      console.warn('Verification polling failed:', error?.message || error);
    } finally {
      setCheckingVerification(false);
    }
  };

  useEffect(() => {
    if (!isAuthenticated || user?.is_verified) {
      return;
    }

    // Run an immediate check, then poll every 8 seconds while on this screen.
    checkVerificationStatus();
    const interval = setInterval(checkVerificationStatus, 8000);
    return () => clearInterval(interval);
  }, [isAuthenticated, user?.is_verified]);

  const handleResendCode = async () => {
    if (!email) {
      openDialog({
        title: 'Error',
        message: 'Email address is required',
        primaryAction: { label: 'OK' },
      });
      return;
    }

    setResending(true);
    try {
      // resend_verification requires auth, try unauthenticated-friendly approach
      await apiClient.client.post('/users/resend_verification/', { email });
      openDialog({
        title: 'Success',
        message: 'Verification email has been resent. Please check your inbox and spam folder.',
        primaryAction: { label: 'OK' },
      });
    } catch (error: any) {
      console.error('Resend error:', error);
      // If it fails due to auth, show a helpful message
      if (error.response?.status === 401) {
        openDialog({
          title: 'Sign In Required',
          message: 'Please sign in first, then request a new verification email from your profile settings.',
          primaryAction: { label: 'Sign In', onPress: () => router.replace('/(auth)/login') },
        });
      } else {
        openDialog({
          title: 'Error',
          message:
            error.response?.data?.detail ||
            error.response?.data?.error ||
            'Failed to resend email. Please try again.',
          primaryAction: { label: 'OK' },
        });
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

            {/* Verification status */}
            <View className="bg-blue-50 rounded-xl p-4 mb-6">
              <View className="flex-row items-center justify-between">
                <View className="flex-1 pr-3">
                  <Text className="text-blue-900 font-semibold mb-1">
                    Waiting for verification
                  </Text>
                  <Text className="text-blue-700 text-xs">
                    This page checks automatically every few seconds and will open your dashboard once verified.
                  </Text>
                </View>
                {checkingVerification ? (
                  <ActivityIndicator size="small" color="#1d4ed8" />
                ) : null}
              </View>
              <TouchableOpacity
                onPress={checkVerificationStatus}
                disabled={checkingVerification}
                className="mt-3 self-start"
              >
                <Text className="text-blue-700 font-semibold text-sm">
                  {checkingVerification ? 'Checking...' : 'Check Now'}
                </Text>
              </TouchableOpacity>
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
