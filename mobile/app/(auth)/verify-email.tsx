import React, { useState, useRef } from 'react';
import {
  View,
  Text,
  TextInput,
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
  
  const [code, setCode] = useState(['', '', '', '', '', '']);
  const [loading, setLoading] = useState(false);
  const [resending, setResending] = useState(false);
  
  const inputRefs = useRef<(TextInput | null)[]>([]);

  const handleCodeChange = (text: string, index: number) => {
    // Only allow numbers
    const numericText = text.replace(/[^0-9]/g, '');
    
    if (numericText.length > 1) {
      // Handle paste of multiple digits
      const digits = numericText.split('').slice(0, 6);
      const newCode = [...code];
      digits.forEach((digit, i) => {
        if (index + i < 6) {
          newCode[index + i] = digit;
        }
      });
      setCode(newCode);
      
      // Focus on the next empty field or last field
      const nextIndex = Math.min(index + digits.length, 5);
      inputRefs.current[nextIndex]?.focus();
    } else {
      // Handle single digit input
      const newCode = [...code];
      newCode[index] = numericText;
      setCode(newCode);
      
      // Auto-focus next input
      if (numericText && index < 5) {
        inputRefs.current[index + 1]?.focus();
      }
    }
  };

  const handleKeyPress = (e: any, index: number) => {
    if (e.nativeEvent.key === 'Backspace' && !code[index] && index > 0) {
      inputRefs.current[index - 1]?.focus();
    }
  };

  const handleVerify = async () => {
    const verificationCode = code.join('');
    
    if (verificationCode.length !== 6) {
      Alert.alert('Error', 'Please enter the complete 6-digit code');
      return;
    }

    setLoading(true);
    try {
      await apiClient.post('/api/v1/auth/verify-email/', {
        email,
        code: verificationCode,
      });
      
      Alert.alert(
        'Success',
        'Your email has been verified successfully!',
        [
          {
            text: 'Continue',
            onPress: () => router.replace('/(auth)/login'),
          },
        ]
      );
    } catch (error: any) {
      console.error('Verification error:', error);
      Alert.alert(
        'Error',
        error.response?.data?.detail || 
        error.response?.data?.code?.[0] || 
        'Invalid verification code. Please try again.'
      );
      setCode(['', '', '', '', '', '']);
      inputRefs.current[0]?.focus();
    } finally {
      setLoading(false);
    }
  };

  const handleResendCode = async () => {
    if (!email) {
      Alert.alert('Error', 'Email address is required');
      return;
    }

    setResending(true);
    try {
      await apiClient.post('/api/v1/auth/resend-verification/', { email });
      Alert.alert('Success', 'Verification code has been resent to your email');
      setCode(['', '', '', '', '', '']);
      inputRefs.current[0]?.focus();
    } catch (error: any) {
      console.error('Resend error:', error);
      Alert.alert(
        'Error',
        error.response?.data?.detail || 
        'Failed to resend code. Please try again.'
      );
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
                We've sent a 6-digit verification code to {email || 'your email'}. Please enter it below.
              </Text>
            </View>

            {/* Code Input */}
            <View className="mb-8">
              <View className="flex-row justify-between mb-4">
                {code.map((digit, index) => (
                  <View
                    key={index}
                    className="w-[50px] h-[60px] border-2 border-gray-300 rounded-xl bg-gray-50 justify-center items-center"
                  >
                    <TextInput
                      ref={(ref) => (inputRefs.current[index] = ref)}
                      className="text-2xl font-bold text-gray-900 text-center w-full h-full"
                      value={digit}
                      onChangeText={(text) => handleCodeChange(text, index)}
                      onKeyPress={(e) => handleKeyPress(e, index)}
                      keyboardType="number-pad"
                      maxLength={2}
                      selectTextOnFocus
                      editable={!loading && !resending}
                    />
                  </View>
                ))}
              </View>

              <Text className="text-sm text-gray-500 text-center">
                Enter the 6-digit code sent to your email
              </Text>
            </View>

            {/* Verify Button */}
            <TouchableOpacity
              onPress={handleVerify}
              disabled={loading || resending || code.join('').length !== 6}
              className={`py-4 rounded-xl mb-6 ${
                loading || resending || code.join('').length !== 6
                  ? 'bg-emerald-300'
                  : 'bg-emerald-600'
              }`}
            >
              {loading ? (
                <ActivityIndicator color="#fff" />
              ) : (
                <Text className="text-white text-center font-semibold text-base">
                  Verify Email
                </Text>
              )}
            </TouchableOpacity>

            {/* Resend Code */}
            <View className="flex-row justify-center items-center mb-4">
              <Text className="text-gray-600">Didn't receive the code? </Text>
              <TouchableOpacity
                onPress={handleResendCode}
                disabled={loading || resending}
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

VerifyEmailScreen.displayName = 'VerifyEmailScreen';
