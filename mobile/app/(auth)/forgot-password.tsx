import React, { useState } from 'react';
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
import { router } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { apiClient } from '@/services/api-client';

export default function ForgotPasswordScreen() {
  const [email, setEmail] = useState('');
  const [loading, setLoading] = useState(false);
  const [submitted, setSubmitted] = useState(false);

  const handleResetPassword = async () => {
    if (!email.trim()) {
      Alert.alert('Error', 'Please enter your email address');
      return;
    }

    // Basic email validation
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    if (!emailRegex.test(email)) {
      Alert.alert('Error', 'Please enter a valid email address');
      return;
    }

    setLoading(true);
    try {
      await apiClient.post('/api/v1/auth/password-reset/', { email: email.toLowerCase() });
      setSubmitted(true);
    } catch (error: any) {
      console.error('Password reset error:', error);
      Alert.alert(
        'Error',
        error.response?.data?.detail || 
        error.response?.data?.email?.[0] || 
        'Failed to send reset email. Please try again.'
      );
    } finally {
      setLoading(false);
    }
  };

  if (submitted) {
    return (
      <SafeAreaView className="flex-1 bg-white">
        <View className="flex-1 justify-center items-center px-6">
          <View className="w-20 h-20 bg-green-100 rounded-full items-center justify-center mb-6">
            <Ionicons name="checkmark-circle" size={48} color="#22c55e" />
          </View>
          
          <Text className="text-2xl font-bold text-gray-900 mb-3 text-center">
            Check Your Email
          </Text>
          
          <Text className="text-base text-gray-600 text-center mb-8">
            We've sent password reset instructions to {email}
          </Text>
          
          <Text className="text-sm text-gray-500 text-center mb-8">
            Didn't receive the email? Check your spam folder or try again.
          </Text>
          
          <TouchableOpacity
            onPress={() => router.back()}
            className="bg-emerald-600 py-4 px-8 rounded-xl w-full mb-4"
          >
            <Text className="text-white text-center font-semibold text-base">
              Back to Login
            </Text>
          </TouchableOpacity>
          
          <TouchableOpacity
            onPress={() => setSubmitted(false)}
            className="py-3"
          >
            <Text className="text-emerald-600 text-center font-medium">
              Try Another Email
            </Text>
          </TouchableOpacity>
        </View>
      </SafeAreaView>
    );
  }

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
                <Ionicons name="key-outline" size={32} color="#059669" />
              </View>
              
              <Text className="text-3xl font-bold text-gray-900 mb-3">
                Forgot Password?
              </Text>
              
              <Text className="text-base text-gray-600">
                No worries! Enter your email address and we'll send you instructions to reset your password.
              </Text>
            </View>

            {/* Email Input */}
            <View className="mb-6">
              <Text className="text-sm font-medium text-gray-700 mb-2">
                Email Address
              </Text>
              <View className="flex-row items-center border border-gray-300 rounded-xl px-4 py-3 bg-gray-50">
                <Ionicons name="mail-outline" size={20} color="#6b7280" />
                <TextInput
                  className="flex-1 ml-3 text-base text-gray-900"
                  placeholder="Enter your email"
                  placeholderTextColor="#9ca3af"
                  value={email}
                  onChangeText={setEmail}
                  keyboardType="email-address"
                  autoCapitalize="none"
                  autoComplete="email"
                  editable={!loading}
                />
              </View>
            </View>

            {/* Reset Button */}
            <TouchableOpacity
              onPress={handleResetPassword}
              disabled={loading}
              className={`py-4 rounded-xl mb-6 ${
                loading ? 'bg-emerald-400' : 'bg-emerald-600'
              }`}
            >
              {loading ? (
                <ActivityIndicator color="#fff" />
              ) : (
                <Text className="text-white text-center font-semibold text-base">
                  Send Reset Link
                </Text>
              )}
            </TouchableOpacity>

            {/* Back to Login */}
            <View className="flex-row justify-center items-center">
              <Text className="text-gray-600">Remember your password? </Text>
              <TouchableOpacity onPress={() => router.back()}>
                <Text className="text-emerald-600 font-semibold">
                  Back to Login
                </Text>
              </TouchableOpacity>
            </View>
          </View>
        </ScrollView>
      </KeyboardAvoidingView>
    </SafeAreaView>
  );
}
