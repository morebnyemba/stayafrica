import { View, Text, TouchableOpacity, ScrollView, TextInput, Alert, Platform, ActivityIndicator } from 'react-native';
import { useState } from 'react';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { apiClient } from '@/services/api-client';

export default function ChangePasswordScreen() {
  const router = useRouter();
  const [isLoading, setIsLoading] = useState(false);
  const [showOldPassword, setShowOldPassword] = useState(false);
  const [showNewPassword, setShowNewPassword] = useState(false);
  const [showConfirmPassword, setShowConfirmPassword] = useState(false);

  const [formData, setFormData] = useState({
    old_password: '',
    new_password: '',
    confirm_password: '',
  });

  const handleChangePassword = async () => {
    // Validation
    if (!formData.old_password || !formData.new_password || !formData.confirm_password) {
      Alert.alert('Error', 'Please fill in all fields');
      return;
    }

    if (formData.new_password !== formData.confirm_password) {
      Alert.alert('Error', 'New passwords do not match');
      return;
    }

    if (formData.new_password.length < 8) {
      Alert.alert('Error', 'Password must be at least 8 characters long');
      return;
    }

    setIsLoading(true);
    
    try {
      await apiClient.post('/users/change_password/', {
        old_password: formData.old_password,
        new_password: formData.new_password,
      });
      
      Alert.alert('Success', 'Password changed successfully!', [
        { text: 'OK', onPress: () => router.back() }
      ]);
    } catch (error: any) {
      const message = error.response?.data?.old_password || 
                      error.response?.data?.new_password?.[0] ||
                      error.response?.data?.detail || 
                      'Failed to change password';
      Alert.alert('Error', message);
    } finally {
      setIsLoading(false);
    }
  };

  const PasswordField = ({ 
    label, 
    value, 
    onChangeText, 
    placeholder,
    showPassword,
    onTogglePassword,
  }: any) => (
    <View className="mb-4">
      <Text className="text-sm font-medium text-forest mb-2">{label}</Text>
      <View className="flex-row items-center bg-white rounded-2xl border border-sand-300">
        <View className="pl-4">
          <Ionicons name="lock-closed-outline" size={20} color="#3A5C50" />
        </View>
        <TextInput
          value={value}
          onChangeText={onChangeText}
          placeholder={placeholder}
          secureTextEntry={!showPassword}
          className="flex-1 px-4 py-4 text-forest"
          placeholderTextColor="#94a3b8"
        />
        <TouchableOpacity onPress={onTogglePassword} className="pr-4">
          <Ionicons 
            name={showPassword ? "eye-off-outline" : "eye-outline"} 
            size={20} 
            color="#3A5C50" 
          />
        </TouchableOpacity>
      </View>
    </View>
  );

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
        {/* Top Navigation */}
        <View className="flex-row items-center justify-between mb-4">
          <TouchableOpacity
            onPress={() => router.back()}
            className="w-10 h-10 rounded-xl items-center justify-center"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </TouchableOpacity>

          <Text className="text-xl font-bold text-white">Change Password</Text>

          <View className="w-10" />
        </View>

        {/* Icon */}
        <View className="items-center mt-4">
          <View className="w-20 h-20 rounded-full bg-gold/20 items-center justify-center">
            <Ionicons name="shield-checkmark-outline" size={40} color="#D9B168" />
          </View>
          <Text className="text-sand-200 mt-4 text-center">
            Choose a strong password to secure your account
          </Text>
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
          <Text className="text-lg font-bold text-forest mb-4">Security Settings</Text>

          <PasswordField
            label="Current Password"
            value={formData.old_password}
            onChangeText={(text: string) => setFormData({ ...formData, old_password: text })}
            placeholder="Enter your current password"
            showPassword={showOldPassword}
            onTogglePassword={() => setShowOldPassword(!showOldPassword)}
          />

          <PasswordField
            label="New Password"
            value={formData.new_password}
            onChangeText={(text: string) => setFormData({ ...formData, new_password: text })}
            placeholder="Enter your new password"
            showPassword={showNewPassword}
            onTogglePassword={() => setShowNewPassword(!showNewPassword)}
          />

          <PasswordField
            label="Confirm New Password"
            value={formData.confirm_password}
            onChangeText={(text: string) => setFormData({ ...formData, confirm_password: text })}
            placeholder="Confirm your new password"
            showPassword={showConfirmPassword}
            onTogglePassword={() => setShowConfirmPassword(!showConfirmPassword)}
          />

          {/* Password requirements */}
          <View className="bg-sand-100 rounded-2xl p-4 mt-2">
            <Text className="text-sm font-medium text-forest mb-2">Password Requirements:</Text>
            <View className="flex-row items-center mt-1">
              <Ionicons 
                name={formData.new_password.length >= 8 ? "checkmark-circle" : "ellipse-outline"} 
                size={16} 
                color={formData.new_password.length >= 8 ? "#10B981" : "#94a3b8"} 
              />
              <Text className="text-sm text-moss ml-2">At least 8 characters</Text>
            </View>
            <View className="flex-row items-center mt-1">
              <Ionicons 
                name={/[A-Z]/.test(formData.new_password) ? "checkmark-circle" : "ellipse-outline"} 
                size={16} 
                color={/[A-Z]/.test(formData.new_password) ? "#10B981" : "#94a3b8"} 
              />
              <Text className="text-sm text-moss ml-2">One uppercase letter</Text>
            </View>
            <View className="flex-row items-center mt-1">
              <Ionicons 
                name={/[0-9]/.test(formData.new_password) ? "checkmark-circle" : "ellipse-outline"} 
                size={16} 
                color={/[0-9]/.test(formData.new_password) ? "#10B981" : "#94a3b8"} 
              />
              <Text className="text-sm text-moss ml-2">One number</Text>
            </View>
          </View>
        </View>

        {/* Save Button */}
        <TouchableOpacity
          onPress={handleChangePassword}
          disabled={isLoading}
          className="mt-6 mb-4"
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
                <Text className="text-forest font-bold ml-2 text-base">Update Password</Text>
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
  );
}
