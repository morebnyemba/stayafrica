import { View, Text, TouchableOpacity, ScrollView, Alert } from 'react-native';
import { useAuth } from '@/context/auth-context';
import { useRouter } from 'expo-router';
import { useUserProfile } from '@/hooks/api-hooks';
import { Ionicons } from '@expo/vector-icons';

export default function ProfileScreen() {
  const { user, logout, isAuthenticated } = useAuth();
  const { data: profile } = useUserProfile();
  const router = useRouter();

  if (!isAuthenticated) {
    return (
      <View className="flex-1 items-center justify-center bg-white">
        <Ionicons name="person" size={48} color="#999" />
        <Text className="text-gray-600 mb-4 mt-4">Please login to view your profile</Text>
        <TouchableOpacity
          className="bg-secondary-600 px-8 py-3 rounded-lg"
          onPress={() => router.replace('/(auth)/login')}
        >
          <Text className="text-white font-semibold">Go to Login</Text>
        </TouchableOpacity>
      </View>
    );
  }

  const handleLogout = () => {
    Alert.alert('Logout', 'Are you sure you want to logout?', [
      { text: 'Cancel', style: 'cancel' },
      {
        text: 'Logout',
        style: 'destructive',
        onPress: () => {
          logout();
          router.replace('/(auth)/login');
        },
      },
    ]);
  };

  const MenuItem = ({ iconName, label, value, onPress }: any) => (
    <TouchableOpacity
      className="flex-row items-center py-4 px-4 border-b border-gray-100"
      onPress={onPress}
    >
      <Ionicons name={iconName} size={20} color="#666" />
      <View className="flex-1 ml-4">
        <Text className="text-gray-600 text-sm">{label}</Text>
        <Text className="text-primary-900 font-medium mt-1">{value}</Text>
      </View>
    </TouchableOpacity>
  );

  return (
    <ScrollView className="flex-1 bg-gray-50" showsVerticalScrollIndicator={false}>
      {/* Header/Profile Card */}
      <View className="bg-white px-4 py-8 border-b border-gray-200">
        {/* Avatar */}
        <View className="w-24 h-24 rounded-full bg-gradient-to-br from-secondary-400 to-secondary-600 items-center justify-center mb-4 self-center">
          <Ionicons name="person" size={48} color="white" />
        </View>

        {/* Name */}
        <Text className="text-2xl font-bold text-primary-900 text-center">
          {user?.first_name} {user?.last_name}
        </Text>

        {/* Email */}
        <Text className="text-gray-600 text-center mt-1">{user?.email}</Text>

        {/* Role Badge */}
        <View className="mt-4 items-center">
          <View className={`px-4 py-1 rounded-full ${
            user?.role === 'host' ? 'bg-blue-100' : 'bg-green-100'
          }`}>
            <Text className={`font-semibold text-xs ${
              user?.role === 'host' ? 'text-blue-800' : 'text-green-800'
            }`}>
              {user?.role === 'host' ? '🏠 Property Host' : '🏖️ Guest'}
            </Text>
          </View>
        </View>
      </View>

      {/* Account Information */}
      <View className="bg-white mt-3 mb-3">
        <View className="px-4 py-3 border-b border-gray-200">
          <Text className="text-lg font-bold text-primary-900">Account Information</Text>
        </View>
        <MenuItem
          iconName="person"
          label="Full Name"
          value={`${user?.first_name} ${user?.last_name}`}
        />
        <MenuItem
          iconName="call"
          label="Phone Number"
          value={user?.phone_number || 'Not provided'}
        />
        <MenuItem
          iconName="location"
          label="Country"
          value={user?.country_of_residence || 'Not provided'}
        />
      </View>

      {/* Actions */}
      <View className="bg-white">
        <TouchableOpacity
          className="flex-row items-center py-4 px-4 border-b border-gray-100"
          onPress={() => router.push('/profile/edit')}
        >
          <Ionicons name="pencil" size={20} color="#D4A574" />
          <Text className="ml-4 text-primary-900 font-medium">Edit Profile</Text>
        </TouchableOpacity>

        <TouchableOpacity
          className="flex-row items-center py-4 px-4 border-b border-gray-100"
          onPress={() => router.push('/profile/change-password')}
        >
          <Ionicons name="lock-closed" size={20} color="#D4A574" />
          <Text className="ml-4 text-primary-900 font-medium">Change Password</Text>
        </TouchableOpacity>
      </View>

      {/* Logout */}
      <View className="p-4 mt-4">
        <TouchableOpacity
          className="bg-red-600 py-4 rounded-lg flex-row items-center justify-center"
          onPress={handleLogout}
        >
          <Ionicons name="log-out" size={20} color="white" />
          <Text className="text-white font-semibold ml-2">Logout</Text>
        </TouchableOpacity>
      </View>
    </ScrollView>
  );
}
