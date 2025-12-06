import { View, Text, TouchableOpacity, ScrollView } from 'react-native';
import { useAuth } from '@/context/auth-context';
import { useRouter } from 'expo-router';
import { useUserProfile } from '@/hooks/api-hooks';

export default function ProfileScreen() {
  const { user, logout, isAuthenticated } = useAuth();
  const { data: profile } = useUserProfile();
  const router = useRouter();

  if (!isAuthenticated) {
    return (
      <View className="flex-1 items-center justify-center bg-white">
        <Text className="text-gray-600 mb-4">Please login to view your profile</Text>
        <TouchableOpacity
          className="bg-primary-600 px-6 py-3 rounded-lg"
          onPress={() => router.replace('/(auth)/login')}
        >
          <Text className="text-white font-semibold">Go to Login</Text>
        </TouchableOpacity>
      </View>
    );
  }

  const handleLogout = () => {
    logout();
    router.replace('/(auth)/login');
  };

  return (
    <ScrollView className="flex-1 bg-white">
      <View className="px-4 py-6 border-b border-gray-200">
        <View className="w-20 h-20 rounded-full bg-gray-300 mb-4" />
        <Text className="text-2xl font-bold">
          {user?.first_name} {user?.last_name}
        </Text>
        <Text className="text-gray-600">{user?.email}</Text>
        <View className="mt-2 flex-row items-center">
          <Text className="bg-primary-100 text-primary-600 px-3 py-1 rounded-full text-sm font-semibold">
            {user?.role}
          </Text>
        </View>
      </View>

      <View className="px-4 py-6">
        <Text className="text-gray-600 mb-2">Phone</Text>
        <Text className="text-lg mb-6">{user?.phone_number}</Text>

        <Text className="text-gray-600 mb-2">Country</Text>
        <Text className="text-lg mb-6">{user?.country_of_residence}</Text>

        <TouchableOpacity className="bg-gray-200 py-3 rounded-lg mb-3">
          <Text className="text-center font-semibold">Edit Profile</Text>
        </TouchableOpacity>

        <TouchableOpacity className="bg-gray-200 py-3 rounded-lg mb-6">
          <Text className="text-center font-semibold">Change Password</Text>
        </TouchableOpacity>

        <TouchableOpacity className="bg-red-600 py-3 rounded-lg" onPress={handleLogout}>
          <Text className="text-center font-semibold text-white">Logout</Text>
        </TouchableOpacity>
      </View>
    </ScrollView>
  );
}
