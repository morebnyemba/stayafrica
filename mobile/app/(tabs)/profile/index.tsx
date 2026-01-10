import { View, Text, TouchableOpacity, ScrollView, Alert } from 'react-native';
import { useAuth } from '@/context/auth-context';
import { useRouter } from 'expo-router';
import { useUserProfile } from '@/hooks/api-hooks';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';

export default function ProfileScreen() {
  const { user, logout, isAuthenticated } = useAuth();
  const router = useRouter();

  if (!isAuthenticated) {
    return (
      <View className="flex-1 bg-sand-100">
        {/* Header */}
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pt-12 pb-6"
        >
          <Text className="text-3xl font-black text-white tracking-tight">
            Profile
          </Text>
          <Text className="text-sand-200 text-sm mt-1">
            Manage your account
          </Text>
        </LinearGradient>

        <View className="flex-1 items-center justify-center px-6">
          <View className="bg-white rounded-3xl p-8 items-center" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}>
            <View className="bg-sand-200 rounded-full p-8 mb-6">
              <Ionicons name="person-outline" size={72} color="#D9B168" />
            </View>
            <Text className="text-2xl font-bold text-forest mb-3">Welcome to StayAfrica</Text>
            <Text className="text-moss text-center mb-8 px-4 leading-6">
              Sign in to access your profile and personalize your experience
            </Text>
            <TouchableOpacity
              onPress={() => router.replace('/(auth)/login')}
            >
              <LinearGradient
                colors={['#D9B168', '#bea04f']}
                className="px-8 py-4 rounded-2xl"
                style={{
                  shadowColor: '#D9B168',
                  shadowOffset: { width: 0, height: 4 },
                  shadowOpacity: 0.3,
                  shadowRadius: 8,
                  elevation: 5,
                }}
              >
                <Text className="text-forest font-bold text-base">Sign In</Text>
              </LinearGradient>
            </TouchableOpacity>
          </View>
        </View>
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

  const MenuItem = ({ iconName, label, value, onPress, gradient = false }: any) => (
    <TouchableOpacity
      className="mb-3 rounded-2xl overflow-hidden"
      onPress={onPress}
      style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 2 },
        shadowOpacity: 0.05,
        shadowRadius: 4,
        elevation: 2,
      }}
    >
      {gradient ? (
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="flex-row items-center p-4"
        >
          <View className="bg-gold/20 rounded-full p-2.5 mr-4">
            <Ionicons name={iconName} size={20} color="#D9B168" />
          </View>
          <View className="flex-1">
            <Text className="text-sand-200 text-xs mb-0.5">{label}</Text>
            <Text className="text-white font-semibold">{value}</Text>
          </View>
          <Ionicons name="chevron-forward" size={20} color="#D9B168" />
        </LinearGradient>
      ) : (
        <View className="bg-white flex-row items-center p-4">
          <View className="bg-sand-200 rounded-full p-2.5 mr-4">
            <Ionicons name={iconName} size={20} color="#3A5C50" />
          </View>
          <View className="flex-1">
            <Text className="text-moss text-xs mb-0.5">{label}</Text>
            <Text className="text-forest font-semibold">{value}</Text>
          </View>
          <Ionicons name="chevron-forward" size={20} color="#3A5C50" />
        </View>
      )}
    </TouchableOpacity>
  );

  return (
    <ScrollView className="flex-1 bg-sand-100" showsVerticalScrollIndicator={false}>
      {/* Header with Profile Card */}
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="px-4 pt-12 pb-8"
      >
        {/* Avatar */}
        <View className="items-center mb-4">
          <LinearGradient
            colors={['#D9B168', '#bea04f']}
            className="w-28 h-28 rounded-full items-center justify-center mb-4"
            style={{
              shadowColor: '#D9B168',
              shadowOffset: { width: 0, height: 8 },
              shadowOpacity: 0.4,
              shadowRadius: 16,
              elevation: 8,
            }}
          >
            <Ionicons name="person" size={56} color="#122F26" />
          </LinearGradient>

          {/* Name */}
          <Text className="text-2xl font-black text-white text-center">
            {user?.first_name} {user?.last_name}
          </Text>

          {/* Email */}
          <Text className="text-sand-200 text-center mt-1">{user?.email}</Text>

          {/* Role Badge */}
          <View className="mt-4">
            <LinearGradient
              colors={user?.role === 'host' ? ['#3A5C50', '#2d4a40'] : ['#D9B168', '#bea04f']}
              className="px-5 py-2 rounded-full"
            >
              <Text className={`font-semibold text-sm ${
                user?.role === 'host' ? 'text-gold' : 'text-forest'
              }`}>
                {user?.role === 'host' ? '🏠 Property Host' : '✨ Guest'}
              </Text>
            </LinearGradient>
          </View>
        </View>
      </LinearGradient>

      {/* Account Information */}
      <View className="px-4 mt-6">
        <Text className="text-lg font-bold text-forest mb-4 px-2">Account Information</Text>
        
        <MenuItem
          iconName="person"
          label="Full Name"
          value={`${user?.first_name} ${user?.last_name}`}
          gradient={false}
        />
        <MenuItem
          iconName="call"
          label="Phone Number"
          value={user?.phone_number || 'Not provided'}
          gradient={false}
        />
        <MenuItem
          iconName="location"
          label="Country"
          value={user?.country_of_residence || 'Not provided'}
          gradient={false}
        />
      </View>

      {/* Actions */}
      <View className="px-4 mt-6">
        <Text className="text-lg font-bold text-forest mb-4 px-2">Settings</Text>
        
        <MenuItem
          iconName="create-outline"
          label="Edit Profile"
          value="Update your information"
          onPress={() => router.push('/profile/edit')}
          gradient={true}
        />

        <MenuItem
          iconName="lock-closed-outline"
          label="Change Password"
          value="Secure your account"
          onPress={() => router.push('/profile/change-password')}
          gradient={true}
        />
      </View>

      {/* Logout */}
      <View className="p-4 mt-6 mb-8">
        <TouchableOpacity
          className="rounded-2xl overflow-hidden"
          onPress={handleLogout}
          style={{
            shadowColor: '#EF4444',
            shadowOffset: { width: 0, height: 4 },
            shadowOpacity: 0.3,
            shadowRadius: 8,
            elevation: 5,
          }}
        >
          <LinearGradient
            colors={['#EF4444', '#DC2626']}
            className="py-4 flex-row items-center justify-center"
          >
            <Ionicons name="log-out-outline" size={22} color="white" />
            <Text className="text-white font-bold ml-2 text-base">Logout</Text>
          </LinearGradient>
        </TouchableOpacity>
      </View>
    </ScrollView>
  );
}
