import { View, Text, TouchableOpacity, ScrollView, Alert } from 'react-native';
import { useState } from 'react';
import { useAuth } from '@/context/auth-context';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { GlassmorphicView } from '@/components/common/GlassmorphicView';
import { Avatar } from '@/components/common/Avatar';
import { Sidebar } from '@/components/common/Sidebar';
import { useSafeAreaInsets } from 'react-native-safe-area-context';

export default function ProfileScreen() {
  const { user, logout, isAuthenticated } = useAuth();
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const [sidebarVisible, setSidebarVisible] = useState(false);

  if (!isAuthenticated) {
    return (
      <View className="flex-1 bg-sand-100">
        {/* Sidebar */}
        <Sidebar
          isVisible={sidebarVisible}
          onClose={() => setSidebarVisible(false)}
        />

        {/* Header */}
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: insets.top + 12 }}
        >
          {/* Top Navigation Bar with Menu */}
          <View className="flex-row items-center justify-between mb-4">
            {/* Hamburger Menu with Glassmorphism */}
            <GlassmorphicView
              intensity={40}
              tint="dark"
              borderRadius={12}
              style={{ width: 40, height: 40 }}
            >
              <TouchableOpacity
                onPress={() => setSidebarVisible(true)}
                className="w-10 h-10 rounded-xl items-center justify-center"
              >
                <Ionicons name="menu" size={24} color="#fff" />
              </TouchableOpacity>
            </GlassmorphicView>

            {/* Avatar for Auth Options */}
            <Avatar
              uri={null}
              firstName={undefined}
              lastName={undefined}
              size="small"
              onPress={() => router.push('/(auth)/login')}
            />
          </View>

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

  const MenuItem = ({ iconName, label, value, onPress, gradient = false, iconColor }: any) => (
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
            <Ionicons name={iconName} size={20} color={iconColor || "#D9B168"} />
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
            <Ionicons name={iconName} size={20} color={iconColor || "#3A5C50"} />
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

  // Verification Status Banner Component
  const VerificationBanner = () => {
    if (user?.is_verified) {
      return (
        <View className="mx-4 mt-4">
          <LinearGradient
            colors={['#10B981', '#059669']}
            className="p-4 rounded-2xl flex-row items-center"
            start={{ x: 0, y: 0 }}
            end={{ x: 1, y: 0 }}
          >
            <View className="bg-white/20 rounded-full p-2 mr-3">
              <Ionicons name="shield-checkmark" size={24} color="#fff" />
            </View>
            <View className="flex-1">
              <Text className="text-white font-bold text-base">Verified Account</Text>
              <Text className="text-white/80 text-sm mt-0.5">Your identity has been verified</Text>
            </View>
            <Ionicons name="checkmark-circle" size={28} color="#fff" />
          </LinearGradient>
        </View>
      );
    }

    return (
      <View className="mx-4 mt-4">
        <TouchableOpacity onPress={() => router.push('/profile/verification')}>
          <LinearGradient
            colors={['#F59E0B', '#D97706']}
            className="p-4 rounded-2xl flex-row items-center"
            start={{ x: 0, y: 0 }}
            end={{ x: 1, y: 0 }}
          >
            <View className="bg-white/20 rounded-full p-2 mr-3">
              <Ionicons name="alert-circle" size={24} color="#fff" />
            </View>
            <View className="flex-1">
              <Text className="text-white font-bold text-base">Verification Required</Text>
              <Text className="text-white/80 text-sm mt-0.5">Verify your identity to unlock all features</Text>
            </View>
            <Ionicons name="chevron-forward" size={24} color="#fff" />
          </LinearGradient>
        </TouchableOpacity>
      </View>
    );
  };

  // Quick Stats Component
  const QuickStats = () => (
    <View className="flex-row px-4 mt-4">
      <View className="flex-1 bg-white rounded-2xl p-4 mr-2" style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 2 },
        shadowOpacity: 0.05,
        shadowRadius: 4,
        elevation: 2,
      }}>
        <View className="flex-row items-center">
          <View className="bg-blue-100 rounded-full p-2">
            <Ionicons name="calendar" size={18} color="#3B82F6" />
          </View>
          <View className="ml-3">
            <Text className="text-xl font-bold text-forest">0</Text>
            <Text className="text-xs text-moss">Bookings</Text>
          </View>
        </View>
      </View>

      <View className="flex-1 bg-white rounded-2xl p-4 mr-2" style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 2 },
        shadowOpacity: 0.05,
        shadowRadius: 4,
        elevation: 2,
      }}>
        <View className="flex-row items-center">
          <View className="bg-red-100 rounded-full p-2">
            <Ionicons name="heart" size={18} color="#EF4444" />
          </View>
          <View className="ml-3">
            <Text className="text-xl font-bold text-forest">0</Text>
            <Text className="text-xs text-moss">Wishlist</Text>
          </View>
        </View>
      </View>

      <View className="flex-1 bg-white rounded-2xl p-4" style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 2 },
        shadowOpacity: 0.05,
        shadowRadius: 4,
        elevation: 2,
      }}>
        <View className="flex-row items-center">
          <View className="bg-purple-100 rounded-full p-2">
            <Ionicons name="star" size={18} color="#8B5CF6" />
          </View>
          <View className="ml-3">
            <Text className="text-xl font-bold text-forest">0</Text>
            <Text className="text-xs text-moss">Reviews</Text>
          </View>
        </View>
      </View>
    </View>
  );

  return (
    <ScrollView className="flex-1 bg-sand-100" showsVerticalScrollIndicator={false}>
      {/* Sidebar */}
      <Sidebar
        isVisible={sidebarVisible}
        onClose={() => setSidebarVisible(false)}
      />

      {/* Header with Profile Card */}
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="px-4 pb-8"
        style={{ paddingTop: insets.top + 12 }}
      >
        {/* Top Navigation Bar with Menu */}
        <View className="flex-row items-center justify-between mb-4">
          {/* Hamburger Menu */}
          <TouchableOpacity
            onPress={() => setSidebarVisible(true)}
            className="w-10 h-10 rounded-xl items-center justify-center"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="menu" size={24} color="#fff" />
          </TouchableOpacity>

          {/* User Badge */}
          <View className="flex-row items-center">
            <LinearGradient
              colors={user?.role === 'host' ? ['#3A5C50', '#2d4a40'] : ['#D9B168', '#bea04f']}
              className="px-3 py-1.5 rounded-full"
            >
              <Text
                className={`text-xs font-semibold ${
                  user?.role === 'host' ? 'text-gold' : 'text-forest'
                }`}
              >
                {user?.role === 'host' ? '🏠 Host' : '✨ Guest'}
              </Text>
            </LinearGradient>
          </View>
        </View>

        {/* Avatar */}
        <View className="items-center mb-4">
          <Avatar
            uri={null}
            firstName={user?.first_name}
            lastName={user?.last_name}
            size="large"
          />

          {/* Name */}
          <Text className="text-2xl font-black text-white text-center mt-4">
            {user?.first_name} {user?.last_name}
          </Text>

          {/* Email */}
          <Text className="text-sand-200 text-center mt-1">{user?.email}</Text>

          {/* Verification Badge inline */}
          <View className="flex-row items-center mt-2">
            {user?.is_verified ? (
              <View className="flex-row items-center bg-green-500/20 px-3 py-1 rounded-full">
                <Ionicons name="shield-checkmark" size={14} color="#10B981" />
                <Text className="text-green-400 text-xs font-semibold ml-1">Verified</Text>
              </View>
            ) : (
              <View className="flex-row items-center bg-yellow-500/20 px-3 py-1 rounded-full">
                <Ionicons name="alert-circle" size={14} color="#F59E0B" />
                <Text className="text-yellow-400 text-xs font-semibold ml-1">Unverified</Text>
              </View>
            )}
          </View>
        </View>
      </LinearGradient>

      {/* Verification Banner */}
      <VerificationBanner />

      {/* Quick Stats */}
      <QuickStats />

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

      {/* Quick Actions */}
      <View className="px-4 mt-6">
        <Text className="text-lg font-bold text-forest mb-4 px-2">Quick Actions</Text>
        
        <View className="flex-row flex-wrap">
          <TouchableOpacity 
            className="w-1/3 p-2"
            onPress={() => router.push('/(tabs)/bookings')}
          >
            <View className="bg-white rounded-2xl p-4 items-center" style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 2 },
              shadowOpacity: 0.05,
              shadowRadius: 4,
              elevation: 2,
            }}>
              <View className="bg-blue-100 rounded-full p-3 mb-2">
                <Ionicons name="calendar" size={24} color="#3B82F6" />
              </View>
              <Text className="text-forest font-semibold text-xs text-center">Bookings</Text>
            </View>
          </TouchableOpacity>

          <TouchableOpacity 
            className="w-1/3 p-2"
            onPress={() => router.push('/(tabs)/wishlist')}
          >
            <View className="bg-white rounded-2xl p-4 items-center" style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 2 },
              shadowOpacity: 0.05,
              shadowRadius: 4,
              elevation: 2,
            }}>
              <View className="bg-red-100 rounded-full p-3 mb-2">
                <Ionicons name="heart" size={24} color="#EF4444" />
              </View>
              <Text className="text-forest font-semibold text-xs text-center">Wishlist</Text>
            </View>
          </TouchableOpacity>

          <TouchableOpacity 
            className="w-1/3 p-2"
            onPress={() => router.push('/(tabs)/messages')}
          >
            <View className="bg-white rounded-2xl p-4 items-center" style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 2 },
              shadowOpacity: 0.05,
              shadowRadius: 4,
              elevation: 2,
            }}>
              <View className="bg-purple-100 rounded-full p-3 mb-2">
                <Ionicons name="chatbubbles" size={24} color="#8B5CF6" />
              </View>
              <Text className="text-forest font-semibold text-xs text-center">Messages</Text>
            </View>
          </TouchableOpacity>

          <TouchableOpacity 
            className="w-1/3 p-2"
            onPress={() => router.push('/(tabs)/wallet')}
          >
            <View className="bg-white rounded-2xl p-4 items-center" style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 2 },
              shadowOpacity: 0.05,
              shadowRadius: 4,
              elevation: 2,
            }}>
              <View className="bg-green-100 rounded-full p-3 mb-2">
                <Ionicons name="wallet" size={24} color="#10B981" />
              </View>
              <Text className="text-forest font-semibold text-xs text-center">Wallet</Text>
            </View>
          </TouchableOpacity>

          <TouchableOpacity 
            className="w-1/3 p-2"
            onPress={() => router.push('/reviews/my-reviews')}
          >
            <View className="bg-white rounded-2xl p-4 items-center" style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 2 },
              shadowOpacity: 0.05,
              shadowRadius: 4,
              elevation: 2,
            }}>
              <View className="bg-yellow-100 rounded-full p-3 mb-2">
                <Ionicons name="star" size={24} color="#F59E0B" />
              </View>
              <Text className="text-forest font-semibold text-xs text-center">Reviews</Text>
            </View>
          </TouchableOpacity>

          <TouchableOpacity 
            className="w-1/3 p-2"
            onPress={() => router.push('/(tabs)/host')}
          >
            <View className="bg-white rounded-2xl p-4 items-center" style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 2 },
              shadowOpacity: 0.05,
              shadowRadius: 4,
              elevation: 2,
            }}>
              <View className="bg-orange-100 rounded-full p-3 mb-2">
                <Ionicons name="business" size={24} color="#F97316" />
              </View>
              <Text className="text-forest font-semibold text-xs text-center">Hosting</Text>
            </View>
          </TouchableOpacity>
        </View>
      </View>

      {/* Settings */}
      <View className="px-4 mt-6">
        <Text className="text-lg font-bold text-forest mb-4 px-2">Settings</Text>
        
        <MenuItem
          iconName="create-outline"
          label="Edit Profile"
          value="Update your information"
          onPress={() => router.push('/(tabs)/profile/edit')}
          gradient={true}
        />

        <MenuItem
          iconName="lock-closed-outline"
          label="Change Password"
          value="Secure your account"
          onPress={() => router.push('/(tabs)/profile/change-password')}
          gradient={true}
        />

        <MenuItem
          iconName="notifications-outline"
          label="Notifications"
          value="Manage notification preferences"
          onPress={() => {}}
          gradient={true}
        />

        <MenuItem
          iconName="card-outline"
          label="Payment Methods"
          value="Manage your payment options"
          onPress={() => router.push('/(tabs)/wallet')}
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
