import React, { useState } from 'react';
import { View, TouchableOpacity, Platform } from 'react-native';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { Avatar } from './Avatar';
import { Sidebar } from './Sidebar';
import { useRouter } from 'expo-router';

interface AppHeaderProps {
  transparent?: boolean;
}

export function AppHeader({ transparent = false }: AppHeaderProps) {
  const [sidebarVisible, setSidebarVisible] = useState(false);
  const { user, isAuthenticated } = useAuth();
  const router = useRouter();

  const handleAvatarPress = () => {
    if (isAuthenticated) {
      router.push('/(tabs)/profile');
    } else {
      router.push('/(auth)/login');
    }
  };

  const headerContent = (
    <View
      className="flex-row items-center justify-between px-4"
      style={{
        paddingTop: Platform.OS === 'ios' ? 50 : 35,
        paddingBottom: 12,
      }}
    >
      {/* Hamburger Menu */}
      <TouchableOpacity
        onPress={() => setSidebarVisible(true)}
        className="w-10 h-10 rounded-xl items-center justify-center"
        style={{
          backgroundColor: transparent ? 'rgba(255, 255, 255, 0.2)' : '#f4f1ea',
        }}
      >
        <Ionicons
          name="menu"
          size={24}
          color={transparent ? '#fff' : '#122F26'}
        />
      </TouchableOpacity>

      {/* Avatar */}
      <Avatar
        uri={null}
        firstName={user?.first_name}
        lastName={user?.last_name}
        size="small"
        onPress={handleAvatarPress}
        showBadge={isAuthenticated}
      />

      {/* Sidebar */}
      <Sidebar
        isVisible={sidebarVisible}
        onClose={() => setSidebarVisible(false)}
      />
    </View>
  );

  if (transparent) {
    return (
      <View style={{ position: 'absolute', top: 0, left: 0, right: 0, zIndex: 10 }}>
        {headerContent}
      </View>
    );
  }

  return (
    <LinearGradient
      colors={['#122F26', '#1d392f']}
      start={{ x: 0, y: 0 }}
      end={{ x: 1, y: 1 }}
    >
      {headerContent}
    </LinearGradient>
  );
}
