import React, { useState } from 'react';
import {
  View,
  Text,
  TouchableOpacity,
  Modal,
  Animated,
  Dimensions,
  ScrollView,
  Platform,
} from 'react-native';
import { Ionicons } from '@expo/vector-icons';
import { AnimatedCompassIcon } from '@/components/common/AnimatedCompassIcon';
import { LinearGradient } from 'expo-linear-gradient';
import { useRouter, Href } from 'expo-router';
import { useAuth } from '@/context/auth-context';
import { Avatar } from './Avatar';

const { width: SCREEN_WIDTH } = Dimensions.get('window');
const SIDEBAR_WIDTH = SCREEN_WIDTH * 0.8;

interface MenuItem {
  id: string;
  label: string;
  icon: keyof typeof Ionicons.glyphMap;
  route?: Href;
  action?: () => void;
  badge?: number;
  children?: MenuItem[];
  authRequired?: boolean;
  guestOnly?: boolean;
}

interface SidebarProps {
  isVisible: boolean;
  onClose: () => void;
}

export function Sidebar({ isVisible, onClose }: SidebarProps) {
  const router = useRouter();
  const { user, isAuthenticated, logout } = useAuth();
  const [expandedItems, setExpandedItems] = useState<string[]>([]);
  const slideAnim = React.useRef(new Animated.Value(-SIDEBAR_WIDTH)).current;

  React.useEffect(() => {
    Animated.timing(slideAnim, {
      toValue: isVisible ? 0 : -SIDEBAR_WIDTH,
      duration: 250,
      useNativeDriver: true,
    }).start();
  }, [isVisible, slideAnim]);

  const toggleExpand = (itemId: string) => {
    setExpandedItems((prev) =>
      prev.includes(itemId)
        ? prev.filter((id) => id !== itemId)
        : [...prev, itemId]
    );
  };

  const handleNavigation = (route: Href) => {
    onClose();
    setTimeout(() => {
      router.push(route);
    }, 250);
  };

  const handleLogout = () => {
    onClose();
    logout();
    setTimeout(() => {
      router.replace('/(auth)/login');
    }, 250);
  };

  const menuItems: MenuItem[] = [
    {
      id: 'explore',
      label: 'Explore',
      icon: 'compass-outline',
      route: '/(tabs)/explore',
    },
    {
      id: 'bookings',
      label: 'My Bookings',
      icon: 'calendar-outline',
      route: '/(tabs)/bookings',
      authRequired: true,
    },
    {
      id: 'wishlist',
      label: 'Wishlist',
      icon: 'heart-outline',
      route: '/(tabs)/wishlist',
      authRequired: true,
    },
    {
      id: 'messages',
      label: 'Messages',
      icon: 'chatbubbles-outline',
      route: '/(tabs)/messages',
      authRequired: true,
    },
    {
      id: 'notifications',
      label: 'Notifications',
      icon: 'notifications-outline',
      route: '/notifications',
      authRequired: true,
    },
    {
      id: 'host',
      label: 'Host Dashboard',
      icon: 'business-outline',
      route: '/(tabs)/host',
      authRequired: true,
    },
    {
      id: 'wallet',
      label: 'Wallet',
      icon: 'wallet-outline',
      route: '/(tabs)/wallet',
      authRequired: true,
    },
    {
      id: 'settings',
      label: 'Settings',
      icon: 'settings-outline',
      route: '/(tabs)/profile',
      authRequired: true,
    },
  ];

  const guestMenuItems: MenuItem[] = [
    {
      id: 'login',
      label: 'Sign In',
      icon: 'log-in-outline',
      route: '/(auth)/login',
      guestOnly: true,
    },
    {
      id: 'register',
      label: 'Create Account',
      icon: 'person-add-outline',
      route: '/(auth)/register',
      guestOnly: true,
    },
  ];

  const filteredMenuItems = menuItems.filter((item) => {
    if (item.authRequired && !isAuthenticated) return false;
    if (item.guestOnly && isAuthenticated) return false;
    return true;
  });

  const renderMenuItem = (item: MenuItem, depth = 0) => {
    const hasChildren = item.children && item.children.length > 0;
    const isExpanded = expandedItems.includes(item.id);

    return (
      <View key={item.id}>
        <TouchableOpacity
          onPress={() => {
            if (hasChildren) {
              toggleExpand(item.id);
            } else if (item.route) {
              handleNavigation(item.route);
            } else if (item.action) {
              item.action();
            }
          }}
          className={`flex-row items-center py-3.5 px-4 ${
            depth > 0 ? 'ml-8 border-l border-sand-200' : ''
          }`}
          style={{
            backgroundColor: depth > 0 ? 'rgba(244, 241, 234, 0.5)' : 'transparent',
          }}
        >
          <View
            className={`w-10 h-10 rounded-xl items-center justify-center ${
              depth > 0 ? 'bg-sand-100' : 'bg-sand-200'
            }`}
          >
            {item.id === 'explore' ? (
              <AnimatedCompassIcon
                size={depth > 0 ? 18 : 22}
                color="#3A5C50"
              />
            ) : (
              <Ionicons
                name={item.icon}
                size={depth > 0 ? 18 : 22}
                color="#3A5C50"
              />
            )}
          </View>
          <Text
            className={`flex-1 ml-3 font-semibold ${
              depth > 0 ? 'text-sm text-moss' : 'text-base text-forest'
            }`}
          >
            {item.label}
          </Text>
          {item.badge !== undefined && item.badge > 0 && (
            <View className="bg-gold px-2 py-0.5 rounded-full mr-2">
              <Text className="text-forest text-xs font-bold">{item.badge}</Text>
            </View>
          )}
          {hasChildren && (
            <Ionicons
              name={isExpanded ? 'chevron-up' : 'chevron-down'}
              size={20}
              color="#3A5C50"
            />
          )}
        </TouchableOpacity>
        {hasChildren && isExpanded && item.children && (
          <View className="bg-sand-50">
            {item.children.map((child) => renderMenuItem(child, depth + 1))}
          </View>
        )}
      </View>
    );
  };

  return (
    <Modal
      visible={isVisible}
      transparent
      animationType="none"
      onRequestClose={onClose}
    >
      <View className="flex-1 flex-row">
        {/* Overlay */}
        <TouchableOpacity
          activeOpacity={1}
          onPress={onClose}
          style={{
            position: 'absolute',
            top: 0,
            left: 0,
            right: 0,
            bottom: 0,
            backgroundColor: 'rgba(0, 0, 0, 0.5)',
          }}
        />

        {/* Sidebar */}
        <Animated.View
          style={{
            width: SIDEBAR_WIDTH,
            transform: [{ translateX: slideAnim }],
            backgroundColor: '#fff',
            shadowColor: '#000',
            shadowOffset: { width: 4, height: 0 },
            shadowOpacity: 0.3,
            shadowRadius: 12,
            elevation: 10,
          }}
        >
          {/* Header with Avatar */}
          <LinearGradient
            colors={['#122F26', '#1d392f', '#2d4a40']}
            start={{ x: 0, y: 0 }}
            end={{ x: 1, y: 1 }}
            style={{
              paddingTop: Platform.OS === 'ios' ? 60 : 40,
              paddingBottom: 24,
              paddingHorizontal: 20,
            }}
          >
            {/* Close button */}
            <TouchableOpacity
              onPress={onClose}
              style={{
                position: 'absolute',
                top: Platform.OS === 'ios' ? 50 : 30,
                right: 16,
                padding: 8,
              }}
            >
              <Ionicons name="close" size={24} color="#D9B168" />
            </TouchableOpacity>

            {/* Avatar and User Info */}
            <View className="items-center">
              <Avatar
                uri={null}
                firstName={user?.first_name}
                lastName={user?.last_name}
                size="large"
                onPress={() => {
                  if (isAuthenticated) {
                    handleNavigation('/(tabs)/profile');
                  } else {
                    handleNavigation('/(auth)/login');
                  }
                }}
              />
              
              {isAuthenticated && user ? (
                <View className="items-center mt-4">
                  <Text className="text-xl font-bold text-white">
                    {user.first_name} {user.last_name}
                  </Text>
                  <Text className="text-sand-200 text-sm mt-1">
                    {user.email}
                  </Text>
                  <View className="mt-3">
                    <LinearGradient
                      colors={user.role === 'host' ? ['#3A5C50', '#2d4a40'] : ['#D9B168', '#bea04f']}
                      className="px-4 py-1.5 rounded-full"
                    >
                      <Text
                        className={`text-xs font-semibold ${
                          user.role === 'host' ? 'text-gold' : 'text-forest'
                        }`}
                      >
                        {user.role === 'host' ? '🏠 Host' : '✨ Guest'}
                      </Text>
                    </LinearGradient>
                  </View>
                </View>
              ) : (
                <View className="items-center mt-4">
                  <Text className="text-xl font-bold text-white">
                    Welcome to StayAfrica
                  </Text>
                  <Text className="text-sand-200 text-sm mt-1 text-center">
                    Sign in to access all features
                  </Text>
                </View>
              )}
            </View>
          </LinearGradient>

          {/* Menu Items */}
          <ScrollView
            className="flex-1"
            showsVerticalScrollIndicator={false}
            contentContainerStyle={{ paddingBottom: 20 }}
          >
            {/* Navigation Section */}
            <View className="pt-4">
              <Text className="px-4 text-xs font-semibold text-moss uppercase tracking-wider mb-2">
                Navigation
              </Text>
              {filteredMenuItems.map((item) => renderMenuItem(item))}
            </View>

            {/* Guest options */}
            {!isAuthenticated && (
              <View className="mt-4 pt-4 border-t border-sand-200">
                <Text className="px-4 text-xs font-semibold text-moss uppercase tracking-wider mb-2">
                  Account
                </Text>
                {guestMenuItems.map((item) => renderMenuItem(item))}
              </View>
            )}

            {/* Logout for authenticated users */}
            {isAuthenticated && (
              <View className="mt-4 pt-4 border-t border-sand-200">
                <TouchableOpacity
                  onPress={handleLogout}
                  className="flex-row items-center py-3.5 px-4"
                >
                  <View className="w-10 h-10 rounded-xl items-center justify-center bg-red-100">
                    <Ionicons name="log-out-outline" size={22} color="#EF4444" />
                  </View>
                  <Text className="flex-1 ml-3 text-base font-semibold text-red-500">
                    Logout
                  </Text>
                </TouchableOpacity>
              </View>
            )}
          </ScrollView>

          {/* Footer */}
          <View className="border-t border-sand-200 p-4">
            <Text className="text-center text-xs text-moss">
              StayAfrica v1.0.0
            </Text>
          </View>
        </Animated.View>
      </View>
    </Modal>
  );
}
