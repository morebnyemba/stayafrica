import '../global.css';
import { useEffect, useState } from 'react';
import { Stack } from 'expo-router';
import { useRouter, SplashScreen } from 'expo-router';
import { GestureHandlerRootView } from 'react-native-gesture-handler';
import { SafeAreaProvider } from 'react-native-safe-area-context';
import { ActivityIndicator, View } from 'react-native';
import AsyncStorage from '@react-native-async-storage/async-storage';
import { Providers } from '@/context/providers';
import { useAuth } from '@/context/auth-context';

const ONBOARDING_KEY = 'has_seen_onboarding';

// Expo Router's sitemap dev screen expects window.location on web; provide a safe shim for native.
if (typeof globalThis.window === 'undefined') {
  (globalThis as any).window = { location: { origin: '' } };
}
if (typeof globalThis.location === 'undefined') {
  (globalThis as any).location = (globalThis as any).window.location;
} else if (!(globalThis as any).location.origin) {
  (globalThis as any).location.origin = '';
}

SplashScreen.preventAutoHideAsync();

function RootLayoutContent() {
  const router = useRouter();
  const { isLoading } = useAuth();
  const [hasSeenOnboarding, setHasSeenOnboarding] = useState<boolean | null>(null);
  const [checkingOnboarding, setCheckingOnboarding] = useState(true);

  // Check if user has seen onboarding
  useEffect(() => {
    const checkOnboarding = async () => {
      try {
        const value = await AsyncStorage.getItem(ONBOARDING_KEY);
        setHasSeenOnboarding(value === 'true');
      } catch (error) {
        console.error('Error checking onboarding state:', error);
        setHasSeenOnboarding(true); // Default to true on error
      } finally {
        setCheckingOnboarding(false);
      }
    };
    checkOnboarding();
  }, []);

  useEffect(() => {
    if (!isLoading && !checkingOnboarding) {
      SplashScreen.hideAsync();
      
      // Show onboarding for first-time users
      if (hasSeenOnboarding === false) {
        router.replace('/(onboarding)/welcome');
      } else {
        // Show tabs for returning users (explore is public)
        router.replace('/(tabs)');
      }
    }
  }, [isLoading, checkingOnboarding, hasSeenOnboarding, router]);

  if (isLoading || checkingOnboarding) {
    return (
      <View style={{ flex: 1, justifyContent: 'center', alignItems: 'center' }}>
        <ActivityIndicator size="large" color="#3A5C50" />
      </View>
    );
  }

  return (
    <Stack screenOptions={{ headerShown: false }}>
      <Stack.Screen name="(onboarding)" options={{ animation: 'none' }} />
      <Stack.Screen name="(auth)" options={{ animation: 'none' }} />
      <Stack.Screen name="(tabs)" options={{ animation: 'none' }} />
      <Stack.Screen name="host" options={{ animation: 'fade' }} />
      <Stack.Screen name="reviews" options={{ animation: 'fade' }} />
    </Stack>
  );
}

export default function RootLayout() {
  return (
    <GestureHandlerRootView style={{ flex: 1 }}>
      <SafeAreaProvider>
        <Providers>
          <RootLayoutContent />
        </Providers>
      </SafeAreaProvider>
    </GestureHandlerRootView>
  );
}
