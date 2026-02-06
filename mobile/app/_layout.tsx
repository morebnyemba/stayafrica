import '../global.css';
import { useEffect, useState, useCallback } from 'react';
import { Stack } from 'expo-router';
import { useRouter, SplashScreen } from 'expo-router';
import { GestureHandlerRootView } from 'react-native-gesture-handler';
import { SafeAreaProvider } from 'react-native-safe-area-context';
import { ActivityIndicator, View, Platform } from 'react-native';
import AsyncStorage from '@react-native-async-storage/async-storage';
import { Providers } from '@/context/providers';
import { useAuth } from '@/context/auth-context';
import { Ionicons } from '@expo/vector-icons';
import * as Font from 'expo-font';

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

// Preload fonts with timeout handling for web
async function loadFontsWithFallback() {
  try {
    // Only attempt font loading on non-web platforms or with a timeout
    if (Platform.OS !== 'web') {
      await Font.loadAsync(Ionicons.font);
    } else {
      // On web, use a race condition with timeout
      const fontLoadPromise = Font.loadAsync(Ionicons.font);
      const timeoutPromise = new Promise((resolve) => setTimeout(resolve, 3000));
      await Promise.race([fontLoadPromise, timeoutPromise]);
    }
  } catch (error) {
    console.warn('Font loading failed, using system fallback:', error);
  }
}

function RootLayoutContent() {
  const router = useRouter();
  const { isLoading } = useAuth();
  const [hasSeenOnboarding, setHasSeenOnboarding] = useState<boolean | null>(null);
  const [checkingOnboarding, setCheckingOnboarding] = useState(true);
  const [fontsLoaded, setFontsLoaded] = useState(false);

  // Load fonts
  useEffect(() => {
    loadFontsWithFallback().then(() => setFontsLoaded(true));
  }, []);

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
    if (!isLoading && !checkingOnboarding && fontsLoaded) {
      SplashScreen.hideAsync();
      
      // Show onboarding for first-time users
      if (hasSeenOnboarding === false) {
        router.replace('/(onboarding)/welcome');
      } else {
        // Show tabs for returning users (explore is public)
        router.replace('/(tabs)');
      }
    }
  }, [isLoading, checkingOnboarding, hasSeenOnboarding, fontsLoaded, router]);

  if (isLoading || checkingOnboarding || !fontsLoaded) {
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
      <SafeAreaProvider initialMetrics={{
        frame: { x: 0, y: 0, width: 0, height: 0 },
        insets: { top: 0, left: 0, right: 0, bottom: 0 },
      }}>
        <Providers>
          <RootLayoutContent />
        </Providers>
      </SafeAreaProvider>
    </GestureHandlerRootView>
  );
}
