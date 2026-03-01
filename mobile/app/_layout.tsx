import '../global.css';
import { useEffect, useState, useCallback, useRef } from 'react';
import { Stack } from 'expo-router';
import { useRouter, SplashScreen } from 'expo-router';
import { GestureHandlerRootView } from 'react-native-gesture-handler';
import { SafeAreaProvider } from 'react-native-safe-area-context';
import { ActivityIndicator, View, Platform, StatusBar } from 'react-native';
import AsyncStorage from '@react-native-async-storage/async-storage';
import * as Notifications from 'expo-notifications';
import { Providers } from '@/context/providers';
import { useAuth } from '@/context/auth-context';
import { Ionicons } from '@expo/vector-icons';
import * as Font from 'expo-font';
import { BrandedSplash } from '@/components/common/BrandedSplash';
import {
  registerForPushNotificationsAsync,
  addNotificationReceivedListener,
  addNotificationResponseListener,
  setBadgeCount,
} from '@/services/push-notifications';

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
  const { isLoading, user } = useAuth();
  const [hasSeenOnboarding, setHasSeenOnboarding] = useState<boolean | null>(null);
  const [checkingOnboarding, setCheckingOnboarding] = useState(true);
  const [fontsLoaded, setFontsLoaded] = useState(false);
  const [showBrandedSplash, setShowBrandedSplash] = useState(true);
  const hasNavigated = useRef(false);
  const notificationListener = useRef<Notifications.EventSubscription>();
  const responseListener = useRef<Notifications.EventSubscription>();

  // Set status bar style — light during splash, dark after
  useEffect(() => {
    StatusBar.setBarStyle(showBrandedSplash ? 'light-content' : 'dark-content');
    if (Platform.OS === 'android') {
      StatusBar.setBackgroundColor('transparent');
      StatusBar.setTranslucent(true);
    }
  }, [showBrandedSplash]);

  // Register for push notifications when user is authenticated
  useEffect(() => {
    if (user) {
      registerForPushNotificationsAsync().catch(console.warn);
    }
  }, [user]);

  // Listen for incoming notifications & tap responses
  useEffect(() => {
    // Notification received while app is in foreground
    notificationListener.current = addNotificationReceivedListener((notification) => {
      console.log('Notification received:', notification.request.content.title);
    });

    // User tapped on a notification
    responseListener.current = addNotificationResponseListener((response) => {
      const data = response.notification.request.content.data;
      // Navigate based on deep_link or notification type
      if (data?.deep_link) {
        const link = data.deep_link as string;
        if (link.includes('bookings/')) {
          const bookingId = link.split('bookings/').pop();
          if (bookingId) router.push(`/booking/${bookingId}` as any);
        } else if (link.includes('messages/')) {
          router.push(`/(tabs)/inbox` as any);
        } else if (link.includes('properties/')) {
          const propId = link.split('properties/').pop();
          if (propId) router.push(`/property/${propId}` as any);
        } else if (link.includes('reviews/')) {
          router.push(`/reviews` as any);
        }
      } else {
        // Default: open notifications screen
        router.push('/notifications' as any);
      }
      // Clear badge
      setBadgeCount(0);
    });

    return () => {
      notificationListener.current?.remove();
      responseListener.current?.remove();
    };
  }, [router]);

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
      // Hide the native splash screen; our branded splash is showing on top
      SplashScreen.hideAsync();
    }
  }, [isLoading, checkingOnboarding, fontsLoaded]);

  // Navigate after branded splash finishes
  const handleSplashFinish = useCallback(() => {
    setShowBrandedSplash(false);
    if (!hasNavigated.current) {
      hasNavigated.current = true;
      if (hasSeenOnboarding === false) {
        router.replace('/(onboarding)/welcome');
      } else {
        router.replace('/(tabs)');
      }
    }
  }, [hasSeenOnboarding, router]);

  const isReady = !isLoading && !checkingOnboarding && fontsLoaded;

  if (!isReady) {
    // Still loading — show green background matching splash
    return (
      <View style={{ flex: 1, justifyContent: 'center', alignItems: 'center', backgroundColor: '#122F26' }}>
        <ActivityIndicator size="large" color="#D9B168" />
      </View>
    );
  }

  return (
    <>
      <Stack screenOptions={{ headerShown: false }}>
        <Stack.Screen name="(onboarding)" options={{ animation: 'none' }} />
        <Stack.Screen name="(auth)" options={{ animation: 'none' }} />
        <Stack.Screen name="(tabs)" options={{ animation: 'none' }} />
        <Stack.Screen name="host" options={{ animation: 'fade' }} />
        <Stack.Screen name="reviews" options={{ animation: 'fade' }} />
        <Stack.Screen name="booking" options={{ animation: 'slide_from_right' }} />
        <Stack.Screen name="experiences" options={{ animation: 'slide_from_right' }} />
        <Stack.Screen name="notifications" options={{ animation: 'slide_from_right' }} />
        <Stack.Screen name="help" options={{ animation: 'slide_from_right' }} />
      </Stack>
      {showBrandedSplash && <BrandedSplash onFinish={handleSplashFinish} />}
    </>
  );
}

export default function RootLayout() {
  return (
    <GestureHandlerRootView style={{ flex: 1 }}>
      <SafeAreaProvider>
        <StatusBar barStyle="dark-content" backgroundColor="transparent" translucent={true} />
        <Providers>
          <RootLayoutContent />
        </Providers>
      </SafeAreaProvider>
    </GestureHandlerRootView>
  );
}
