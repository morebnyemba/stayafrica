import '../global.css';
import { useEffect } from 'react';
import { Stack } from 'expo-router';
import { useRouter, SplashScreen } from 'expo-router';
import { GestureHandlerRootView } from 'react-native-gesture-handler';
import { SafeAreaProvider } from 'react-native-safe-area-context';
import { ActivityIndicator, View } from 'react-native';
import { Providers } from '@/context/providers';
import { useAuth } from '@/context/auth-context';

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
  const { isAuthenticated, isLoading } = useAuth();

  useEffect(() => {
    if (!isLoading) {
      SplashScreen.hideAsync();
      // Always show tabs (explore is public), users can login from there
      router.replace('/(tabs)');
    }
  }, [isLoading, router]);

  if (isLoading) {
    return (
      <View style={{ flex: 1, justifyContent: 'center', alignItems: 'center' }}>
        <ActivityIndicator size="large" color="#3A5C50" />
      </View>
    );
  }

  return (
    <Stack screenOptions={{ headerShown: false }}>
      <Stack.Screen name="(auth)" options={{ animationEnabled: false }} />
      <Stack.Screen name="(tabs)" options={{ animationEnabled: false }} />
      <Stack.Screen name="host" options={{ animationEnabled: true }} />
      <Stack.Screen name="reviews" options={{ animationEnabled: true }} />
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
