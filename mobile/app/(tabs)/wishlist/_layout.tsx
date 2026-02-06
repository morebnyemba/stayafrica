import { Stack } from 'expo-router';

export default function WishlistLayout() {
  return (
    <Stack screenOptions={{ headerShown: false }}>
      <Stack.Screen name="index" />
    </Stack>
  );
}

WishlistLayout.displayName = 'WishlistLayout';
