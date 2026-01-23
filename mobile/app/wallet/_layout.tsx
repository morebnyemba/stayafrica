import { Stack } from 'expo-router';

export default function WalletLayout() {
  return (
    <Stack screenOptions={{ headerShown: false }}>
      <Stack.Screen name="withdraw" />
      <Stack.Screen name="add-funds" />
      <Stack.Screen name="payment-methods" />
    </Stack>
  );
}
