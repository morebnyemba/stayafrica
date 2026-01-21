import { Redirect } from 'expo-router';

export default function TabsIndex() {
  // Redirect to the explore tab as the default
  return <Redirect href="/(tabs)/explore" />;
}
