import AsyncStorage from '@react-native-async-storage/async-storage';
import * as Device from 'expo-device';
import * as Notifications from 'expo-notifications';
import Constants from 'expo-constants';
import { Platform } from 'react-native';
import { STORAGE_KEYS } from '@/constants';

export type PushRegistrationResult = {
  token: string | null;
  status: Notifications.PermissionStatus;
};

Notifications.setNotificationHandler({
  handleNotification: async () => ({
    shouldShowAlert: true,
    shouldPlaySound: true,
    shouldSetBadge: false,
  }),
});

async function ensureAndroidChannel() {
  if (Platform.OS !== 'android') return;

  await Notifications.setNotificationChannelAsync('default', {
    name: 'default',
    importance: Notifications.AndroidImportance.DEFAULT,
  });
}

export async function registerForPushNotificationsAsync(): Promise<PushRegistrationResult> {
  if (!Device.isDevice) {
    return { token: null, status: 'denied' };
  }

  let { status } = await Notifications.getPermissionsAsync();

  if (status !== 'granted') {
    const permission = await Notifications.requestPermissionsAsync();
    status = permission.status;
  }

  if (status !== 'granted') {
    return { token: null, status };
  }

  await ensureAndroidChannel();

  const projectId =
    Constants.expoConfig?.extra?.eas?.projectId ?? Constants.easConfig?.projectId;

  let token = null;

  try {
    const expoToken = projectId
      ? await Notifications.getExpoPushTokenAsync({ projectId })
      : await Notifications.getExpoPushTokenAsync();
    token = expoToken.data;
  } catch (error) {
    console.warn('Failed to fetch Expo push token', error);
  }

  if (token) {
    await AsyncStorage.setItem(STORAGE_KEYS.PUSH_TOKEN, token);
  }

  return { token, status };
}

export async function getStoredPushToken() {
  return AsyncStorage.getItem(STORAGE_KEYS.PUSH_TOKEN);
}

export async function clearStoredPushToken() {
  await AsyncStorage.removeItem(STORAGE_KEYS.PUSH_TOKEN);
}
