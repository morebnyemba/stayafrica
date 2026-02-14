/**
 * Push Notification Service
 * Handles Expo push notifications registration, permissions, and token management.
 * Works with both Expo Go (Expo Push Token) and standalone builds (FCM/APNs).
 */
import { Platform } from 'react-native';
import * as Notifications from 'expo-notifications';
import * as Device from 'expo-device';
import Constants from 'expo-constants';
import { apiClient } from './api-client';

// Configure how notifications appear when app is in foreground
Notifications.setNotificationHandler({
  handleNotification: async () => ({
    shouldShowAlert: true,
    shouldPlaySound: true,
    shouldSetBadge: true,
    shouldShowBanner: true,
    shouldShowList: true,
  }),
});

/**
 * Register for push notifications and return the Expo push token.
 * On physical devices it requests permission, gets the token,
 * and registers it with the backend.
 */
export async function registerForPushNotificationsAsync(): Promise<string | null> {
  // Push notifications only work on physical devices
  if (!Device.isDevice) {
    console.log('Push notifications require a physical device');
    return null;
  }

  // Check / request permission
  const { status: existingStatus } = await Notifications.getPermissionsAsync();
  let finalStatus = existingStatus;

  if (existingStatus !== 'granted') {
    const { status } = await Notifications.requestPermissionsAsync();
    finalStatus = status;
  }

  if (finalStatus !== 'granted') {
    console.log('Push notification permission not granted');
    return null;
  }

  try {
    // Get Expo push token (works in Expo Go and standalone)
    const projectId = Constants.expoConfig?.extra?.eas?.projectId;
    const tokenData = await Notifications.getExpoPushTokenAsync({
      projectId,
    });
    const token = tokenData.data;

    // Android needs a notification channel
    if (Platform.OS === 'android') {
      await Notifications.setNotificationChannelAsync('default', {
        name: 'Default',
        importance: Notifications.AndroidImportance.MAX,
        vibrationPattern: [0, 250, 250, 250],
        lightColor: '#3A5C50',
        sound: 'default',
      });

      await Notifications.setNotificationChannelAsync('bookings', {
        name: 'Bookings',
        description: 'Booking confirmations, reminders, and updates',
        importance: Notifications.AndroidImportance.HIGH,
        sound: 'default',
      });

      await Notifications.setNotificationChannelAsync('messages', {
        name: 'Messages',
        description: 'New messages from guests and hosts',
        importance: Notifications.AndroidImportance.HIGH,
        sound: 'default',
      });

      await Notifications.setNotificationChannelAsync('payments', {
        name: 'Payments',
        description: 'Payment receipts and payout notifications',
        importance: Notifications.AndroidImportance.DEFAULT,
        sound: 'default',
      });
    }

    // Register token with backend
    try {
      await apiClient.registerPushToken({
        token,
        platform: Platform.OS as 'ios' | 'android',
        device_name: Device.modelName || undefined,
      });
      console.log('Push token registered with backend:', token.substring(0, 30) + '...');
    } catch (err) {
      console.warn('Failed to register push token with backend:', err);
    }

    return token;
  } catch (error) {
    console.error('Error getting push token:', error);
    return null;
  }
}

/**
 * Schedule a local notification (useful for testing or offline reminders)
 */
export async function scheduleLocalNotification(
  title: string,
  body: string,
  data?: Record<string, any>,
  seconds: number = 1
): Promise<string> {
  return await Notifications.scheduleNotificationAsync({
    content: {
      title,
      body,
      data,
      sound: 'default',
    },
    trigger: {
      type: Notifications.SchedulableTriggerInputTypes.TIME_INTERVAL,
      seconds,
    },
  });
}

/**
 * Get the current badge count
 */
export async function getBadgeCount(): Promise<number> {
  return await Notifications.getBadgeCountAsync();
}

/**
 * Set the badge count
 */
export async function setBadgeCount(count: number): Promise<void> {
  await Notifications.setBadgeCountAsync(count);
}

/**
 * Dismiss all displayed notifications
 */
export async function dismissAllNotifications(): Promise<void> {
  await Notifications.dismissAllNotificationsAsync();
}

/**
 * Add a listener for notifications received while app is in foreground
 */
export function addNotificationReceivedListener(
  callback: (notification: Notifications.Notification) => void
) {
  return Notifications.addNotificationReceivedListener(callback);
}

/**
 * Add a listener for when user taps on a notification
 */
export function addNotificationResponseListener(
  callback: (response: Notifications.NotificationResponse) => void
) {
  return Notifications.addNotificationResponseReceivedListener(callback);
}
