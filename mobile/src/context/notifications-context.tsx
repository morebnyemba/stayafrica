import React, { createContext, useContext, useEffect, useMemo, useState } from 'react';
import * as Notifications from 'expo-notifications';
import {
  clearStoredPushToken,
  getStoredPushToken,
  registerForPushNotificationsAsync,
} from '@/services/notifications';

export type NotificationsContextValue = {
  pushToken: string | null;
  permissionStatus: Notifications.PermissionStatus | null;
  lastNotification: Notifications.Notification | null;
  refreshRegistration: () => Promise<void>;
};

const NotificationsContext = createContext<NotificationsContextValue | undefined>(undefined);

export function NotificationsProvider({ children }: { children: React.ReactNode }) {
  const [pushToken, setPushToken] = useState<string | null>(null);
  const [permissionStatus, setPermissionStatus] = useState<Notifications.PermissionStatus | null>(null);
  const [lastNotification, setLastNotification] = useState<Notifications.Notification | null>(null);

  useEffect(() => {
    const bootstrap = async () => {
      const storedToken = await getStoredPushToken();
      if (storedToken) {
        setPushToken(storedToken);
      }
    };

    const subscription = Notifications.addNotificationResponseReceivedListener((response) => {
      setLastNotification(response.notification);
    });

    bootstrap();

    return () => {
      Notifications.removeNotificationSubscription(subscription);
    };
  }, []);

  useEffect(() => {
    const register = async () => {
      const result = await registerForPushNotificationsAsync();
      setPermissionStatus(result.status);
      setPushToken(result.token);
    };

    const notificationListener = Notifications.addNotificationReceivedListener((notification) => {
      setLastNotification(notification);
    });

    register();

    return () => {
      Notifications.removeNotificationSubscription(notificationListener);
    };
  }, []);

  const refreshRegistration = async () => {
    const result = await registerForPushNotificationsAsync();
    setPermissionStatus(result.status);
    setPushToken(result.token);

    if (!result.token) {
      await clearStoredPushToken();
    }
  };

  const value = useMemo(
    () => ({ pushToken, permissionStatus, lastNotification, refreshRegistration }),
    [pushToken, permissionStatus, lastNotification],
  );

  return <NotificationsContext.Provider value={value}>{children}</NotificationsContext.Provider>;
}

export function useNotifications() {
  const context = useContext(NotificationsContext);

  if (!context) {
    throw new Error('useNotifications must be used within a NotificationsProvider');
  }

  return context;
}
