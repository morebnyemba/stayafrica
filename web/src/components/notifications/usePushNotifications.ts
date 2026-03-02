import { useState, useEffect, useCallback } from 'react';
import { useMutation, useQueryClient } from '@tanstack/react-query';
import axios from 'axios';
import { requestFCMToken, getFirebaseMessaging, onMessage } from '@/lib/firebase';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8000';

interface PushTokenData {
  token: string;
  platform: 'web' | 'android' | 'ios';
}

export const usePushNotifications = () => {
  const [isSupported, setIsSupported] = useState(false);
  const [permission, setPermission] = useState<NotificationPermission>('default');
  const [fcmToken, setFcmToken] = useState<string | null>(null);
  const queryClient = useQueryClient();

  useEffect(() => {
    if (typeof window !== 'undefined' && 'Notification' in window) {
      setIsSupported(true);
      setPermission(Notification.permission);
    }
    // Restore cached token
    const cached = typeof window !== 'undefined' ? localStorage.getItem('fcm_token') : null;
    if (cached) setFcmToken(cached);
  }, []);

  // Listen for foreground messages
  useEffect(() => {
    const msg = getFirebaseMessaging();
    if (!msg) return;

    const unsubscribe = onMessage(msg, (payload) => {
      const title = payload.notification?.title || 'StayAfrica';
      const body = payload.notification?.body || '';
      if (Notification.permission === 'granted') {
        new Notification(title, { body, icon: '/logo.png' });
      }
      queryClient.invalidateQueries({ queryKey: ['notifications'] });
    });

    return () => unsubscribe();
  }, [queryClient]);

  const registerTokenMutation = useMutation({
    mutationFn: async (data: PushTokenData) => {
      const token = localStorage.getItem('access_token');
      const response = await axios.post(
        `${API_BASE_URL}/api/v1/tokens/`,
        data,
        {
          headers: {
            Authorization: `Bearer ${token}`,
            'Content-Type': 'application/json',
          },
        }
      );
      return response.data;
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['push-tokens'] });
    },
  });

  const requestPermission = useCallback(async () => {
    if (!isSupported) {
      throw new Error('Push notifications are not supported');
    }

    const result = await Notification.requestPermission();
    setPermission(result);

    if (result === 'granted') {
      // Get FCM token from Firebase SDK
      const token = await requestFCMToken();
      if (token) {
        setFcmToken(token);
        localStorage.setItem('fcm_token', token);
        // Register with backend
        await registerTokenMutation.mutateAsync({ token, platform: 'web' });
      }
    }

    return result;
  }, [isSupported, registerTokenMutation]);

  const registerToken = async (token: string) => {
    try {
      await registerTokenMutation.mutateAsync({
        token,
        platform: 'web',
      });
      setFcmToken(token);
      localStorage.setItem('fcm_token', token);
    } catch (error) {
      console.error('Error registering push token:', error);
      throw error;
    }
  };

  const sendNotification = (title: string, options?: NotificationOptions) => {
    if (!isSupported || permission !== 'granted') {
      console.warn('Cannot send notification: permission not granted');
      return null;
    }

    return new Notification(title, {
      icon: '/logo.png',
      badge: '/logo.png',
      ...options,
    });
  };

  return {
    isSupported,
    permission,
    fcmToken,
    requestPermission,
    registerToken,
    sendNotification,
    isRegistering: registerTokenMutation.isPending,
  };
};
