import { useState, useEffect } from 'react';
import { useMutation, useQueryClient } from '@tanstack/react-query';
import axios from 'axios';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8000';

interface PushTokenData {
  token: string;
  device_type: 'web' | 'android' | 'ios';
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
  }, []);

  const registerTokenMutation = useMutation({
    mutationFn: async (data: PushTokenData) => {
      const token = localStorage.getItem('access_token');
      const response = await axios.post(
        `${API_BASE_URL}/api/v1/push-tokens/`,
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

  const requestPermission = async () => {
    if (!isSupported) {
      throw new Error('Push notifications are not supported');
    }

    try {
      const result = await Notification.requestPermission();
      setPermission(result);
      return result;
    } catch (error) {
      console.error('Error requesting notification permission:', error);
      throw error;
    }
  };

  const registerToken = async (token: string) => {
    try {
      await registerTokenMutation.mutateAsync({
        token,
        device_type: 'web',
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

    return new Notification(title, options);
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
