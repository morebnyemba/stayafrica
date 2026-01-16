'use client';

import { useEffect, useState } from 'react';
import { usePushNotifications } from './usePushNotifications';
import { Bell, BellOff, Loader2 } from 'lucide-react';

export const PushNotificationSetup = () => {
  const { 
    isSupported, 
    permission, 
    requestPermission, 
    registerToken,
    isRegistering 
  } = usePushNotifications();
  
  const [isInitializing, setIsInitializing] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    // Check if Firebase token exists in localStorage
    const existingToken = localStorage.getItem('fcm_token');
    if (existingToken && permission === 'granted') {
      // Token already registered
      return;
    }
  }, [permission]);

  const handleEnableNotifications = async () => {
    setIsInitializing(true);
    setError(null);

    try {
      const result = await requestPermission();
      
      if (result === 'granted') {
        // In a real implementation, initialize Firebase here
        // const messaging = getMessaging();
        // const token = await getToken(messaging);
        
        // For now, generate a mock token for demonstration
        const mockToken = `fcm_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
        await registerToken(mockToken);
      }
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to enable notifications');
    } finally {
      setIsInitializing(false);
    }
  };

  if (!isSupported) {
    return null;
  }

  if (permission === 'granted') {
    return (
      <div className="flex items-center gap-2 text-sm text-green-600">
        <Bell className="h-4 w-4" />
        <span>Notifications enabled</span>
      </div>
    );
  }

  if (permission === 'denied') {
    return (
      <div className="flex items-center gap-2 text-sm text-gray-500">
        <BellOff className="h-4 w-4" />
        <span>Notifications blocked</span>
      </div>
    );
  }

  return (
    <div className="space-y-2">
      <button
        onClick={handleEnableNotifications}
        disabled={isInitializing || isRegistering}
        className="inline-flex items-center gap-2 px-4 py-2 text-sm font-medium text-white bg-blue-600 rounded-lg hover:bg-blue-700 disabled:opacity-50 disabled:cursor-not-allowed"
        aria-label="Enable push notifications"
      >
        {(isInitializing || isRegistering) ? (
          <>
            <Loader2 className="h-4 w-4 animate-spin" />
            <span>Enabling...</span>
          </>
        ) : (
          <>
            <Bell className="h-4 w-4" />
            <span>Enable Notifications</span>
          </>
        )}
      </button>
      
      {error && (
        <p className="text-sm text-red-600" role="alert">
          {error}
        </p>
      )}
    </div>
  );
};
