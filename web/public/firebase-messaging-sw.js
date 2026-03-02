/**
 * Firebase Cloud Messaging Service Worker
 * Handles background push notifications for the web app.
 * 
 * SETUP: Replace the empty strings below with your Firebase project config
 * from https://console.firebase.google.com → Project Settings → General → Your apps
 * These MUST match the NEXT_PUBLIC_FIREBASE_* env vars in your .env.local
 */

// Import Firebase scripts for the service worker
importScripts('https://www.gstatic.com/firebasejs/10.12.0/firebase-app-compat.js');
importScripts('https://www.gstatic.com/firebasejs/10.12.0/firebase-messaging-compat.js');

// Firebase configuration — copy values from your Firebase Console
const firebaseConfig = {
  apiKey: self.__FIREBASE_CONFIG__?.apiKey || '',
  authDomain: self.__FIREBASE_CONFIG__?.authDomain || '',
  projectId: self.__FIREBASE_CONFIG__?.projectId || '',
  storageBucket: self.__FIREBASE_CONFIG__?.storageBucket || '',
  messagingSenderId: self.__FIREBASE_CONFIG__?.messagingSenderId || '',
  appId: self.__FIREBASE_CONFIG__?.appId || '',
};

// Only initialize if config is set
if (firebaseConfig.apiKey) {
  firebase.initializeApp(firebaseConfig);
  const messaging = firebase.messaging();

  // Handle background messages (when app tab is not focused)
  messaging.onBackgroundMessage((payload) => {
    console.log('[SW] Background message received:', payload);

    const notificationTitle = payload.notification?.title || 'StayAfrica';
    const notificationOptions = {
      body: payload.notification?.body || '',
      icon: '/logo.png',
      badge: '/logo.png',
      tag: payload.data?.notification_id || 'stayafrica-notification',
      data: payload.data || {},
      actions: [
        { action: 'open', title: 'View' },
        { action: 'dismiss', title: 'Dismiss' },
      ],
    };

    self.registration.showNotification(notificationTitle, notificationOptions);
  });

  // Handle notification click
  self.addEventListener('notificationclick', (event) => {
    event.notification.close();

    const deepLink = event.notification.data?.deep_link;
    let url = '/';

    if (deepLink) {
      // Convert stayafrica:// deep links to web paths
      if (deepLink.includes('bookings/')) {
        const id = deepLink.split('bookings/').pop();
        url = `/bookings/${id}`;
      } else if (deepLink.includes('messages/')) {
        url = '/inbox';
      } else if (deepLink.includes('properties/')) {
        const id = deepLink.split('properties/').pop();
        url = `/properties/${id}`;
      } else if (deepLink.includes('reviews/')) {
        url = '/reviews';
      }
    }

    event.waitUntil(
      clients.matchAll({ type: 'window', includeUncontrolled: true }).then((clientList) => {
        // Focus existing tab if available
        for (const client of clientList) {
          if (client.url.includes(self.location.origin) && 'focus' in client) {
            client.navigate(url);
            return client.focus();
          }
        }
        // Otherwise open new tab
        if (clients.openWindow) {
          return clients.openWindow(url);
        }
      })
    );
  });
} else {
  console.log('[SW] Firebase not configured — waiting for config via postMessage');
}

// Accept config from the main app via postMessage
self.addEventListener('message', (event) => {
  if (event.data?.type === 'FIREBASE_CONFIG' && event.data.config?.apiKey) {
    self.__FIREBASE_CONFIG__ = event.data.config;
    if (!firebase.apps.length) {
      firebase.initializeApp(event.data.config);
      console.log('[SW] Firebase initialized via postMessage');
    }
  }
});
