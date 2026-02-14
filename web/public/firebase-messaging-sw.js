/**
 * Firebase Cloud Messaging Service Worker
 * This service worker handles background push notifications for the web app.
 * 
 * SETUP REQUIRED:
 *   1. Create a Firebase project at https://console.firebase.google.com
 *   2. Enable Cloud Messaging in the project settings
 *   3. Replace the firebaseConfig below with your project's config
 *   4. Add NEXT_PUBLIC_FIREBASE_* env vars to web/.env.local
 */

// Import Firebase scripts for the service worker
importScripts('https://www.gstatic.com/firebasejs/10.12.0/firebase-app-compat.js');
importScripts('https://www.gstatic.com/firebasejs/10.12.0/firebase-messaging-compat.js');

// Firebase configuration — replace with your project's config
// These values should match the env vars in your Next.js app
const firebaseConfig = {
  apiKey: '',
  authDomain: '',
  projectId: '',
  storageBucket: '',
  messagingSenderId: '',
  appId: '',
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
  console.log('[SW] Firebase not configured — push notifications disabled');
}
