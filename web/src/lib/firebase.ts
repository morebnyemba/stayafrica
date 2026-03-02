import { initializeApp, getApps, type FirebaseApp } from 'firebase/app';
import { getMessaging, getToken, onMessage, type Messaging } from 'firebase/messaging';

const firebaseConfig = {
  apiKey: process.env.NEXT_PUBLIC_FIREBASE_API_KEY,
  authDomain: process.env.NEXT_PUBLIC_FIREBASE_AUTH_DOMAIN,
  projectId: process.env.NEXT_PUBLIC_FIREBASE_PROJECT_ID,
  storageBucket: process.env.NEXT_PUBLIC_FIREBASE_STORAGE_BUCKET,
  messagingSenderId: process.env.NEXT_PUBLIC_FIREBASE_MESSAGING_SENDER_ID,
  appId: process.env.NEXT_PUBLIC_FIREBASE_APP_ID,
};

let app: FirebaseApp | null = null;
let messaging_instance: Messaging | null = null;

function isConfigured(): boolean {
  return !!(firebaseConfig.apiKey && firebaseConfig.projectId);
}

export function getFirebaseApp(): FirebaseApp | null {
  if (!isConfigured()) return null;
  if (!app) {
    app = getApps().length === 0 ? initializeApp(firebaseConfig) : getApps()[0];
  }
  return app;
}

export function getFirebaseMessaging(): Messaging | null {
  if (typeof window === 'undefined') return null;
  const fbApp = getFirebaseApp();
  if (!fbApp) return null;
  if (!messaging_instance) {
    messaging_instance = getMessaging(fbApp);
  }
  return messaging_instance;
}

/**
 * Request an FCM token for web push.
 * Registers the service worker and injects Firebase config into it.
 */
export async function requestFCMToken(): Promise<string | null> {
  const msg = getFirebaseMessaging();
  if (!msg) return null;

  const vapidKey = process.env.NEXT_PUBLIC_FIREBASE_VAPID_KEY;
  if (!vapidKey) {
    console.warn('NEXT_PUBLIC_FIREBASE_VAPID_KEY not set — cannot get FCM token');
    return null;
  }

  // Register SW and inject config so background messages work
  const registration = await navigator.serviceWorker.register('/firebase-messaging-sw.js');
  if (registration.active) {
    registration.active.postMessage({
      type: 'FIREBASE_CONFIG',
      config: firebaseConfig,
    });
  }

  const token = await getToken(msg, { vapidKey, serviceWorkerRegistration: registration });
  return token || null;
}

export { onMessage };
