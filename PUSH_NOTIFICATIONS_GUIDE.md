# Push Notifications Implementation

## Overview
Push notification system using Firebase Cloud Messaging (FCM) for cross-platform support.

## Features Implemented
- ✅ Push token management (iOS, Android, Web)
- ✅ Notification preferences per user
- ✅ Notification history and tracking
- ✅ Automated notifications for key events:
  - Booking confirmed
  - Booking cancelled
  - New message received
  - Payment received (for hosts)
  - Review reminder after checkout
  - Price drops on wishlisted properties
- ✅ Admin interface for managing notifications
- ✅ REST API endpoints for mobile/web integration

## Setup Required

### 1. Install Firebase Admin SDK
```bash
pip install firebase-admin
```

### 2. Get Firebase Credentials
1. Go to Firebase Console: https://console.firebase.google.com/
2. Select your project (or create new one)
3. Go to Project Settings > Service Accounts
4. Click "Generate New Private Key"
5. Download the JSON file
6. Save it securely (e.g., `/path/to/firebase-credentials.json`)

### 3. Configure Django Settings
Add to `settings.py`:
```python
# Firebase configuration
FIREBASE_CREDENTIALS_PATH = '/path/to/firebase-credentials.json'
```

### 4. Run Migrations
```bash
python manage.py makemigrations notifications
python manage.py migrate notifications
```

## API Endpoints

### Register Push Token
```
POST /api/v1/notifications/tokens/
{
  "token": "fcm_device_token_here",
  "platform": "ios|android|web",
  "device_id": "optional_device_id"
}
```

### Get Notification Preferences
```
GET /api/v1/notifications/preferences/
```

### Update Notification Preferences
```
PUT /api/v1/notifications/preferences/
{
  "booking_confirmed": true,
  "new_message": true,
  "price_drop": false,
  ...
}
```

### Get Notifications
```
GET /api/v1/notifications/notifications/
```

### Get Unread Count
```
GET /api/v1/notifications/notifications/unread_count/
```

### Mark as Read
```
POST /api/v1/notifications/notifications/{id}/mark_read/
```

### Mark All as Read
```
POST /api/v1/notifications/notifications/mark_all_read/
```

## Mobile Integration

### iOS (React Native with Expo)
```javascript
import * as Notifications from 'expo-notifications';
import Constants from 'expo-constants';

// Request permission
const { status } = await Notifications.requestPermissionsAsync();

// Get token
const token = (await Notifications.getExpoPushTokenAsync({
  projectId: Constants.expoConfig.extra.eas.projectId,
})).data;

// Register token with backend
await api.post('/notifications/tokens/', {
  token: token,
  platform: 'ios',
  device_id: Constants.deviceId,
});

// Handle received notifications
Notifications.addNotificationReceivedListener(notification => {
  console.log('Notification received:', notification);
});

// Handle notification taps
Notifications.addNotificationResponseReceivedListener(response => {
  const { deep_link } = response.notification.request.content.data;
  // Navigate to deep_link
});
```

### Android (Same as iOS)
Just change platform to 'android':
```javascript
await api.post('/notifications/tokens/', {
  token: token,
  platform: 'android',
  device_id: Constants.deviceId,
});
```

### Web (Firebase SDK)
```javascript
import { initializeApp } from 'firebase/app';
import { getMessaging, getToken, onMessage } from 'firebase/messaging';

// Initialize Firebase
const app = initializeApp(firebaseConfig);
const messaging = getMessaging(app);

// Request permission and get token
const token = await getToken(messaging, {
  vapidKey: 'your_vapid_key',
});

// Register with backend
await api.post('/notifications/tokens/', {
  token: token,
  platform: 'web',
});

// Listen for messages
onMessage(messaging, (payload) => {
  console.log('Message received:', payload);
  // Show notification to user
});
```

## Sending Notifications (Backend)

### From Code
```python
from services.notification_service import NotificationService

# Send booking confirmation
NotificationService.send_booking_confirmation(booking)

# Send new message notification
NotificationService.send_new_message(message)

# Send custom notification
NotificationService.send_notification(
    user=user,
    title="Custom Title",
    body="Custom message body",
    notification_type='system',
    data={'custom_key': 'custom_value'},
    deep_link='stayafrica://custom/path'
)
```

### From Admin
- Go to Django admin
- Navigate to Notifications > Notifications
- View all sent notifications
- Filter by user, type, status

## Testing Without Firebase

The system is designed to work without Firebase configured:
- Notifications are logged to database
- API endpoints work normally
- No FCM messages are sent
- Perfect for development/testing

## Deep Linking

Notifications include deep links for navigation:
- `stayafrica://bookings/{id}` - View booking
- `stayafrica://messages/{conversation_id}` - Open conversation
- `stayafrica://properties/{id}` - View property
- `stayafrica://reviews/create/{booking_id}` - Create review
- `stayafrica://payments/{id}` - View payment

Mobile app should handle these deep links for seamless navigation.

## Monitoring

### Check Notification Delivery
- Admin > Notifications > Notifications
- Filter by status: sent, delivered, failed
- Check `fcm_message_id` for tracking
- Review `error_message` for failures

### Common Issues
1. **Invalid token**: Token deactivated automatically
2. **Firebase not initialized**: Check credentials path
3. **Permission denied**: User hasn't granted permission
4. **Token expired**: User needs to re-register

## Security

- FCM tokens are encrypted in transit (HTTPS)
- Tokens stored securely in database
- User can deactivate tokens anytime
- Notification content never includes sensitive data
- Deep links validated before navigation

## Performance

- Batch sending for multiple users
- Async processing (no blocking)
- Failed tokens auto-deactivated
- Database indexes on key fields
- Notification history auto-pruned (optional)

## Future Enhancements
- [ ] Scheduled notifications (Celery Beat)
- [ ] Rich notifications with images
- [ ] Action buttons in notifications
- [ ] Notification grouping/threading
- [ ] Analytics dashboard
- [ ] A/B testing notifications
