# Mobile App - Development & Emulation Guide

## Quick Start

This guide will help you set up and run the StayAfrica mobile app in an emulator.

### Prerequisites

1. **Node.js** (v16+) - Download from https://nodejs.org
2. **Expo CLI** - Install globally:
   ```bash
   npm install -g expo-cli
   ```

### Android Emulator Setup (Windows)

#### Option 1: Using Android Studio (Recommended)
1. Download Android Studio from https://developer.android.com/studio
2. Install Android Studio with default settings
3. Open Android Studio and go to **Virtual Device Manager**
4. Create a new virtual device (Pixel 5 recommended)
5. Start the emulator

#### Option 2: Using Expo Go App
1. Download **Expo Go** app on your physical Android device
2. No emulator installation needed

### iOS Emulator Setup (Mac only)
1. Install Xcode from App Store
2. Open Terminal and run:
   ```bash
   xcode-select --install
   ```
3. Xcode will handle the iOS simulator

---

## Starting the Mobile App

### Step 1: Install Dependencies

```bash
cd "d:\High Value Projects\stayafrica\mobile"
npm install
```

### Step 2: Start Expo Development Server

```bash
npm start
```

This will show a QR code and menu options:
- Press `a` for Android emulator
- Press `i` for iOS simulator
- Press `w` for web preview
- Press `e` to send the link to your phone via email

### Step 3: Connect to Emulator

#### If using Android Emulator:
1. Keep the emulator running
2. Press `a` in the Expo terminal
3. Wait for the app to build and install

#### If using Expo Go App:
1. Open **Expo Go** on your phone
2. Scan the QR code from the terminal
3. Wait for the app to load

---

## Complete Startup Commands (All at Once)

Run these in separate terminal windows:

**Terminal 1: Backend Server**
```bash
cd "d:\High Value Projects\stayafrica\backend"
python manage.py runserver
```

**Terminal 2: Frontend Web**
```bash
cd "d:\High Value Projects\stayafrica\web"
npm run dev
```

**Terminal 3: Mobile App**
```bash
cd "d:\High Value Projects\stayafrica\mobile"
npm install
npm start
```

Then press `a` in Terminal 3 to start the Android emulator.

---

## Mobile App Features Implemented

### 🏠 Guest Features

**Explore Screen** (`/(tabs)/explore`)
- Browse all available properties
- Search properties by name or city
- View property details with pricing and amenities
- Save properties for later
- Beautiful card-based UI with images

**Property Details** (`/(tabs)/explore/[id]`)
- Full property information
- Price breakdown with service fees
- Amenities list with icons
- Rating and reviews
- Book Now button

**Bookings Screen** (`/(tabs)/bookings`)
- View all bookings
- Filter by status (pending, confirmed, cancelled, completed)
- See booking dates and total price
- Cancel or modify bookings
- Track booking history

**Messages Screen** (`/(tabs)/messages`)
- Chat with hosts
- View conversation list
- Unread message count badges
- Quick reply functionality
- Typing indicators

**Profile Screen** (`/(tabs)/profile`)
- View user information
- Edit profile details
- Change password
- Logout functionality
- Role badge (Guest/Host)

### 🏨 Host Features (when user role = host)

Additional screens will display:
- **My Listings** - View and manage properties
- **Bookings** - Accept/decline booking requests
- **Earnings** - View revenue and statistics
- **Reviews** - Manage guest reviews

---

## Screens Structure

```
app/
├── (auth)/                    # Authentication screens
│   ├── login.tsx             # Login screen
│   ├── register.tsx          # Registration (multi-step form)
│   └── _layout.tsx           # Auth layout wrapper
│
├── (tabs)/                   # Main app screens
│   ├── explore/
│   │   ├── index.tsx         # Property list
│   │   └── [id].tsx          # Property details
│   │
│   ├── bookings/
│   │   ├── index.tsx         # Bookings list
│   │   ├── [id].tsx          # Booking details
│   │   └── create.tsx        # Create booking
│   │
│   ├── messages/
│   │   ├── index.tsx         # Conversations list
│   │   └── [id].tsx          # Chat screen
│   │
│   ├── profile/
│   │   ├── index.tsx         # User profile
│   │   ├── edit.tsx          # Edit profile
│   │   └── change-password.tsx
│   │
│   └── _layout.tsx           # Bottom tab navigator
│
└── _layout.tsx               # Root layout
```

---

## API Integration

The mobile app connects to the backend at:
- **Base URL**: `http://localhost:8000`
- **API Version**: `v1`

Key endpoints:
```
POST   /api/v1/users/register/          # User registration
POST   /api/v1/auth/login/               # User login
GET    /api/v1/properties/               # List properties
GET    /api/v1/properties/{id}/          # Property details
POST   /api/v1/bookings/                 # Create booking
GET    /api/v1/bookings/                 # List bookings
GET    /api/v1/users/profile/            # User profile
GET    /api/v1/messaging/conversations/  # List conversations
```

---

## Styling & Design

The app uses:
- **NativeWind** - Tailwind CSS for React Native
- **Lucide React Native** - Beautiful icons
- **React Native Paper** - Material Design components
- **Custom Colors** - Defined in Tailwind config:
  - Primary: `#8B4513` (Brown)
  - Secondary: `#D4A574` (Sandy)
  - Accent: `#C67C4E` (Burnt Orange)

---

## Testing Credentials

Use these to test the app:

**Account 1 (Guest)**
- Email: `guest@example.com`
- Password: `Password123`

**Account 2 (Host)**
- Email: `host@example.com`
- Password: `Password123`

---

## Common Issues & Solutions

### Issue: "Cannot find module 'react-native'"
**Solution**: Run `npm install` in the mobile directory

### Issue: Emulator won't start
**Solution**: 
1. Restart Android Studio
2. Delete and recreate the virtual device
3. Check if 8GB RAM is available

### Issue: App shows blank white screen
**Solution**:
1. Check backend is running on http://localhost:8000
2. Clear Expo cache: `expo start -c`
3. Check console for errors: `npm start` will show them

### Issue: Can't connect to backend
**Solution**:
1. Ensure backend is running: `python manage.py runserver`
2. Check .env file has correct API URL
3. If using emulator, use `10.0.2.2` instead of `localhost`

---

## Development Tips

1. **Hot Reload**: Changes to code automatically reload the app
2. **Developer Menu**: Press `Cmd+D` (iOS) or `Ctrl+M` (Android)
3. **Debug Logs**: Check the terminal running `npm start`
4. **Network Tab**: Press `D` to open Developer Menu for network debugging
5. **Reload**: Press `R` in terminal to force reload

---

## Next Steps

After getting the app running:

1. Test user registration and login
2. Explore properties and view details
3. Create test bookings
4. Test messaging between users
5. Verify payment integration with backend

---

## For More Help

- Expo Documentation: https://docs.expo.dev
- React Native: https://reactnative.dev
- Tailwind NativeWind: https://www.nativewind.dev

---

**Last Updated**: December 11, 2025
**Maintainer**: StayAfrica Dev Team
