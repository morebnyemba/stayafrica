# StayAfrica Mobile - Build Guide

## Prerequisites

### For All Builds
- Node.js 18+ installed
- npm or yarn package manager
- Expo account (create free at https://expo.dev)

### For Local Builds (Option 2)
- Android Studio installed
- Android SDK (API 34+)
- Java Development Kit (JDK 17+)
- Android device with USB debugging enabled OR Android emulator

## Build Options

### Option 1: EAS Cloud Build (Recommended - No Android Studio Required)

#### Setup (One-time)
```bash
# Install EAS CLI globally
npm install -g eas-cli

# Login to your Expo account
eas login

# Configure the project (if not already done)
cd mobile
eas build:configure
```

#### Build Commands

**Development Build** (with debugging, fast iteration):
```bash
cd mobile
eas build --profile development --platform android
```
- Creates a debug APK (~50MB)
- Includes dev menu and debugging tools
- Hot reload enabled

**Preview Build** (testing/QA):
```bash
cd mobile
eas build --profile preview --platform android
```
- Creates a release APK (~30MB)
- Optimized but still allows testing
- No debugging overhead

**Production Build** (Play Store):
```bash
cd mobile
eas build --profile production --platform android
```
- Creates an AAB (Android App Bundle)
- Fully optimized and signed
- Ready for Google Play Store

#### Install on Device
1. After build completes, download the APK from the EAS dashboard
2. Transfer to your Android device
3. Enable "Install from unknown sources" in Settings
4. Open the APK file to install

---

### Option 2: Local Build (Requires Android Studio)

#### Setup Android Environment

1. **Install Android Studio**
   - Download from https://developer.android.com/studio
   - Install with default settings
   - Open Android Studio → SDK Manager
   - Install Android SDK Platform 34 (or latest)

2. **Set Environment Variables** (Windows PowerShell):
```powershell
# Add to your PowerShell profile or set temporarily
$env:ANDROID_HOME = "$env:LOCALAPPDATA\Android\Sdk"
$env:PATH += ";$env:ANDROID_HOME\platform-tools"
$env:PATH += ";$env:ANDROID_HOME\emulator"
```

3. **Install JDK 17**
   - Download from https://adoptium.net/
   - Or use Android Studio's embedded JDK

4. **Enable USB Debugging on Device**
   - Settings → About Phone → Tap "Build Number" 7 times
   - Settings → Developer Options → Enable "USB Debugging"
   - Connect device via USB

5. **Verify Device Connection**
```bash
adb devices
# Should show your device listed
```

#### Build Commands

**Generate Native Android Project** (first time only):
```bash
cd mobile
npx expo prebuild --platform android --clean
```

**Build and Run on Connected Device**:
```bash
cd mobile
npx expo run:android --device
```

**Build APK Only** (without running):
```bash
cd mobile/android
./gradlew assembleDebug
# APK will be at: android/app/build/outputs/apk/debug/app-debug.apk
```

**Build Release APK**:
```bash
cd mobile/android
./gradlew assembleRelease
# APK will be at: android/app/build/outputs/apk/release/app-release.apk
```

---

## Quick Start Commands

### Fastest Way to Test (Connected Device + Android Studio)
```bash
cd mobile
npx expo run:android --device
```

### Fastest Way to Share APK (No Android Studio)
```bash
cd mobile
eas build --profile preview --platform android
# Wait ~5-10 minutes, download APK from link
```

### Development with Live Reload
```bash
cd mobile
npm start
# Then press 'a' to open on Android device/emulator
```

---

## Troubleshooting

### "adb: command not found"
- Ensure Android SDK is installed
- Add `platform-tools` to PATH
- Restart terminal after setting PATH

### "Gradle build failed"
- Clean build: `cd mobile/android && ./gradlew clean`
- Clear cache: `npx expo prebuild --clean`
- Check Java version: `java -version` (should be 17+)

### "Device not detected"
- Run `adb devices`
- If "unauthorized", accept USB debugging prompt on device
- If not listed, check USB cable and drivers

### "Out of memory" during build
- Edit `mobile/android/gradle.properties`:
```
org.gradle.jvmargs=-Xmx4096m -XX:MaxMetaspaceSize=512m
```

### EAS Build Timeout
- Check build logs at https://expo.dev/accounts/[your-account]/projects/stayafrica/builds
- Large dependencies may need optimization
- Retry build: `eas build --profile preview --platform android`

---

## Production Signing (Required for Play Store)

### Generate Upload Key
```bash
keytool -genkeypair -v -storetype PKCS12 -keystore stayafrica-upload-key.keystore -alias stayafrica-key -keyalg RSA -keysize 2048 -validity 10000
```

### Configure Signing in EAS
1. Create `mobile/eas.json` (already configured)
2. Run: `eas build --profile production --platform android`
3. Follow prompts to create/upload signing credentials

---

## Environment Variables

The app uses these environment variables (configured in `eas.json`):

- `EXPO_PUBLIC_API_BASE_URL`: API server URL (default: https://api.stayafrica.app/api)
- `EXPO_PUBLIC_API_VERSION`: API version (default: v1)

To test against local backend:
```bash
# In mobile/.env
EXPO_PUBLIC_API_BASE_URL=http://10.0.2.2:8000/api  # For Android emulator
# OR
EXPO_PUBLIC_API_BASE_URL=http://YOUR_LOCAL_IP:8000/api  # For physical device
```

---

## Build Profiles Summary

| Profile | Use Case | Output | Size | Time |
|---------|----------|--------|------|------|
| `development` | Active development | Debug APK | ~50MB | ~5-10min |
| `preview` | QA/Testing | Release APK | ~30MB | ~10-15min |
| `production` | Play Store | AAB | ~20MB | ~15-20min |

---

## Next Steps

1. **First build?** Start with EAS preview build:
   ```bash
   eas build --profile preview --platform android
   ```

2. **Regular development?** Use local build:
   ```bash
   npx expo run:android --device
   ```

3. **Ready for Play Store?** Use production build:
   ```bash
   eas build --profile production --platform android
   ```

For more help: https://docs.expo.dev/build/introduction/
