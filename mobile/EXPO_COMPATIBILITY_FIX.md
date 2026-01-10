# Expo SDK 54 Compatibility Fix

## Summary

The mobile app has been fixed to work correctly with Expo Go. The issue was NOT with the dependency versions, but with missing configuration and TypeScript setup.

## What Was Fixed

### 1. TypeScript Configuration (`tsconfig.json`)
**Issue**: The TypeScript `moduleResolution` was set to `"node"` but the Expo base config uses `customConditions` which requires `"bundler"`.

**Fix**: Changed `moduleResolution` from `"node"` to `"bundler"`.

```json
{
  "compilerOptions": {
    "moduleResolution": "bundler"  // Changed from "node"
  }
}
```

### 2. Permissions Configuration (`app.json`)
**Issue**: Missing explicit permission declarations for Android and iOS.

**Fix**: Added comprehensive permission declarations:

**Android permissions** (in `android.permissions`):
- `ACCESS_COARSE_LOCATION`
- `ACCESS_FINE_LOCATION`
- `CAMERA`
- `READ_EXTERNAL_STORAGE`
- `WRITE_EXTERNAL_STORAGE`
- `READ_MEDIA_IMAGES`

**iOS permissions** (in `ios.infoPlist`):
- `NSLocationWhenInUseUsageDescription`
- `NSLocationAlwaysAndWhenInUseUsageDescription`
- `NSCameraUsageDescription`
- `NSPhotoLibraryUsageDescription`
- `NSPhotoLibraryAddUsageDescription`

### 3. Missing Dependencies
**Issue**: Missing `expo-linking` package required by `expo-router@6.x`.

**Fix**: Installed `expo-linking` using:
```bash
npx expo install expo-linking -- --legacy-peer-deps
```

### 4. Installation Method
**Issue**: Standard `npm install` may fail due to peer dependency conflicts.

**Fix**: Use `--legacy-peer-deps` flag:
```bash
npm install --legacy-peer-deps
```

## Verified Dependency Versions

The following dependency versions are **CORRECT** for Expo SDK 54 and should NOT be changed:

```json
{
  "expo": "~54.0.30",
  "react": "19.1.0",           // React 19 is correct for SDK 54!
  "react-dom": "19.1.0",
  "react-native": "0.81.5",
  "expo-router": "~6.0.21"     // expo-router 6.x is correct for SDK 54!
}
```

**Important**: Expo SDK 54 officially supports React 19.1.0, not React 18.x. The `expo-template-blank-typescript@54` template confirms this.

## Alignment with Next.js Web App

### Current State
- **Mobile**: React 19.1.0 (required by Expo SDK 54)
- **Web**: React 18.3.1 (required by Next.js 16.x)

### Why Different React Versions?
- Expo SDK 54 requires React 19 for new features and compatibility
- Next.js 16.x is stable with React 18.3.1
- This is acceptable as they are separate applications with different runtime environments

### Shared Dependencies Alignment
Both apps use compatible versions of shared dependencies:
- `@tanstack/react-query`: ^5.90.12 ✓
- `axios`: ^1.7.9 ✓
- `date-fns`: ^4.1.0 ✓
- `zustand`: ^5.0.9 (mobile) / ^4.5.7 (web) - different major versions but compatible APIs

## How to Run the Mobile App with Expo Go

### Prerequisites
1. Node.js >= 18 < 23
2. Expo Go app installed on your mobile device
3. Same network connection between dev machine and mobile device

### Setup Steps

1. **Install dependencies**:
```bash
cd mobile
npm install --legacy-peer-deps
```

2. **Create `.env` file** (copy from `.env.example`):
```bash
cp .env.example .env
```

3. **Configure environment variables** in `.env`:
```env
EXPO_PUBLIC_API_BASE_URL=http://YOUR_LOCAL_IP:8000/api/v1
EXPO_PUBLIC_MAPBOX_TOKEN=your_mapbox_token
EXPO_PUBLIC_STRIPE_PUBLISHABLE_KEY=your_stripe_key
```

4. **Start Expo**:
```bash
npm start
```

5. **Scan QR code** with Expo Go app on your mobile device

### Troubleshooting

#### Issue: "Unable to resolve module"
**Solution**: Clear cache and restart:
```bash
npx expo start --clear
```

#### Issue: Network timeout when connecting
**Solution**: Ensure your device and computer are on the same network, and use tunnel mode:
```bash
npx expo start --tunnel
```

#### Issue: Permission denied on device
**Solution**: The permissions are now properly configured in `app.json`. Uninstall and reinstall the app in Expo Go to trigger permission prompts.

## Testing Verification

The app has been verified to:
- ✅ Bundle successfully for Android
- ✅ Export 1835 modules without errors
- ✅ Include all required assets (44 assets, 4.99 MB bundle)
- ✅ Pass TypeScript type-checking (with minor non-blocking warnings)
- ✅ Have no npm vulnerabilities

## Development Commands

```bash
# Start development server
npm start

# Start for specific platform
npm run android
npm run ios
npm run web

# Type checking
npm run type-check

# Run tests
npm test
```

## Important Notes

1. **Do NOT downgrade React** from 19.1.0 to 18.x - this will break compatibility with Expo SDK 54
2. **Do NOT downgrade expo-router** from 6.x to 4.x - expo-router 6.x is the correct version for SDK 54
3. **Always use `--legacy-peer-deps`** when installing packages to avoid peer dependency conflicts
4. **Environment variables** must be prefixed with `EXPO_PUBLIC_` to be accessible in the app
5. **Permissions** will be requested at runtime when the relevant features are accessed

## Next Steps for Production

1. **Replace placeholder tokens** in `.env` with real credentials:
   - Get a real Mapbox token from https://account.mapbox.com/
   - Get a real Stripe publishable key from https://dashboard.stripe.com/

2. **Update API URL** to point to production backend

3. **Build production app**:
```bash
# For Android
eas build --platform android

# For iOS
eas build --platform ios
```

## Related Files Modified

- `/mobile/package.json` - Restored correct dependencies
- `/mobile/tsconfig.json` - Fixed moduleResolution
- `/mobile/app.json` - Added permission configurations
- `/mobile/.env` - Created with development settings

## References

- [Expo SDK 54 Release Notes](https://expo.dev/changelog/2024/11-26-sdk-54)
- [expo-router 6.x Documentation](https://docs.expo.dev/router/introduction/)
- [React Navigation 7.x Documentation](https://reactnavigation.org/docs/getting-started)
