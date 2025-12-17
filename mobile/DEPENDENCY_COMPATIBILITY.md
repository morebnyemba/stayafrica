# Mobile App Dependency Compatibility Guide

> **⚠️ Updated Configuration**: This app now uses `react-native-reanimated` v4.1.1 (Expo SDK 54's default) along with `react-native-worklets` v0.5.1. This resolves the bundling error "Cannot find module 'react-native-worklets/plugin'" and ensures compatibility with Expo SDK 54.

## Overview
This document explains the dependency configuration for the StayAfrica mobile app, which uses Expo SDK 54 with NativeWind v4 for styling.

## Key Dependencies and Versions

### Core Framework
- **Expo SDK**: `~54.0.0` (currently 54.0.29)
- **React**: `19.1.0` (exact version required by Expo SDK 54)
- **React Native**: `0.81.5` (bundled with Expo SDK 54)
- **React DOM**: `19.1.0`

### Animation and Styling
- **react-native-reanimated**: `~4.1.1` (Expo SDK 54's default)
  - ✅ Now using v4.1.1 as recommended by Expo
  - Requires `react-native-worklets` v0.5.1 as a peer dependency
  - Requires the reanimated plugin in babel configuration
- **react-native-worklets**: `~0.5.1`
  - Required by react-native-reanimated v4.x
  - Version ~0.5.1 is the stable version for Expo SDK 54
- **NativeWind**: `^4.2.1`
  - Provides Tailwind CSS-like styling for React Native

### Testing
- **@testing-library/react-native**: `^13.3.0`
  - v13.3+ required for React 19 support
- **react-test-renderer**: `19.1.0`
  - Must match React version exactly
- **Jest**: `^29.7.0`

## Configuration Files

### babel.config.js
```javascript
module.exports = function(api) {
  api.cache(true);
  return {
    presets: [
      ['babel-preset-expo', { jsxImportSource: 'nativewind' }],
      'nativewind/babel',
    ],
    plugins: [
      'react-native-reanimated/plugin',
    ],
  };
};
```

**Key Configuration:**
- `nativewind/babel` is in `presets` array, NOT in `plugins` array
- Added `jsxImportSource: 'nativewind'` to `babel-preset-expo` for proper JSX transformation with NativeWind
- **NEW**: Added `react-native-reanimated/plugin` to the `plugins` array (required for Reanimated v4)
  - This plugin must be listed last in the plugins array
  - It enables worklet support and optimizations for Reanimated v4

### metro.config.js
```javascript
const { getDefaultConfig } = require('expo/metro-config');
const { withNativeWind } = require('nativewind/metro');

const config = getDefaultConfig(__dirname);

module.exports = withNativeWind(config, { input: './global.css' });
```

**Key Changes:**
- Wrapped default Metro config with `withNativeWind`
- Configured to use `global.css` for Tailwind directives

### app/_layout.tsx
```typescript
import '../global.css'; // Must be first import
// ... other imports
```

**Key Changes:**
- Added import of `global.css` at the top of the root layout file
- This ensures Tailwind styles are available throughout the app

## Why These Versions?

### react-native-reanimated: v4.1.1 (Updated from v3.x)

**Why we upgraded to v4.1.1:**
1. Expo SDK 54 recommends and expects `react-native-reanimated` v4.1.1
2. The build was failing with error "Cannot find module 'react-native-worklets/plugin'"
3. v4.x provides better performance and is the future-proof choice
4. v4.x works with NativeWind v4.2.1 when properly configured

**Reanimated v4 requirements:**
- Requires `react-native-worklets` v0.5.1 as a peer dependency
- Requires the `react-native-reanimated/plugin` in babel configuration
- Breaking API changes from v3 (e.g., `runOnJS` → `scheduleOnRN`) - migration may be needed if using advanced features
- For best performance, consider enabling the New Architecture (Fabric) in the future

**Note on NativeWind compatibility:**
- While some sources indicate potential issues between NativeWind v4 and Reanimated v4, the configuration we use (with proper babel setup) resolves these issues
- If you encounter animation issues, ensure all worklet-based code is properly migrated to v4 APIs

### React 19.1.0 (not 19.2.x)

**Why we stay on 19.1.0:**
1. Expo SDK 54 officially ships with React 19.1.0
2. Upgrading to 19.2.x can cause version mismatch errors with react-native-renderer
3. `npx expo install --check` and `npx expo-doctor` verify this version
4. Minor React updates should wait for Expo SDK updates

## Installation Instructions

### First Time Setup
```bash
cd mobile
npm install

# For clean native builds (important after config changes)
npx expo prebuild --clean
```

### Updating Dependencies
To ensure all dependencies are compatible with Expo SDK 54:
```bash
npx expo install --fix
npx expo-doctor
```

### Clearing Cache
If you encounter bundling issues:
```bash
npm start -- --clear
# or
npx expo start --clear
```

## Compatibility Matrix

| Package | Version | Expo SDK 54 | React 19 | Notes |
|---------|---------|-------------|----------|-------|
| expo | ~54.0.0 | ✅ | ✅ | Primary dependency |
| react | 19.1.0 | ✅ | ✅ | Exact version required |
| react-native | 0.81.5 | ✅ | ✅ | Bundled with Expo |
| react-native-reanimated | ~4.1.1 | ✅ | ✅ | v4 as recommended by Expo SDK 54 |
| react-native-worklets | ~0.5.1 | ✅ | ✅ | Required by Reanimated v4 |
| nativewind | ^4.2.1 | ✅ | ✅ | v4 requires specific config |
| @testing-library/react-native | ^13.3.0 | ✅ | ✅ | v13.3+ for React 19 |
| react-test-renderer | 19.1.0 | ✅ | ✅ | Must match React version |

## Known Issues and Fixes

### Issue 1: "Cannot find module 'react-native-worklets/plugin'" Error
**Cause:** React Native Reanimated v4 requires `react-native-worklets` as a peer dependency
**Fix:** 
1. Install `react-native-worklets@~0.5.1` (as done in current package.json)
2. Add `react-native-reanimated/plugin` to babel.config.js plugins array
3. Clear cache with `npm start -- --clear`

### Issue 2: Babel Error - ".plugins is not a valid Plugin property"
**Cause:** `nativewind/babel` was in `plugins` array instead of `presets`
**Fix:** Move it to `presets` array as shown in babel.config.js above

### Issue 3: Testing library peer dependency errors
**Cause:** Old version of @testing-library/react-native (v12.x) doesn't support React 19
**Fix:** Upgraded to v13.3.0 and pinned react-test-renderer to 19.1.0

## Migration from Reanimated v3 to v4

If you have existing code using Reanimated v3 APIs, you may need to update:

### Common API Changes:
- `runOnJS` → `scheduleOnRN` (for calling JS functions from worklets)
- Some animation utilities may have changed
- Worklet callbacks may need adjustment

### Testing:
After upgrading, thoroughly test all animations and gestures in your app to ensure they work correctly with v4.

## References

- [Expo SDK 54 Upgrade Guide](https://expo.dev/blog/expo-sdk-upgrade-guide)
- [Expo SDK 54 Changelog](https://expo.dev/changelog/sdk-54)
- [NativeWind v4 Installation](https://www.nativewind.dev/docs/getting-started/installation)
- [Reanimated Compatibility Table](https://docs.swmansion.com/react-native-reanimated/docs/guides/compatibility/)
- [React Native Testing Library v13](https://callstack.github.io/react-native-testing-library/)

## Support

For issues or questions:
1. Run `npx expo-doctor` to check for configuration issues
2. Check Expo's troubleshooting docs
3. Review [expo/fyi repo](https://github.com/expo/fyi) for known issues

---

**Last Updated:** December 17, 2025
**Expo SDK Version:** 54.0.29
**React Version:** 19.1.0
