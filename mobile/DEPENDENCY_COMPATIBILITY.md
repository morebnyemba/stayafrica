# Mobile App Dependency Compatibility Guide

## Overview
This document explains the dependency configuration for the StayAfrica mobile app, which uses Expo SDK 54 with NativeWind v4 for styling.

## Key Dependencies and Versions

### Core Framework
- **Expo SDK**: `~54.0.0` (currently 54.0.29)
- **React**: `19.1.0` (exact version required by Expo SDK 54)
- **React Native**: `0.81.5` (bundled with Expo SDK 54)
- **React DOM**: `19.1.0`

### Animation and Styling
- **react-native-reanimated**: `~3.17.4` (v3.17.5 installed)
  - ⚠️ **Important**: We use v3.x instead of v4.x because NativeWind v4 is not yet fully compatible with Reanimated v4
  - Expo SDK 54 supports both v3 and v4, but v3 is more stable with NativeWind
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
  };
};
```

**Key Changes:**
- `nativewind/babel` is in `presets` array, NOT in `plugins` array
- Added `jsxImportSource: 'nativewind'` to `babel-preset-expo` for proper JSX transformation
- This fixes the "`.plugins is not a valid Plugin property`" error

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

### react-native-reanimated: v3.x vs v4.x

**Why we chose v3.17.4:**
1. NativeWind v4.2.1 is not yet fully compatible with Reanimated v4
2. Expo SDK 54 is the last SDK to support both v3 and v4
3. v3.17.x provides stable animations with NativeWind
4. Future migration to v4 planned when NativeWind v5 is released

**Reanimated v4 differences:**
- Requires New Architecture (Fabric)
- Uses separate `react-native-worklets` package
- Breaking API changes (e.g., `runOnJS` → `scheduleOnRN`)
- Better performance but less compatible with current ecosystem

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
| react-native-reanimated | ~3.17.4 | ✅ | ✅ | v3 for NativeWind compatibility |
| nativewind | ^4.2.1 | ✅ | ✅ | v4 requires specific config |
| @testing-library/react-native | ^13.3.0 | ✅ | ✅ | v13.3+ for React 19 |
| react-test-renderer | 19.1.0 | ✅ | ✅ | Must match React version |

## Known Issues and Fixes

### Issue 1: Babel Error - ".plugins is not a valid Plugin property"
**Cause:** `nativewind/babel` was in `plugins` array instead of `presets`
**Fix:** Move it to `presets` array as shown in babel.config.js above

### Issue 2: react-native-reanimated version warning
**Warning:** "react-native-reanimated@3.15.0 - expected version: ~4.1.1"
**Explanation:** This warning can be ignored. We intentionally use v3.17.4 for NativeWind compatibility
**Alternative:** If you don't use NativeWind, upgrade to `~4.1.1` and add `react-native-worklets@~0.5.1`

### Issue 3: Testing library peer dependency errors
**Cause:** Old version of @testing-library/react-native (v12.x) doesn't support React 19
**Fix:** Upgraded to v13.3.0 and pinned react-test-renderer to 19.1.0

## Future Upgrades

### When to upgrade react-native-reanimated to v4:
1. ✅ NativeWind v5 is released with Reanimated v4 support
2. ✅ All animation-dependent libraries are v4 compatible
3. ✅ Ready to migrate to New Architecture

### Migration steps (future):
```bash
# When ready for Reanimated v4
npm install react-native-reanimated@~4.1.1 react-native-worklets@~0.5.1
# Update imports: runOnJS → scheduleOnRN, etc.
# Test thoroughly
```

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
