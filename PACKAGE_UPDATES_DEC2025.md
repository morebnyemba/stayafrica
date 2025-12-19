# Package Updates - December 2025

## Summary

This document outlines all package updates made to the StayAfrica project to ensure we're using the latest stable and secure versions of all dependencies.

## Date: December 19, 2025

---

## Web Frontend Updates

### Critical Security Updates

#### Next.js 16.0.3 → 16.1.0
- **Reason**: Security patches for CVE-2025-55184 and CVE-2025-55183 (React2Shell vulnerability)
- **Impact**: Critical security fix for React Server Components
- **Breaking Changes**: None for standard usage
- **Reference**: https://nextjs.org/blog/security-update-2025-12-11

### Major Version Updates

#### mapbox-gl 2.15.0 → 3.17.0
- **Reason**: Latest stable version with improved 3D rendering and performance
- **Impact**: Major version upgrade with API changes
- **Breaking Changes**: 
  - Must update imports if using with react-map-gl
  - Some deprecated APIs removed
- **Migration**: If using mapbox-gl directly, review the Mapbox GL JS v3 migration guide

#### react-map-gl 7.1.7 → 8.1.0
- **Reason**: Required for compatibility with mapbox-gl 3.x
- **Impact**: Major version upgrade with breaking changes
- **Breaking Changes**:
  - Core logic rewritten to use Proxy for camera updates
  - Must import from specific endpoints: `react-map-gl/mapbox` instead of root
  - ESM modules only
- **Migration**: 
  ```javascript
  // Old (v7)
  import Map from 'react-map-gl';
  
  // New (v8)
  import Map from 'react-map-gl/mapbox';
  ```
- **Note**: Currently not used in code, so no immediate code changes required

#### react-datepicker 4.28.0 → 9.1.0
- **Reason**: Latest stable version
- **Impact**: Major version upgrade, may have API changes
- **Breaking Changes**: Check react-datepicker changelog for v5, v6, v7, v8, v9 changes
- **Note**: @types/react-datepicker removed as it's now deprecated (types included in main package)

### Dependency Updates

#### Core Dependencies
- `@tanstack/react-query`: 5.45.0 → 5.90.12
- `axios`: 1.7.7 → 1.7.9
- `class-variance-authority`: 0.7.0 → 0.7.1
- `react`: 18.3.0 → 18.3.1
- `react-dom`: 18.3.0 → 18.3.1
- `react-hot-toast`: 2.4.1 → 2.6.0
- `recharts`: 2.14.0 → 2.15.4
- `stripe`: 14.8.0 → 14.25.0
- `zustand`: 4.4.7 → 4.5.7
- `next-auth`: 4.24.0 → 4.24.13

#### Dev Dependencies
- `@tailwindcss/typography`: 0.5.10 → 0.5.15
- `@types/mapbox-gl`: 2.7.0 → 3.4.0 (for mapbox-gl 3.x compatibility)
- `@types/node`: 20.11.0 → 20.17.12
- `@types/react`: 18.3.0 → 18.3.18
- `@types/react-dom`: 18.3.0 → 18.3.5
- `@typescript-eslint/eslint-plugin`: 7.0.0 → 8.21.0
- `@typescript-eslint/parser`: 7.0.0 → 8.21.0
- `autoprefixer`: 10.4.18 → 10.4.20
- `eslint`: 8.56.0 → 8.57.1
- `eslint-config-next`: 16.0.0 → 16.1.0
- `postcss`: 8.4.33 → 8.4.49
- `prettier`: 3.2.0 → 3.4.2
- `tailwindcss`: 3.4.0 → 3.4.17
- `typescript`: 5.3.0 → 5.7.2

#### Removed Dependencies
- `@types/react-datepicker`: Deprecated (types now in react-datepicker package)

---

## Mobile App Updates

### Expo SDK Updates

#### Expo SDK 54.0.0 → 54.0.30
- **Reason**: Latest stable patch version
- **Impact**: Bug fixes and improvements
- **Breaking Changes**: None

### Core Dependencies
- `@react-native-async-storage/async-storage`: 2.1.0 → 2.2.0
- `@react-navigation/bottom-tabs`: 7.2.1 → 7.9.0
- `@react-navigation/native`: 7.0.16 → 7.1.26
- `@react-navigation/stack`: 7.2.1 → 7.6.13
- `@tanstack/react-query`: 5.62.0 → 5.90.12
- `expo-router`: 6.0.19 → 6.0.21
- `react-native-gesture-handler`: 2.28.0 → 2.30.0
- `react-native-reanimated`: 4.1.1 → 4.1.6
- `react-native-safe-area-context`: 5.6.0 → 5.6.2
- `react-native-screens`: 4.16.0 → 4.19.0
- `react-native-web`: 0.21.0 → 0.21.2
- `react-native-worklets`: 0.5.1 → 0.5.2
- `uuid`: 11.0.3 → 11.1.0
- `zustand`: 5.0.2 → 5.0.9

### Dev Dependencies
- `@typescript-eslint/eslint-plugin`: 8.18.2 → 8.21.0
- `@typescript-eslint/parser`: 8.18.2 → 8.21.0

---

## Configuration Updates

### web/next.config.js
- **Removed**: `swcMinify: true` (now default in Next.js 16)
- **Removed**: `experimental.turbopack: true` (moved to top-level)
- **Added**: `turbopack: {}` (enable with default settings)
- **Removed**: Custom webpack config for code splitting (Turbopack handles this)
- **Impact**: Faster builds with Turbopack enabled by default

### web/Dockerfile
- **Fixed**: Node.js version inconsistency
  - Changed builder stage from `node:18-alpine` to `node:20-alpine`
  - All stages now use `node:20-alpine` consistently
- **Impact**: Consistent Node.js version across all build stages

---

## Security Status

### Web Dependencies
- ✅ **0 vulnerabilities** found (npm audit)
- ✅ All dependencies updated to latest secure versions
- ✅ Critical Next.js security patches applied

### Mobile Dependencies
- ✅ **0 vulnerabilities** found (npm audit)
- ✅ All dependencies updated to latest stable versions compatible with Expo SDK 54

---

## Installation Instructions

### Web Application
```bash
cd web
rm -rf node_modules package-lock.json
npm install --legacy-peer-deps
```

### Mobile Application
```bash
cd mobile
rm -rf node_modules package-lock.json
npm install --legacy-peer-deps
```

### Root (if needed)
```bash
npm install
```

---

## Known Issues

### Pre-existing Code Issues
The following build errors existed before the package updates and are unrelated to the dependency changes:

1. **web/src/components/buyer/wishlist-content.tsx:199** - JSX syntax error (missing closing tag)
2. **web/src/components/host/host-properties-content.tsx:198** - JSX syntax error (missing closing tag)
3. **web/src/components/property/property-details-content.tsx:6** - Import error (default export issue)

These issues need to be fixed separately from the package updates.

### Packages with Breaking Changes (Not Currently Used)

The following packages have major version upgrades with breaking changes, but they are **not currently used** in the codebase:

1. **mapbox-gl** (2.15.0 → 3.17.0) - Listed in dependencies but no imports found
2. **react-map-gl** (7.1.7 → 8.1.0) - Listed in dependencies but no imports found  
3. **react-datepicker** (4.28.0 → 9.1.0) - Listed in dependencies but no imports found

When these packages are eventually used in the code, developers should follow the migration guides in the references section below.

---

## Migration Guide

### For Developers

#### If Using mapbox-gl or react-map-gl Directly

If your code imports these packages directly, update imports:

**Before:**
```javascript
import Map from 'react-map-gl';
import mapboxgl from 'mapbox-gl';
```

**After:**
```javascript
import Map from 'react-map-gl/mapbox';
import mapboxgl from 'mapbox-gl';
```

#### If Using react-datepicker

Review the react-datepicker changelog for versions 5-9 to see if any API changes affect your usage:
- https://github.com/Hacker0x01/react-datepicker/releases

#### TypeScript Changes

Update any type imports:
```typescript
// No longer needed - remove this import
import { ReactDatePickerProps } from '@types/react-datepicker';

// Types are now included in react-datepicker
import { ReactDatePickerProps } from 'react-datepicker';
```

---

## Testing Recommendations

1. **Test Map Components**: If you use Mapbox or map components, test thoroughly after upgrade
2. **Test Date Pickers**: Verify all date picker functionality works with react-datepicker v9
3. **Test Navigation**: Verify React Navigation works correctly in mobile app
4. **Test Auth**: Verify NextAuth still works correctly with Next.js 16.1
5. **Run Full Test Suite**: `npm test` in both web and mobile directories

---

## References

- [Next.js 16 Upgrade Guide](https://nextjs.org/docs/app/guides/upgrading/version-16)
- [Next.js Security Update Dec 2025](https://nextjs.org/blog/security-update-2025-12-11)
- [Mapbox GL JS v3 Migration](https://docs.mapbox.com/mapbox-gl-js/guides/migrate-to-v3/)
- [react-map-gl v8 Upgrade Guide](https://visgl.github.io/react-map-gl/docs/upgrade-guide)
- [Expo SDK 54 Changelog](https://expo.dev/changelog/sdk-54)

---

## Support

If you encounter issues after these updates:

1. Clear all caches: `rm -rf node_modules package-lock.json .next`
2. Reinstall: `npm install --legacy-peer-deps`
3. Check the references above for specific migration steps
4. Review error messages carefully - they often indicate required code changes

---

**Status**: ✅ All package updates completed successfully with 0 security vulnerabilities
