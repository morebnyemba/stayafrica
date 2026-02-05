# Mobile ↔ Web Parity Checklist

**Last Updated:** February 3, 2026

---

## Executive Summary

This document tracks feature parity between the Web (Next.js) and Mobile (React Native/Expo) frontends.

- **Web routes**: 15+ main routes + auth
- **Mobile routes**: 18 main routes + auth + additional flows
- **Missing in Mobile**: 1 screen (Property landing page)
- **Form parity gap**: Property form is single-page in mobile vs. 4-step form in web
- **Keyboard/SafeArea issues**: 7 screens need keyboard handling standardization

**Recent Updates (Feb 5, 2026):**
- ✅ Added Forgot Password screen (mobile + web)
- ✅ Added Email Verification screen (mobile + web)
- ✅ Added Payments Overview screen (mobile)
- ✅ Added About/Help screen (mobile)
- ✅ **Upgraded Mobile Property Forms to Multi-Step Wizard** (Feb 5, 2026)
- ✅ **Added Web Host Reviews Page** (Feb 5, 2026)

---

## 1. Screen-by-Screen Parity Matrix

### Authentication Flows

| Feature | Web | Mobile | Status | Notes |
|---------|-----|--------|--------|-------|
| **Login** | [web/src/app/(auth)/login](web/src/app/(auth)/login) | [mobile/app/(auth)/login.tsx](mobile/app/(auth)/login.tsx) | ✅ Present | Both have email/password |
| **Register** | [web/src/app/(auth)/register](web/src/app/(auth)/register) | [mobile/app/(auth)/register.tsx](mobile/app/(auth)/register.tsx) | ✅ Present | Both have signup flow |
| **Forgot Password** | [web/(auth)/forgot-password](web/src/app/(auth)/forgot-password/page.tsx) | [mobile/app/(auth)/forgot-password.tsx](mobile/app/(auth)/forgot-password.tsx) | ✅ **COMPLETE** | Implemented with email input & reset link |
| **Email Verification** | [web/(auth)/verify-email](web/src/app/(auth)/verify-email/page.tsx) | [mobile/app/(auth)/verify-email.tsx](mobile/app/(auth)/verify-email.tsx) | ✅ **COMPLETE** | Implemented with 6-digit code input |
| **2FA/MFA** | unknown | ❌ Not found | ❌ **MISSING** | Consider for security |

### Main Dashboard & Overview

| Feature | Web | Mobile | Status | Notes |
|---------|-----|--------|--------|-------|
| **Home/Dashboard** | [web/(main)/dashboard](web/src/app/(main)/dashboard) | [mobile/(tabs)/dashboard](mobile/app/(tabs)/dashboard) | ✅ Parity | Overview stats, quick actions |
| **About/Help** | [web/(main)/about](web/src/app/(main)/about) | [mobile/app/(tabs)/about.tsx](mobile/app/(tabs)/about.tsx) | ✅ **COMPLETE** | App info, features, contact, legal |
| **Landing/Property** | [web/property/page.tsx](web/src/app/property/page.tsx) | ❌ Not found | ⚠️ **PARTIAL** | Mobile doesn't have dedicated landing |

### Guest Features (Browse & Book)

| Feature | Web | Mobile | Status | Notes |
|---------|-----|--------|--------|-------|
| **Explore/Search** | [web/(main)/explore](web/src/app/(main)/explore) | [mobile/(tabs)/explore](mobile/app/(tabs)/explore) | ✅ Parity | Property search grid |
| **Property List** | [web/(main)/properties](web/src/app/(main)/properties) | ❌ Not found | ⚠️ **PARTIAL** | Mobile uses explore instead |
| **Property Details** | [web/(main)/properties/[id]](web/src/app/(main)/properties/[id]) | [mobile/(tabs)/explore/[id].tsx](mobile/app/(tabs)/explore/[id].tsx) | ✅ Parity | Full property details page |
| **Create Booking** | [web/(main)/booking](web/src/app/(main)/booking) | [mobile/booking/confirm.tsx](mobile/app/booking/confirm.tsx) | ✅ Parity | Booking confirmation flow |
| **View My Bookings** | [web/(main)/bookings](web/src/app/(main)/bookings) | [mobile/(tabs)/bookings](mobile/app/(tabs)/bookings) | ✅ Parity | List user bookings |
| **Booking Details** | inferred | [mobile/(tabs)/bookings/[id].tsx](mobile/app/(tabs)/bookings/[id].tsx) | ✅ Parity | Single booking view |
| **Experiences** | [web/(main)/experiences](web/src/app/(main)/experiences) | [mobile/experiences](mobile/app/experiences) | ✅ Parity | Activities/tours |

### Messaging & Communication

| Feature | Web | Mobile | Status | Notes |
|---------|-----|--------|--------|-------|
| **Inbox/Conversations** | [web/(main)/messages](web/src/app/(main)/messages) | [mobile/(tabs)/messages](mobile/app/(tabs)/messages) | ✅ Parity | Conversation list |
| **Chat Thread** | inferred | [mobile/(tabs)/messages/[id].tsx](mobile/app/(tabs)/messages/[id].tsx) | ✅ Parity | Single conversation |
| **New Message** | inferred | [mobile/(tabs)/messages/new.tsx](mobile/app/(tabs)/messages/new.tsx) | ✅ Parity | Start new chat |

### Host/Property Management

| Feature | Web | Mobile | Status | Notes |
|---------|-----|--------|--------|-------|
| **Host Landing** | [web/(main)/host/page.tsx](web/src/app/(main)/host/page.tsx) | N/A | ⚠️ Different | Web has marketing page, mobile goes straight to dashboard |
| **Host Dashboard** | [web/(main)/host/dashboard](web/src/app/(main)/host/dashboard) | [mobile/(tabs)/host/index.tsx](mobile/app/(tabs)/host/index.tsx) | ✅ **Excellent Parity** | Both 550+ lines, 2 tabs (Overview/Analytics), stats, charts |
| **Host/Properties List** | [web/(main)/host/properties](web/src/app/(main)/host/properties) | [mobile/host/properties](mobile/app/host/properties) | ✅ Parity | Manage properties |
| **Add New Property** | [web/(main)/host/properties/new](web/src/app/(main)/host/properties/new) | [mobile/host/properties/new.tsx](mobile/app/host/properties/new.tsx) | ✅ **COMPLETE** | Both now use 4-step wizard (basic/location/images/pricing) |
| **Edit Property** | [web/(main)/host/properties/[id]/edit](web/src/app/(main)/host/properties/[id]/edit) | [mobile/host/properties/[id]/edit.tsx](mobile/app/host/properties/[id]/edit.tsx) | ✅ **COMPLETE** | Mobile upgraded to multi-step matching web |
| **Property Details** | [web/(main)/host/properties/[id]](web/src/app/(main)/host/properties/[id]) | [mobile/host/properties/[id].tsx](mobile/app/host/properties/[id].tsx) | ✅ Parity | View property |
| **Property Calendar** | [web/(main)/host/properties/[id]/calendar](web/src/app/(main)/host/properties/[id]/calendar) | [mobile/host/properties/[id]/calendar.tsx](mobile/app/host/properties/[id]/calendar.tsx) | ✅ Parity | Booking calendar |
| **Property Pricing** | [web/(main)/host/properties/[id]/pricing](web/src/app/(main)/host/properties/[id]/pricing) | [mobile/host/properties/[id]/pricing.tsx](mobile/app/host/properties/[id]/pricing.tsx) | ✅ Parity | Per-property pricing |
| **Bookings List** | [web/(main)/host/bookings](web/src/app/(main)/host/bookings) | [mobile/host/bookings](mobile/app/host/bookings) | ✅ Parity | Host's bookings |
| **Booking Details** | inferred | [mobile/host/bookings/[id].tsx](mobile/app/host/bookings/[id].tsx) | ✅ Parity | Single booking view |
| **Earnings** | [web/(main)/host/earnings](web/src/app/(main)/host/earnings) | [mobile/host/earnings](mobile/app/host/earnings) | ✅ Parity | Revenue dashboard |
| **Dynamic Pricing** | [web/(main)/host/pricing](web/src/app/(main)/host/pricing) | [mobile/host/pricing](mobile/app/host/pricing) | ✅ Parity | Global pricing rules |
| **Host Reviews** | [web/(main)/host/reviews](web/src/app/(main)/host/reviews) | [mobile/host/reviews](mobile/app/host/reviews) | ✅ **COMPLETE** | Web page added with rating stats, reviews list, empty state |
| **Verification** | [web/(main)/host/verification](web/src/app/(main)/host/verification) | [mobile/host/verification](mobile/app/host/verification) | ✅ Parity | ID verification |
| **Tax Reports** | [web/(main)/host/tax-reports](web/src/app/(main)/host/tax-reports) | [mobile/host/tax-reports](mobile/app/host/tax-reports) | ✅ Parity | Tax documents |
| **Settings** | [web/(main)/host/settings](web/src/app/(main)/host/settings) | [mobile/host/settings](mobile/app/host/settings) | ✅ Parity | Host config |

### User Profile & Account

| Feature | Web | Mobile | Status | Notes |
|---------|-----|--------|--------|-------|
| **My Profile** | [web/(main)/profile](web/src/app/(main)/profile) | [mobile/(tabs)/profile](mobile/app/(tabs)/profile) | ✅ Parity | User info display |
| **Edit Profile** | inferred | [mobile/(tabs)/profile/edit.tsx](mobile/app/(tabs)/profile/edit.tsx) | ✅ Parity | Update user info |
| **Change Password** | inferred | [mobile/(tabs)/profile/change-password.tsx](mobile/app/(tabs)/profile/change-password.tsx) | ✅ Parity | Password change |
| **Verification (User)** | inferred | [mobile/profile/verification.tsx](mobile/app/profile/verification.tsx) | ✅ Parity | ID verification |

### Reviews & Ratings

| Feature | Web | Mobile | Status | Notes |
|---------|-----|--------|--------|-------|
| **Reviews (General)** | [web/(main)/reviews](web/src/app/(main)/reviews) | [mobile/reviews](mobile/app/reviews) | ✅ Parity | View all reviews |
| **My Reviews** | inferred | [mobile/reviews/my-reviews.tsx](mobile/app/reviews/my-reviews.tsx) | ✅ Parity | User's reviews |
| **Leave Review** | inferred | integrated in booking | ✅ Parity | Post-booking review |

### Payments & Wallet

| Feature | Web | Mobile | Status | Notes |
|---------|-----|--------|--------|-------|
| **Payments Overview** | [web/(main)/payments](web/src/app/(main)/payments) | [mobile/app/(tabs)/payments.tsx](mobile/app/(tabs)/payments.tsx) | ✅ **COMPLETE** | Payment history, methods, transactions |
| **Wallet** | [web/(main)/wallet](web/src/app/(main)/wallet) | [mobile/(tabs)/wallet](mobile/app/(tabs)/wallet) | ✅ Parity | Balance & history |
| **Add Funds** | inferred | [mobile/wallet/add-funds.tsx](mobile/app/wallet/add-funds.tsx) | ✅ Parity | Deposit money |
| **Withdraw** | inferred | [mobile/wallet/withdraw.tsx](mobile/app/wallet/withdraw.tsx) | ✅ Parity | Cash out |
| **Payment Methods** | inferred | [mobile/wallet/payment-methods.tsx](mobile/app/wallet/payment-methods.tsx) | ✅ Parity | Manage cards/accounts |

### Wishlist & Saved

| Feature | Web | Mobile | Status | Notes |
|---------|-----|--------|--------|-------|
| **Wishlist** | [web/(main)/wishlist](web/src/app/(main)/wishlist) | [mobile/(tabs)/wishlist](mobile/app/(tabs)/wishlist) | ✅ Parity | Saved properties |

### Payment Success/Failure Flows

| Feature | Web | Mobile | Status | Notes |
|---------|-----|--------|--------|-------|
| **Success Page** | inferred | [mobile/booking/success.tsx](mobile/app/booking/success.tsx) | ✅ Parity | Payment confirmation |
| **Failure Page** | inferred | [mobile/booking/failure.tsx](mobile/app/booking/failure.tsx) | ✅ Parity | Payment error |

---

## 2. Form Parity Analysis

### Property Creation/Edit Form

**Web Form** ([web/src/components/host/property-form-multistep.tsx](web/src/components/host/property-form-multistep.tsx)):
- **Step 1: Basic Info**
  - title, description, property_type
- **Step 2: Location**
  - address, city, suburb, country
  - Map pin (Mapbox integration)
  - Lat/Long (GeoJSON Point)
  - Location search with suggestions
  - Geocoding support
- **Step 3: Images**
  - Multiple image upload (max 10)
  - Preview & delete functionality
  - Image reordering
- **Step 4: Pricing**
  - price_per_night, currency
  - Validation per step
- **Features**: Step validation, error handling, submit to API

**Mobile Form** ([mobile/app/host/properties/new.tsx](mobile/app/host/properties/new.tsx)):
- Single page form with:
  - title, description, address, city, country
  - price, bedrooms, bathrooms, maxGuests
- **Missing fields**: property_type, suburb, currency, latitude/longitude, map pin
- **Missing features**: image upload, multi-step flow, per-step validation, location search, geocoding
- **Status**: Placeholder implementation (TODO comments)

### Booking Form

**Web** ([web/(main)/booking](web/src/app/(main)/booking)):
- Date selection, guest count, special requests
- Payment method selection
- Cost breakdown

**Mobile** ([mobile/app/(tabs)/bookings/create.tsx](mobile/app/(tabs)/bookings/create.tsx)):
- Similar structure
- Status: ✅ Functional

### Review Form

**Web** ([web/(main)/reviews](web/src/app/(main)/reviews)):
- Rating selection, comment text
- Optional photo upload

**Mobile** ([mobile/reviews](mobile/app/reviews)):
- Status: ✅ Integrated in booking flow

---

## 3. Keyboard & SafeArea Issues

### SafeAreaView Warning
- **Issue**: Some screen imports `SafeAreaView` from `react-native` (deprecated) instead of `react-native-safe-area-context`
- **Affected screens**: Need to audit all imports
- **Status**: Most files use correct import, but one or more still use old import

### Keyboard Handling Gaps
**Screens with TextInput that need fixes:**

| Screen | Current State | Needs |
|--------|---------------|-------|
| [mobile/app/host/properties/new.tsx](mobile/app/host/properties/new.tsx) | ScrollView + keyboardShouldPersistTaps | KeyboardAvoidingView, Keyboard.dismiss on tap |
| [mobile/app/host/properties/[id]/edit.tsx](mobile/app/host/properties/[id]/edit.tsx) | ScrollView + keyboardShouldPersistTaps | KeyboardAvoidingView, Keyboard.dismiss on tap |
| [mobile/app/(tabs)/bookings/create.tsx](mobile/app/(tabs)/bookings/create.tsx) | ScrollView + keyboardShouldPersistTaps | KeyboardAvoidingView, Keyboard.dismiss on tap |
| [mobile/app/wallet/add-funds.tsx](mobile/app/wallet/add-funds.tsx) | ScrollView + keyboardShouldPersistTaps | KeyboardAvoidingView, Keyboard.dismiss on tap |
| [mobile/app/wallet/withdraw.tsx](mobile/app/wallet/withdraw.tsx) | ScrollView + keyboardShouldPersistTaps | KeyboardAvoidingView, Keyboard.dismiss on tap |
| [mobile/app/(tabs)/profile/edit.tsx](mobile/app/(tabs)/profile/edit.tsx) | ❌ No handling | KeyboardAvoidingView, Keyboard.dismiss, keyboardShouldPersistTaps |
| [mobile/app/(tabs)/profile/change-password.tsx](mobile/app/(tabs)/profile/change-password.tsx) | ❌ No handling | KeyboardAvoidingView, Keyboard.dismiss, keyboardShouldPersistTaps |

---

## 4. Missing Screens (Priority)

### ✅ Recently Completed (Feb 5, 2026)

1. **Forgot Password** - ✅ IMPLEMENTED
   - Web: [web/(auth)/forgot-password](web/src/app/(auth)/forgot-password/page.tsx)
   - Mobile: [mobile/app/(auth)/forgot-password.tsx](mobile/app/(auth)/forgot-password.tsx)
   - Features: Email input, validation, reset link, success state

2. **Email Verification** - ✅ IMPLEMENTED
   - Web: [web/(auth)/verify-email](web/src/app/(auth)/verify-email/page.tsx)
   - Mobile: [mobile/app/(auth)/verify-email.tsx](mobile/app/(auth)/verify-email.tsx)
   - Features: 6-digit code input, auto-focus, resend, paste support

3. **Payments Overview** - ✅ IMPLEMENTED (Mobile)
   - Mobile: [mobile/app/(tabs)/payments.tsx](mobile/app/(tabs)/payments.tsx)
   - Features: Payment history, methods management, tabs, delete

4. **About Page** - ✅ IMPLEMENTED (Mobile)
   - Mobile: [mobile/app/(tabs)/about.tsx](mobile/app/(tabs)/about.tsx)
   - Features: App info, features, contact, legal links, markets

### 🔴 High Priority (Affects Core Flows)

1. **Property Landing Page** - Not critical but recommended
   - Web: Exists at /property
   - Mobile: ❌ Missing
   - Impact: Medium - users can still access via explore

2. **Property Form Alignment** - Mobile form is incomplete
   - Web: 4-step form with map, images, geocoding
   - Mobile: Single-page placeholder
   - Impact: High - property creation is broken

3. **Payments Overview** - No central payments page in mobile
   - Web: [web/(main)/payments](web/src/app/(main)/payments)
   - Mobile: ❌ Missing
   - Impact: Medium - users can't see payment history centrally

### 🟠 Medium Priority (Nice-to-Have)

4. **About Page** - Missing info screen
   - Web: [web/(main)/about](web/src/app/(main)/about)
   - Mobile: ❌ Missing
   - Impact: Low - informational only

5. **Email Verification** - After signup
   - Web: Likely exists
   - Mobile: ❌ Missing
   - Impact: Medium - depends on backend requirements

---

## 5. Implementation Roadmap

### Phase 1: Critical Fixes (This Session)
- [ ] Fix SafeAreaView warnings (audit all imports)
- [ ] Standardize keyboard handling across 7 form screens
- [ ] Restructure mobile property form to match web (4 steps)
- [ ] Add missing property form fields (property_type, suburb, currency, lat/long, images, map)

### Phase 2: Missing Screens (Next)
- [ ] Add Forgot Password screen
- [ ] Add Payments overview screen
- [ ] Add About page
- [ ] Add Email Verification flow

### Phase 3: Polish & Testing
- [ ] Test all forms end-to-end
- [ ] Test keyboard behavior on iOS/Android
- [ ] Cross-browser testing (web)
- [ ] API payload validation

---

## 6. API Contract Alignment

### Property Creation Payload

**Current Web API expectation** (from backend):
```json
{
  "title": "string",
  "description": "string",
  "property_type": "house|apartment|villa|lodge|cottage",
  "address": "string",
  "city": "string",
  "suburb": "string (optional)",
  "country": "string",
  "location": {
    "type": "Point",
    "coordinates": [longitude, latitude]
  },
  "price_per_night": number,
  "currency": "USD|ZWL|etc",
  "bedrooms": number,
  "bathrooms": number,
  "max_guests": number,
  "status": "pending_approval"
}
```

**Current Mobile payload** (from [mobile/app/host/properties/new.tsx](mobile/app/host/properties/new.tsx)):
```json
{
  "title": "string",
  "description": "string",
  "address": "string",
  "city": "string",
  "country": "string",
  "price": "string",
  "bedrooms": "string",
  "bathrooms": "string",
  "maxGuests": "string"
}
```

**Gap**: Mobile is missing 6 fields and uses different field names (price vs price_per_night, maxGuests vs max_guests).

**Action**: Update mobile form to match backend API contract exactly.

---

## Summary

| Category | Status | Count | Action |
|----------|--------|-------|--------|
| Screens present in both | ✅ | ~20 | No action |
| Missing in mobile | ❌ | 4 | Add these screens |
| Form mismatches | ⚠️ | 2 | Restructure property form |
| Keyboard issues | ⚠️ | 7 | Standardize handling |
| SafeArea issues | ⚠️ | 1+ | Audit imports |
| API contract gaps | ⚠️ | 1 | Update payload |

**Total work items**: ~15 actionable tasks
**Estimated effort**: 6-8 hours for complete alignment

---

*Generated by: Mobile/Web Parity Audit*
