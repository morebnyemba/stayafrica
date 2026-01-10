# Mobile App Testing Guide

## Prerequisites
- Expo Go app installed on your mobile device
- Mobile device and development machine on the same network
- Node.js 18+ installed

## Setup Instructions

### 1. Install Dependencies
```bash
cd mobile
npm install
```

### 2. Start Development Server
```bash
npm start
# or
npx expo start
```

### 3. Connect Your Device
1. Open Expo Go app on your mobile device
2. Scan the QR code displayed in terminal
3. Wait for the app to load

## Testing Checklist

### Bottom Tab Bar ✓
- [ ] Verify all 6 tabs are visible: Explore, Wishlist, Bookings, Host, Wallet, Profile
- [ ] Check icons change from outline to filled when selected
- [ ] Verify active tab shows Safari Gold color (#D9B168)
- [ ] Confirm inactive tabs show gray color
- [ ] Test tab bar background is Deep Forest (#122F26)
- [ ] Verify tab bar has shadow effect
- [ ] Check icon size increases when tab is selected

### Explore Screen ✓
**Header**
- [ ] Verify gradient header (Deep Forest to Moss Green)
- [ ] Check "Discover Africa" title is prominent
- [ ] Confirm property count displays correctly
- [ ] Verify location icon is visible

**Search Bar**
- [ ] Test glassmorphic search bar appears correctly
- [ ] Type in search box and verify text is visible
- [ ] Check clear button (X) appears when typing
- [ ] Tap clear button and verify it clears search
- [ ] Verify search filters properties by title/city

**Category Filters**
- [ ] Scroll horizontally through categories
- [ ] Tap each category and verify selection state changes
- [ ] Check selected category has Dark background
- [ ] Verify selected category shows gold text
- [ ] Confirm fade-in animation plays on load
- [ ] Test icon displays correctly for each category

**Property Cards**
- [ ] Verify cards display with rounded corners
- [ ] Check property image loads correctly
- [ ] Confirm gradient overlay at bottom of image
- [ ] Verify rating badge appears in top-left
- [ ] Test press animation (card should scale down slightly)
- [ ] Release and verify card springs back to normal
- [ ] Check property title, location, price display
- [ ] Verify amenity icons show correctly (bed, bath, guests)
- [ ] Tap "View Details" and verify navigation

**Empty State**
- [ ] Search for non-existent property
- [ ] Verify beautiful empty state card appears
- [ ] Check search icon is displayed
- [ ] Confirm helpful text is shown

### Wishlist Screen ✓
**Header**
- [ ] Verify gradient header matches brand
- [ ] Check wishlist count displays (should be 0 initially)
- [ ] Verify heart icon next to count

**Unauthenticated State**
- [ ] Log out if logged in
- [ ] Navigate to Wishlist tab
- [ ] Verify beautiful empty state card
- [ ] Check large heart icon is displayed
- [ ] Tap "Sign In Now" button
- [ ] Verify navigation to login screen

**Authenticated State (if implemented)**
- [ ] Log in to account
- [ ] Navigate to Wishlist tab
- [ ] If empty, verify empty state
- [ ] Tap "Explore Properties" button
- [ ] Verify navigation to Explore

### Bookings Screen ✓
**Header**
- [ ] Verify gradient header
- [ ] Check bookings count displays
- [ ] Verify calendar icon next to count

**Unauthenticated State**
- [ ] Navigate to Bookings while logged out
- [ ] Verify beautiful empty state
- [ ] Check calendar icon is large and visible
- [ ] Tap "Sign In Now" button

**Booking Cards (if bookings exist)**
- [ ] Verify gradient header on each card
- [ ] Check property name and location display
- [ ] Verify status badge shows correct color
- [ ] Confirm check-in/check-out dates display
- [ ] Check arrow between dates is visible
- [ ] Verify total price is prominent
- [ ] Tap card and verify navigation

**Empty State**
- [ ] If logged in with no bookings
- [ ] Verify calendar icon empty state
- [ ] Tap "Start Exploring" button
- [ ] Confirm navigation to Explore

### Profile Screen ✓
**Header**
- [ ] Verify gradient background
- [ ] Check large avatar with gradient
- [ ] Confirm user icon inside avatar
- [ ] Verify user name displays
- [ ] Check email displays below name
- [ ] Verify role badge shows (Guest/Host)

**Unauthenticated State**
- [ ] Navigate to Profile while logged out
- [ ] Verify empty state card
- [ ] Check person icon is visible
- [ ] Tap "Sign In" button

**Information Cards (if logged in)**
- [ ] Verify "Account Information" section
- [ ] Check Full Name card
- [ ] Verify Phone Number card
- [ ] Check Country card
- [ ] Confirm icons display for each

**Actions (if logged in)**
- [ ] Verify "Settings" section
- [ ] Check "Edit Profile" card (gradient background)
- [ ] Verify "Change Password" card (gradient background)
- [ ] Tap logout button
- [ ] Confirm logout dialog appears

## Animation Testing

### PropertyCard Animations
1. Tap and hold a property card
2. Verify card scales down to ~97% smoothly
3. Release and verify card springs back to 100%
4. Test multiple rapid taps for smoothness

### Category Filter Animations
1. Navigate to Explore screen
2. Observe category chips loading
3. Verify staggered fade-in from left to right
4. Delay should be ~50ms between each chip

## Performance Testing

### Scroll Performance
- [ ] Scroll through property list quickly
- [ ] Verify no lag or stuttering
- [ ] Check images load progressively
- [ ] Confirm animations remain smooth

### Touch Response
- [ ] Test all buttons respond immediately
- [ ] Verify no delayed reactions
- [ ] Check animation feedback is instant

## Visual Regression Testing

### Colors
- [ ] Deep Forest: #122F26 (headers, tab bar)
- [ ] Safari Gold: #D9B168 (active states, CTAs)
- [ ] Moss Green: #3A5C50 (icons, secondary text)
- [ ] Ivory Sand: #F4F1EA (backgrounds)

### Shadows
- [ ] Cards have soft shadows
- [ ] Tab bar has subtle top shadow
- [ ] Buttons have elevation shadows
- [ ] Badges have small shadows

### Typography
- [ ] Headers are bold and prominent
- [ ] Body text is readable
- [ ] Labels are appropriately sized
- [ ] Hierarchy is clear

## Edge Cases

### Network Issues
- [ ] Turn off WiFi
- [ ] Verify loading states show
- [ ] Turn WiFi back on
- [ ] Confirm data loads

### Empty Data
- [ ] Test with no properties
- [ ] Test with no bookings
- [ ] Test with no wishlist items
- [ ] Verify all empty states

### Long Text
- [ ] Property with very long title
- [ ] City name with long name
- [ ] Verify text truncates properly

## Bug Reporting Template

If you find any issues, report them with:
```
**Screen:** [Screen Name]
**Issue:** [Description]
**Expected:** [What should happen]
**Actual:** [What actually happens]
**Steps to Reproduce:**
1. Step 1
2. Step 2
3. Step 3

**Device:** [e.g., iPhone 14, Pixel 6]
**OS Version:** [e.g., iOS 17, Android 14]
**Screenshot:** [Attach if possible]
```

## Success Criteria

The mobile app modernization is successful if:
- ✅ All screens render without errors
- ✅ Animations are smooth (60fps)
- ✅ Touch interactions feel responsive
- ✅ Design is consistent across screens
- ✅ Empty states are helpful and beautiful
- ✅ Brand colors are applied correctly
- ✅ Typography hierarchy is clear
- ✅ Shadows and elevations are appropriate
- ✅ Navigation works correctly
- ✅ No TypeScript errors
- ✅ No security vulnerabilities

## Additional Notes

- If the app crashes, check the terminal for error messages
- Clear cache if needed: `npx expo start -c`
- Reload app by shaking device and selecting "Reload"
- Report any performance issues
- Suggest improvements based on user experience

## Next Steps After Testing

1. Gather user feedback
2. Document any issues found
3. Prioritize fixes if needed
4. Consider A/B testing new design
5. Plan for production deployment
