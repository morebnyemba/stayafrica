# StayAfrica Web UI/UX Improvement Prompts
**Generated:** December 13, 2025  
**Context:** Next.js 14 web application for property rentals across Africa  
**Design System:** Luxury Safari aesthetic with Deep Forest, Safari Gold, and Ivory Sand colors

---

## 📋 Quick Reference

| Priority | Category | Prompts Count | Estimated Impact |
|----------|----------|---------------|------------------|
| 🔴 **Critical** | Navigation & Accessibility | 3 | High |
| 🟠 **High** | Forms & Input UX | 5 | High |
| 🟡 **Medium** | Property Cards & Visual Hierarchy | 6 | Medium |
| 🟢 **Low** | Micro-interactions & Polish | 4 | Medium |

**Total Prompts:** 18 actionable items

---

## 🔴 CRITICAL PRIORITY - Fix These First

### 1. Mobile Navigation Menu Improvements

**Current Issue:** Mobile menu is functional but lacks smooth animations and proper touch targets.

**Copy-Paste Prompt:**
```
Improve the mobile navigation menu in /web/src/components/common/navigation.tsx:
1. Add smooth slide-in/slide-out animations for the mobile menu using Tailwind's transition classes
2. Increase touch target sizes to minimum 44x44px for all menu items
3. Add a backdrop overlay (semi-transparent dark background) when menu is open
4. Add backdrop blur effect for modern iOS-style menu
5. Implement swipe-to-close functionality using touch events
6. Add proper focus trapping so keyboard navigation stays within the menu
7. Ensure menu closes when clicking outside or pressing Escape key
8. Add proper ARIA labels: aria-expanded, aria-haspopup, aria-label="Main menu"

Reference the existing theme colors from tailwind.config.ts (primary, secondary, sand).
Maintain the luxury safari aesthetic with subtle animations (300ms ease-in-out).
```

---

### 2. Search Bar Accessibility & Mobile Optimization

**Current Issue:** Hero section search bar doesn't adapt well to mobile and lacks accessibility features.

**Copy-Paste Prompt:**
```
Enhance the search bar component in /web/src/components/common/hero-section.tsx for mobile and accessibility:
1. Make the search form stack vertically on mobile (single column) instead of attempting 4 columns
2. Add proper labels for screen readers: <label> elements with htmlFor attributes linked to inputs
3. Add aria-describedby for date inputs explaining the format
4. Implement autocomplete suggestions for location input using Mapbox Places API or similar
5. Add a clear button (X icon) for each input when it has content
6. Show validation errors inline (e.g., "Check-out must be after check-in")
7. Add loading state indicator when search is processing
8. Improve date input UX by adding a date picker overlay instead of native date input
9. Make the search button sticky at bottom on mobile for easy thumb access

Ensure all inputs have visible focus indicators with the Safari Gold color (#d9b168).
Use Lucide React icons for consistency.
```

---

### 3. Loading States & Error Handling Throughout App

**Current Issue:** Many components show no feedback during API calls or when errors occur.

**Copy-Paste Prompt:**
```
Implement comprehensive loading states and error handling across the application:

**Files to update:**
- /web/src/components/property/explore-content.tsx
- /web/src/components/property/home-properties.tsx
- /web/src/components/host/host-dashboard.tsx
- /web/src/components/booking/booking-content.tsx

**Requirements:**
1. Add skeleton screens using the existing Skeleton component for initial loads
2. Create an ErrorBoundary component to catch React errors gracefully
3. Add inline error messages with retry buttons for failed API calls
4. Implement optimistic updates for user actions (like wishlist toggles)
5. Show toast notifications for success/error feedback using react-hot-toast
6. Add empty states with helpful CTAs when no data exists (e.g., "No properties found, try adjusting filters")
7. Use the AlertCircle icon from lucide-react for error states
8. Add timeout handling for slow network connections (show "Still loading..." after 5 seconds)

Style error messages with subtle red borders and backgrounds that match the luxury theme.
Use the secondary gold color for success states.
```

---

## 🟠 HIGH PRIORITY - Improve Core User Flows

### 4. Property Card Hover Effects & Visual Feedback

**Current Issue:** Property cards have basic hover states but lack the polish of modern booking platforms.

**Copy-Paste Prompt:**
```
Enhance property cards in /web/src/components/property/featured-properties.tsx:
1. Add subtle shadow elevation on hover (shadow-elevated defined in tailwind.config.ts)
2. Implement image zoom effect on hover (scale-105 with overflow-hidden)
3. Add a favorite/wishlist heart icon button in top-right corner of image
4. Show "Quick View" button overlay on image hover
5. Add smooth transitions for all hover effects (duration-300)
6. Implement skeleton pulse animation during image loading
7. Add aria-label for all interactive elements
8. Show property amenities as small icons below the location
9. Add a "New" or "Featured" badge for special properties
10. Implement lazy loading for property images using Next.js Image component

Use the existing card class from globals.css and extend with group-hover utilities.
Ensure accessibility: all interactive elements should be keyboard navigable.
```

---

### 5. Form Input Improvements Across Registration & Login

**Current Issue:** Forms use basic input styling and lack modern input enhancements.

**Copy-Paste Prompt:**
```
Modernize form inputs in /web/src/components/common/login-content.tsx and register-content.tsx:

1. **Floating Labels:** Implement floating label animation (label moves up when input is focused/filled)
2. **Show/Hide Password:** Add eye icon toggle for password fields
3. **Password Strength Meter:** Visual indicator (weak/medium/strong) for password input
4. **Real-time Validation:** Show validation feedback as user types (green check / red X icon)
5. **Input Icons:** Add relevant icons inside inputs (Mail icon for email, Lock for password)
6. **Autocomplete Attributes:** Add proper autocomplete attributes (autocomplete="email", "new-password")
7. **Focus States:** Enhance focus rings with Safari Gold glow effect
8. **Error Messages:** Display below inputs with smooth slide-down animation
9. **Success States:** Green border and checkmark when validation passes
10. **Disable Submit:** Keep submit button disabled until all validations pass, with tooltip explaining why

Use the input-base class from globals.css as foundation.
Add CSS transitions for smooth state changes.
Implement using React hooks (useState for each input's state).
```

---

### 6. Dashboard Data Visualization Enhancements

**Current Issue:** Host and buyer dashboards show basic data without compelling visualizations.

**Copy-Paste Prompt:**
```
Enhance dashboard visualizations in /web/src/components/host/host-dashboard.tsx:

1. **Stats Cards Redesign:**
   - Add gradient backgrounds for each stat card using primary/secondary colors
   - Include trend indicators (↑ 12% from last month) with green/red colors
   - Add animated number counter effect when stats load
   - Use appropriate icons for each metric (DollarSign, Calendar, Star)

2. **Charts Integration:**
   - Implement Recharts library (already in package.json) for earnings over time
   - Create a line chart showing revenue trends (last 6 months)
   - Add a bar chart for bookings by property
   - Use brand colors: Safari Gold (#d9b168) for chart lines/bars
   - Add interactive tooltips on hover

3. **Recent Activity Feed:**
   - Create a timeline component showing recent bookings/reviews
   - Add relative timestamps ("2 hours ago", "Yesterday")
   - Include property thumbnails and guest avatars
   - Make items clickable to navigate to details

4. **Quick Actions Panel:**
   - Add prominent CTAs: "Add New Property", "Respond to Messages"
   - Use btn-primary and btn-secondary classes
   - Position in prominent location (top-right or dedicated section)

Ensure all visualizations are responsive and adapt to mobile screens.
```

---

### 7. Search Filters UI/UX Polish

**Current Issue:** Filter panel is functional but not intuitive or visually appealing.

**Copy-Paste Prompt:**
```
Redesign the search filters in /web/src/components/common/search-filters.tsx:

1. **Modal/Drawer Design:**
   - Convert filter section into a modal on mobile, sidebar on desktop
   - Add smooth slide-in animation from right side
   - Include "Apply Filters" and "Clear All" buttons at bottom
   - Show active filter count badge on filter button

2. **Range Sliders:**
   - Replace number inputs with visual range sliders for price
   - Use dual-thumb slider for min/max price selection
   - Display selected range as formatted currency ($50 - $200)
   - Style slider track with Safari Gold color

3. **Amenities Selection:**
   - Create visual checkbox cards (not plain checkboxes)
   - Add icons for each amenity using Lucide React
   - Show selected count and allow "Select All" / "Clear All"
   - Use grid layout for better scanability

4. **Property Type Pills:**
   - Replace dropdown with clickable pill buttons
   - Single or multi-select toggle based on context
   - Highlight selected with Safari Gold background

5. **Guest Counter:**
   - Replace dropdown with +/- stepper buttons
   - Show icons for adults/children/infants separately
   - Add smooth increment/decrement animations

6. **Sort Options:**
   - Add sort dropdown: Price (Low to High), Rating, Newest
   - Position prominently at top of results

Add micro-interactions: subtle scale on button click, color transitions.
Ensure filter state persists in URL query parameters for shareability.
```

---

### 8. Booking Flow Improvements

**Current Issue:** Booking process lacks clear progress indication and confirmations.

**Copy-Paste Prompt:**
```
Improve the booking flow across these files:
- /web/src/components/booking/booking-content.tsx
- /web/src/app/(main)/booking/confirm/page.tsx
- /web/src/app/(main)/booking/payment/page.tsx

1. **Progress Indicator:**
   - Add step indicator: 1. Dates → 2. Details → 3. Payment → 4. Confirmation
   - Highlight current step, show completed steps with checkmarks
   - Make steps clickable to navigate back (if data is saved)

2. **Date Selection Enhancement:**
   - Implement calendar picker showing available/unavailable dates
   - Highlight date range selection with Safari Gold
   - Show nightly rate and total price calculation dynamically
   - Display "Minimum stay: X nights" if applicable

3. **Guest Details Form:**
   - Pre-fill from user profile if authenticated
   - Add special requests textarea (dietary, accessibility needs)
   - Show check-in/check-out times clearly

4. **Price Breakdown:**
   - Create sticky sidebar showing: nightly rate × nights, cleaning fee, service fee, taxes, total
   - Use the existing fee calculation hook: use-fees.ts
   - Add tooltip explaining each fee
   - Highlight total in Safari Gold

5. **Payment UI:**
   - Integrate Stripe Elements with Safari Gold accent color
   - Add accepted payment methods icons
   - Show security badges ("Secure payment", "256-bit encryption")
   - Add "Book now" button with loading state

6. **Confirmation Screen:**
   - Show success animation (checkmark animation)
   - Display booking reference number prominently
   - Add "Add to Calendar" button (download .ics file)
   - Show host contact info and next steps
   - Provide PDF download option for booking confirmation

Ensure all steps are mobile-responsive with touch-friendly controls.
Add validation at each step before allowing progression.
```

---

### 9. Profile & Settings Page Redesign

**Current Issue:** Profile page has basic forms without engaging user experience.

**Copy-Paste Prompt:**
```
Redesign the profile page in /web/src/components/common/profile-content.tsx:

1. **Profile Header:**
   - Large profile photo with edit overlay on hover
   - Add background banner image upload option
   - Show verification badges (Email verified, Phone verified, ID verified)
   - Display member since date and user role (Guest/Host)

2. **Tabbed Interface:**
   - Create tabs: Personal Info | Security | Notifications | Payment Methods | Preferences
   - Use Tailwind's tab styling with underline indicator in Safari Gold
   - Add icons for each tab

3. **Personal Info Section:**
   - Two-column layout on desktop
   - Inline edit mode (click field to edit, not separate edit button)
   - Show save indicator when changes are made
   - Add profile completion percentage (e.g., "Your profile is 75% complete")

4. **Security Section:**
   - Change password form with current/new password fields
   - Two-factor authentication toggle
   - Active sessions list with "Log out other devices" option
   - Show last login time and location

5. **Notification Preferences:**
   - Toggle switches for: Email notifications, SMS alerts, Push notifications
   - Granular controls: Booking updates, Messages, Promotions
   - Use styled toggle switches (not checkboxes)

6. **Payment Methods:**
   - Saved payment methods list with card icons (Visa, Mastercard, etc.)
   - Add new payment method button
   - Set default payment method option

7. **Host-Specific Settings:**
   - Instant booking toggle
   - Pricing rules and discounts
   - Availability calendar settings

Add auto-save functionality with toast notification "Saved!" when changes persist.
Include unsaved changes warning if user tries to navigate away.
```

---

## 🟡 MEDIUM PRIORITY - Polish & Enhancement

### 10. Footer Responsive Layout & Links

**Current Issue:** Footer is functional but could be more engaging and better organized.

**Copy-Paste Prompt:**
```
Enhance the footer in /web/src/components/common/footer.tsx:

1. **Mobile Accordion:** Make footer sections collapsible on mobile (accordion style)
2. **Newsletter Signup:** Add email subscription form in footer with "Subscribe" button
3. **Language Selector:** Add dropdown for language selection (EN, FR, PT for African markets)
4. **Currency Selector:** Add currency switcher (USD, ZAR, EUR)
5. **Trust Badges:** Add security/trust badges (SSL secured, verified by...)
6. **Popular Destinations:** Add quick links to popular cities
7. **App Download Buttons:** Add Apple App Store and Google Play buttons
8. **Back to Top Button:** Floating button that appears on scroll

Style accordion with Safari Gold accent for open sections.
Ensure all links are keyboard navigable and have proper focus states.
Add smooth scroll behavior for "Back to Top" button.
```

---

### 11. Property Detail Page Full Experience

**Current Issue:** Property detail page is placeholder with minimal content.

**Copy-Paste Prompt:**
```
Build out the complete property detail page in /web/src/components/property/property-details-content.tsx:

1. **Image Gallery:**
   - Implement lightbox/modal for full-screen image viewing
   - Show 5 images in grid (1 large + 4 thumbnails) with "Show all photos" button
   - Add image navigation arrows and thumbnails
   - Include zoom on hover functionality

2. **Property Header:**
   - Property name as H1 with share and wishlist buttons
   - Location with map pin icon (clickable to show on map)
   - Rating with star icons and review count link
   - Host profile mini-card with avatar, name, join date

3. **Key Details Bar:**
   - Icons showing: guests capacity, bedrooms, beds, bathrooms
   - Property type badge (Entire place, Private room, etc.)

4. **Description Section:**
   - About this space with expandable "Read more" if text is long
   - Highlight key features as bullet points

5. **Amenities Grid:**
   - Show top 10 amenities with icons
   - "Show all X amenities" button to open modal with complete list
   - Group by category: Essentials, Kitchen, Safety, etc.

6. **Location Map:**
   - Embed Mapbox map showing approximate location (not exact for privacy)
   - Show neighborhood highlights
   - Distance to popular attractions

7. **Reviews Section:**
   - Overall rating breakdown by category (Cleanliness, Accuracy, etc.)
   - Review cards with guest photo, name, date, rating, comment
   - "Show more reviews" pagination

8. **Host Info Panel:**
   - Host name, join date, response rate, response time
   - Verified badge if applicable
   - "Contact Host" button

9. **Booking Widget (Sticky Sidebar):**
   - Price per night prominently displayed
   - Date picker for check-in/check-out
   - Guest selector dropdown
   - "Reserve" or "Request to Book" button
   - Price breakdown calculation
   - Sticky on scroll (desktop only)

10. **Similar Properties:**
    - "You might also like" section at bottom
    - Carousel of 3-4 similar properties

Use existing property-amenities.tsx, property-host-card.tsx, availability-calendar.tsx components.
Ensure entire page is accessible and SEO-optimized.
```

---

### 12. Wishlist Page Enhancements

**Current Issue:** Wishlist shows basic grid but lacks organization features.

**Copy-Paste Prompt:**
```
Enhance wishlist functionality in /web/src/components/wishlist/wishlist-content.tsx:

1. **List Organization:**
   - Allow users to create multiple wishlists (e.g., "Summer Trip", "Family Vacation")
   - Default "Saved" list for quick saves
   - List cover image (uses first property image)

2. **List Management:**
   - Rename list option
   - Delete list option with confirmation
   - Share list via link (make it public/private toggle)

3. **Property Actions:**
   - Quick remove from wishlist with undo option (toast)
   - Add notes to saved properties
   - View availability calendar without leaving page

4. **Filters & Sort:**
   - Filter by: Location, Price range, Dates
   - Sort by: Recently added, Price, Rating
   - Search within wishlist

5. **Empty State:**
   - Attractive illustration when wishlist is empty
   - CTA button: "Start exploring" that navigates to /explore

Add smooth animations for adding/removing items.
Use optimistic updates for instant feedback.
```

---

### 13. Dark Mode Refinements

**Current Issue:** Dark mode works but some components don't adapt well.

**Copy-Paste Prompt:**
```
Audit and improve dark mode across the application:

**Specific Issues to Fix:**
- /web/src/components/property/property-card: Ensure card shadows are visible in dark mode
- /web/src/components/common/hero-section.tsx: Check search bar readability
- /web/src/components/host/property-form.tsx: Form inputs need better dark mode contrast

**Requirements:**
1. Images should have subtle overlay in dark mode to prevent harsh whites
2. All text must meet WCAG AA contrast requirements (4.5:1 for normal text)
3. Safari Gold accent should be slightly lighter in dark mode (#edcb74)
4. Borders should be visible but subtle (use primary-700 in dark mode)
5. Cards should have darker shadows (black with 30% opacity)
6. Loading skeletons should use darker shimmer effect
7. Toast notifications should have dark variants
8. Modal backdrops should be darker (95% opacity)

Add dark: prefix to all color utilities that need adjustment.
Test with browser DevTools in dark mode.
Consider adding "System" option to theme toggle (auto-detect OS preference).
```

---

### 14. Micro-interactions & Animations

**Current Issue:** Application feels static and lacks delightful interactions.

**Copy-Paste Prompt:**
```
Add micro-interactions throughout the application for better UX:

1. **Button Feedback:**
   - Add subtle scale-down on click (transform: scale(0.98))
   - Ripple effect on primary buttons
   - Loading spinner inside button when processing

2. **Navigation Transitions:**
   - Fade in animations for new content using Framer Motion
   - Slide transitions for modals/drawers

3. **List Animations:**
   - Stagger animation when property cards load (each card animates in sequence)
   - Fade-in + slide-up for list items

4. **Form Interactions:**
   - Shake animation for validation errors
   - Success checkmark animation
   - Progress bar fill animation for multi-step forms

5. **Hover States:**
   - Smooth color transitions (150-300ms)
   - Icon scale on hover
   - Tooltip fade-in animations

6. **Image Loading:**
   - Blur-up effect (low-res placeholder → high-res)
   - Fade in when loaded

**Implementation:**
- Install framer-motion: `npm install framer-motion`
- Create animation variants in /web/src/lib/animations.ts
- Keep animations subtle (200-400ms duration)
- Respect prefers-reduced-motion for accessibility

Example animations:
```typescript
// In lib/animations.ts
export const fadeIn = {
  hidden: { opacity: 0 },
  visible: { opacity: 1, transition: { duration: 0.3 } }
};

export const slideUp = {
  hidden: { y: 20, opacity: 0 },
  visible: { y: 0, opacity: 1, transition: { duration: 0.4 } }
};
```

Add motion components to property cards, forms, and navigation elements.
```

---

## 🟢 LOW PRIORITY - Nice to Have

### 15. Progressive Image Loading

**Current Issue:** Large images load slowly without placeholder feedback.

**Copy-Paste Prompt:**
```
Implement progressive image loading:

1. Replace all <img> tags with Next.js <Image /> component
2. Add width, height, and alt attributes
3. Use placeholder="blur" with blurDataURL
4. Set loading="lazy" for below-fold images
5. Configure next.config.js for image optimization:

```javascript
// In next.config.js
module.exports = {
  images: {
    domains: ['images.unsplash.com', 'your-cdn-domain.com'],
    formats: ['image/webp', 'image/avif'],
  },
};
```

Files to update:
- /web/src/components/property/property-image-carousel.tsx
- /web/src/components/property/featured-properties.tsx
- All property card components

This will significantly improve perceived performance and Core Web Vitals scores.
```

---

### 16. Accessibility Audit & ARIA Implementation

**Current Issue:** Many interactive elements lack proper ARIA labels and keyboard navigation.

**Copy-Paste Prompt:**
```
Conduct full accessibility audit and implement improvements:

1. **Keyboard Navigation:**
   - Test all interactive elements with Tab key
   - Ensure logical tab order
   - Add skip links ("Skip to main content")

2. **ARIA Labels:**
   - Add aria-label to all icon-only buttons
   - Use aria-describedby for input hints
   - Implement aria-live regions for dynamic content
   - Add role attributes where semantic HTML isn't sufficient

3. **Focus Management:**
   - Visible focus indicators (2px Safari Gold outline)
   - Focus trap in modals
   - Return focus to trigger element when modal closes

4. **Screen Reader Support:**
   - Add sr-only class for screen reader-only text
   - Proper heading hierarchy (h1 → h2 → h3, no skipping)
   - Alt text for all images (descriptive, not just filename)

5. **Form Accessibility:**
   - Associate labels with inputs using htmlFor
   - Group related inputs with <fieldset> and <legend>
   - Mark required fields with required attribute and visual indicator
   - Announce validation errors to screen readers

**Testing Checklist:**
- [ ] Test with keyboard only (no mouse)
- [ ] Test with screen reader (NVDA, JAWS, VoiceOver)
- [ ] Run Lighthouse accessibility audit (aim for 100 score)
- [ ] Use axe DevTools to catch issues

**Files to prioritize:**
- /web/src/components/common/navigation.tsx
- /web/src/components/common/hero-section.tsx
- /web/src/components/property/explore-content.tsx
- All form components (login, register, booking)

Create /web/src/components/ui/visually-hidden.tsx:
```typescript
export const VisuallyHidden = ({ children }: { children: React.ReactNode }) => (
  <span className="sr-only">{children}</span>
);
```

Add to globals.css:
```css
.sr-only {
  position: absolute;
  width: 1px;
  height: 1px;
  padding: 0;
  margin: -1px;
  overflow: hidden;
  clip: rect(0, 0, 0, 0);
  white-space: nowrap;
  border-width: 0;
}
```
```

---

### 17. Performance Optimization

**Current Issue:** Application could be more performant with code splitting and optimization.

**Copy-Paste Prompt:**
```
Optimize application performance:

1. **Code Splitting:**
   - Use dynamic imports: `const Map = dynamic(() => import('./Map'), { ssr: false })`
   - Lazy load modal content
   - Defer non-critical JavaScript

2. **React Optimization:**
   - Wrap expensive components with React.memo()
   - Use useMemo for expensive calculations
   - Use useCallback for function props

3. **Data Fetching:**
   - Implement pagination for property lists
   - Use React Query caching strategies
   - Prefetch data on hover

4. **Asset Optimization:**
   - Use Next.js Image optimization
   - Implement resource hints: preload, prefetch

**Run performance audit:**
```bash
npm run build
npm run start
# Open Chrome DevTools → Lighthouse → Run audit
```

**Target metrics:**
- First Contentful Paint (FCP): < 1.8s
- Largest Contentful Paint (LCP): < 2.5s
- Time to Interactive (TTI): < 3.9s
- Cumulative Layout Shift (CLS): < 0.1

**Files to optimize:**
- Property list components (implement windowing)
- Image components (use Next.js Image)
- Dashboard components (lazy load charts)
```

---

### 18. Advanced Search & Filtering

**Current Issue:** Search is basic and doesn't support advanced use cases.

**Copy-Paste Prompt:**
```
Implement advanced search capabilities:

1. **Map-Based Search:**
   - Add interactive map view toggle
   - Show properties as pins on map
   - Click pin to see property card preview
   - "Search this area" button when dragging map

2. **Smart Filters:**
   - "Instant Book" filter
   - "Superhost" filter
   - "New Listings" filter (last 30 days)
   - Accessibility features filter

3. **Saved Searches:**
   - Allow users to save search criteria
   - Email alerts for new matches
   - Quick access in dashboard

4. **Location Autocomplete:**
   - Integrate Mapbox or Google Places API
   - Show suggestions as user types
   - Include neighborhoods, cities, landmarks

5. **Faceted Search:**
   - Show filter counts (e.g., "Pool (23)")
   - Update counts dynamically as filters change
   - Disable filters that result in zero results

**Files to create/update:**
- /web/src/components/search/advanced-search-modal.tsx (new)
- /web/src/components/search/map-view.tsx (new)
- /web/src/components/common/search-filters.tsx (enhance)

This will significantly improve property discovery.
```

---

## 📊 Implementation Priority Matrix

| Prompt # | Title | Impact | Effort | Priority Score |
|----------|-------|--------|--------|----------------|
| 1 | Mobile Navigation | High | Low | 🔴 9/10 |
| 2 | Search Bar A11y | High | Medium | 🔴 8/10 |
| 3 | Loading/Error States | High | Medium | 🔴 8/10 |
| 4 | Property Card Polish | High | Low | 🟠 7/10 |
| 5 | Form Improvements | High | Medium | 🟠 7/10 |
| 6 | Dashboard Viz | Medium | High | 🟡 6/10 |
| 7 | Filter UX | High | High | 🟠 7/10 |
| 8 | Booking Flow | High | High | 🟠 8/10 |
| 9 | Profile Redesign | Medium | Medium | 🟡 6/10 |
| 10 | Footer Enhancement | Low | Low | 🟢 4/10 |
| 11 | Property Detail | High | High | 🟠 7/10 |
| 12 | Wishlist Features | Medium | Medium | 🟡 5/10 |
| 13 | Dark Mode Fix | Medium | Medium | 🟡 6/10 |
| 14 | Micro-interactions | Medium | Medium | 🟡 6/10 |
| 15 | Image Loading | Low | Low | 🟢 4/10 |
| 16 | Accessibility | High | High | 🟠 7/10 |
| 17 | Performance | Medium | High | 🟡 6/10 |
| 18 | Advanced Search | Low | High | 🟢 4/10 |

---

## 🎯 Quick Start Guide

### For Maximum Impact, Start Here:
1. **Week 1:** Prompts #1, #2, #3 (Critical fixes)
2. **Week 2:** Prompts #4, #5, #7 (Core UX improvements)
3. **Week 3:** Prompts #8, #11 (Key user flows)
4. **Week 4:** Prompts #6, #13, #14 (Polish)

### How to Use These Prompts:
1. Copy the entire prompt (including context and requirements)
2. Paste to your AI coding agent (Cursor, GitHub Copilot, Claude, etc.)
3. Review the generated code
4. Test thoroughly
5. Commit with descriptive message referencing prompt number

### Testing Checklist After Each Implementation:
- [ ] Works on mobile (iOS Safari, Chrome)
- [ ] Works on desktop (Chrome, Firefox, Safari)
- [ ] Keyboard navigation works
- [ ] Screen reader announces correctly
- [ ] Dark mode looks good
- [ ] No console errors
- [ ] Performance is acceptable (no jank)

---

## 🔗 Related Files & Resources

### Key Files You'll Be Editing:
- `/web/src/components/common/` - Navigation, footer, search
- `/web/src/components/property/` - Property cards and details
- `/web/src/components/booking/` - Booking flow
- `/web/src/styles/globals.css` - Global styles
- `/web/tailwind.config.ts` - Design tokens

### Reference Documents:
- `/BRAND_COLORS.md` - Color palette and usage guidelines
- `/web/package.json` - Available dependencies
- `/WEB_PROGRESS.md` - Current implementation status

### Design System:
- **Colors:** See `tailwind.config.ts` for complete palette
- **Components:** See `globals.css` for btn-primary, btn-secondary, card classes
- **Icons:** Using Lucide React - https://lucide.dev
- **Typography:** System fonts with antialiasing

---

## 💡 Pro Tips for AI Coding Agents

1. **Context is King:** Always include file paths and existing code structure in prompts
2. **Incremental Changes:** Make one improvement at a time, test, then move to next
3. **Preserve Existing Patterns:** Match the coding style of the codebase
4. **Accessibility First:** Build it accessible from the start, not as an afterthought
5. **Mobile First:** Design for mobile, enhance for desktop
6. **Performance Budget:** Keep bundle size in check, lazy load when possible
7. **Brand Consistency:** Always reference the brand colors and luxury safari aesthetic
8. **User Feedback:** Every action should have feedback (loading, success, error)

---

## 📝 Notes

- All prompts are designed to work with the existing tech stack (Next.js 14, Tailwind CSS, TypeScript)
- Color codes and component class names reference the actual codebase
- Prompts maintain the "Luxury Safari" brand aesthetic
- Each prompt is self-contained but can be combined for larger features
- Estimated effort levels: Low (< 4 hours), Medium (4-8 hours), High (> 8 hours)

**Last Updated:** December 13, 2025  
**Maintainer:** StayAfrica Development Team  
**Version:** 1.0
