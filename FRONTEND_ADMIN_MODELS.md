# Frontend Admin - Non-System-Critical Models

## Summary

This document outlines which backend models have been added to the frontend admin dashboard and which were excluded as "system-critical".

## Implementation Status

### ✅ IMPLEMENTED - Tax Configuration
**Page:** `/admin/tax-config`
**Models:**
- Tax Jurisdictions (view/search)
- Tax Rates (view/search)
- Tax Remittances (planned)

**Features:**
- Tab-based interface
- Search and pagination
- Color-coded badges
- Active/inactive status

### 🚧 PLANNED - Additional Models

#### POI Management
- POI Categories (view)
- Points of Interest (view/verify)
- Bulk verification actions

#### Messaging Automation (Optional)
- Host Message Settings (view)
- Automated Messages (view/manage)
- Scheduled Messages (monitor queue)
- Quick Replies (view)

### ❌ EXCLUDED - System Critical Models

**Analytics Models (Readonly/System Generated):**
- PropertyAnalytics - Auto-generated metrics
- HostAnalyticsSummary - Auto-calculated summaries
- RevenueProjection - AI forecasts
- PerformanceBenchmark - Market data
- MessageAnalytics - Auto-tracked stats

**Auto-Generated Relationships:**
- ReviewVote - Auto-tracked votes
- WishlistVote - Auto-tracked engagement
- PropertyPOI - Auto-calculated distances
- BookingTax - Auto-calculated taxes

**Reasons for Exclusion:**
1. These are system-generated/calculated data
2. No admin management needed (readonly display only)
3. Would clutter admin interface
4. Better suited for analytics/reporting dashboards

## Design Principles

### What Makes a Model "Non-System-Critical"?
✅ **Include** if:
- Admins need to configure/manage the data
- User-facing features depend on admin configuration
- Requires moderation or approval workflows
- Business rules can be adjusted

❌ **Exclude** if:
- System automatically generates the data
- Readonly display only (no admin actions)
- Analytics/reporting purpose
- No configuration needed

## Current Frontend Admin Pages

1. **Dashboard** - Overview stats
2. **Users** - User management with verification
3. **Properties** - Property approval/management
4. **Bookings** - Booking monitoring/cancellation
5. **Reviews** - Review moderation
6. **Payments** - Payment tracking/refunds
7. **Wallets** - Wallet management
8. **Tax Config** - Tax jurisdiction/rate configuration ⭐ NEW
9. **Identity Verification** - KYC verification
10. **Audit Logs** - Activity monitoring
11. **Settings** - System configuration

## Recommended Additions (Low Priority)

### Wishlist Moderation (Optional)
- Public wishlists with inappropriate content
- Comment moderation on wishlists
- User-reported content

### Payment Methods (Optional)
- View user saved payment methods
- Handle payment method issues
- Token management (sensitive)

## Implementation Notes

### Tax Configuration
- Most critical addition for business operations
- Enables multi-jurisdiction tax compliance
- Requires backend API endpoints for CRUD operations
- Frontend provides search, filter, and view capabilities

### Future Enhancements
- Add/Edit functionality for tax jurisdictions
- Add/Edit functionality for tax rates
- Tax remittance tracking and reporting
- Tax exemption management
- POI verification workflow
- Bulk POI import/export

