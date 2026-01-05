# Frontend Integration - Wallet Management

This document describes the frontend integration for the wallet management system.

## Overview

Complete frontend integration for the wallet management, transaction tracking, and withdrawal features. The implementation provides a user-friendly interface for hosts and users to manage their funds.

## New Features

### 1. Wallet Dashboard (`/wallet`)

A comprehensive dashboard that displays:
- **Wallet Balance**: Large, prominent display of available funds
- **Currency**: Shows the wallet currency (USD, etc.)
- **Status**: Active/suspended wallet status
- **Quick Actions**: Withdraw funds, manage bank accounts

### 2. Transaction History Tab

- Lists all wallet transactions (credits, debits, refunds)
- Color-coded transaction types:
  - **Green**: Credits and refunds (money in)
  - **Red**: Debits (money out)
- Shows transaction details:
  - Reference number
  - Amount
  - Status (completed, pending, failed)
  - Date and time
- Visual status indicators with icons

### 3. Withdrawals Tab

- Track all withdrawal requests
- Shows withdrawal details:
  - Amount
  - Destination bank account
  - Reference number
  - Status badges (completed, pending, processing, failed)
- View notes/reasons for failed withdrawals

### 4. Bank Accounts Tab

- Add new bank accounts
- Manage existing accounts:
  - View masked account numbers (•••• 1234)
  - Set primary account
  - Delete accounts
- Primary account indicator badge

### 5. Withdrawal Modal

Form to initiate withdrawals with:
- Amount input (validated against balance)
- Bank account selection dropdown
- Optional notes field
- Minimum withdrawal validation ($10)
- Real-time error feedback

### 6. Bank Account Modal

Form to add bank accounts with fields:
- Bank name *
- Account holder name *
- Account number *
- Branch code (optional)
- Country (optional)
- Primary account checkbox

## API Client Methods

Added to `src/services/api-client.ts`:

### Wallet Operations
```typescript
getMyWallet()                           // Get user's wallet
getWalletBalance(walletId)              // Get balance for specific wallet
getWalletTransactions(walletId, params) // Get paginated transactions
```

### Bank Account Management
```typescript
getBankAccounts()                       // List all user's bank accounts
createBankAccount(data)                 // Add new bank account
updateBankAccount(accountId, data)      // Update existing account
deleteBankAccount(accountId)            // Delete account
setPrimaryBankAccount(accountId)        // Set as primary
```

### Withdrawal Operations
```typescript
getWithdrawals(params)                  // Get withdrawal history
initiateWithdrawal(data)                // Start new withdrawal
```

### Transaction Queries
```typescript
getTransactions(params)                 // Get all transactions with filters
```

## Components

### WalletDashboard
Main component that orchestrates the wallet interface.

**State Management:**
- Active tab selection
- Modal visibility
- Data fetching with React Query

**Features:**
- Tab navigation (Transactions, Withdrawals, Bank Accounts)
- Conditional rendering based on data state
- Loading states with skeleton screens
- Error handling with retry options

### TransactionsList
Displays transaction history with visual formatting.

**Features:**
- Transaction type icons
- Color-coded amounts
- Status indicators
- Date/time display
- Empty state message

### WithdrawalsList
Shows withdrawal history and status.

**Features:**
- Bank account details
- Status badges
- Reference numbers
- Notes display for failed withdrawals
- Empty state message

### BankAccountsList
Manages bank accounts.

**Features:**
- Add account button
- Primary account indicator
- Set primary action
- Delete with confirmation
- Empty state with action prompt

### WithdrawModal
Modal form for withdrawal initiation.

**Features:**
- Amount validation
- Bank account dropdown
- Balance display
- Minimum amount enforcement
- Error display
- Loading state during submission

### BankAccountModal
Modal form for adding bank accounts.

**Features:**
- Required field validation
- Primary account selection
- Country input
- Error display
- Loading state during submission

## User Flows

### Withdraw Funds
1. User clicks "Withdraw" button on wallet card
2. Modal opens with withdrawal form
3. User enters amount (validated against balance)
4. User selects destination bank account
5. Optional: User adds notes
6. User submits form
7. System validates and initiates withdrawal
8. Withdrawal appears in Withdrawals tab as "Pending"

### Add Bank Account
1. User clicks "Manage Banks" or "Add Bank Account"
2. Modal opens with bank account form
3. User fills in required fields
4. Optional: User sets as primary account
5. User submits form
6. Account added and appears in Bank Accounts tab

### View Transaction History
1. User navigates to /wallet
2. Transactions tab is active by default
3. User sees list of all transactions
4. Transactions show type, amount, status, and date
5. User can identify credits (green) vs debits (red) at a glance

## Design Features

### Responsive Design
- Mobile-first approach
- Breakpoints for tablet and desktop
- Touch-friendly buttons and forms
- Stacked layout on mobile, side-by-side on desktop

### Dark Mode Support
- Full dark mode implementation
- Uses theme-aware colors
- Accessible contrast ratios
- Smooth transitions

### Visual Indicators
- **Icons**: 
  - Wallet icon for balance
  - Arrow icons for transaction direction
  - Building icon for bank accounts
  - Status icons (check, clock, X)
- **Colors**:
  - Primary brand colors for actions
  - Green for positive transactions
  - Red for negative transactions
  - Yellow/blue for pending states
- **Badges**: 
  - Status badges with appropriate colors
  - Primary account badge

### Loading States
- Skeleton screens for initial load
- Animated pulse effect
- Button loading states
- Disabled states during operations

### Error Handling
- Inline error messages
- Retry buttons for failed loads
- Form validation errors
- API error display in modals

## Integration with Backend

### Endpoints Used
```
GET  /api/v1/payments/wallets/my_wallet/
GET  /api/v1/payments/wallets/:id/balance/
GET  /api/v1/payments/wallets/:id/transactions/
GET  /api/v1/payments/bank-accounts/
POST /api/v1/payments/bank-accounts/
PATCH /api/v1/payments/bank-accounts/:id/
DELETE /api/v1/payments/bank-accounts/:id/
POST /api/v1/payments/bank-accounts/:id/set_primary/
GET  /api/v1/payments/withdrawals/
POST /api/v1/payments/withdrawals/
GET  /api/v1/payments/transactions/
```

### Data Flow
1. User action triggers API call via apiClient
2. React Query manages caching and state
3. Success: UI updates with new data
4. Error: Display error message with retry option
5. Mutations invalidate relevant queries to refetch data

## Security Considerations

- Authentication required (ProtectedRoute wrapper)
- API token passed in Authorization header
- Bank account numbers masked in display
- Deletion requires confirmation
- Amount validation prevents overdrafts
- Server-side validation for all operations

## Future Enhancements

Potential improvements:
- [ ] Real-time balance updates via WebSocket
- [ ] Transaction filters and search
- [ ] Export transaction history (CSV/PDF)
- [ ] Scheduled withdrawals
- [ ] Multi-currency support UI
- [ ] Transaction receipts
- [ ] Push notifications for status changes
- [ ] Biometric confirmation for withdrawals
- [ ] Transaction analytics/charts

## Testing Recommendations

### Manual Testing
1. Test wallet display with different balances
2. Verify transaction list pagination
3. Test withdrawal with insufficient funds
4. Test adding/deleting bank accounts
5. Verify primary account switching
6. Test responsive layout on mobile
7. Test dark mode transitions
8. Test error states (network failures)

### E2E Tests
```typescript
test('complete withdrawal flow', async () => {
  // Navigate to wallet
  // Add bank account
  // Initiate withdrawal
  // Verify withdrawal appears in list
});

test('bank account management', async () => {
  // Add multiple accounts
  // Set one as primary
  // Verify only one primary exists
  // Delete account
});
```

## Deployment

No additional configuration required. The frontend will automatically connect to the backend API endpoints defined in `NEXT_PUBLIC_API_BASE_URL`.

Ensure backend has CORS configured to allow requests from the frontend domain.

## Summary

The frontend integration provides a complete, production-ready wallet management interface that seamlessly integrates with the backend transaction service. Users can view balances, track transactions, manage bank accounts, and initiate withdrawals through an intuitive, responsive interface.

**Files Added:**
- `web/src/app/(main)/wallet/page.tsx` - Wallet page
- `web/src/components/wallet/wallet-dashboard.tsx` - Main component
- `web/src/services/api-client.ts` - Updated with new methods

**Commit:** 20701aa
