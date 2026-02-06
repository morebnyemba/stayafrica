/**
 * Payment Methods & Gateways
 * 
 * This file documents the payment system integration for the StayAfrica mobile app.
 * The system supports multiple payment providers across different regions.
 */

// =============================================
// SUPPORTED PAYMENT PROVIDERS
// =============================================

/**
 * Stripe - Global payment processing
 * - Supports: Credit/Debit Cards
 * - Region: Global
 * - SDK: stripe-react-native
 * - Webhook: stripe event handling
 */

/**
 * Paynow - Zimbabwe payment gateway
 * - Supports: Ecocash, Visa
 * - Region: Zimbabwe
 * - SDK: Official Paynow SDK
 * - Webhook: Paynow status callbacks
 */

/**
 * Flutterwave - Africa-wide payment processor
 * - Supports: Cards, Mobile Money, USSD, Bank Transfers
 * - Region: 33+ African countries
 * - SDK: REST API integration
 * - Webhook: Flutterwave event handling
 */

/**
 * Paystack - Nigeria, Ghana, South Africa
 * - Supports: Cards, Bank Transfers
 * - Region: Nigeria, Ghana, South Africa, Kenya
 * - SDK: Pypaystack2 (Backend), REST API (Frontend)
 * - Webhook: Paystack event handling
 */

// =============================================
// API ENDPOINTS
// =============================================

/**
 * GET /wallet/payment-methods/
 * Fetch all payment methods for the authenticated user
 * 
 * Response:
 * {
 *   "results": [
 *     {
 *       "id": "pm_123456",
 *       "type": "card" | "mobile" | "bank",
 *       "provider": "stripe" | "paynow" | "flutterwave" | "paystack",
 *       "name": "Visa •••• 4242",
 *       "lastFour": "4242",
 *       "isDefault": true,
 *       "createdAt": "2026-02-06T10:30:00Z"
 *     }
 *   ]
 * }
 */

/**
 * POST /wallet/payment-methods/
 * Add a new payment method
 * 
 * Request:
 * {
 *   "provider": "stripe" | "paynow" | "flutterwave" | "paystack",
 *   "method": "card" | "mobile_money" | "bank_transfer" | "ussd",
 *   
 *   // For card payments:
 *   "cardNumber": "4242424242424242",
 *   "cardHolder": "John Doe",
 *   "expiryMonth": "12",
 *   "expiryYear": "26",
 *   "cvv": "123",
 *   
 *   // For mobile money:
 *   "phoneNumber": "+1234567890",
 *   
 *   // For bank transfers:
 *   "accountNumber": "1234567890",
 *   "accountHolder": "John Doe",
 *   "bankCode": "GTB"
 * }
 * 
 * Response:
 * {
 *   "id": "pm_123456",
 *   "type": "card",
 *   "provider": "stripe",
 *   "status": "initiated",
 *   "checkout_url": "https://checkout.stripe.com/...", // Redirect for verification
 *   "message": "Payment method added successfully"
 * }
 */

/**
 * PATCH /wallet/payment-methods/{id}/
 * Update payment method (set as default, etc.)
 * 
 * Request:
 * {
 *   "is_default": true
 * }
 */

/**
 * DELETE /wallet/payment-methods/{id}/
 * Remove a payment method
 */

// =============================================
// PAYMENT PROCESSING FLOW
// =============================================

/**
 * 1. Initialize Payment
 * POST /bookings/{booking_id}/pay/
 * 
 * Request:
 * {
 *   "payment_method_id": "pm_123456",
 *   "provider": "stripe"
 * }
 * 
 * Response:
 * {
 *   "id": "pay_123456",
 *   "status": "initiated",
 *   "amount": "250.00",
 *   "currency": "USD",
 *   "provider": "stripe",
 *   "gateway_ref": "ch_1234567890abcdef",
 *   "checkout_url": "https://checkout.stripe.com/..." // Redirect user
 * }
 */

/**
 * 2. Webhook - Payment Completed
 * The backend receives webhook from payment provider
 * and updates booking status automatically
 * 
 * Webhook Events:
 * - charge.succeeded → Payment successful
 * - charge.failed → Payment failed
 * - charge.refunded → Payment refunded
 */

/**
 * 3. Query Payment Status
 * GET /bookings/{booking_id}/payment/status/
 * 
 * Response:
 * {
 *   "id": "pay_123456",
 *   "status": "completed" | "pending" | "failed",
 *   "amount": "250.00",
 *   "currency": "USD",
 *   "provider": "stripe"
 * }
 */

// =============================================
// INTEGRATION CHECKLIST
// =============================================

/**
 * Mobile App Integration Requirements:
 * 
 * ✅ Payment Methods Management Screen
 *    - List existing payment methods
 *    - Add new payment method (wizard)
 *    - Set default payment method
 *    - Delete payment method
 * 
 * ✅ Payment Wizard
 *    - Step 1: Provider Selection (Stripe, Paynow, Flutterwave, Paystack)
 *    - Step 2: Payment Method Selection (Card, Mobile Money, Bank Transfer, USSD)
 *    - Step 3: Details Entry (Form validation)
 *    - Step 4: Confirmation (Success/Error handling)
 * 
 * ✅ Payment Processing for Bookings
 *    - Select payment method at checkout
 *    - Redirect to provider (if required)
 *    - Handle webhook callbacks
 *    - Display payment status
 * 
 * Backend Integration:
 * ✅ Payment Gateway Service (payment_gateway_enhanced.py)
 * ✅ Webhook Handlers (all providers)
 * ✅ Payment Models (Payment, PaymentMethod)
 * ✅ Currency Exchange Rate Logic
 * ✅ Commission Calculation
 */

// =============================================
// SECURITY BEST PRACTICES
// =============================================

/**
 * Card Information:
 * - NEVER store full card numbers in app
 * - Use tokenization (provider handles it)
 * - Never log sensitive data
 * - Use HTTPS only for API calls
 * - Validate all inputs on both client/backend
 * 
 * Compliance:
 * - PCI DSS compliance handled by payment providers
 * - Use official SDKs for payment processing
 * - Never implement custom payment card handling
 */

export type PaymentProvider = 'stripe' | 'paynow' | 'flutterwave' | 'paystack';
export type PaymentMethod = 'card' | 'mobile_money' | 'bank_transfer' | 'ussd';
export type PaymentStatus = 'initiated' | 'pending' | 'completed' | 'failed' | 'refunded';

export interface PaymentMethodResponse {
  id: string;
  type: 'card' | 'mobile' | 'bank';
  provider: PaymentProvider;
  name: string;
  lastFour?: string;
  isDefault: boolean;
  createdAt: string;
}

export interface PaymentInitResponse {
  id: string;
  status: PaymentStatus;
  amount: string;
  currency: string;
  provider: PaymentProvider;
  gateway_ref: string;
  checkout_url?: string;
  redirect_url?: string;
  payment_link?: string;
}
