import type { Metadata } from 'next';
import { PaymentHistory } from '@/components/buyer/payment-history';

export const metadata: Metadata = {
  title: 'Payment History - StayAfrica',
  description: 'View all your payments',
};

export default function PaymentsPage() {
  return <PaymentHistory />;
}
