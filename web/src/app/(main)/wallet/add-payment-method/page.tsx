import { AddPaymentMethodWizard } from '@/components/wallet/add-payment-method';
import type { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'Add Payment Method | StayAfrica',
  description: 'Add a new payment method to your account',
};

export default function AddPaymentMethodPage() {
  return <AddPaymentMethodWizard />;
}
