import type { Metadata } from 'next';
import { WalletDashboard } from '@/components/wallet/wallet-dashboard';

export const metadata: Metadata = {
  title: 'My Wallet - StayAfrica',
  description: 'Manage your wallet, view transactions, and withdraw funds',
};

export default function WalletPage() {
  return <WalletDashboard />;
}
