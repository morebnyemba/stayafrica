import type { Metadata } from 'next';
import { HostEarningsContent } from '@/components/host/host-earnings-content';

export const metadata: Metadata = {
  title: 'Earnings - StayAfrica Host',
  description: 'Track your earnings and financial performance',
};

export default function HostEarningsPage() {
  return <HostEarningsContent />;
}
