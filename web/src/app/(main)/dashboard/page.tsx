import type { Metadata } from 'next';
import { DashboardContent } from '@/components/common/dashboard-content';

export const metadata: Metadata = {
  title: 'Dashboard - StayAfrica',
  description: 'Your StayAfrica dashboard',
};

export default function DashboardPage() {
  return <DashboardContent />;
}
