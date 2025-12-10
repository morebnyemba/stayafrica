import type { Metadata } from 'next';
import { DashboardRouter } from '@/components/common/dashboard-router';

export const metadata: Metadata = {
  title: 'Dashboard - StayAfrica',
  description: 'Your StayAfrica dashboard',
};

export default function DashboardPage() {
  return <DashboardRouter />;
}
