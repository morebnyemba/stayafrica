import type { Metadata } from 'next';
import { HostDashboard } from '@/components/host/host-dashboard';

export const metadata: Metadata = {
  title: 'Host Dashboard - StayAfrica',
  description: 'Manage your properties and bookings on StayAfrica',
};

export default function HostDashboardPage() {
  return <HostDashboard />;
}
