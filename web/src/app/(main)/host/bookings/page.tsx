import type { Metadata } from 'next';
import { HostBookingsContent } from '@/components/host/host-bookings-content';

export const metadata: Metadata = {
  title: 'Manage Bookings - StayAfrica Host',
  description: 'Manage your property bookings',
};

export default function HostBookingsPage() {
  return <HostBookingsContent />;
}
