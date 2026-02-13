import type { Metadata } from 'next';
import { HostExperienceBookingsContent } from '@/components/host/host-experience-bookings-content';

export const metadata: Metadata = {
  title: 'Experience Bookings - StayAfrica Host',
  description: 'Manage bookings for your experiences',
};

export default function HostExperienceBookingsPage() {
  return <HostExperienceBookingsContent />;
}
