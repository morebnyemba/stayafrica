import type { Metadata } from 'next';
import { BookingContent } from '@/components/booking/booking-content';

export const metadata: Metadata = {
  title: 'My Bookings - StayAfrica',
  description: 'View and manage your bookings',
};

export default function BookingsPage() {
  return <BookingContent />;
}
