import type { Metadata } from 'next';
import { HostCalendarContent } from '@/components/host/host-calendar-content';

export const metadata: Metadata = {
    title: 'Calendar - StayAfrica Host',
    description: 'View and manage bookings across all your properties',
};

export default function HostCalendarPage() {
    return <HostCalendarContent />;
}
