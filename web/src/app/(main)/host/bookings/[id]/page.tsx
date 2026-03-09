import type { Metadata } from 'next';
import { HostBookingDetailContent } from '@/components/host/host-booking-detail-content';

export const metadata: Metadata = {
    title: 'Booking Details - StayAfrica Host',
    description: 'View full booking details',
};

export default function HostBookingDetailPage({ params }: { params: Promise<{ id: string }> }) {
    return <HostBookingDetailContent params={params} />;
}
