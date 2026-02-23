import type { Metadata } from 'next';
import { NotificationsContent } from '@/components/notifications/notifications-content';

export const metadata: Metadata = {
    title: 'Notifications - StayAfrica',
    description: 'View your notifications about bookings, messages, and more',
};

export default function NotificationsPage() {
    return <NotificationsContent />;
}
