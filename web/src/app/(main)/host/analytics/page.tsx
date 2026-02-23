import type { Metadata } from 'next';
import { HostAnalyticsContent } from '@/components/host/host-analytics-content';

export const metadata: Metadata = {
    title: 'Analytics - StayAfrica Host',
    description: 'View performance analytics and market benchmarks for your properties',
};

export default function HostAnalyticsPage() {
    return <HostAnalyticsContent />;
}
