import type { Metadata } from 'next';
import { HelpContent } from '@/components/help/help-content';

export const metadata: Metadata = {
    title: 'Help Center - StayAfrica',
    description: 'Find answers to common questions about booking, hosting, payments, and more on StayAfrica',
};

export default function HelpPage() {
    return <HelpContent />;
}
