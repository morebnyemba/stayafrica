import type { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'Messaging & Automation | StayAfrica Host',
  description: 'Manage automated replies, message templates, quick replies, and scheduled messages for your properties.',
};

export default function MessagingLayout({ children }: { children: React.ReactNode }) {
  return <>{children}</>;
}
