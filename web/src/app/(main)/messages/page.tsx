import type { Metadata } from 'next';
import { MessagesContent } from '@/components/common/messages-content';

export const metadata: Metadata = {
  title: 'Messages - StayAfrica',
  description: 'Chat with hosts and guests',
};

export default function MessagesPage() {
  return <MessagesContent />;
}
