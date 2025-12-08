import type { Metadata } from 'next';
import { HostContent } from '@/components/common/host-content';

export const metadata: Metadata = {
  title: 'Become a Host - StayAfrica',
  description: 'List your property on StayAfrica and earn income by hosting travelers from around the world',
};

export default function HostPage() {
  return <HostContent />;
}
