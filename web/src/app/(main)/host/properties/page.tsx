import type { Metadata } from 'next';
import { HostPropertiesContent } from '@/components/host/host-properties-content';

export const metadata: Metadata = {
  title: 'My Properties - StayAfrica',
  description: 'Manage your properties on StayAfrica',
};

export default function HostPropertiesPage() {
  return <HostPropertiesContent />;
}
