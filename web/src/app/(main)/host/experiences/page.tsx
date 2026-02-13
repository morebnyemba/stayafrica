import type { Metadata } from 'next';
import { HostExperiencesContent } from '@/components/host/host-experiences-content';

export const metadata: Metadata = {
  title: 'My Experiences - StayAfrica',
  description: 'Manage your experiences on StayAfrica',
};

export default function HostExperiencesPage() {
  return <HostExperiencesContent />;
}
