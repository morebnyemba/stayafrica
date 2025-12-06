import type { Metadata } from 'next';
import { ProfileContent } from '@/components/common/profile-content';

export const metadata: Metadata = {
  title: 'Profile - StayAfrica',
  description: 'Manage your profile',
};

export default function ProfilePage() {
  return <ProfileContent />;
}
