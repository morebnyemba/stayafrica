import type { Metadata } from 'next';
import { ExploreContent } from '@/components/property/explore-content';

export const metadata: Metadata = {
  title: 'Explore Properties - StayAfrica',
  description: 'Browse and search properties across Africa',
};

export default function ExplorePage() {
  return <ExploreContent />;
}
