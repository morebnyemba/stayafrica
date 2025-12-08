import type { Metadata } from 'next';
import { AboutContent } from '@/components/common/about-content';

export const metadata: Metadata = {
  title: 'About StayAfrica - Our Story',
  description: 'Learn about StayAfrica and our mission to connect travelers with unique accommodations across Africa',
};

export default function AboutPage() {
  return <AboutContent />;
}
