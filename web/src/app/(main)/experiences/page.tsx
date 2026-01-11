import type { Metadata } from 'next';
import { ExperiencesContent } from '@/components/experience/experiences-content';

export const metadata: Metadata = {
  title: 'Experiences - StayAfrica',
  description: 'Discover unique experiences and activities across Africa',
};

export default function ExperiencesPage() {
  return <ExperiencesContent />;
}
