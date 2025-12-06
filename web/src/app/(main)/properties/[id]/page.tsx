import type { Metadata } from 'next';
import { PropertyDetailContent } from '@/components/property/property-detail';

export const metadata: Metadata = {
  title: 'Property Details - StayAfrica',
  description: 'View detailed property information',
};

export default function PropertyPage({ params }: { params: { id: string } }) {
  return <PropertyDetailContent propertyId={params.id} />;
}
