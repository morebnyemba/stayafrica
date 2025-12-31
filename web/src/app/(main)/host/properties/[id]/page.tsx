import type { Metadata } from 'next';
import { PropertyDetailContent } from '@/components/property/property-detail';

export const metadata: Metadata = {
  title: 'Property Details - StayAfrica',
  description: 'View your property details',
};

export default function HostPropertyViewPage({ params }: { params: { id: string } }) {
  return <PropertyDetailContent propertyId={params.id} />;
}
