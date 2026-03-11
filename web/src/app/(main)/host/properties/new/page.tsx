import type { Metadata } from 'next';
import { PropertyForm } from '@/components/host/property-form-multistep';

export const metadata: Metadata = {
  title: 'Add New Property - StayAfrica',
  description: 'List a new property on StayAfrica',
};

export default function NewPropertyPage() {
  return (
    <div className="min-h-screen bg-sand-100">
      <div className="max-w-4xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        <div className="mb-8">
          <h1 className="text-3xl md:text-4xl font-bold text-primary-900 mb-2">
            List Your Property
          </h1>
          <p className="text-lg text-primary-600">
            Fill in the details below to list your property on StayAfrica
          </p>
        </div>

        <PropertyForm />
      </div>
    </div>
  );
}
