'use client';

import { useParams } from 'next/navigation';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import Link from 'next/link';
import { ArrowLeft } from 'lucide-react';
import PricingCalendar from '@/components/pricing/PricingCalendar';

export default function PropertyPricingPage() {
  const params = useParams();
  const propertyId = params?.id as string;

  const { data: property, isLoading } = useQuery({
    queryKey: ['property', propertyId],
    queryFn: async () => {
      const response = await apiClient.getPropertyById(propertyId);
      return response.data;
    },
    enabled: !!propertyId,
  });

  if (isLoading) {
    return (
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          <div className="animate-pulse space-y-4">
            <div className="h-8 bg-primary-200 dark:bg-primary-700 rounded w-1/3"></div>
            <div className="h-64 bg-primary-200 dark:bg-primary-700 rounded"></div>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Back Link */}
        <Link
          href="/host/pricing"
          className="inline-flex items-center gap-2 text-primary-600 dark:text-sand-400 hover:text-primary-900 dark:hover:text-sand-50 mb-6 transition-colors"
        >
          <ArrowLeft className="w-4 h-4" />
          <span>Back to Dynamic Pricing</span>
        </Link>

        {/* Header */}
        <div className="mb-8">
          <h1 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-2">
            {property?.title || 'Property'} - Pricing
          </h1>
          <p className="text-lg text-primary-600 dark:text-sand-300">
            {property?.city}, {property?.country} • Base price: ${property?.price_per_night}/night
          </p>
        </div>

        {/* Pricing Calendar */}
        <div className="mb-8">
          <PricingCalendar propertyId={propertyId} />
        </div>

        {/* Info Card */}
        <div className="bg-white dark:bg-primary-800 rounded-lg shadow-sm border border-primary-200 dark:border-primary-700 p-6">
          <h2 className="text-xl font-bold text-primary-900 dark:text-sand-50 mb-4">
            How Dynamic Pricing Works
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div>
              <h3 className="font-semibold text-primary-900 dark:text-sand-50 mb-2">
                Price Adjustments
              </h3>
              <p className="text-sm text-primary-600 dark:text-sand-400">
                Pricing rules automatically adjust your nightly rate based on demand, 
                seasonality, and booking patterns. Green indicates increased prices, 
                red indicates discounted prices.
              </p>
            </div>
            <div>
              <h3 className="font-semibold text-primary-900 dark:text-sand-50 mb-2">
                Stacking Rules
              </h3>
              <p className="text-sm text-primary-600 dark:text-sand-400">
                Multiple pricing rules can apply to the same date. Rules are applied 
                in priority order, with higher priority rules taking precedence.
              </p>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
