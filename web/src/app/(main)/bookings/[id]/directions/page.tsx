'use client';

import { useEffect, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { MapDirections } from '@/components/map/map-directions';
import { Button } from '@/components/ui';
import { ArrowLeft, MapPin } from 'lucide-react';
import { useRouter } from 'next/navigation';

export default function BookingDirectionsPage({ params }: { params: Promise<{ id: string }> }) {
  const router = useRouter();
  const [bookingId, setBookingId] = useState<string | null>(null);

  useEffect(() => {
    async function initializeParams() {
      const resolvedParams = await params;
      setBookingId(resolvedParams.id);
    }
    initializeParams();
  }, [params]);

  const { data: bookingData, isLoading, error } = useQuery({
    queryKey: ['booking', bookingId],
    queryFn: async () => {
      if (!bookingId) throw new Error('Booking ID not available');
      const response = await apiClient.getBookings({ id: bookingId });
      return response.data?.results?.[0];
    },
    enabled: !!bookingId,
  });

  if (!bookingId) {
    return (
      <div className="max-w-7xl mx-auto px-4 py-12">
        <div className="text-center">
          <p className="text-primary-900 dark:text-sand-100">Loading...</p>
        </div>
      </div>
    );
  }

  if (isLoading) {
    return (
      <div className="bg-sand-100 dark:bg-primary-900 min-h-screen">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
          <div className="card p-12 text-center">
            <p className="text-primary-600 dark:text-sand-300">Loading booking details...</p>
          </div>
        </div>
      </div>
    );
  }

  if (error || !bookingData) {
    return (
      <div className="bg-sand-100 dark:bg-primary-900 min-h-screen">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
          <div className="card p-12 text-center">
            <p className="text-primary-600 dark:text-sand-300 mb-4">
              Unable to load booking details.
            </p>
            <Button onClick={() => router.push('/bookings')}>
              Back to Bookings
            </Button>
          </div>
        </div>
      </div>
    );
  }

  // Check if booking has property location data
  const property = bookingData.property;
  if (!property?.latitude || !property?.longitude) {
    return (
      <div className="bg-sand-100 dark:bg-primary-900 min-h-screen">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
          <div className="mb-6">
            <Button onClick={() => router.back()} variant="outline" size="sm">
              <ArrowLeft className="w-4 h-4 mr-2" />
              Back
            </Button>
          </div>
          <div className="card p-12 text-center">
            <MapPin className="w-12 h-12 text-primary-400 dark:text-sand-500 mx-auto mb-4" />
            <p className="text-primary-600 dark:text-sand-300 mb-4">
              Location information is not available for this property.
            </p>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="bg-sand-100 dark:bg-primary-900 min-h-screen">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
        {/* Header */}
        <div className="mb-6">
          <Button onClick={() => router.back()} variant="outline" size="sm" className="mb-4">
            <ArrowLeft className="w-4 h-4 mr-2" />
            Back
          </Button>
          <h1 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-2">
            Directions to Property
          </h1>
          <p className="text-lg text-primary-600 dark:text-sand-200">
            {property.title}
          </p>
        </div>

        {/* Property Info */}
        <div className="card p-6 mb-6">
          <div className="flex items-start gap-4">
            <MapPin className="w-6 h-6 text-secondary-600 flex-shrink-0 mt-1" />
            <div>
              <h2 className="text-lg font-semibold text-primary-900 dark:text-sand-50 mb-1">
                {property.title}
              </h2>
              <p className="text-primary-700 dark:text-sand-300 mb-2">
                {property.city}, {property.country}
              </p>
              {property.address && (
                <p className="text-sm text-primary-600 dark:text-sand-400">
                  {property.address}
                </p>
              )}
            </div>
          </div>

          {/* Booking dates */}
          <div className="mt-4 pt-4 border-t border-primary-200 dark:border-primary-700">
            <div className="grid grid-cols-2 gap-4 text-sm">
              <div>
                <span className="text-primary-600 dark:text-sand-400">Check-in:</span>
                <span className="ml-2 font-semibold text-primary-900 dark:text-sand-50">
                  {new Date(bookingData.check_in).toLocaleDateString()}
                </span>
              </div>
              <div>
                <span className="text-primary-600 dark:text-sand-400">Check-out:</span>
                <span className="ml-2 font-semibold text-primary-900 dark:text-sand-50">
                  {new Date(bookingData.check_out).toLocaleDateString()}
                </span>
              </div>
            </div>
          </div>
        </div>

        {/* Map with Directions */}
        <MapDirections
          destination={{
            lat: property.latitude,
            lng: property.longitude,
            address: property.address,
            name: property.title,
          }}
        />
      </div>
    </div>
  );
}
