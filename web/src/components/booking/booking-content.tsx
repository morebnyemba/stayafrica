'use client';

import { useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { Calendar, MapPin } from 'lucide-react';
import { useAuth } from '@/store/auth-store';

export function BookingContent() {
  const { isAuthenticated } = useAuth();
  const [statusFilter, setStatusFilter] = useState('');

  const { data: bookingsData, isLoading, error } = useQuery({
    queryKey: ['bookings', statusFilter],
    queryFn: async () => {
      const response = await apiClient.getBookings({
        status: statusFilter || undefined,
      });
      return response.data;
    },
    enabled: isAuthenticated,
  });

  const bookings = bookingsData?.results || [];

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'CONFIRMED':
        return 'bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-200';
      case 'PENDING':
        return 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-200';
      case 'CANCELLED':
        return 'bg-red-100 text-red-800 dark:bg-red-900 dark:text-red-200';
      default:
        return 'bg-primary-100 text-primary-800 dark:bg-primary-700 dark:text-sand-200';
    }
  };

  if (!isAuthenticated) {
    return (
      <div className="bg-sand-100 dark:bg-primary-900 min-h-screen">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
          <div className="card p-12 text-center">
            <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-4">
              Sign In Required
            </h2>
            <p className="text-primary-600 dark:text-sand-300 mb-8">
              Please sign in to view your bookings.
            </p>
            <a href="/login" className="btn-primary px-8 py-3">
              Sign In
            </a>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="bg-sand-100 dark:bg-primary-900 min-h-screen">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
        {/* Header */}
        <div className="mb-8">
          <h1 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-2">
            My Bookings
          </h1>
          <p className="text-lg text-primary-600 dark:text-sand-200">
            View and manage your property bookings
          </p>
        </div>

        {/* Filters */}
        <div className="card p-6 mb-8">
          <div className="flex flex-wrap gap-4">
            <button
              onClick={() => setStatusFilter('')}
              className={`px-4 py-2 rounded-lg font-medium transition ${
                statusFilter === ''
                  ? 'bg-secondary-500 text-primary-900'
                  : 'bg-primary-100 dark:bg-primary-700 text-primary-700 dark:text-sand-200 hover:bg-secondary-100'
              }`}
            >
              All Bookings
            </button>
            <button
              onClick={() => setStatusFilter('PENDING')}
              className={`px-4 py-2 rounded-lg font-medium transition ${
                statusFilter === 'PENDING'
                  ? 'bg-secondary-500 text-primary-900'
                  : 'bg-primary-100 dark:bg-primary-700 text-primary-700 dark:text-sand-200 hover:bg-secondary-100'
              }`}
            >
              Pending
            </button>
            <button
              onClick={() => setStatusFilter('CONFIRMED')}
              className={`px-4 py-2 rounded-lg font-medium transition ${
                statusFilter === 'CONFIRMED'
                  ? 'bg-secondary-500 text-primary-900'
                  : 'bg-primary-100 dark:bg-primary-700 text-primary-700 dark:text-sand-200 hover:bg-secondary-100'
              }`}
            >
              Confirmed
            </button>
            <button
              onClick={() => setStatusFilter('CANCELLED')}
              className={`px-4 py-2 rounded-lg font-medium transition ${
                statusFilter === 'CANCELLED'
                  ? 'bg-secondary-500 text-primary-900'
                  : 'bg-primary-100 dark:bg-primary-700 text-primary-700 dark:text-sand-200 hover:bg-secondary-100'
              }`}
            >
              Cancelled
            </button>
          </div>
        </div>

        {/* Bookings List */}
        {isLoading ? (
          <div className="space-y-4">
            {[1, 2, 3].map((i) => (
              <div key={i} className="card p-6 animate-pulse">
                <div className="h-6 bg-primary-200 dark:bg-primary-700 rounded mb-4 w-1/2"></div>
                <div className="h-4 bg-primary-200 dark:bg-primary-700 rounded mb-2"></div>
                <div className="h-4 bg-primary-200 dark:bg-primary-700 rounded w-3/4"></div>
              </div>
            ))}
          </div>
        ) : error ? (
          <div className="card p-12 text-center">
            <p className="text-primary-600 dark:text-sand-300 mb-4">
              Unable to load bookings. Please try again later.
            </p>
            <p className="text-sm text-primary-500 dark:text-sand-400">
              Error: {error instanceof Error ? error.message : 'Unknown error'}
            </p>
          </div>
        ) : bookings.length === 0 ? (
          <div className="card p-12 text-center">
            <Calendar className="w-16 h-16 text-primary-400 dark:text-sand-500 mx-auto mb-4" />
            <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-2">
              No Bookings Found
            </h3>
            <p className="text-primary-600 dark:text-sand-300 mb-8">
              You haven&apos;t made any bookings yet. Start exploring properties!
            </p>
            <a href="/explore" className="btn-primary px-8 py-3">
              Explore Properties
            </a>
          </div>
        ) : (
          <div className="space-y-6">
            {bookings.map((booking: any) => (
              <article key={booking.id} className="card overflow-hidden">
                <div className="p-6">
                  <div className="flex flex-col md:flex-row md:items-start justify-between mb-4">
                    <div className="flex-1 mb-4 md:mb-0">
                      <div className="flex items-center gap-3 mb-2">
                        <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50">
                          {booking.property?.title || booking.property_title || 'Property'}
                        </h3>
                        <span className={`px-3 py-1 rounded-full text-xs font-semibold ${getStatusColor(booking.status)}`}>
                          {booking.status}
                        </span>
                      </div>
                      <div className="flex items-center gap-2 text-primary-600 dark:text-sand-300 text-sm mb-2">
                        <MapPin className="w-4 h-4" />
                        <span>{booking.property?.city || 'Unknown'}, {booking.property?.country || 'Unknown'}</span>
                      </div>
                    </div>
                    <div className="text-right">
                      <div className="text-2xl font-bold text-primary-900 dark:text-sand-50">
                        ${booking.grand_total}
                      </div>
                      <div className="text-sm text-primary-600 dark:text-sand-300">
                        Total Amount
                      </div>
                    </div>
                  </div>

                  <div className="grid md:grid-cols-3 gap-4 py-4 border-t border-primary-200 dark:border-primary-700">
                    <div>
                      <div className="text-sm text-primary-600 dark:text-sand-400 mb-1">Check-in</div>
                      <div className="font-semibold text-primary-900 dark:text-sand-50">
                        {new Date(booking.check_in).toLocaleDateString()}
                      </div>
                    </div>
                    <div>
                      <div className="text-sm text-primary-600 dark:text-sand-400 mb-1">Check-out</div>
                      <div className="font-semibold text-primary-900 dark:text-sand-50">
                        {new Date(booking.check_out).toLocaleDateString()}
                      </div>
                    </div>
                    <div>
                      <div className="text-sm text-primary-600 dark:text-sand-400 mb-1">Nights</div>
                      <div className="font-semibold text-primary-900 dark:text-sand-50">
                        {booking.nights}
                      </div>
                    </div>
                  </div>

                  <div className="flex gap-4 mt-4">
                    <button className="btn-secondary px-6 py-2 text-sm">
                      View Details
                    </button>
                    {booking.status === 'CONFIRMED' && (
                      <button className="btn-primary px-6 py-2 text-sm">
                        Contact Host
                      </button>
                    )}
                  </div>
                </div>
              </article>
            ))}
          </div>
        )}
      </div>
    </div>
  );
}
