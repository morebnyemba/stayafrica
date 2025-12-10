'use client';

import { useAuth } from '@/context/auth-context';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { Calendar, MapPin, User, CheckCircle, XCircle, Clock, DollarSign } from 'lucide-react';
import { ProtectedRoute } from '@/components/auth/protected-route';
import { useState } from 'react';

export function HostBookingsContent() {
  const { user, isAuthenticated } = useAuth();
  const queryClient = useQueryClient();
  const [filter, setFilter] = useState<string>('all');

  // Fetch host bookings
  const { data: bookingsData, isLoading } = useQuery({
    queryKey: ['host', 'bookings', filter],
    queryFn: async () => {
      const params = filter !== 'all' ? { status: filter } : {};
      const response = await apiClient.getHostBookings(params);
      return response.data;
    },
    enabled: isAuthenticated && user?.role === 'host',
  });

  // Confirm booking mutation
  const confirmMutation = useMutation({
    mutationFn: (bookingId: string) => apiClient.confirmBooking(bookingId),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['host', 'bookings'] });
      queryClient.invalidateQueries({ queryKey: ['host', 'analytics'] });
    },
  });

  // Cancel booking mutation
  const cancelMutation = useMutation({
    mutationFn: (bookingId: string) => apiClient.cancelBooking(bookingId),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['host', 'bookings'] });
      queryClient.invalidateQueries({ queryKey: ['host', 'analytics'] });
    },
  });

  const handleConfirm = (bookingId: string) => {
    if (window.confirm('Are you sure you want to confirm this booking?')) {
      confirmMutation.mutate(bookingId);
    }
  };

  const handleCancel = (bookingId: string) => {
    if (window.confirm('Are you sure you want to cancel this booking? This action cannot be undone.')) {
      cancelMutation.mutate(bookingId);
    }
  };

  const bookings = bookingsData?.results || [];

  const getStatusBadge = (status: string) => {
    const badges: Record<string, { color: string; icon: any }> = {
      pending: { color: 'bg-yellow-100 dark:bg-yellow-900/30 text-yellow-800 dark:text-yellow-300', icon: Clock },
      confirmed: { color: 'bg-green-100 dark:bg-green-900/30 text-green-800 dark:text-green-300', icon: CheckCircle },
      cancelled: { color: 'bg-red-100 dark:bg-red-900/30 text-red-800 dark:text-red-300', icon: XCircle },
      completed: { color: 'bg-blue-100 dark:bg-blue-900/30 text-blue-800 dark:text-blue-300', icon: CheckCircle },
    };

    const badge = badges[status] || badges.pending;
    const Icon = badge.icon;

    return (
      <span className={`inline-flex items-center gap-1 px-3 py-1 text-xs font-semibold rounded-full ${badge.color}`}>
        <Icon className="w-3 h-3" />
        {status.charAt(0).toUpperCase() + status.slice(1)}
      </span>
    );
  };

  return (
    <ProtectedRoute>
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          {/* Header */}
          <div className="mb-8">
            <h1 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-2">
              Manage Bookings
            </h1>
            <p className="text-lg text-primary-600 dark:text-sand-300">
              View and manage reservations for your properties
            </p>
          </div>

          {/* Filter Tabs */}
          <div className="flex gap-2 mb-6 overflow-x-auto">
            {['all', 'pending', 'confirmed', 'completed', 'cancelled'].map((status) => (
              <button
                key={status}
                onClick={() => setFilter(status)}
                className={`px-4 py-2 rounded-lg font-medium whitespace-nowrap transition-colors ${
                  filter === status
                    ? 'bg-secondary-500 text-white'
                    : 'bg-white dark:bg-primary-800 text-primary-700 dark:text-sand-300 hover:bg-secondary-50 dark:hover:bg-secondary-900/10'
                }`}
              >
                {status.charAt(0).toUpperCase() + status.slice(1)}
              </button>
            ))}
          </div>

          {/* Bookings List */}
          {isLoading ? (
            <div className="space-y-4">
              {[1, 2, 3].map((i) => (
                <div key={i} className="card p-6 animate-pulse">
                  <div className="h-24 bg-primary-200 dark:bg-primary-700 rounded"></div>
                </div>
              ))}
            </div>
          ) : bookings.length > 0 ? (
            <div className="space-y-4">
              {bookings.map((booking: any) => (
                <div key={booking.id} className="card p-6 hover:shadow-lg transition-shadow">
                  <div className="flex flex-col lg:flex-row lg:items-center lg:justify-between gap-4">
                    {/* Booking Info */}
                    <div className="flex-1">
                      <div className="flex items-start justify-between mb-3">
                        <div>
                          <h3 className="text-lg font-bold text-primary-900 dark:text-sand-50 mb-1">
                            {booking.property?.title || 'Property'}
                          </h3>
                          <p className="text-sm text-primary-600 dark:text-sand-300 flex items-center gap-1">
                            <MapPin className="w-4 h-4" />
                            {booking.property?.city}, {booking.property?.country}
                          </p>
                        </div>
                        {getStatusBadge(booking.status)}
                      </div>

                      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4 text-sm">
                        <div>
                          <div className="text-primary-600 dark:text-sand-400 mb-1">Guest</div>
                          <div className="font-medium text-primary-900 dark:text-sand-50 flex items-center gap-1">
                            <User className="w-4 h-4" />
                            {booking.guest?.first_name} {booking.guest?.last_name}
                          </div>
                        </div>

                        <div>
                          <div className="text-primary-600 dark:text-sand-400 mb-1">Check-in</div>
                          <div className="font-medium text-primary-900 dark:text-sand-50 flex items-center gap-1">
                            <Calendar className="w-4 h-4" />
                            {new Date(booking.check_in).toLocaleDateString()}
                          </div>
                        </div>

                        <div>
                          <div className="text-primary-600 dark:text-sand-400 mb-1">Check-out</div>
                          <div className="font-medium text-primary-900 dark:text-sand-50 flex items-center gap-1">
                            <Calendar className="w-4 h-4" />
                            {new Date(booking.check_out).toLocaleDateString()}
                          </div>
                        </div>

                        <div>
                          <div className="text-primary-600 dark:text-sand-400 mb-1">Your Earnings</div>
                          <div className="font-medium text-green-600 dark:text-green-400 flex items-center gap-1">
                            <DollarSign className="w-4 h-4" />
                            {(
                              parseFloat(booking.nightly_total || 0) +
                              parseFloat(booking.cleaning_fee || 0) -
                              parseFloat(booking.commission_fee || 0)
                            ).toFixed(2)}
                          </div>
                        </div>
                      </div>

                      <div className="mt-3 pt-3 border-t border-primary-200 dark:border-primary-700 text-sm text-primary-600 dark:text-sand-400">
                        <span className="font-medium">Booking Ref:</span> {booking.booking_ref} •{' '}
                        <span className="font-medium">Nights:</span> {booking.nights || 
                          Math.ceil((new Date(booking.check_out).getTime() - new Date(booking.check_in).getTime()) / (1000 * 60 * 60 * 24))}
                      </div>
                    </div>

                    {/* Actions */}
                    {booking.status === 'pending' && (
                      <div className="flex gap-2 lg:flex-col">
                        <button
                          onClick={() => handleConfirm(booking.id)}
                          disabled={confirmMutation.isPending}
                          className="flex-1 lg:flex-none btn-primary px-4 py-2 text-sm flex items-center justify-center gap-1"
                        >
                          <CheckCircle className="w-4 h-4" />
                          Confirm
                        </button>
                        <button
                          onClick={() => handleCancel(booking.id)}
                          disabled={cancelMutation.isPending}
                          className="flex-1 lg:flex-none px-4 py-2 bg-red-600 hover:bg-red-700 text-white rounded-lg transition-colors text-sm flex items-center justify-center gap-1"
                        >
                          <XCircle className="w-4 h-4" />
                          Decline
                        </button>
                      </div>
                    )}

                    {booking.status === 'confirmed' && (
                      <div className="flex gap-2 lg:flex-col">
                        <button
                          onClick={() => handleCancel(booking.id)}
                          disabled={cancelMutation.isPending}
                          className="flex-1 lg:flex-none px-4 py-2 bg-red-600 hover:bg-red-700 text-white rounded-lg transition-colors text-sm"
                        >
                          Cancel Booking
                        </button>
                      </div>
                    )}
                  </div>
                </div>
              ))}
            </div>
          ) : (
            <div className="card p-12 text-center">
              <Calendar className="w-20 h-20 text-primary-300 dark:text-primary-700 mx-auto mb-4" />
              <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-2">
                No Bookings Found
              </h2>
              <p className="text-primary-600 dark:text-sand-300">
                {filter === 'all'
                  ? 'You don\'t have any bookings yet.'
                  : `No ${filter} bookings at the moment.`}
              </p>
            </div>
          )}

          {/* Summary Stats */}
          {bookings.length > 0 && (
            <div className="mt-8 grid grid-cols-2 md:grid-cols-4 gap-4">
              <div className="card p-4">
                <div className="text-sm text-primary-600 dark:text-sand-400 mb-1">Total Bookings</div>
                <div className="text-2xl font-bold text-primary-900 dark:text-sand-50">
                  {bookingsData?.count || bookings.length}
                </div>
              </div>
              <div className="card p-4">
                <div className="text-sm text-primary-600 dark:text-sand-400 mb-1">Confirmed</div>
                <div className="text-2xl font-bold text-green-600">
                  {bookings.filter((b: any) => b.status === 'confirmed').length}
                </div>
              </div>
              <div className="card p-4">
                <div className="text-sm text-primary-600 dark:text-sand-400 mb-1">Pending</div>
                <div className="text-2xl font-bold text-yellow-600">
                  {bookings.filter((b: any) => b.status === 'pending').length}
                </div>
              </div>
              <div className="card p-4">
                <div className="text-sm text-primary-600 dark:text-sand-400 mb-1">Completed</div>
                <div className="text-2xl font-bold text-blue-600">
                  {bookings.filter((b: any) => b.status === 'completed').length}
                </div>
              </div>
            </div>
          )}
        </div>
      </div>
    </ProtectedRoute>
  );
}
