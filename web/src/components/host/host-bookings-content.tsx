'use client';

import { useAuth } from '@/store/auth-store';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { Calendar, MapPin, User, CheckCircle, XCircle, Clock, DollarSign, List } from 'lucide-react';
import dynamic from 'next/dynamic';
const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });
import { useState } from 'react';

export function HostBookingsContent() {
  const { user, isAuthenticated } = useAuth();
  const queryClient = useQueryClient();
  const [filter, setFilter] = useState<string>('all');
  const [viewMode, setViewMode] = useState<'list' | 'calendar'>('list');

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
          <header className="mb-8">
            <div className="flex flex-col sm:flex-row sm:items-center sm:justify-between gap-4 mb-4">
              <div>
                <h1 className="text-2xl sm:text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-2">
                  Manage Bookings
                </h1>
                <p className="text-base sm:text-lg text-primary-600 dark:text-sand-300">
                  View and manage reservations for your properties
                </p>
              </div>
              
              {/* View Mode Toggle */}
              <div className="inline-flex rounded-lg border border-primary-200 dark:border-primary-700 bg-white dark:bg-primary-800 p-1">
                <button
                  onClick={() => setViewMode('list')}
                  className={`px-3 sm:px-4 py-2 rounded-md font-medium transition-colors flex items-center gap-2 ${
                    viewMode === 'list'
                      ? 'bg-secondary-500 text-white'
                      : 'text-primary-700 dark:text-sand-300 hover:bg-primary-50 dark:hover:bg-primary-700'
                  }`}
                  aria-label="List view"
                  aria-pressed={viewMode === 'list'}
                >
                  <List className="w-4 h-4" />
                  <span className="hidden sm:inline">List</span>
                </button>
                <button
                  onClick={() => setViewMode('calendar')}
                  className={`px-3 sm:px-4 py-2 rounded-md font-medium transition-colors flex items-center gap-2 ${
                    viewMode === 'calendar'
                      ? 'bg-secondary-500 text-white'
                      : 'text-primary-700 dark:text-sand-300 hover:bg-primary-50 dark:hover:bg-primary-700'
                  }`}
                  aria-label="Calendar view"
                  aria-pressed={viewMode === 'calendar'}
                >
                  <Calendar className="w-4 h-4" />
                  <span className="hidden sm:inline">Calendar</span>
                </button>
              </div>
            </div>
          </header>

          {/* Filter Tabs */}
          <nav aria-label="Booking status filter" className="mb-6">
            <div className="flex gap-2 overflow-x-auto pb-2">
              {['all', 'pending', 'confirmed', 'completed', 'cancelled'].map((status) => (
                <button
                  key={status}
                  onClick={() => setFilter(status)}
                  className={`px-4 py-2 rounded-lg font-medium whitespace-nowrap transition-colors min-h-[44px] ${
                    filter === status
                      ? 'bg-secondary-500 text-white'
                      : 'bg-white dark:bg-primary-800 text-primary-700 dark:text-sand-300 hover:bg-secondary-50 dark:hover:bg-secondary-900/10'
                  }`}
                  aria-label={`Filter by ${status} bookings`}
                  aria-pressed={filter === status}
                >
                  {status.charAt(0).toUpperCase() + status.slice(1)}
                </button>
              ))}
            </div>
          </nav>

          {/* Calendar View */}
          {viewMode === 'calendar' && (
            <section aria-labelledby="calendar-view-heading" className="card p-4 sm:p-6 mb-8">
              <h2 id="calendar-view-heading" className="text-xl sm:text-2xl font-bold text-primary-900 dark:text-sand-50 mb-6">
                Bookings Calendar
              </h2>
              <div className="grid grid-cols-7 gap-2 mb-4">
                {['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'].map((day) => (
                  <div key={day} className="text-center text-xs sm:text-sm font-semibold text-primary-600 dark:text-sand-400 py-2">
                    {day}
                  </div>
                ))}
              </div>
              <div className="grid grid-cols-7 gap-2">
                {Array.from({ length: 35 }).map((_, i) => {
                  const date = new Date();
                  date.setDate(date.getDate() - date.getDay() + i);
                  const hasBooking = bookings.some((b: any) => {
                    const checkIn = new Date(b.check_in);
                    const checkOut = new Date(b.check_out);
                    return date >= checkIn && date <= checkOut;
                  });
                  const isToday = date.toDateString() === new Date().toDateString();
                  
                  return (
                    <button
                      key={i}
                      className={`aspect-square rounded-lg text-xs sm:text-sm font-medium transition-colors ${
                        isToday
                          ? 'bg-secondary-500 text-white'
                          : hasBooking
                          ? 'bg-green-100 dark:bg-green-900/30 text-green-800 dark:text-green-300'
                          : 'bg-white dark:bg-primary-800 text-primary-700 dark:text-sand-300 hover:bg-primary-50 dark:hover:bg-primary-700'
                      }`}
                      aria-label={`${date.toLocaleDateString()} ${hasBooking ? '- has booking' : ''}`}
                    >
                      {date.getDate()}
                    </button>
                  );
                })}
              </div>
              <div className="mt-6 flex flex-wrap items-center gap-4 text-sm">
                <div className="flex items-center gap-2">
                  <div className="w-4 h-4 rounded bg-secondary-500"></div>
                  <span className="text-primary-600 dark:text-sand-300">Today</span>
                </div>
                <div className="flex items-center gap-2">
                  <div className="w-4 h-4 rounded bg-green-100 dark:bg-green-900/30 border border-green-300 dark:border-green-700"></div>
                  <span className="text-primary-600 dark:text-sand-300">Booked</span>
                </div>
                <div className="flex items-center gap-2">
                  <div className="w-4 h-4 rounded bg-white dark:bg-primary-800 border border-primary-200 dark:border-primary-700"></div>
                  <span className="text-primary-600 dark:text-sand-300">Available</span>
                </div>
              </div>
            </section>
          )}

          {/* Bookings List */}
          {viewMode === 'list' && (
            <>
              {isLoading ? (
                <div className="space-y-4" aria-busy="true" aria-live="polite">
                  {[1, 2, 3].map((i) => (
                    <div key={i} className="card p-6 animate-pulse">
                      <div className="h-24 bg-primary-200 dark:bg-primary-700 rounded"></div>
                    </div>
                  ))}
                </div>
              ) : bookings.length > 0 ? (
                <div className="space-y-4">
                  {bookings.map((booking: any) => (
                    <article 
                      key={booking.id} 
                      className="card p-4 sm:p-6 hover:shadow-lg transition-shadow"
                      aria-label={`Booking for ${booking.property?.title || 'Property'}`}
                    >
                      <div className="flex flex-col lg:flex-row lg:items-start lg:justify-between gap-4">
                        {/* Booking Info */}
                        <div className="flex-1 min-w-0">
                          <div className="flex flex-col sm:flex-row sm:items-start sm:justify-between gap-3 mb-4">
                            <div className="flex-1 min-w-0">
                              <h3 className="text-base sm:text-lg font-bold text-primary-900 dark:text-sand-50 mb-2 truncate">
                                {booking.property?.title || 'Property'}
                              </h3>
                              <p className="text-xs sm:text-sm text-primary-600 dark:text-sand-300 flex items-center gap-1">
                                <MapPin className="w-3 h-3 sm:w-4 sm:h-4 flex-shrink-0" aria-hidden="true" />
                                <span className="truncate">{booking.property?.city}, {booking.property?.country}</span>
                              </p>
                            </div>
                            {getStatusBadge(booking.status)}
                          </div>

                          <dl className="grid grid-cols-2 lg:grid-cols-4 gap-3 sm:gap-4 text-xs sm:text-sm">
                            <div className="space-y-1">
                              <dt className="text-primary-600 dark:text-sand-400">Guest</dt>
                              <dd className="font-medium text-primary-900 dark:text-sand-50 flex items-center gap-1">
                                <User className="w-3 h-3 sm:w-4 sm:h-4 flex-shrink-0" aria-hidden="true" />
                                <span className="truncate">{booking.guest?.first_name} {booking.guest?.last_name}</span>
                              </dd>
                            </div>

                            <div className="space-y-1">
                              <dt className="text-primary-600 dark:text-sand-400">Check-in</dt>
                              <dd className="font-medium text-primary-900 dark:text-sand-50 flex items-center gap-1">
                                <Calendar className="w-3 h-3 sm:w-4 sm:h-4 flex-shrink-0" aria-hidden="true" />
                                <time dateTime={booking.check_in}>{new Date(booking.check_in).toLocaleDateString()}</time>
                              </dd>
                            </div>

                            <div className="space-y-1">
                              <dt className="text-primary-600 dark:text-sand-400">Check-out</dt>
                              <dd className="font-medium text-primary-900 dark:text-sand-50 flex items-center gap-1">
                                <Calendar className="w-3 h-3 sm:w-4 sm:h-4 flex-shrink-0" aria-hidden="true" />
                                <time dateTime={booking.check_out}>{new Date(booking.check_out).toLocaleDateString()}</time>
                              </dd>
                            </div>

                            <div className="space-y-1">
                              <dt className="text-primary-600 dark:text-sand-400">Your Earnings</dt>
                              <dd className="font-medium text-green-600 dark:text-green-400 flex items-center gap-1">
                                <DollarSign className="w-3 h-3 sm:w-4 sm:h-4 flex-shrink-0" aria-hidden="true" />
                                <span>
                                  ${(
                                    parseFloat(booking.nightly_total || 0) +
                                    parseFloat(booking.cleaning_fee || 0) -
                                    parseFloat(booking.commission_fee || 0)
                                  ).toFixed(2)}
                                </span>
                              </dd>
                            </div>
                          </dl>

                          <div className="mt-3 pt-3 border-t border-primary-200 dark:border-primary-700 text-xs sm:text-sm text-primary-600 dark:text-sand-400 flex flex-wrap gap-2">
                            <span><span className="font-medium">Ref:</span> {booking.booking_ref}</span>
                            <span aria-hidden="true">•</span>
                            <span><span className="font-medium">Nights:</span> {booking.nights || 
                              Math.ceil((new Date(booking.check_out).getTime() - new Date(booking.check_in).getTime()) / (1000 * 60 * 60 * 24))}</span>
                          </div>
                        </div>

                        {/* Actions */}
                        {booking.status === 'pending' && (
                          <div className="flex flex-row gap-2 lg:flex-col mt-4 lg:mt-0">
                            <button
                              onClick={() => handleConfirm(booking.id)}
                              disabled={confirmMutation.isPending}
                              className="flex-1 lg:flex-none btn-primary px-3 sm:px-4 py-2 text-sm min-h-[44px] flex items-center justify-center gap-1 sm:gap-2"
                              aria-label={`Confirm booking for ${booking.property?.title}`}
                            >
                              <CheckCircle className="w-4 h-4" aria-hidden="true" />
                              <span>Confirm</span>
                            </button>
                            <button
                              onClick={() => handleCancel(booking.id)}
                              disabled={cancelMutation.isPending}
                              className="flex-1 lg:flex-none px-3 sm:px-4 py-2 bg-red-600 hover:bg-red-700 disabled:opacity-50 disabled:cursor-not-allowed text-white rounded-lg transition-colors text-sm min-h-[44px] flex items-center justify-center gap-1 sm:gap-2"
                              aria-label={`Decline booking for ${booking.property?.title}`}
                            >
                              <XCircle className="w-4 h-4" aria-hidden="true" />
                              <span>Decline</span>
                            </button>
                          </div>
                        )}

                        {booking.status === 'confirmed' && (
                          <div className="flex gap-2 lg:flex-col mt-4 lg:mt-0">
                            <button
                              onClick={() => handleCancel(booking.id)}
                              disabled={cancelMutation.isPending}
                              className="flex-1 lg:flex-none px-3 sm:px-4 py-2 bg-red-600 hover:bg-red-700 disabled:opacity-50 disabled:cursor-not-allowed text-white rounded-lg transition-colors text-sm min-h-[44px]"
                              aria-label={`Cancel booking for ${booking.property?.title}`}
                            >
                              Cancel Booking
                            </button>
                          </div>
                        )}
                      </div>
                    </article>
                  ))}
                </div>
              ) : (
                <div className="card p-8 sm:p-12 text-center">
                  <Calendar className="w-16 h-16 sm:w-20 sm:h-20 text-primary-300 dark:text-primary-700 mx-auto mb-4" aria-hidden="true" />
                  <h2 className="text-xl sm:text-2xl font-bold text-primary-900 dark:text-sand-50 mb-2">
                    No Bookings Found
                  </h2>
                  <p className="text-sm sm:text-base text-primary-600 dark:text-sand-300">
                    {filter === 'all'
                      ? 'You don\'t have any bookings yet.'
                      : `No ${filter} bookings at the moment.`}
                  </p>
                </div>
              )}
            </>
          )}

          {/* Summary Stats */}
          {bookings.length > 0 && (
            <section aria-labelledby="booking-stats-heading" className="mt-8">
              <h2 id="booking-stats-heading" className="sr-only">Booking Statistics</h2>
              <div className="grid grid-cols-2 md:grid-cols-4 gap-3 sm:gap-4">
                <div className="card p-3 sm:p-4">
                  <div className="text-xs sm:text-sm text-primary-600 dark:text-sand-400 mb-1">Total Bookings</div>
                  <div className="text-xl sm:text-2xl font-bold text-primary-900 dark:text-sand-50">
                    {bookingsData?.count || bookings.length}
                  </div>
                </div>
                <div className="card p-3 sm:p-4">
                  <div className="text-xs sm:text-sm text-primary-600 dark:text-sand-400 mb-1">Confirmed</div>
                  <div className="text-xl sm:text-2xl font-bold text-green-600">
                    {bookings.filter((b: any) => b.status === 'confirmed').length}
                  </div>
                </div>
                <div className="card p-3 sm:p-4">
                  <div className="text-xs sm:text-sm text-primary-600 dark:text-sand-400 mb-1">Pending</div>
                  <div className="text-xl sm:text-2xl font-bold text-yellow-600">
                    {bookings.filter((b: any) => b.status === 'pending').length}
                  </div>
                </div>
                <div className="card p-3 sm:p-4">
                  <div className="text-xs sm:text-sm text-primary-600 dark:text-sand-400 mb-1">Completed</div>
                  <div className="text-xl sm:text-2xl font-bold text-blue-600">
                    {bookings.filter((b: any) => b.status === 'completed').length}
                  </div>
                </div>
              </div>
            </section>
          )}
        </div>
      </div>
    </ProtectedRoute>
  );
}
