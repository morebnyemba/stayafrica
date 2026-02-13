'use client';

import { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { useAuth } from '@/store/auth-store';
import Link from 'next/link';
import {
  Calendar,
  Users,
  DollarSign,
  CheckCircle2,
  XCircle,
  Clock,
  Search,
  Compass,
  User,
} from 'lucide-react';
import { Button } from '@/components/ui';
import { toast } from 'react-hot-toast';
import type { ExperienceBooking, ExperienceBookingStatus } from '@/types';

const statusConfig: Record<ExperienceBookingStatus, { label: string; color: string; icon: typeof Clock }> = {
  pending: { label: 'Pending', color: 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900/30 dark:text-yellow-300', icon: Clock },
  confirmed: { label: 'Confirmed', color: 'bg-blue-100 text-blue-800 dark:bg-blue-900/30 dark:text-blue-300', icon: CheckCircle2 },
  completed: { label: 'Completed', color: 'bg-green-100 text-green-800 dark:bg-green-900/30 dark:text-green-300', icon: CheckCircle2 },
  cancelled: { label: 'Cancelled', color: 'bg-red-100 text-red-800 dark:bg-red-900/30 dark:text-red-300', icon: XCircle },
};

const statusFilters: { value: string; label: string }[] = [
  { value: '', label: 'All' },
  { value: 'pending', label: 'Pending' },
  { value: 'confirmed', label: 'Confirmed' },
  { value: 'completed', label: 'Completed' },
  { value: 'cancelled', label: 'Cancelled' },
];

export function HostExperienceBookingsContent() {
  const { isAuthenticated } = useAuth();
  const queryClient = useQueryClient();
  const [statusFilter, setStatusFilter] = useState('');
  const [searchTerm, setSearchTerm] = useState('');

  const { data, isLoading } = useQuery({
    queryKey: ['host-experience-bookings', statusFilter, searchTerm],
    queryFn: async () => {
      const params: Record<string, string> = {};
      if (statusFilter) params.status = statusFilter;
      const response = await apiClient.getHostExperienceBookings(params);
      return response.data;
    },
    enabled: isAuthenticated,
  });

  const confirmMutation = useMutation({
    mutationFn: (bookingId: number) => apiClient.confirmExperienceBooking(bookingId),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['host-experience-bookings'] });
      toast.success('Booking confirmed');
    },
    onError: () => toast.error('Failed to confirm booking'),
  });

  const completeMutation = useMutation({
    mutationFn: (bookingId: number) => apiClient.completeExperienceBooking(bookingId),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['host-experience-bookings'] });
      toast.success('Booking marked as completed');
    },
    onError: () => toast.error('Failed to complete booking'),
  });

  const cancelMutation = useMutation({
    mutationFn: (bookingId: number) => apiClient.cancelExperienceBooking(bookingId),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['host-experience-bookings'] });
      toast.success('Booking cancelled');
    },
    onError: () => toast.error('Failed to cancel booking'),
  });

  const bookings: ExperienceBooking[] = data?.results || data || [];

  // Client-side search filter (by experience title or guest name)
  const filteredBookings = searchTerm
    ? bookings.filter(
        (b) =>
          b.experience_title?.toLowerCase().includes(searchTerm.toLowerCase()) ||
          b.guest_name?.toLowerCase().includes(searchTerm.toLowerCase())
      )
    : bookings;

  // Stats
  const stats = {
    total: bookings.length,
    pending: bookings.filter((b) => b.status === 'pending').length,
    confirmed: bookings.filter((b) => b.status === 'confirmed').length,
    completed: bookings.filter((b) => b.status === 'completed').length,
  };

  return (
    <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Header */}
        <div className="flex flex-col md:flex-row md:items-center md:justify-between gap-4 mb-8">
          <div>
            <h1 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50">
              Experience Bookings
            </h1>
            <p className="text-primary-600 dark:text-sand-300 mt-1">
              Manage bookings for your experiences
            </p>
          </div>
          <Link href="/host/experiences">
            <Button variant="outline">
              <Compass className="w-4 h-4 mr-2" />
              My Experiences
            </Button>
          </Link>
        </div>

        {/* Stats */}
        <div className="grid grid-cols-2 sm:grid-cols-4 gap-4 mb-8">
          <div className="card-gradient p-4 text-center">
            <p className="text-2xl font-bold text-primary-900 dark:text-sand-50">{stats.total}</p>
            <p className="text-sm text-primary-600 dark:text-sand-300">Total</p>
          </div>
          <div className="card-gradient p-4 text-center">
            <p className="text-2xl font-bold text-yellow-600 dark:text-yellow-400">{stats.pending}</p>
            <p className="text-sm text-primary-600 dark:text-sand-300">Pending</p>
          </div>
          <div className="card-gradient p-4 text-center">
            <p className="text-2xl font-bold text-blue-600 dark:text-blue-400">{stats.confirmed}</p>
            <p className="text-sm text-primary-600 dark:text-sand-300">Confirmed</p>
          </div>
          <div className="card-gradient p-4 text-center">
            <p className="text-2xl font-bold text-green-600 dark:text-green-400">{stats.completed}</p>
            <p className="text-sm text-primary-600 dark:text-sand-300">Completed</p>
          </div>
        </div>

        {/* Filters */}
        <div className="flex flex-col sm:flex-row gap-4 mb-6">
          <div className="relative flex-1">
            <Search className="absolute left-3 top-1/2 -translate-y-1/2 w-4 h-4 text-primary-400" />
            <input
              type="text"
              placeholder="Search by experience or guest name..."
              value={searchTerm}
              onChange={(e) => setSearchTerm(e.target.value)}
              className="w-full pl-10 pr-4 py-2.5 rounded-lg border border-primary-200 dark:border-primary-700 bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50 focus:ring-2 focus:ring-secondary-500"
            />
          </div>
          <div className="flex gap-2 flex-wrap">
            {statusFilters.map((sf) => (
              <button
                key={sf.value}
                onClick={() => setStatusFilter(sf.value)}
                className={`px-4 py-2 rounded-full text-sm font-medium transition-colors ${
                  statusFilter === sf.value
                    ? 'bg-secondary-500 text-white'
                    : 'bg-white dark:bg-primary-800 text-primary-700 dark:text-sand-300 border border-primary-200 dark:border-primary-700'
                }`}
              >
                {sf.label}
              </button>
            ))}
          </div>
        </div>

        {/* Bookings List */}
        {isLoading ? (
          <div className="space-y-4">
            {[1, 2, 3].map((i) => (
              <div key={i} className="card-gradient p-6 animate-pulse">
                <div className="flex gap-4">
                  <div className="h-12 w-12 bg-primary-200 dark:bg-primary-700 rounded-full" />
                  <div className="flex-1 space-y-2">
                    <div className="h-5 bg-primary-200 dark:bg-primary-700 rounded w-1/3" />
                    <div className="h-4 bg-primary-200 dark:bg-primary-700 rounded w-2/3" />
                    <div className="h-4 bg-primary-200 dark:bg-primary-700 rounded w-1/4" />
                  </div>
                </div>
              </div>
            ))}
          </div>
        ) : filteredBookings.length === 0 ? (
          <div className="card-gradient p-12 text-center">
            <Calendar className="w-16 h-16 text-primary-300 dark:text-primary-700 mx-auto mb-4" />
            <h3 className="text-lg font-semibold text-primary-900 dark:text-sand-50 mb-2">
              {statusFilter ? 'No bookings match your filter' : 'No experience bookings yet'}
            </h3>
            <p className="text-primary-600 dark:text-sand-300 mb-6">
              {statusFilter
                ? 'Try changing the filter to see bookings.'
                : 'Once guests book your experiences, they will appear here.'}
            </p>
            {!statusFilter && (
              <Link href="/host/experiences">
                <Button variant="primary">View My Experiences</Button>
              </Link>
            )}
          </div>
        ) : (
          <div className="space-y-4">
            {filteredBookings.map((booking) => {
              const config = statusConfig[booking.status] || statusConfig.pending;
              const StatusIcon = config.icon;

              return (
                <div
                  key={booking.id}
                  className="card-gradient p-5 sm:p-6 hover:shadow-lg transition-shadow"
                >
                  <div className="flex flex-col sm:flex-row sm:items-start justify-between gap-4">
                    {/* Booking Info */}
                    <div className="flex-1 space-y-3">
                      <div className="flex items-center gap-3">
                        <span className={`inline-flex items-center gap-1.5 px-3 py-1 text-xs font-semibold rounded-full ${config.color}`}>
                          <StatusIcon className="w-3.5 h-3.5" />
                          {config.label}
                        </span>
                        <span className="text-xs text-primary-500 dark:text-sand-400">
                          #{booking.id}
                        </span>
                      </div>

                      <h3 className="text-lg font-semibold text-primary-900 dark:text-sand-50">
                        {booking.experience_title || `Experience #${booking.experience}`}
                      </h3>

                      <div className="grid grid-cols-1 sm:grid-cols-3 gap-3 text-sm text-primary-600 dark:text-sand-300">
                        <div className="flex items-center gap-2">
                          <User className="w-4 h-4 text-primary-400" />
                          <span>{booking.guest_name || 'Guest'}</span>
                        </div>
                        <div className="flex items-center gap-2">
                          <Calendar className="w-4 h-4 text-primary-400" />
                          <span>{new Date(booking.booking_date).toLocaleDateString()}</span>
                        </div>
                        <div className="flex items-center gap-2">
                          <Users className="w-4 h-4 text-primary-400" />
                          <span>{booking.num_participants} participant(s)</span>
                        </div>
                      </div>

                      <div className="flex items-center gap-2 text-sm font-medium">
                        <DollarSign className="w-4 h-4 text-green-500" />
                        <span className="text-primary-900 dark:text-sand-50">
                          {booking.currency || 'USD'} {booking.total_amount?.toFixed(2) || ((booking.price_per_person || 0) * (booking.num_participants || 1)).toFixed(2)}
                        </span>
                        <span className="text-primary-400 dark:text-sand-500">
                          ({booking.currency || 'USD'} {booking.price_per_person?.toFixed(2)} × {booking.num_participants})
                        </span>
                      </div>

                      {booking.special_requests && (
                        <p className="text-sm text-primary-500 dark:text-sand-400 italic">
                          Note: {booking.special_requests}
                        </p>
                      )}
                    </div>

                    {/* Actions */}
                    <div className="flex flex-wrap gap-2 sm:flex-col sm:items-end">
                      {booking.status === 'pending' && (
                        <>
                          <Button
                            variant="primary"
                            size="sm"
                            onClick={() => confirmMutation.mutate(booking.id)}
                            disabled={confirmMutation.isPending}
                          >
                            <CheckCircle2 className="w-4 h-4 mr-1.5" />
                            Confirm
                          </Button>
                          <Button
                            variant="outline"
                            size="sm"
                            onClick={() => {
                              if (confirm('Are you sure you want to cancel this booking?')) {
                                cancelMutation.mutate(booking.id);
                              }
                            }}
                            disabled={cancelMutation.isPending}
                          >
                            <XCircle className="w-4 h-4 mr-1.5" />
                            Decline
                          </Button>
                        </>
                      )}
                      {booking.status === 'confirmed' && (
                        <>
                          <Button
                            variant="primary"
                            size="sm"
                            onClick={() => completeMutation.mutate(booking.id)}
                            disabled={completeMutation.isPending}
                          >
                            <CheckCircle2 className="w-4 h-4 mr-1.5" />
                            Mark Complete
                          </Button>
                          <Button
                            variant="outline"
                            size="sm"
                            onClick={() => {
                              if (confirm('Are you sure you want to cancel this booking?')) {
                                cancelMutation.mutate(booking.id);
                              }
                            }}
                            disabled={cancelMutation.isPending}
                          >
                            <XCircle className="w-4 h-4 mr-1.5" />
                            Cancel
                          </Button>
                        </>
                      )}
                    </div>
                  </div>
                </div>
              );
            })}
          </div>
        )}
      </div>
    </div>
  );
}
