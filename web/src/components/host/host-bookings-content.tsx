'use client';

import { useAuth } from '@/store/auth-store';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import {
  Calendar, MapPin, CheckCircle, XCircle, Clock,
  List, Building2, Moon, Search, MessageSquare,
  ArrowRight, TrendingUp, Eye,
} from 'lucide-react';
import { Button } from '@/components/ui';
import dynamic from 'next/dynamic';
const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });
import { useState, useMemo } from 'react';
import Link from 'next/link';
import Image from 'next/image';

const formatDate = (dateStr: string) => {
  const d = new Date(dateStr);
  return d.toLocaleDateString('en-US', { weekday: 'short', month: 'short', day: 'numeric' });
};

const getNights = (checkIn: string, checkOut: string) =>
  Math.ceil((new Date(checkOut).getTime() - new Date(checkIn).getTime()) / (1000 * 60 * 60 * 24));

const getInitials = (firstName?: string, lastName?: string) =>
  `${(firstName || '?')[0]}${(lastName || '')[0] || ''}`.toUpperCase();

export function HostBookingsContent() {
  const { user, isAuthenticated } = useAuth();
  const queryClient = useQueryClient();
  const [filter, setFilter] = useState<string>('all');
  const [viewMode, setViewMode] = useState<'list' | 'calendar'>('list');
  const [searchTerm, setSearchTerm] = useState('');

  const { data: bookingsData, isLoading } = useQuery({
    queryKey: ['host', 'bookings', filter],
    queryFn: async () => {
      const params = filter !== 'all' ? { status: filter } : {};
      const response = await apiClient.getHostBookings(params);
      return response.data;
    },
    enabled: isAuthenticated && user?.role === 'host',
  });

  const confirmMutation = useMutation({
    mutationFn: (bookingId: string) => apiClient.confirmBooking(bookingId),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['host', 'bookings'] });
      queryClient.invalidateQueries({ queryKey: ['host', 'analytics'] });
    },
  });

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

  const allBookings = bookingsData?.results || [];

  // Client-side search
  const bookings = useMemo(() => {
    if (!searchTerm) return allBookings;
    const q = searchTerm.toLowerCase();
    return allBookings.filter((b: any) =>
      b.property?.title?.toLowerCase().includes(q) ||
      b.guest?.first_name?.toLowerCase().includes(q) ||
      b.guest?.last_name?.toLowerCase().includes(q) ||
      b.booking_ref?.toLowerCase().includes(q)
    );
  }, [allBookings, searchTerm]);

  const stats = useMemo(() => ({
    total: allBookings.length,
    pending: allBookings.filter((b: any) => b.status === 'pending').length,
    confirmed: allBookings.filter((b: any) => b.status === 'confirmed').length,
    completed: allBookings.filter((b: any) => b.status === 'completed').length,
    totalEarnings: allBookings.reduce((sum: number, b: any) =>
      sum + (parseFloat(b.nightly_total || 0) + parseFloat(b.cleaning_fee || 0) - parseFloat(b.commission_fee || 0)), 0),
  }), [allBookings]);

  const statusConfig: Record<string, { color: string; bg: string; icon: any; dotColor: string }> = {
    pending: { color: 'text-yellow-700 dark:text-yellow-300', bg: 'bg-yellow-50 dark:bg-yellow-900/20', icon: Clock, dotColor: 'bg-yellow-500' },
    confirmed: { color: 'text-green-700 dark:text-green-300', bg: 'bg-green-50 dark:bg-green-900/20', icon: CheckCircle, dotColor: 'bg-green-500' },
    cancelled: { color: 'text-red-700 dark:text-red-300', bg: 'bg-red-50 dark:bg-red-900/20', icon: XCircle, dotColor: 'bg-red-500' },
    completed: { color: 'text-blue-700 dark:text-blue-300', bg: 'bg-blue-50 dark:bg-blue-900/20', icon: CheckCircle, dotColor: 'bg-blue-500' },
  };

  const filterCounts: Record<string, number> = {
    all: stats.total,
    pending: stats.pending,
    confirmed: stats.confirmed,
    completed: stats.completed,
    cancelled: allBookings.filter((b: any) => b.status === 'cancelled').length,
  };

  const getPropertyImage = (booking: any) =>
    booking.property?.images?.[0]?.image || booking.property?.images?.[0]?.image_url || booking.property?.main_image_url || booking.property?.main_image || null;

  return (
    <ProtectedRoute>
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          {/* Header */}
          <header className="mb-8">
            <div className="flex flex-col sm:flex-row sm:items-center sm:justify-between gap-4">
              <div>
                <h1 className="text-2xl sm:text-3xl font-bold text-primary-900 dark:text-sand-50">
                  Manage Bookings
                </h1>
                <p className="text-primary-600 dark:text-sand-300 mt-1">
                  {stats.total} total booking{stats.total !== 1 ? 's' : ''} · ${stats.totalEarnings.toFixed(0)} earned
                </p>
              </div>
              
              <div className="flex items-center gap-2">
                {/* View Toggle */}
                <div className="inline-flex rounded-lg border border-primary-200 dark:border-primary-700 bg-white dark:bg-primary-800 p-1">
                  <button
                    onClick={() => setViewMode('list')}
                    className={`px-3 py-1.5 rounded-md text-sm font-medium transition-colors flex items-center gap-1.5 ${
                      viewMode === 'list' ? 'bg-secondary-500 text-white' : 'text-primary-600 dark:text-sand-300'
                    }`}
                  >
                    <List className="w-4 h-4" />
                    <span className="hidden sm:inline">List</span>
                  </button>
                  <button
                    onClick={() => setViewMode('calendar')}
                    className={`px-3 py-1.5 rounded-md text-sm font-medium transition-colors flex items-center gap-1.5 ${
                      viewMode === 'calendar' ? 'bg-secondary-500 text-white' : 'text-primary-600 dark:text-sand-300'
                    }`}
                  >
                    <Calendar className="w-4 h-4" />
                    <span className="hidden sm:inline">Calendar</span>
                  </button>
                </div>
              </div>
            </div>
          </header>

          {/* Stats Row */}
          <div className="grid grid-cols-2 sm:grid-cols-4 gap-3 mb-6">
            {[
              { label: 'Pending', value: stats.pending, icon: Clock, color: 'text-yellow-600 dark:text-yellow-400', bg: 'bg-yellow-50 dark:bg-yellow-900/20' },
              { label: 'Confirmed', value: stats.confirmed, icon: CheckCircle, color: 'text-green-600 dark:text-green-400', bg: 'bg-green-50 dark:bg-green-900/20' },
              { label: 'Completed', value: stats.completed, icon: CheckCircle, color: 'text-blue-600 dark:text-blue-400', bg: 'bg-blue-50 dark:bg-blue-900/20' },
              { label: 'Revenue', value: `$${stats.totalEarnings.toFixed(0)}`, icon: TrendingUp, color: 'text-secondary-600 dark:text-secondary-400', bg: 'bg-secondary-50 dark:bg-secondary-900/20' },
            ].map((stat) => (
              <div key={stat.label} className="card p-4 flex items-center gap-3">
                <div className={`p-2 rounded-lg ${stat.bg}`}>
                  <stat.icon className={`w-5 h-5 ${stat.color}`} />
                </div>
                <div>
                  <p className="text-lg sm:text-xl font-bold text-primary-900 dark:text-sand-50">{stat.value}</p>
                  <p className="text-xs text-primary-500 dark:text-sand-400">{stat.label}</p>
                </div>
              </div>
            ))}
          </div>

          {/* Search + Filters */}
          <div className="flex flex-col sm:flex-row gap-3 mb-6">
            <div className="relative flex-1">
              <Search className="absolute left-3 top-1/2 -translate-y-1/2 w-4 h-4 text-primary-400" />
              <input
                type="text"
                placeholder="Search by property, guest, or ref..."
                value={searchTerm}
                onChange={(e) => setSearchTerm(e.target.value)}
                className="w-full pl-10 pr-4 py-2.5 rounded-lg border border-primary-200 dark:border-primary-700 bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50 text-sm focus:ring-2 focus:ring-secondary-500 focus:border-transparent"
              />
            </div>
            <div className="flex gap-2 overflow-x-auto pb-1">
              {['all', 'pending', 'confirmed', 'completed', 'cancelled'].map((status) => (
                <button
                  key={status}
                  onClick={() => setFilter(status)}
                  className={`px-3 py-2 rounded-full text-sm font-medium whitespace-nowrap transition-colors ${
                    filter === status
                      ? 'bg-secondary-500 text-white'
                      : 'bg-white dark:bg-primary-800 text-primary-600 dark:text-sand-300 border border-primary-200 dark:border-primary-700 hover:border-secondary-300'
                  }`}
                >
                  {status.charAt(0).toUpperCase() + status.slice(1)}
                  <span className="ml-1.5 text-xs opacity-70">{filterCounts[status]}</span>
                </button>
              ))}
            </div>
          </div>

          {/* Calendar View */}
          {viewMode === 'calendar' && (
            <section className="card p-4 sm:p-6 mb-8">
              <h2 className="text-lg font-bold text-primary-900 dark:text-sand-50 mb-4">
                Bookings Calendar
              </h2>
              <div className="grid grid-cols-7 gap-1 sm:gap-2 mb-3">
                {['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'].map((day) => (
                  <div key={day} className="text-center text-xs font-semibold text-primary-500 dark:text-sand-400 py-2">
                    {day}
                  </div>
                ))}
              </div>
              <div className="grid grid-cols-7 gap-1 sm:gap-2">
                {Array.from({ length: 35 }).map((_, i) => {
                  const date = new Date();
                  date.setDate(date.getDate() - date.getDay() + i);
                  const dayBookings = bookings.filter((b: any) => {
                    const checkIn = new Date(b.check_in);
                    const checkOut = new Date(b.check_out);
                    return date >= checkIn && date <= checkOut;
                  });
                  const isToday = date.toDateString() === new Date().toDateString();
                  
                  return (
                    <div
                      key={i}
                      className={`aspect-square rounded-lg text-xs sm:text-sm font-medium transition-colors flex flex-col items-center justify-center gap-0.5 ${
                        isToday
                          ? 'bg-secondary-500 text-white ring-2 ring-secondary-300'
                          : dayBookings.length > 0
                          ? 'bg-green-50 dark:bg-green-900/20 text-green-700 dark:text-green-300 border border-green-200 dark:border-green-800'
                          : 'bg-white dark:bg-primary-800 text-primary-600 dark:text-sand-300'
                      }`}
                    >
                      {date.getDate()}
                      {dayBookings.length > 0 && (
                        <span className="text-[10px] leading-none">{dayBookings.length}b</span>
                      )}
                    </div>
                  );
                })}
              </div>
              <div className="mt-4 flex flex-wrap items-center gap-4 text-xs text-primary-500 dark:text-sand-400">
                <div className="flex items-center gap-1.5">
                  <div className="w-3 h-3 rounded bg-secondary-500" />
                  Today
                </div>
                <div className="flex items-center gap-1.5">
                  <div className="w-3 h-3 rounded bg-green-100 dark:bg-green-900/30 border border-green-300 dark:border-green-700" />
                  Booked
                </div>
              </div>
            </section>
          )}

          {/* Bookings List */}
          {viewMode === 'list' && (
            <>
              {isLoading ? (
                <div className="space-y-4">
                  {[1, 2, 3].map((i) => (
                    <div key={i} className="card overflow-hidden animate-pulse">
                      <div className="flex">
                        <div className="hidden sm:block w-32 h-32 bg-primary-200 dark:bg-primary-700" />
                        <div className="flex-1 p-5 space-y-3">
                          <div className="h-5 w-2/5 bg-primary-200 dark:bg-primary-700 rounded" />
                          <div className="h-4 w-3/5 bg-primary-200 dark:bg-primary-700 rounded" />
                          <div className="h-4 w-1/4 bg-primary-200 dark:bg-primary-700 rounded" />
                        </div>
                      </div>
                    </div>
                  ))}
                </div>
              ) : bookings.length > 0 ? (
                <div className="space-y-3">
                  {bookings.map((booking: any) => {
                    const sc = statusConfig[booking.status] || statusConfig.pending;
                    const propertyImage = getPropertyImage(booking);
                    const nights = getNights(booking.check_in, booking.check_out);
                    const earnings = (
                      parseFloat(booking.nightly_total || 0) +
                      parseFloat(booking.cleaning_fee || 0) -
                      parseFloat(booking.commission_fee || 0)
                    );

                    return (
                      <article
                        key={booking.id}
                        className="card overflow-hidden hover:shadow-lg transition-all group"
                      >
                        <div className="flex flex-col sm:flex-row">
                          {/* Property Thumbnail */}
                          <div className="relative sm:w-40 lg:w-48 h-40 sm:h-auto bg-primary-100 dark:bg-primary-800 flex-shrink-0">
                            {propertyImage ? (
                              <Image
                                src={propertyImage}
                                alt={booking.property?.title || 'Property'}
                                fill
                                className="object-cover"
                                sizes="(max-width: 640px) 100vw, 192px"
                              />
                            ) : (
                              <div className="w-full h-full flex items-center justify-center min-h-[120px]">
                                <Building2 className="w-10 h-10 text-primary-300 dark:text-primary-600" />
                              </div>
                            )}
                            {/* Status overlay on image */}
                            <div className="absolute top-2 left-2">
                              <span className={`inline-flex items-center gap-1 px-2 py-0.5 text-xs font-semibold rounded-full ${sc.bg} ${sc.color} backdrop-blur-sm`}>
                                <span className={`w-1.5 h-1.5 rounded-full ${sc.dotColor}`} />
                                {booking.status.charAt(0).toUpperCase() + booking.status.slice(1)}
                              </span>
                            </div>
                            {/* Nights badge */}
                            <div className="absolute bottom-2 right-2 bg-black/60 text-white text-xs px-2 py-0.5 rounded-full flex items-center gap-1">
                              <Moon className="w-3 h-3" />
                              {nights} night{nights !== 1 ? 's' : ''}
                            </div>
                          </div>

                          {/* Content */}
                          <div className="flex-1 p-4 sm:p-5 flex flex-col">
                            <div className="flex items-start justify-between gap-3 mb-3">
                              <div className="min-w-0">
                                <h3 className="text-base font-bold text-primary-900 dark:text-sand-50 truncate group-hover:text-secondary-600 transition-colors">
                                  {booking.property?.title || 'Property'}
                                </h3>
                                <p className="text-xs text-primary-500 dark:text-sand-400 flex items-center gap-1 mt-0.5">
                                  <MapPin className="w-3 h-3" />
                                  {booking.property?.city}, {booking.property?.country}
                                </p>
                              </div>
                              <p className="text-lg font-bold text-green-600 dark:text-green-400 whitespace-nowrap">
                                ${earnings.toFixed(2)}
                              </p>
                            </div>

                            {/* Guest + Dates row */}
                            <div className="flex flex-wrap items-center gap-4 text-sm mb-3">
                              {/* Guest avatar */}
                              <div className="flex items-center gap-2">
                                <div className="w-7 h-7 rounded-full bg-primary-200 dark:bg-primary-700 flex items-center justify-center text-xs font-bold text-primary-600 dark:text-sand-300">
                                  {getInitials(booking.guest?.first_name, booking.guest?.last_name)}
                                </div>
                                <span className="text-primary-900 dark:text-sand-100 font-medium">
                                  {booking.guest?.first_name} {booking.guest?.last_name}
                                </span>
                              </div>
                              <span className="text-primary-300 dark:text-primary-600">|</span>
                              <span className="text-primary-600 dark:text-sand-300">
                                {formatDate(booking.check_in)} → {formatDate(booking.check_out)}
                              </span>
                            </div>

                            {/* Bottom row: ref + actions */}
                            <div className="flex items-center justify-between mt-auto pt-3 border-t border-primary-100 dark:border-primary-700/50">
                              <div className="flex items-center gap-3 text-xs text-primary-500 dark:text-sand-400">
                                <span className="font-mono bg-primary-50 dark:bg-primary-800 px-2 py-0.5 rounded">
                                  {booking.booking_ref}
                                </span>
                                {booking.guest?.email && (
                                  <Link
                                    href={`/host/messages?guest=${booking.guest?.id}`}
                                    className="flex items-center gap-1 hover:text-secondary-600 transition-colors"
                                  >
                                    <MessageSquare className="w-3.5 h-3.5" />
                                    Message
                                  </Link>
                                )}
                              </div>

                              {/* Action buttons */}
                              <div className="flex gap-2">
                                {booking.status === 'pending' && (
                                  <>
                                    <Button
                                      onClick={() => handleConfirm(booking.id)}
                                      disabled={confirmMutation.isPending}
                                      variant="primary"
                                      size="sm"
                                      className="text-xs px-3"
                                    >
                                      <CheckCircle className="w-3.5 h-3.5 mr-1" />
                                      Confirm
                                    </Button>
                                    <Button
                                      onClick={() => handleCancel(booking.id)}
                                      disabled={cancelMutation.isPending}
                                      variant="danger"
                                      size="sm"
                                      className="text-xs px-3"
                                    >
                                      <XCircle className="w-3.5 h-3.5 mr-1" />
                                      Decline
                                    </Button>
                                  </>
                                )}
                                {booking.status === 'confirmed' && (
                                  <Button
                                    onClick={() => handleCancel(booking.id)}
                                    disabled={cancelMutation.isPending}
                                    variant="danger"
                                    size="sm"
                                    className="text-xs px-3"
                                  >
                                    Cancel
                                  </Button>
                                )}
                                <Link href={`/property/${booking.property?.id || booking.rental_property}`}>
                                  <Button variant="secondary" size="sm" className="text-xs px-3">
                                    <Eye className="w-3.5 h-3.5 mr-1" />
                                    View
                                  </Button>
                                </Link>
                              </div>
                            </div>
                          </div>
                        </div>
                      </article>
                    );
                  })}
                </div>
              ) : (
                <div className="card p-12 text-center">
                  <div className="w-20 h-20 rounded-full bg-primary-100 dark:bg-primary-800 mx-auto mb-5 flex items-center justify-center">
                    <Calendar className="w-10 h-10 text-primary-300 dark:text-primary-600" />
                  </div>
                  <h2 className="text-xl font-bold text-primary-900 dark:text-sand-50 mb-2">
                    No Bookings Found
                  </h2>
                  <p className="text-primary-600 dark:text-sand-300 max-w-sm mx-auto">
                    {filter === 'all'
                      ? 'When guests book your properties, their reservations will appear here.'
                      : `No ${filter} bookings at the moment. Try a different filter.`}
                  </p>
                  {filter !== 'all' && (
                    <button
                      onClick={() => setFilter('all')}
                      className="mt-4 text-sm text-secondary-600 hover:text-secondary-700 font-medium inline-flex items-center gap-1"
                    >
                      View all bookings <ArrowRight className="w-4 h-4" />
                    </button>
                  )}
                </div>
              )}
            </>
          )}
        </div>
      </div>
    </ProtectedRoute>
  );
}
