'use client';

import { useState } from 'react';
import { useQuery, useMutation } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { Button } from '@/components/ui';
import { Calendar, MapPin, Home, Compass, Users, Clock } from 'lucide-react';
import { useAuth } from '@/store/auth-store';
import { useRouter } from 'next/navigation';
import toast from 'react-hot-toast';

type BookingTab = 'properties' | 'experiences';

export function BookingContent() {
  const { isAuthenticated, user } = useAuth();
  const router = useRouter();
  const [activeTab, setActiveTab] = useState<BookingTab>('properties');
  const [statusFilter, setStatusFilter] = useState('');
  const [contactingHost, setContactingHost] = useState<string | null>(null);

  // Property bookings
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

  // Experience bookings
  const { data: expBookingsData, isLoading: expLoading, error: expError } = useQuery({
    queryKey: ['experience-bookings', statusFilter],
    queryFn: async () => {
      const response = await apiClient.getExperienceBookings({
        status: statusFilter || undefined,
      });
      const data = response.data;
      return Array.isArray(data) ? data : data?.results ?? [];
    },
    enabled: isAuthenticated && activeTab === 'experiences',
  });

  const bookings = bookingsData?.results || [];
  const expBookings = expBookingsData || [];

  const contactHostMutation = useMutation({
    mutationFn: async (booking: any) => {
      if (!user) throw new Error('User not authenticated');
      
      const response = await apiClient.createConversation({
        participants: [parseInt(booking.property?.host_id || booking.host_id), parseInt(user.id)],
        property: parseInt(booking.property?.id || booking.property_id),
        booking: parseInt(booking.id),
        subject: `Booking inquiry for ${booking.property?.title || booking.property_title}`,
      });
      return response.data;
    },
    onSuccess: () => {
      toast.success('Conversation started!');
      router.push('/messages');
    },
    onError: (error: any) => {
      toast.error(error.response?.data?.error || 'Failed to start conversation');
    },
    onSettled: () => {
      setContactingHost(null);
    },
  });

  const handleContactHost = (booking: any) => {
    setContactingHost(booking.id);
    contactHostMutation.mutate(booking);
  };

  const getStatusColor = (status: string) => {
    const s = status.toLowerCase();
    switch (s) {
      case 'confirmed':
        return 'bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-200';
      case 'pending':
        return 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-200';
      case 'cancelled':
        return 'bg-red-100 text-red-800 dark:bg-red-900 dark:text-red-200';
      case 'completed':
        return 'bg-blue-100 text-blue-800 dark:bg-blue-900 dark:text-blue-200';
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
            <a href="/login" className="inline-block">
              <Button variant="secondary">Sign In</Button>
            </a>
          </div>
        </div>
      </div>
    );
  }

  const currentLoading = activeTab === 'properties' ? isLoading : expLoading;
  const currentError = activeTab === 'properties' ? error : expError;

  return (
    <div className="bg-sand-100 dark:bg-primary-900 min-h-screen">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
        {/* Header */}
        <div className="mb-8">
          <h1 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-2">
            My Bookings
          </h1>
          <p className="text-lg text-primary-600 dark:text-sand-200">
            View and manage your reservations
          </p>
        </div>

        {/* Tab Toggle */}
        <div className="card p-2 mb-6">
          <div className="flex">
            <button
              onClick={() => { setActiveTab('properties'); setStatusFilter(''); }}
              className={`flex-1 flex items-center justify-center gap-2 px-4 py-3 rounded-lg font-semibold transition-all ${
                activeTab === 'properties'
                  ? 'bg-primary-900 text-sand-50 dark:bg-sand-50 dark:text-primary-900 shadow-md'
                  : 'text-primary-600 dark:text-sand-400 hover:bg-primary-100 dark:hover:bg-primary-800'
              }`}
            >
              <Home className="w-4 h-4" />
              Stays
            </button>
            <button
              onClick={() => { setActiveTab('experiences'); setStatusFilter(''); }}
              className={`flex-1 flex items-center justify-center gap-2 px-4 py-3 rounded-lg font-semibold transition-all ${
                activeTab === 'experiences'
                  ? 'bg-primary-900 text-sand-50 dark:bg-sand-50 dark:text-primary-900 shadow-md'
                  : 'text-primary-600 dark:text-sand-400 hover:bg-primary-100 dark:hover:bg-primary-800'
              }`}
            >
              <Compass className="w-4 h-4" />
              Experiences
            </button>
          </div>
        </div>

        {/* Status Filters */}
        <div className="card p-6 mb-8">
          <div className="flex flex-wrap gap-4">
            <Button
              onClick={() => setStatusFilter('')}
              variant={statusFilter === '' ? 'primary' : 'outline'}
              size="sm"
            >
              All
            </Button>
            <Button
              onClick={() => setStatusFilter('pending')}
              variant={statusFilter === 'pending' ? 'primary' : 'outline'}
              size="sm"
            >
              Pending
            </Button>
            <Button
              onClick={() => setStatusFilter('confirmed')}
              variant={statusFilter === 'confirmed' ? 'primary' : 'outline'}
              size="sm"
            >
              Confirmed
            </Button>
            {activeTab === 'experiences' && (
              <Button
                onClick={() => setStatusFilter('completed')}
                variant={statusFilter === 'completed' ? 'primary' : 'outline'}
                size="sm"
              >
                Completed
              </Button>
            )}
            <Button
              onClick={() => setStatusFilter('cancelled')}
              variant={statusFilter === 'cancelled' ? 'primary' : 'outline'}
              size="sm"
            >
              Cancelled
            </Button>
          </div>
        </div>

        {/* Loading State */}
        {currentLoading ? (
          <div className="space-y-4">
            {[1, 2, 3].map((i) => (
              <div key={i} className="card p-6 animate-pulse">
                <div className="h-6 bg-primary-200 dark:bg-primary-700 rounded mb-4 w-1/2"></div>
                <div className="h-4 bg-primary-200 dark:bg-primary-700 rounded mb-2"></div>
                <div className="h-4 bg-primary-200 dark:bg-primary-700 rounded w-3/4"></div>
              </div>
            ))}
          </div>
        ) : currentError ? (
          <div className="card p-12 text-center">
            <p className="text-primary-600 dark:text-sand-300 mb-4">
              Unable to load bookings. Please try again later.
            </p>
            <p className="text-sm text-primary-500 dark:text-sand-400">
              Error: {currentError instanceof Error ? currentError.message : 'Unknown error'}
            </p>
          </div>
        ) : activeTab === 'properties' ? (
          /* ── Property Bookings ── */
          bookings.length === 0 ? (
            <div className="card p-12 text-center">
              <Calendar className="w-16 h-16 text-primary-400 dark:text-sand-500 mx-auto mb-4" />
              <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-2">
                No Stays Found
              </h3>
              <p className="text-primary-600 dark:text-sand-300 mb-8">
                You haven&apos;t booked any stays yet. Start exploring properties!
              </p>
              <a href="/explore" className="inline-block">
                <Button>Explore Properties</Button>
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
                          <span className="inline-flex items-center gap-1 text-xs font-medium text-primary-500 dark:text-sand-400 bg-primary-100 dark:bg-primary-800 px-2 py-0.5 rounded">
                            <Home className="w-3 h-3" /> Stay
                          </span>
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

                    <div className="flex flex-wrap gap-3 mt-4">
                      <a href={`/bookings/${booking.id}`} className="inline-block">
                        <Button variant="secondary" size="sm">View Details</Button>
                      </a>
                      {(booking.status === 'CONFIRMED' || booking.status === 'confirmed') && (
                        <>
                          <Button
                            onClick={() => handleContactHost(booking)}
                            disabled={contactingHost === booking.id}
                            size="sm"
                          >
                            {contactingHost === booking.id ? 'Starting...' : 'Contact Host'}
                          </Button>
                          <a href={`/bookings/${booking.id}/directions`} className="inline-block">
                            <Button variant="outline" size="sm">Get Directions</Button>
                          </a>
                        </>
                      )}
                    </div>
                  </div>
                </article>
              ))}
            </div>
          )
        ) : (
          /* ── Experience Bookings ── */
          expBookings.length === 0 ? (
            <div className="card p-12 text-center">
              <Compass className="w-16 h-16 text-primary-400 dark:text-sand-500 mx-auto mb-4" />
              <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-2">
                No Experiences Found
              </h3>
              <p className="text-primary-600 dark:text-sand-300 mb-8">
                You haven&apos;t booked any experiences yet. Discover exciting activities!
              </p>
              <a href="/experiences" className="inline-block">
                <Button>Explore Experiences</Button>
              </a>
            </div>
          ) : (
            <div className="space-y-6">
              {expBookings.map((booking: any) => (
                <article key={booking.id} className="card overflow-hidden">
                  <div className="p-6">
                    <div className="flex flex-col md:flex-row md:items-start justify-between mb-4">
                      <div className="flex-1 mb-4 md:mb-0">
                        <div className="flex items-center gap-3 mb-2">
                          <span className="inline-flex items-center gap-1 text-xs font-medium text-amber-700 dark:text-amber-300 bg-amber-50 dark:bg-amber-900/30 px-2 py-0.5 rounded">
                            <Compass className="w-3 h-3" /> Experience
                          </span>
                          <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50">
                            {booking.experience_title || booking.experience?.title || 'Experience'}
                          </h3>
                          <span className={`px-3 py-1 rounded-full text-xs font-semibold ${getStatusColor(booking.status)}`}>
                            {booking.status}
                          </span>
                        </div>
                      </div>
                      <div className="text-right">
                        <div className="text-2xl font-bold text-primary-900 dark:text-sand-50">
                          ${booking.total_amount}
                        </div>
                        <div className="text-sm text-primary-600 dark:text-sand-300">
                          Total Amount
                        </div>
                      </div>
                    </div>

                    <div className="grid md:grid-cols-3 gap-4 py-4 border-t border-primary-200 dark:border-primary-700">
                      <div>
                        <div className="text-sm text-primary-600 dark:text-sand-400 mb-1 flex items-center gap-1">
                          <Calendar className="w-3.5 h-3.5" /> Date
                        </div>
                        <div className="font-semibold text-primary-900 dark:text-sand-50">
                          {new Date(booking.booking_date).toLocaleDateString()}
                        </div>
                      </div>
                      {booking.booking_time && (
                        <div>
                          <div className="text-sm text-primary-600 dark:text-sand-400 mb-1 flex items-center gap-1">
                            <Clock className="w-3.5 h-3.5" /> Time
                          </div>
                          <div className="font-semibold text-primary-900 dark:text-sand-50">
                            {booking.booking_time}
                          </div>
                        </div>
                      )}
                      <div>
                        <div className="text-sm text-primary-600 dark:text-sand-400 mb-1 flex items-center gap-1">
                          <Users className="w-3.5 h-3.5" /> Participants
                        </div>
                        <div className="font-semibold text-primary-900 dark:text-sand-50">
                          {booking.num_participants}
                        </div>
                      </div>
                    </div>

                    <div className="flex flex-wrap gap-3 mt-4">
                      {(booking.status === 'confirmed' || booking.status === 'CONFIRMED') && (
                        <Button variant="secondary" size="sm">Contact Host</Button>
                      )}
                    </div>
                  </div>
                </article>
              ))}
            </div>
          )
        )}
      </div>
    </div>
  );
}
