'use client';

import { useState } from 'react';
import { useQuery, useMutation } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { Button } from '@/components/ui';
import { Calendar, MapPin, Image as ImageIcon, Star } from 'lucide-react';
import { useAuth } from '@/store/auth-store';
import { useRouter } from 'next/navigation';
import toast from 'react-hot-toast';
import { ReviewModal } from '@/components/review/review-modal';

export function BookingContent() {
  const { isAuthenticated, user } = useAuth();
  const router = useRouter();
  const [statusFilter, setStatusFilter] = useState('');
  const [contactingHost, setContactingHost] = useState<string | null>(null);
  
  // Review modal state
  const [reviewModalOpen, setReviewModalOpen] = useState(false);
  const [selectedBookingForReview, setSelectedBookingForReview] = useState<{ id: number; title: string } | null>(null);

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

  const bookings = bookingsData?.results || [];

  const contactHostMutation = useMutation({
    mutationFn: async (booking: any) => {
      if (!user) throw new Error('User not authenticated');

      const response = await apiClient.createConversation({
        participants: [booking.property?.host_id || booking.host_id, user.id].map(Number),
        property: Number(booking.property?.id || booking.property_id),
        booking: Number(booking.id),
        subject: `Booking inquiry for ${booking.property?.title || booking.property_title}`,
      });
      return response.data;
    },
    onSuccess: () => {
      toast.success('Conversation started!');
      router.push('/messages');
    },
    onError: (error: any) => {
      const errMsg = typeof error.response?.data?.error === 'string'
        ? error.response.data.error
        : typeof error.response?.data?.detail === 'string'
          ? error.response.data.detail
          : 'Failed to start conversation';
      toast.error(errMsg);
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
      case 'requested':
        return 'bg-amber-100 text-amber-800 border border-amber-200';
      case 'confirmed':
        return 'bg-green-100 text-green-800';
      case 'pending':
        return 'bg-yellow-100 text-yellow-800 border border-yellow-200';
      case 'cancelled':
        return 'bg-red-100 text-red-800';
      case 'completed':
        return 'bg-blue-100 text-blue-800';
      case 'checked_in':
        return 'bg-blue-100 text-blue-800';
      case 'checked_out':
        return 'bg-purple-100 text-purple-800';
      default:
        return 'bg-primary-100 text-primary-800';
    }
  };

  if (!isAuthenticated) {
    return (
      <div className="bg-sand-100 min-h-screen">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
          <div className="card p-12 text-center">
            <h2 className="text-2xl font-bold text-primary-900 mb-4">
              Sign In Required
            </h2>
            <p className="text-primary-600 mb-8">
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

  return (
    <div className="bg-sand-100 min-h-screen">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
        {/* Header */}
        <div className="mb-8">
          <h1 className="text-3xl md:text-4xl font-bold text-primary-900 mb-2">
            My Bookings
          </h1>
          <p className="text-lg text-primary-600">
            View and manage your reservations
          </p>
        </div>

        {/* Status Filters */}
        <div className="card p-4 sm:p-6 mb-6 sm:mb-8">
          <div className="flex flex-wrap gap-2 sm:gap-4">
            <Button
              onClick={() => setStatusFilter('')}
              variant={statusFilter === '' ? 'primary' : 'outline'}
              size="sm"
            >
              All
            </Button>
            <Button
              onClick={() => setStatusFilter('requested')}
              variant={statusFilter === 'requested' ? 'primary' : 'outline'}
              size="sm"
            >
              Requested
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
            <Button
              onClick={() => setStatusFilter('completed')}
              variant={statusFilter === 'completed' ? 'primary' : 'outline'}
              size="sm"
            >
              Completed
            </Button>
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
        {isLoading ? (
          <div className="space-y-4">
            {[1, 2, 3].map((i) => (
              <div key={i} className="card p-6 animate-pulse">
                <div className="h-6 bg-primary-200 rounded mb-4 w-1/2"></div>
                <div className="h-4 bg-primary-200 rounded mb-2"></div>
                <div className="h-4 bg-primary-200 rounded w-3/4"></div>
              </div>
            ))}
          </div>
        ) : error ? (
          <div className="card p-12 text-center">
            <p className="text-primary-600 mb-4">
              Unable to load bookings. Please try again later.
            </p>
            <p className="text-sm text-primary-500">
              Error: {error instanceof Error ? error.message : 'Unknown error'}
            </p>
          </div>
        ) : bookings.length === 0 ? (
          <div className="card p-12 text-center">
            <Calendar className="w-16 h-16 text-primary-400 mx-auto mb-4" />
            <h3 className="text-xl font-semibold text-primary-900 mb-2">
              No Bookings Found
            </h3>
            <p className="text-primary-600 mb-8">
              You haven&apos;t booked any stays yet. Start exploring properties!
            </p>
            <a href="/explore" className="inline-block">
              <Button>Explore Properties</Button>
            </a>
          </div>
        ) : (
          <div className="space-y-4">
            {/* Booking count */}
            <p className="text-sm text-primary-500">
              Showing {bookings.length} {bookings.length === 1 ? 'booking' : 'bookings'}
              {statusFilter ? ` · ${statusFilter}` : ''}
            </p>
            {bookings.map((booking: any) => {
              const img = booking.property?.images?.[0]?.image || booking.property?.images?.[0]?.image_url || booking.property?.main_image_url;
              const checkIn = new Date(booking.check_in);
              const checkOut = new Date(booking.check_out);
              const fmtDate = (d: Date) => d.toLocaleDateString('en-US', { weekday: 'short', month: 'short', day: 'numeric' });
              return (
                <article key={booking.id} className="card overflow-hidden">
                  <div className="flex flex-col sm:flex-row">
                    {/* Property thumbnail */}
                    <div className="sm:w-48 h-40 sm:h-auto flex-shrink-0 bg-primary-200 relative">
                      {img ? (
                        <img src={img} alt={booking.property?.title || ''} className="w-full h-full object-cover" />
                      ) : (
                        <div className="w-full h-full flex items-center justify-center">
                          <ImageIcon className="w-10 h-10 text-primary-400" />
                        </div>
                      )}
                      <span className={`absolute top-2 left-2 px-2.5 py-1 rounded-full text-xs font-semibold ${getStatusColor(booking.status)}`}>
                        {booking.status === 'requested' ? 'Awaiting Host Approval' : booking.status === 'pending' ? 'Awaiting Payment' : booking.status}
                      </span>
                    </div>

                    {/* Card body */}
                    <div className="flex-1 p-4 sm:p-5">
                      <div className="flex flex-col md:flex-row md:items-start justify-between mb-3">
                        <div className="flex-1 mb-3 md:mb-0">
                          <h3 className="text-lg font-semibold text-primary-900 mb-1">
                            {booking.property?.title || booking.property_title || 'Property'}
                          </h3>
                          <div className="flex items-center gap-1.5 text-primary-500 text-sm">
                            <MapPin className="w-3.5 h-3.5" />
                            <span>{booking.property?.city || 'Unknown'}, {booking.property?.country || 'Unknown'}</span>
                          </div>
                        </div>
                        <div className="text-right">
                          <div className="text-xl sm:text-2xl font-bold text-primary-900">
                            ${booking.grand_total}
                          </div>
                          <div className="text-xs text-primary-500">
                            Total Amount
                          </div>
                        </div>
                      </div>

                      <div className="grid grid-cols-2 sm:grid-cols-4 gap-3 py-3 border-t border-primary-200">
                        <div>
                          <div className="text-xs text-primary-500 mb-0.5">Check-in</div>
                          <div className="text-sm font-semibold text-primary-900">
                            {fmtDate(checkIn)}
                          </div>
                        </div>
                        <div>
                          <div className="text-xs text-primary-500 mb-0.5">Check-out</div>
                          <div className="text-sm font-semibold text-primary-900">
                            {fmtDate(checkOut)}
                          </div>
                        </div>
                        <div>
                          <div className="text-xs text-primary-500 mb-0.5">Nights</div>
                          <div className="text-sm font-semibold text-primary-900">
                            {booking.nights}
                          </div>
                        </div>
                        {booking.booking_ref && (
                          <div>
                            <div className="text-xs text-primary-500 mb-0.5">Ref</div>
                            <div className="text-sm font-semibold text-primary-900 font-mono">
                              {booking.booking_ref}
                            </div>
                          </div>
                        )}
                      </div>

                      <div className="flex flex-wrap gap-2 mt-3">
                        <a href={`/bookings/${booking.id}`} className="inline-block">
                          <Button variant="secondary" size="sm">View Details</Button>
                        </a>
                        
                        {/* Leave a Review Button (Completed/Checked out) */}
                        {['completed', 'checked_out', 'COMPLETED', 'CHECKED_OUT'].includes(booking.status) && !booking.review && (
                          <Button 
                            variant="primary" 
                            size="sm"
                            className="bg-secondary-600 hover:bg-secondary-700 text-white"
                            onClick={() => {
                              setSelectedBookingForReview({
                                id: booking.id,
                                title: booking.property?.title || booking.property_title || 'Property'
                              });
                              setReviewModalOpen(true);
                            }}
                          >
                            <Star className="w-4 h-4 mr-1.5" />
                            Leave a Review
                          </Button>
                        )}
                        
                        {/* Pay Now Button */}
                        {['pending', 'confirmed', 'PENDING', 'CONFIRMED'].includes(booking.status) &&
                          booking.payment_status !== 'success' && (
                            <a href={`/booking/payment?bookingId=${booking.id}`} className="inline-block">
                              <Button size="sm">Pay Now</Button>
                            </a>
                          )}
                        {(booking.status === 'CONFIRMED' || booking.status === 'confirmed') && (
                          <>
                            <Button
                              onClick={() => handleContactHost(booking)}
                              disabled={contactingHost === booking.id}
                              variant="outline"
                              size="sm"
                            >
                              {contactingHost === booking.id ? 'Starting...' : 'Contact Host'}
                            </Button>
                            <a href={`/bookings/${booking.id}/directions`} className="inline-block">
                              <Button variant="outline" size="sm">Get Directions</Button>
                            </a>
                          </>
                        )}
                        {(booking.status === 'pending' || booking.status === 'PENDING') && (
                          <Button
                            onClick={() => handleContactHost(booking)}
                            disabled={contactingHost === booking.id}
                            variant="outline"
                            size="sm"
                          >
                            {contactingHost === booking.id ? 'Starting...' : 'Contact Host'}
                          </Button>
                        )}
                      </div>
                    </div>
                  </div>
                </article>
              );
            })}
          </div>
        )}
      </div>

      {/* Review Modal */}
      {selectedBookingForReview && (
        <ReviewModal
          isOpen={reviewModalOpen}
          onClose={() => setReviewModalOpen(false)}
          bookingId={selectedBookingForReview.id}
          propertyTitle={selectedBookingForReview.title}
        />
      )}
    </div>
  );
}

