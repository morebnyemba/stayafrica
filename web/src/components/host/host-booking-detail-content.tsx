'use client';

import { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import {
    ArrowLeft, Calendar, CheckCircle, Clock, MapPin,
    MessageSquare, User, XCircle, LogIn, LogOut, Loader2
} from 'lucide-react';
import { Button } from '@/components/ui';
import dynamic from 'next/dynamic';
import Link from 'next/link';
import { toast } from 'react-hot-toast';

const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });

const formatDate = (dateStr: string) => {
    const d = new Date(dateStr);
    return d.toLocaleDateString('en-US', { month: 'short', day: 'numeric', year: 'numeric' });
};

const getStatusColor = (status: string) => {
    switch (status.toLowerCase()) {
        case 'confirmed': return { bg: 'bg-green-100', text: 'text-green-800' };
        case 'pending': return { bg: 'bg-yellow-100', text: 'text-yellow-800' };
        case 'cancelled': return { bg: 'bg-red-100', text: 'text-red-800' };
        case 'completed': return { bg: 'bg-blue-100', text: 'text-blue-800' };
        case 'checked_in': return { bg: 'bg-purple-100', text: 'text-purple-800' };
        case 'checked_out': return { bg: 'bg-gray-100', text: 'text-gray-800' };
        default: return { bg: 'bg-gray-100', text: 'text-gray-800' };
    }
};

export function HostBookingDetailContent({ params }: { params: Promise<{ id: string }> }) {
    const router = useRouter();
    const queryClient = useQueryClient();
    const [bookingId, setBookingId] = useState<string | null>(null);

    useEffect(() => {
        async function resolveParams() {
            const resolved = await params;
            setBookingId(resolved.id);
        }
        resolveParams();
    }, [params]);

    const { data: bookingData, isLoading, error } = useQuery({
        queryKey: ['booking', bookingId],
        queryFn: async () => {
            if (!bookingId) return null;
            const res = await apiClient.getBooking(bookingId);
            return res.data;
        },
        enabled: !!bookingId,
    });

    const approveMutation = useMutation({
        mutationFn: (id: string) => apiClient.approveBooking(id),
        onSuccess: () => {
            toast.success('Booking approved');
            queryClient.invalidateQueries({ queryKey: ['booking', bookingId] });
            queryClient.invalidateQueries({ queryKey: ['hostBookings'] });
        },
        onError: (err: any) => toast.error(err.response?.data?.error || 'Failed to approve booking'),
    });

    const cancelMutation = useMutation({
        mutationFn: (id: string) => apiClient.cancelBooking(id),
        onSuccess: () => {
            toast.success('Booking cancelled');
            queryClient.invalidateQueries({ queryKey: ['booking', bookingId] });
            queryClient.invalidateQueries({ queryKey: ['hostBookings'] });
        },
        onError: (err: any) => toast.error(err.response?.data?.error || 'Failed to cancel booking'),
    });

    const checkInMutation = useMutation({
        mutationFn: (id: string) => apiClient.checkInBooking(id),
        onSuccess: () => {
            toast.success('Guest checked in');
            queryClient.invalidateQueries({ queryKey: ['booking', bookingId] });
            queryClient.invalidateQueries({ queryKey: ['hostBookings'] });
        },
        onError: (err: any) => toast.error(err.response?.data?.error || 'Failed to check in'),
    });

    const checkOutMutation = useMutation({
        mutationFn: (id: string) => apiClient.checkOutBooking(id),
        onSuccess: () => {
            toast.success('Guest checked out');
            queryClient.invalidateQueries({ queryKey: ['booking', bookingId] });
            queryClient.invalidateQueries({ queryKey: ['hostBookings'] });
        },
        onError: (err: any) => toast.error(err.response?.data?.error || 'Failed to check out'),
    });

    const completeMutation = useMutation({
        mutationFn: (id: string) => apiClient.completeBooking(id),
        onSuccess: () => {
            toast.success('Booking marked as completed');
            queryClient.invalidateQueries({ queryKey: ['booking', bookingId] });
            queryClient.invalidateQueries({ queryKey: ['hostBookings'] });
        },
        onError: (err: any) => toast.error(err.response?.data?.error || 'Failed to complete booking'),
    });

    if (!bookingId || isLoading) {
        return (
            <ProtectedRoute>
                <div className="min-h-screen bg-sand-50 dark:bg-primary-900 flex items-center justify-center">
                    <Loader2 className="w-8 h-8 animate-spin text-secondary-500" />
                </div>
            </ProtectedRoute>
        );
    }

    if (error || !bookingData) {
        return (
            <ProtectedRoute>
                <div className="min-h-screen bg-sand-50 dark:bg-primary-900 p-8 flex flex-col items-center justify-center text-center">
                    <p className="text-red-500 font-semibold text-lg mb-4">Error loading booking details</p>
                    <Button onClick={() => router.push('/host/bookings')} variant="outline">
                        Return to Bookings
                    </Button>
                </div>
            </ProtectedRoute>
        );
    }

    const booking = bookingData;
    const statusColor = getStatusColor(booking.status);

    // Calculate total host earnings
    const earnings = (
        parseFloat(booking.nightly_total || 0) +
        parseFloat(booking.cleaning_fee || 0) -
        parseFloat(booking.commission_fee || 0)
    );

    return (
        <ProtectedRoute>
            <div className="min-h-screen bg-sand-50 dark:bg-primary-900 pt-24 pb-12">
                <div className="max-w-4xl mx-auto px-4 sm:px-6 lg:px-8">

                    {/* Header */}
                    <div className="mb-8">
                        <Link href="/host/bookings" className="inline-flex items-center text-sm font-medium text-primary-600 hover:text-primary-900 dark:text-sand-400 dark:hover:text-sand-100 transition mb-4">
                            <ArrowLeft className="w-4 h-4 mr-1" />
                            Back to Bookings
                        </Link>
                        <div className="flex flex-col md:flex-row md:items-end justify-between gap-4">
                            <div>
                                <div className="flex items-center gap-3 mb-2">
                                    <h1 className="text-2xl sm:text-3xl font-bold text-primary-900 dark:text-sand-50">
                                        Booking Detail
                                    </h1>
                                    <span className={`px-2.5 py-1 rounded-full text-xs font-semibold ${statusColor.bg} ${statusColor.text}`}>
                                        {booking.status.toUpperCase()}
                                    </span>
                                </div>
                                <p className="text-primary-600 dark:text-sand-400 font-mono text-sm inline-flex items-center gap-2">
                                    Ref: <span className="bg-primary-200 dark:bg-primary-800 px-2 py-0.5 rounded">{booking.booking_ref}</span>
                                </p>
                            </div>

                            {/* Action Buttons */}
                            <div className="flex flex-wrap gap-2">
                                {booking.status === 'requested' && (
                                    <>
                                        <Button
                                            onClick={() => approveMutation.mutate(booking.id)}
                                            disabled={approveMutation.isPending}
                                            variant="primary"
                                        >
                                            <CheckCircle className="w-4 h-4 mr-2" />
                                            Approve
                                        </Button>
                                        <Button
                                            onClick={() => cancelMutation.mutate(booking.id)}
                                            disabled={cancelMutation.isPending}
                                            variant="danger"
                                        >
                                            <XCircle className="w-4 h-4 mr-2" />
                                            Decline
                                        </Button>
                                    </>
                                )}
                                {(booking.status === 'pending' || booking.status === 'confirmed') && (
                                    <>
                                        {booking.status === 'confirmed' && new Date(booking.check_in) <= new Date(new Date().toDateString()) && (
                                            <Button
                                                onClick={() => checkInMutation.mutate(booking.id)}
                                                disabled={checkInMutation.isPending}
                                                variant="primary"
                                            >
                                                <LogIn className="w-4 h-4 mr-2" />
                                                Check In
                                            </Button>
                                        )}
                                        <Button
                                            onClick={() => cancelMutation.mutate(booking.id)}
                                            disabled={cancelMutation.isPending}
                                            variant="danger"
                                        >
                                            Cancel Booking
                                        </Button>
                                    </>
                                )}
                                {booking.status === 'checked_in' && (
                                    <Button
                                        onClick={() => checkOutMutation.mutate(booking.id)}
                                        disabled={checkOutMutation.isPending}
                                        variant="secondary"
                                    >
                                        <LogOut className="w-4 h-4 mr-2" />
                                        Check Out
                                    </Button>
                                )}
                                {booking.status === 'checked_out' && (
                                    <Button
                                        onClick={() => completeMutation.mutate(booking.id)}
                                        disabled={completeMutation.isPending}
                                        variant="primary"
                                    >
                                        <CheckCircle className="w-4 h-4 mr-2" />
                                        Complete
                                    </Button>
                                )}
                            </div>
                        </div>
                    </div>

                    <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">

                        {/* Left Column */}
                        <div className="lg:col-span-2 space-y-6">

                            {/* Trip Information */}
                            <div className="card p-6 border border-primary-200 dark:border-primary-700 rounded-xl bg-white dark:bg-primary-800">
                                <h2 className="text-xl font-bold text-primary-900 dark:text-sand-50 mb-4 inline-flex items-center gap-2">
                                    <Calendar className="w-5 h-5 text-secondary-500" />
                                    Trip Details
                                </h2>

                                <div className="grid grid-cols-1 sm:grid-cols-2 gap-6">
                                    <div>
                                        <p className="text-sm font-medium text-primary-500 dark:text-sand-400">Check-In</p>
                                        <p className="text-lg font-semibold text-primary-900 dark:text-sand-50">{formatDate(booking.check_in)}</p>
                                    </div>
                                    <div>
                                        <p className="text-sm font-medium text-primary-500 dark:text-sand-400">Check-Out</p>
                                        <p className="text-lg font-semibold text-primary-900 dark:text-sand-50">{formatDate(booking.check_out)}</p>
                                    </div>
                                    <div>
                                        <p className="text-sm font-medium text-primary-500 dark:text-sand-400">Length of stay</p>
                                        <p className="text-primary-900 dark:text-sand-50">{booking.nights} nights</p>
                                    </div>
                                    <div>
                                        <p className="text-sm font-medium text-primary-500 dark:text-sand-400">Guests</p>
                                        <p className="text-primary-900 dark:text-sand-50">{booking.number_of_guests} {booking.number_of_guests === 1 ? 'guest' : 'guests'}</p>
                                    </div>
                                </div>
                            </div>

                            {/* Guest Information */}
                            <div className="card p-6 border border-primary-200 dark:border-primary-700 rounded-xl bg-white dark:bg-primary-800">
                                <h2 className="text-xl font-bold text-primary-900 dark:text-sand-50 mb-4 inline-flex items-center gap-2">
                                    <User className="w-5 h-5 text-secondary-500" />
                                    Guest Information
                                </h2>
                                <div className="flex flex-col sm:flex-row justify-between items-start sm:items-center gap-4 border border-primary-100 dark:border-primary-700 p-4 rounded-lg">
                                    <div>
                                        <p className="text-lg font-semibold text-primary-900 dark:text-sand-50">
                                            {booking.guest_first_name} {booking.guest_last_name}
                                        </p>
                                        <p className="text-primary-600 dark:text-sand-300">
                                            {booking.guest_email || 'Email unavailable'}
                                        </p>
                                    </div>
                                    {booking.guest && (
                                        <Link href={`/host/messages?guest=${booking.guest}`}>
                                            <Button variant="outline" className="w-full sm:w-auto">
                                                <MessageSquare className="w-4 h-4 mr-2" />
                                                Message Guest
                                            </Button>
                                        </Link>
                                    )}
                                </div>
                            </div>

                            {/* Property Snapshot */}
                            <div className="card p-6 border border-primary-200 dark:border-primary-700 rounded-xl bg-white dark:bg-primary-800">
                                <h2 className="text-xl font-bold text-primary-900 dark:text-sand-50 mb-4 inline-flex items-center gap-2">
                                    <MapPin className="w-5 h-5 text-secondary-500" />
                                    Property
                                </h2>

                                <Link href={`/property/${booking.rental_property || booking.property?.id}`} className="block group">
                                    <div className="flex items-start gap-4 p-3 rounded-lg hover:bg-sand-100 dark:hover:bg-primary-700 transition">
                                        <div className="w-16 h-16 rounded overflow-hidden bg-primary-200 dark:bg-primary-600 flex-shrink-0">
                                            {(booking.property?.images?.[0]?.image_url || booking.property?.main_image_url || booking.property?.main_image) ? (
                                                <img
                                                    src={booking.property?.images?.[0]?.image_url || booking.property?.main_image_url || booking.property?.main_image}
                                                    className="w-full h-full object-cover"
                                                    alt=""
                                                />
                                            ) : (
                                                <div className="w-full h-full flex items-center justify-center text-primary-400">IMG</div>
                                            )}
                                        </div>
                                        <div>
                                            <h3 className="font-semibold text-lg text-primary-900 dark:text-sand-50 group-hover:text-secondary-600 transition">
                                                {booking.property_title || booking.property?.title}
                                            </h3>
                                            <p className="text-sm text-primary-600 dark:text-sand-400">
                                                {booking.property?.city}, {booking.property?.country}
                                            </p>
                                        </div>
                                    </div>
                                </Link>
                            </div>

                        </div>

                        {/* Right Column (Financials) */}
                        <div className="lg:col-span-1">
                            <div className="card p-6 border border-primary-200 dark:border-primary-700 rounded-xl bg-white dark:bg-primary-800 sticky top-24">
                                <h2 className="text-xl font-bold text-primary-900 dark:text-sand-50 mb-6">
                                    Financial Details
                                </h2>

                                <div className="space-y-4 text-sm text-primary-700 dark:text-sand-200">
                                    <div className="flex justify-between items-center pb-3 border-b border-primary-100 dark:border-primary-700">
                                        <span className="font-medium text-primary-900 dark:text-sand-50">Base Price ({booking.nights} nights)</span>
                                        <span className="font-semibold">{booking.currency} {parseFloat(booking.nightly_total || 0).toFixed(2)}</span>
                                    </div>

                                    {parseFloat(booking.cleaning_fee || 0) > 0 && (
                                        <div className="flex justify-between items-center pb-3 border-b border-primary-100 dark:border-primary-700">
                                            <span>Cleaning Fee</span>
                                            <span>{booking.currency} {parseFloat(booking.cleaning_fee || 0).toFixed(2)}</span>
                                        </div>
                                    )}

                                    <div className="flex justify-between items-center pb-3 border-b border-primary-100 dark:border-primary-700 text-red-600 dark:text-red-400">
                                        <span>StayAfrica Commission</span>
                                        <span>-{booking.currency} {parseFloat(booking.commission_fee || 0).toFixed(2)}</span>
                                    </div>

                                    <div className="pt-2 text-lg font-bold flex justify-between items-center text-green-600 dark:text-green-500">
                                        <span>Your Earnings</span>
                                        <span>{booking.currency} {earnings.toFixed(2)}</span>
                                    </div>
                                </div>

                                <div className="mt-8 p-4 bg-sand-100 dark:bg-primary-900 rounded-lg text-xs text-primary-600 dark:text-sand-400">
                                    <div className="flex items-start gap-2 mb-2">
                                        <Clock className="w-4 h-4 text-primary-400 flex-shrink-0" />
                                        <div>
                                            <p className="font-semibold text-primary-900 dark:text-sand-50 mb-0.5">Payment Status</p>
                                            <p>{booking.payment_status || 'Pending'}</p>
                                        </div>
                                    </div>
                                    <p className="mt-2 pt-2 border-t border-primary-200 dark:border-primary-700">
                                        Earnings are typically released to your wallet 24 hours after a guest checks in. Let our support team know if you have any questions.
                                    </p>
                                </div>

                            </div>
                        </div>

                    </div>
                </div>
            </div>
        </ProtectedRoute>
    );
}
