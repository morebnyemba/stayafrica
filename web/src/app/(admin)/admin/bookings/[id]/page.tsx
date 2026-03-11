'use client';

import { useState, useEffect } from 'react';
import { adminApi } from '@/lib/admin-api';
import { Booking } from '@/types';
import { useRouter } from 'next/navigation';
import toast from 'react-hot-toast';
import { ArrowLeft, Save, Calendar, User, CreditCard, Activity } from 'lucide-react';

export default function BookingDetails({ params }: { params: { id: string } }) {
    const router = useRouter();
    const [booking, setBooking] = useState<Booking | null>(null);
    const [loading, setLoading] = useState(true);
    const [saving, setSaving] = useState(false);
    const [editMode, setEditMode] = useState(false);

    // Editable fields
    const [status, setStatus] = useState<Booking['status']>('pending');

    useEffect(() => {
        loadBooking();
    }, [params.id]);

    const loadBooking = async () => {
        try {
            setLoading(true);
            const data = await adminApi.getBookingById(params.id);
            setBooking(data);
            setStatus(data.status);
        } catch (err: any) {
            toast.error('Failed to load booking details');
            console.error(err);
        } finally {
            setLoading(false);
        }
    };

    const handleSave = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!booking) return;

        try {
            setSaving(true);

            // The admin API supports status transitions via dedicated endpoints
            if (status !== booking.status) {
                if (status === 'confirmed') await adminApi.approveBooking(booking.id);
                if (status === 'cancelled') await adminApi.cancelBooking(booking.id, 'Cancelled via admin panel');
                if (status === 'completed') await adminApi.completeBooking(booking.id);
                toast.success('Booking status updated successfully');
            } else {
                toast.success('No changes needed');
            }

            setEditMode(false);
            loadBooking();
        } catch (err: any) {
            toast.error(err?.response?.data?.detail || 'Failed to update booking');
            console.error(err);
        } finally {
            setSaving(false);
        }
    };

    const getStatusColor = (s: string) => {
        const colors: Record<string, string> = {
            pending: 'bg-yellow-100 text-yellow-800',
            confirmed: 'bg-blue-100 text-blue-800',
            checked_in: 'bg-indigo-100 text-indigo-800',
            checked_out: 'bg-purple-100 text-purple-800',
            completed: 'bg-green-100 text-green-800',
            cancelled: 'bg-red-100 text-red-800',
        };
        return colors[s] || 'bg-gray-100 text-gray-800';
    };

    if (loading) {
        return (
            <div className="flex items-center justify-center p-8 h-[calc(100vh-200px)]">
                <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-[#D9B168]"></div>
            </div>
        );
    }

    if (!booking) {
        return (
            <div className="p-8 text-center text-[#3A5C50]">
                Booking not found.
            </div>
        );
    }

    return (
        <div className="p-8">
            <div className="mb-6 flex items-center justify-between">
                <div className="flex items-center gap-4">
                    <button
                        onClick={() => router.back()}
                        className="p-2 border border-[#3A5C50] text-[#122F26] rounded-full hover:bg-sand-50 transition"
                    >
                        <ArrowLeft className="w-5 h-5" />
                    </button>
                    <div>
                        <h1 className="text-3xl font-bold text-[#122F26]">
                            {booking.booking_ref}
                        </h1>
                        <p className="text-[#3A5C50] mt-1 text-sm">Booking ID: {booking.id}</p>
                    </div>
                </div>

                <div className="flexitems-center gap-3">
                    <span className={`px-4 py-1.5 rounded-full text-sm font-semibold uppercase ${getStatusColor(booking.status)}`}>
                        {booking.status}
                    </span>
                    {!editMode && (
                        <button
                            onClick={() => setEditMode(true)}
                            className="ml-4 px-6 py-2 bg-[#D9B168] text-[#122F26] font-medium rounded-lg hover:bg-[#c9a158] transition"
                        >
                            Edit Booking
                        </button>
                    )}
                </div>
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
                {/* Main Details */}
                <div className="lg:col-span-2 space-y-6">
                    <div className="bg-white rounded-xl shadow-sm p-6 border border-primary-200">
                        <h2 className="text-lg font-bold text-[#122F26] mb-6 flex items-center gap-2">
                            <Calendar className="w-5 h-5 text-[#3A5C50]" />
                            Stay Details
                            {editMode && <span className="text-xs font-normal text-red-500 ml-2">(Edit Mode)</span>}
                        </h2>

                        <form id="booking-edit-form" onSubmit={handleSave} className="grid grid-cols-1 md:grid-cols-2 gap-6">
                            <div>
                                <label className="block text-sm font-medium text-[#3A5C50] mb-1">Status</label>
                                {editMode ? (
                                    <select
                                        value={status}
                                        onChange={(e) => setStatus(e.target.value as any)}
                                        className="w-full px-4 py-2 border border-[#3A5C50] rounded-lg focus:ring-2 focus:ring-[#D9B168]"
                                    >
                                        <option value="pending">Pending</option>
                                        <option value="confirmed">Confirmed</option>
                                        <option value="checked_in">Checked In</option>
                                        <option value="checked_out">Checked Out</option>
                                        <option value="completed">Completed</option>
                                        <option value="cancelled">Cancelled</option>
                                    </select>
                                ) : (
                                    <div className="px-4 py-2 bg-sand-50 rounded-lg text-[#122F26] capitalize">
                                        {booking.status.replace('_', ' ')}
                                    </div>
                                )}
                            </div>

                            <div>
                                <label className="block text-sm font-medium text-[#3A5C50] mb-1">Guests</label>
                                <div className="px-4 py-2 bg-sand-50 rounded-lg text-[#122F26]">
                                    {booking.number_of_guests} Guest(s)
                                </div>
                            </div>

                            <div>
                                <label className="block text-sm font-medium text-[#3A5C50] mb-1">Check-in</label>
                                <div className="px-4 py-2 bg-sand-50 rounded-lg text-[#122F26]">
                                    {new Date(booking.check_in).toLocaleDateString()}
                                </div>
                            </div>

                            <div>
                                <label className="block text-sm font-medium text-[#3A5C50] mb-1">Check-out</label>
                                <div className="px-4 py-2 bg-sand-50 rounded-lg text-[#122F26]">
                                    {new Date(booking.check_out).toLocaleDateString()}
                                </div>
                            </div>

                        </form>

                        {editMode && (
                            <div className="mt-8 flex items-center justify-end gap-3 pt-6 border-t border-primary-200">
                                <button
                                    type="button"
                                    onClick={() => {
                                        setEditMode(false);
                                        setStatus(booking.status);
                                    }}
                                    className="px-6 py-2 border border-[#3A5C50] text-[#122F26] rounded-lg hover:bg-sand-50 transition"
                                    disabled={saving}
                                >
                                    Cancel
                                </button>
                                <button
                                    type="submit"
                                    form="booking-edit-form"
                                    disabled={saving}
                                    className="flex items-center gap-2 px-6 py-2 bg-[#122F26] text-white rounded-lg hover:bg-[#3A5C50] transition disabled:opacity-50"
                                >
                                    {saving ? 'Saving...' : <><Save className="w-4 h-4" /> Save Changes</>}
                                </button>
                            </div>
                        )}
                    </div>
                </div>

                {/* Sidebar Information */}
                <div className="space-y-6">
                    <div className="bg-sand-50 rounded-xl p-6 border border-primary-100">
                        <h3 className="text-lg font-bold text-[#122F26] mb-4 flex items-center gap-2">
                            <CreditCard className="w-5 h-5 text-[#3A5C50]" />
                            Financial Breakdown
                        </h3>
                        <div className="space-y-3 pt-2">
                            <div className="flex justify-between text-sm">
                                <span className="text-[#3A5C50]">Nightly Total</span>
                                <span className="font-medium">${booking.nightly_total?.toLocaleString() || '0'}</span>
                            </div>
                            <div className="flex justify-between text-sm">
                                <span className="text-[#3A5C50]">Service Fee</span>
                                <span className="font-medium">${booking.service_fee?.toLocaleString() || '0'}</span>
                            </div>
                            <div className="flex justify-between text-sm">
                                <span className="text-[#3A5C50]">Cleaning Fee</span>
                                <span className="font-medium">${booking.cleaning_fee?.toLocaleString() || '0'}</span>
                            </div>
                            <div className="flex justify-between text-sm">
                                <span className="text-red-700">Commission (Host Deduct)</span>
                                <span className="font-medium text-red-700">-${booking.commission_fee?.toLocaleString() || '0'}</span>
                            </div>
                            <div className="pt-3 border-t border-primary-200 flex justify-between">
                                <span className="font-bold text-[#122F26]">Grand Total</span>
                                <span className="font-bold text-[#122F26]">${booking.grand_total?.toLocaleString() || '0'}</span>
                            </div>
                        </div>
                    </div>

                    <div className="bg-white rounded-xl shadow-sm p-6 border border-primary-200">
                        <h3 className="text-lg font-bold text-[#122F26] mb-4 flex items-center gap-2">
                            <User className="w-5 h-5 text-[#3A5C50]" />
                            Participant IDs
                        </h3>
                        <div className="space-y-4">
                            <div>
                                <p className="text-xs text-[#3A5C50] uppercase tracking-wider font-semibold">Guest ID</p>
                                <div className="mt-1 flex items-center gap-2">
                                    <code className="text-sm bg-sand-50 py-1 px-2 rounded-md">{booking.guest_id || 'N/A'}</code>
                                </div>
                            </div>
                            <div>
                                <p className="text-xs text-[#3A5C50] uppercase tracking-wider font-semibold">Property ID</p>
                                <div className="mt-1 flex items-center gap-2">
                                    <code className="text-sm bg-sand-50 py-1 px-2 rounded-md">{booking.property_id || 'N/A'}</code>
                                </div>
                            </div>
                        </div>
                    </div>

                    <div className="bg-white rounded-xl shadow-sm p-6 border border-primary-200">
                        <h3 className="text-lg font-bold text-[#122F26] mb-4 flex items-center gap-2">
                            <Activity className="w-5 h-5 text-[#3A5C50]" />
                            Metadata
                        </h3>
                        <div className="space-y-3 text-sm">
                            <div className="flex flex-col">
                                <span className="text-[#3A5C50] mb-1">Created At</span>
                                <span className="font-medium">{new Date(booking.created_at).toLocaleString()}</span>
                            </div>
                            <div className="flex flex-col">
                                <span className="text-[#3A5C50] mb-1">Last Updated</span>
                                <span className="font-medium">{new Date(booking.updated_at).toLocaleString()}</span>
                            </div>
                        </div>
                    </div>

                </div>
            </div>
        </div>
    );
}
