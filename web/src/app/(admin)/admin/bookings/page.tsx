'use client';

import { useEffect, useState } from 'react';
import { adminApi } from '@/lib/admin-api';
import { Booking } from '@/types';
import { Search, Eye, Calendar, CheckCircle, XCircle, ChevronDown } from 'lucide-react';
import toast from 'react-hot-toast';
import ConfirmDialog from '@/components/admin/ConfirmDialog';

export default function BookingsManagement() {
  const [bookings, setBookings] = useState<Booking[]>([]);
  const [loading, setLoading] = useState(true);
  const [search, setSearch] = useState('');
  const [statusFilter, setStatusFilter] = useState<string>('');
  const [page, setPage] = useState(1);
  const [totalCount, setTotalCount] = useState(0);
  const [selectedBookings, setSelectedBookings] = useState<string[]>([]);
  const [showBulkActions, setShowBulkActions] = useState(false);
  const [showConfirmDialog, setShowConfirmDialog] = useState(false);
  const [confirmAction, setConfirmAction] = useState<{ type: string; bookingId?: string } | null>(null);
  const ITEMS_PER_PAGE = 20;

  useEffect(() => {
    loadBookings();
  }, [page, statusFilter, search]);

  const loadBookings = async () => {
    try {
      setLoading(true);
      const data = await adminApi.getBookings({ 
        page, 
        status: statusFilter || undefined,
        search: search.trim() || undefined,
        per_page: ITEMS_PER_PAGE,
      });
      setBookings(data.results || []);
      setTotalCount(data.count || 0);
    } catch (err: any) {
      const errorMsg = err?.response?.data?.detail || 'Failed to load bookings';
      toast.error(errorMsg);
      console.error('Bookings load error:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleSearch = () => {
    setPage(1);
  };

  const getStatusColor = (status: string) => {
    const colors: Record<string, string> = {
      pending: 'bg-yellow-100 text-yellow-800',
      confirmed: 'bg-blue-100 text-blue-800',
      completed: 'bg-green-100 text-green-800',
      cancelled: 'bg-red-100 text-red-800',
    };
    return colors[status] || 'bg-gray-100 text-gray-800';
  };

  const toggleSelectBooking = (bookingId: string) => {
    setSelectedBookings(prev => 
      prev.includes(bookingId) 
        ? prev.filter(id => id !== bookingId)
        : [...prev, bookingId]
    );
  };

  const handleMarkConfirmed = (bookingId: string) => {
    setConfirmAction({ type: 'confirm', bookingId });
    setShowConfirmDialog(true);
  };

  const handleCancelBooking = (bookingId: string) => {
    setConfirmAction({ type: 'cancel', bookingId });
    setShowConfirmDialog(true);
  };

  const handleBulkAction = async (action: string) => {
    if (selectedBookings.length === 0) {
      toast.error('No bookings selected');
      return;
    }

    try {
      switch (action) {
        case 'confirm':
          // Would need API endpoint for bulk confirm
          toast.info('Bulk confirm action needs API implementation');
          break;
        case 'cancel':
          await Promise.all(selectedBookings.map(id => adminApi.cancelBooking(id, 'Bulk cancelled by admin')));
          toast.success(`${selectedBookings.length} bookings cancelled`);
          break;
        case 'complete':
          // Would need API endpoint for bulk complete
          toast.info('Bulk complete action needs API implementation');
          break;
        default:
          toast.error('Unknown action');
          return;
      }
      setSelectedBookings([]);
      setShowBulkActions(false);
      loadBookings();
    } catch (err) {
      toast.error('Failed to perform bulk action');
      console.error(err);
    }
  };

  const handleConfirm = async () => {
    if (!confirmAction || !confirmAction.bookingId) return;

    try {
      if (confirmAction.type === 'cancel') {
        await adminApi.cancelBooking(confirmAction.bookingId, 'Cancelled by admin');
        toast.success('Booking cancelled successfully');
      } else if (confirmAction.type === 'confirm') {
        // Would need API endpoint for this
        toast.info('Confirm booking action needs API implementation');
      }
      loadBookings();
    } catch (err) {
      toast.error(`Failed to ${confirmAction.type} booking`);
      console.error(err);
    }
  };

  return (
    <div className="p-8">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-[#122F26]">Booking Management</h1>
        <p className="text-[#3A5C50] mt-2">Monitor and manage all bookings</p>
      </div>

      {/* Filters and Search */}
      <div className="bg-white rounded-lg shadow p-6 mb-6">
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
          <div className="md:col-span-2">
            <label className="block text-sm font-medium text-[#122F26] mb-2">
              Search Bookings
            </label>
            <div className="flex space-x-2">
              <div className="flex-1 relative">
                <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-[#3A5C50] w-5 h-5" />
                <input
                  type="text"
                  value={search}
                  onChange={(e) => setSearch(e.target.value)}
                  onKeyPress={(e) => e.key === 'Enter' && handleSearch()}
                  placeholder="Search by booking reference..."
                  className="w-full pl-10 pr-4 py-2 border border-[#3A5C50] rounded-lg focus:ring-2 focus:ring-[#D9B168] focus:border-transparent"
                />
              </div>
              <button
                onClick={handleSearch}
                className="px-6 py-2 bg-[#D9B168] text-[#122F26] rounded-lg hover:bg-[#c9a158] transition-colors"
              >
                Search
              </button>
            </div>
          </div>
          <div>
            <label className="block text-sm font-medium text-[#122F26] mb-2">
              Filter by Status
            </label>
            <select
              value={statusFilter}
              onChange={(e) => {
                setStatusFilter(e.target.value);
                setPage(1);
              }}
              className="w-full px-4 py-2 border border-[#3A5C50] rounded-lg focus:ring-2 focus:ring-[#D9B168] focus:border-transparent"
            >
              <option value="">All Status</option>
              <option value="pending">Pending</option>
              <option value="confirmed">Confirmed</option>
              <option value="completed">Completed</option>
              <option value="cancelled">Cancelled</option>
            </select>
          </div>
        </div>

        {/* Bulk Actions */}
        {selectedBookings.length > 0 && (
          <div className="mt-4 flex items-center justify-between bg-[#F4F1EA] px-4 py-3 rounded-lg">
            <p className="text-sm text-[#122F26] font-medium">
              {selectedBookings.length} booking{selectedBookings.length > 1 ? 's' : ''} selected
            </p>
            <div className="relative">
              <button
                onClick={() => setShowBulkActions(!showBulkActions)}
                className="flex items-center space-x-2 px-4 py-2 bg-[#D9B168] text-[#122F26] font-medium rounded-lg hover:bg-[#c9a158] transition-colors"
              >
                <span>Bulk Actions</span>
                <ChevronDown className="w-4 h-4" />
              </button>
              {showBulkActions && (
                <div className="absolute right-0 mt-2 w-48 bg-white rounded-lg shadow-lg border border-gray-200 z-10">
                  <button
                    onClick={() => handleBulkAction('confirm')}
                    className="w-full text-left px-4 py-2 text-sm text-gray-700 hover:bg-gray-100 first:rounded-t-lg"
                  >
                    Confirm Bookings
                  </button>
                  <button
                    onClick={() => handleBulkAction('cancel')}
                    className="w-full text-left px-4 py-2 text-sm text-gray-700 hover:bg-gray-100"
                  >
                    Cancel Bookings
                  </button>
                  <button
                    onClick={() => handleBulkAction('complete')}
                    className="w-full text-left px-4 py-2 text-sm text-gray-700 hover:bg-gray-100 last:rounded-b-lg"
                  >
                    Mark as Completed
                  </button>
                </div>
              )}
            </div>
          </div>
        )}
      </div>

      {/* Stats Cards */}
      <div className="grid grid-cols-1 md:grid-cols-4 gap-4 mb-6">
        <div className="bg-white rounded-lg shadow p-4">
          <p className="text-sm text-[#3A5C50]">Total Bookings</p>
          <p className="text-2xl font-bold text-[#122F26]">{totalCount}</p>
        </div>
        <div className="bg-white rounded-lg shadow p-4">
          <p className="text-sm text-[#3A5C50]">Confirmed</p>
          <p className="text-2xl font-bold text-blue-600">
            {bookings.filter(b => b.status === 'confirmed').length}
          </p>
        </div>
        <div className="bg-white rounded-lg shadow p-4">
          <p className="text-sm text-[#3A5C50]">Completed</p>
          <p className="text-2xl font-bold text-green-600">
            {bookings.filter(b => b.status === 'completed').length}
          </p>
        </div>
        <div className="bg-white rounded-lg shadow p-4">
          <p className="text-sm text-[#3A5C50]">Pending</p>
          <p className="text-2xl font-bold text-yellow-600">
            {bookings.filter(b => b.status === 'pending').length}
          </p>
        </div>
      </div>

      {/* Bookings Table */}
      <div className="bg-white rounded-lg shadow overflow-hidden">
        {loading ? (
          <div className="flex items-center justify-center h-64">
            <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-[#D9B168]"></div>
          </div>
        ) : (
          <>
            <div className="overflow-x-auto">
              <table className="min-w-full divide-y divide-gray-200">
                <thead className="bg-gray-50">
                  <tr>
                    <th className="px-6 py-3 text-left">
                      <input
                        type="checkbox"
                        onChange={(e) => {
                          if (e.target.checked) {
                            setSelectedBookings(bookings.map(b => b.id));
                          } else {
                            setSelectedBookings([]);
                          }
                        }}
                        checked={selectedBookings.length === bookings.length && bookings.length > 0}
                        className="rounded border-gray-300"
                      />
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Booking Ref
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Dates
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Guests
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Amount
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Status
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Created
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Actions
                    </th>
                  </tr>
                </thead>
                <tbody className="bg-white divide-y divide-gray-200">
                  {bookings.map((booking) => (
                    <tr key={booking.id} className="hover:bg-gray-50">
                      <td className="px-6 py-4">
                        <input
                          type="checkbox"
                          checked={selectedBookings.includes(booking.id)}
                          onChange={() => toggleSelectBooking(booking.id)}
                          className="rounded border-gray-300"
                        />
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap">
                        <div className="text-sm font-medium text-[#122F26]">
                          {booking.booking_ref}
                        </div>
                        <div className="text-sm text-[#3A5C50]">{booking.id}</div>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap">
                        <div className="flex items-center text-sm text-[#122F26]">
                          <Calendar className="w-4 h-4 mr-2 text-[#3A5C50]" />
                          <div>
                            <div>{new Date(booking.check_in).toLocaleDateString()}</div>
                            <div className="text-[#3A5C50]">to {new Date(booking.check_out).toLocaleDateString()}</div>
                          </div>
                        </div>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-[#122F26]">
                        {booking.number_of_guests}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap">
                        <div className="text-sm font-medium text-[#122F26]">
                          ${booking.grand_total.toLocaleString()}
                        </div>
                        <div className="text-xs text-[#3A5C50]">
                          Fee: ${booking.service_fee.toLocaleString()}
                        </div>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap">
                        <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${getStatusColor(booking.status)}`}>
                          {booking.status}
                        </span>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50]">
                        {new Date(booking.created_at).toLocaleDateString()}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm font-medium">
                        <div className="flex items-center space-x-2">
                          {booking.status === 'pending' && (
                            <button
                              onClick={() => handleMarkConfirmed(booking.id)}
                              className="text-green-600 hover:text-green-900"
                              title="Confirm booking"
                            >
                              <CheckCircle className="w-5 h-5" />
                            </button>
                          )}
                          {booking.status !== 'cancelled' && booking.status !== 'completed' && (
                            <button
                              onClick={() => handleCancelBooking(booking.id)}
                              className="text-red-600 hover:text-red-900"
                              title="Cancel booking"
                            >
                              <XCircle className="w-5 h-5" />
                            </button>
                          )}
                          <button className="text-[#D9B168] hover:text-[#c9a158]" title="View details">
                            <Eye className="w-5 h-5" />
                          </button>
                        </div>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>

            {/* Pagination */}
            <div className="bg-gray-50 px-6 py-4 flex items-center justify-between border-t">
              <div className="text-sm text-[#122F26]">
                Showing {(page - 1) * 30 + 1} to {Math.min(page * 30, totalCount)} of {totalCount} bookings
              </div>
              <div className="flex space-x-2">
                <button
                  onClick={() => setPage(p => Math.max(1, p - 1))}
                  disabled={page === 1}
                  className="px-4 py-2 border border-[#3A5C50] rounded-lg text-sm font-medium text-[#122F26] hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  Previous
                </button>
                <button
                  onClick={() => setPage(p => p + 1)}
                  disabled={page * 30 >= totalCount}
                  className="px-4 py-2 border border-[#3A5C50] rounded-lg text-sm font-medium text-[#122F26] hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  Next
                </button>
              </div>
            </div>
          </>
        )}
      </div>

      {/* Confirm Dialog */}
      <ConfirmDialog
        isOpen={showConfirmDialog}
        onClose={() => setShowConfirmDialog(false)}
        onConfirm={handleConfirm}
        title={confirmAction?.type === 'cancel' ? 'Cancel Booking' : 'Confirm Booking'}
        message={
          confirmAction?.type === 'cancel'
            ? 'Are you sure you want to cancel this booking? This action cannot be undone.'
            : 'Are you sure you want to confirm this booking?'
        }
        variant={confirmAction?.type === 'cancel' ? 'danger' : 'primary'}
        confirmText={confirmAction?.type === 'cancel' ? 'Cancel Booking' : 'Confirm'}
      />
    </div>
  );
}
