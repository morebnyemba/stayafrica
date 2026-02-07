'use client';

import { useEffect, useState } from 'react';
import { adminApi } from '@/lib/admin-api';
import { Search, Star, Eye, EyeOff, Trash2, ChevronDown } from 'lucide-react';
import toast from 'react-hot-toast';
import ConfirmDialog from '@/components/admin/ConfirmDialog';

export default function ReviewsManagement() {
  const [reviews, setReviews] = useState<any[]>([]);
  const [loading, setLoading] = useState(true);
  const [search, setSearch] = useState('');
  const [ratingFilter, setRatingFilter] = useState<string>('');
  const [page, setPage] = useState(1);
  const [totalCount, setTotalCount] = useState(0);
  const [selectedReviews, setSelectedReviews] = useState<string[]>([]);
  const [showBulkActions, setShowBulkActions] = useState(false);
  const [showConfirmDialog, setShowConfirmDialog] = useState(false);
  const [confirmAction, setConfirmAction] = useState<{ type: string; reviewId?: string } | null>(null);
  const ITEMS_PER_PAGE = 20;

  useEffect(() => {
    loadReviews();
  }, [page, ratingFilter, search]);

  const loadReviews = async () => {
    try {
      setLoading(true);
      const data = await adminApi.getReviews({ 
        page, 
        rating: ratingFilter ? parseInt(ratingFilter) : undefined,
        search: search.trim() || undefined,
        per_page: ITEMS_PER_PAGE,
      });
      setReviews(data.results || []);
      setTotalCount(data.count || 0);
    } catch (err: any) {
      const errorMsg = err?.response?.data?.detail || 'Failed to load reviews';
      toast.error(errorMsg);
      console.error('Reviews load error:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleSearch = () => {
    setPage(1);
  };

  const toggleSelectReview = (reviewId: string) => {
    setSelectedReviews(prev => 
      prev.includes(reviewId) 
        ? prev.filter(id => id !== reviewId)
        : [...prev, reviewId]
    );
  };

  const handleModerate = (reviewId: string) => {
    setConfirmAction({ type: 'moderate', reviewId });
    setShowConfirmDialog(true);
  };

  const handleHide = (reviewId: string) => {
    setConfirmAction({ type: 'hide', reviewId });
    setShowConfirmDialog(true);
  };

  const handleDelete = (reviewId: string) => {
    setConfirmAction({ type: 'delete', reviewId });
    setShowConfirmDialog(true);
  };

  const handleBulkAction = async (action: string) => {
    if (selectedReviews.length === 0) {
      toast.error('No reviews selected');
      return;
    }

    try {
      switch (action) {
        case 'moderate':
          await Promise.all(selectedReviews.map(id => adminApi.moderateReview(id)));
          toast.success(`${selectedReviews.length} reviews moderated`);
          break;
        case 'hide':
          await Promise.all(selectedReviews.map(id => adminApi.hideReview(id)));
          toast.success(`${selectedReviews.length} reviews hidden`);
          break;
        case 'delete':
          await Promise.all(selectedReviews.map(id => adminApi.deleteReview(id)));
          toast.success(`${selectedReviews.length} reviews deleted`);
          break;
        default:
          toast.error('Unknown action');
          return;
      }
      setSelectedReviews([]);
      setShowBulkActions(false);
      loadReviews();
    } catch (err) {
      toast.error('Failed to perform bulk action');
      console.error(err);
    }
  };

  const handleConfirm = async () => {
    if (!confirmAction || !confirmAction.reviewId) return;

    try {
      switch (confirmAction.type) {
        case 'moderate':
          await adminApi.moderateReview(confirmAction.reviewId);
          toast.success('Review moderated successfully');
          break;
        case 'hide':
          await adminApi.hideReview(confirmAction.reviewId);
          toast.success('Review hidden successfully');
          break;
        case 'delete':
          await adminApi.deleteReview(confirmAction.reviewId);
          toast.success('Review deleted successfully');
          break;
      }
      loadReviews();
    } catch (err) {
      toast.error(`Failed to ${confirmAction.type} review`);
      console.error(err);
    }
  };

  const renderStars = (rating: number) => {
    return (
      <div className="flex items-center">
        {[1, 2, 3, 4, 5].map((star) => (
          <Star
            key={star}
            className={`w-4 h-4 ${
              star <= rating ? 'text-yellow-400 fill-yellow-400' : 'text-gray-300'
            }`}
          />
        ))}
      </div>
    );
  };

  return (
    <div className="p-8">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-[#122F26]">Reviews Management</h1>
        <p className="text-[#3A5C50] mt-2">Monitor and moderate property reviews</p>
      </div>

      {/* Filters and Search */}
      <div className="bg-white rounded-lg shadow p-6 mb-6">
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
          <div className="md:col-span-2">
            <label className="block text-sm font-medium text-[#122F26] mb-2">
              Search Reviews
            </label>
            <div className="flex space-x-2">
              <div className="flex-1 relative">
                <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-[#3A5C50] w-5 h-5" />
                <input
                  type="text"
                  value={search}
                  onChange={(e) => setSearch(e.target.value)}
                  onKeyPress={(e) => e.key === 'Enter' && handleSearch()}
                  placeholder="Search by review text..."
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
              Filter by Rating
            </label>
            <select
              value={ratingFilter}
              onChange={(e) => {
                setRatingFilter(e.target.value);
                setPage(1);
              }}
              className="w-full px-4 py-2 border border-[#3A5C50] rounded-lg focus:ring-2 focus:ring-[#D9B168] focus:border-transparent"
            >
              <option value="">All Ratings</option>
              <option value="5">5 Stars</option>
              <option value="4">4 Stars</option>
              <option value="3">3 Stars</option>
              <option value="2">2 Stars</option>
              <option value="1">1 Star</option>
            </select>
          </div>
        </div>

        {/* Bulk Actions */}
        {selectedReviews.length > 0 && (
          <div className="mt-4 flex items-center justify-between bg-[#F4F1EA] px-4 py-3 rounded-lg">
            <p className="text-sm text-[#122F26] font-medium">
              {selectedReviews.length} review{selectedReviews.length > 1 ? 's' : ''} selected
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
                    onClick={() => handleBulkAction('moderate')}
                    className="w-full text-left px-4 py-2 text-sm text-gray-700 hover:bg-gray-100 first:rounded-t-lg"
                  >
                    Moderate Reviews
                  </button>
                  <button
                    onClick={() => handleBulkAction('hide')}
                    className="w-full text-left px-4 py-2 text-sm text-gray-700 hover:bg-gray-100"
                  >
                    Hide Reviews
                  </button>
                  <button
                    onClick={() => handleBulkAction('delete')}
                    className="w-full text-left px-4 py-2 text-sm text-gray-700 hover:bg-gray-100 last:rounded-b-lg"
                  >
                    Delete Reviews
                  </button>
                </div>
              )}
            </div>
          </div>
        )}
      </div>

      {/* Stats Cards */}
      <div className="grid grid-cols-1 md:grid-cols-5 gap-4 mb-6">
        <div className="bg-white rounded-lg shadow p-4">
          <p className="text-sm text-[#3A5C50]">Total Reviews</p>
          <p className="text-2xl font-bold text-[#122F26]">{totalCount}</p>
        </div>
        {[5, 4, 3, 2, 1].map((rating) => (
          <div key={rating} className="bg-white rounded-lg shadow p-4">
            <p className="text-sm text-[#3A5C50]">{rating} Stars</p>
            <p className="text-2xl font-bold text-[#122F26]">
              {reviews.filter(r => r.rating === rating).length}
            </p>
          </div>
        ))}
      </div>

      {/* Reviews Table */}
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
                            setSelectedReviews(reviews.map(r => r.id));
                          } else {
                            setSelectedReviews([]);
                          }
                        }}
                        checked={selectedReviews.length === reviews.length && reviews.length > 0}
                        className="rounded border-gray-300"
                      />
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Guest
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Rating
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Review
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
                  {reviews.map((review) => (
                    <tr key={review.id} className="hover:bg-gray-50">
                      <td className="px-6 py-4">
                        <input
                          type="checkbox"
                          checked={selectedReviews.includes(review.id)}
                          onChange={() => toggleSelectReview(review.id)}
                          className="rounded border-gray-300"
                        />
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap">
                        <div className="text-sm font-medium text-[#122F26]">
                          Guest #{review.guest_id}
                        </div>
                        <div className="text-sm text-[#3A5C50]">
                          Booking: {review.booking_id}
                        </div>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap">
                        {renderStars(review.rating)}
                      </td>
                      <td className="px-6 py-4">
                        <div className="text-sm text-[#122F26] max-w-md truncate">
                          {review.text}
                        </div>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50]">
                        {new Date(review.created_at).toLocaleDateString()}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm font-medium">
                        <div className="flex items-center space-x-2">
                          <button
                            onClick={() => handleModerate(review.id)}
                            className="text-yellow-600 hover:text-yellow-900"
                            title="Moderate review"
                          >
                            <EyeOff className="w-5 h-5" />
                          </button>
                          <button
                            onClick={() => handleHide(review.id)}
                            className="text-orange-600 hover:text-orange-900"
                            title="Hide review"
                          >
                            <Eye className="w-5 h-5" />
                          </button>
                          <button
                            onClick={() => handleDelete(review.id)}
                            className="text-red-600 hover:text-red-900"
                            title="Delete review"
                          >
                            <Trash2 className="w-5 h-5" />
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
                Showing {totalCount > 0 ? (page - 1) * ITEMS_PER_PAGE + 1 : 0} to {Math.min(page * ITEMS_PER_PAGE, totalCount)} of {totalCount} reviews
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
                  disabled={page * ITEMS_PER_PAGE >= totalCount}
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
        title={
          confirmAction?.type === 'delete' ? 'Delete Review' :
          confirmAction?.type === 'hide' ? 'Hide Review' : 'Moderate Review'
        }
        message={
          confirmAction?.type === 'delete'
            ? 'Are you sure you want to delete this review? This action cannot be undone.'
            : confirmAction?.type === 'hide'
            ? 'Are you sure you want to hide this review? It will be marked as [HIDDEN].'
            : 'Are you sure you want to moderate this review? It will be marked as [MODERATED].'
        }
        variant={confirmAction?.type === 'delete' ? 'danger' : 'warning'}
        confirmText={confirmAction?.type === 'delete' ? 'Delete' : confirmAction?.type === 'hide' ? 'Hide' : 'Moderate'}
      />
    </div>
  );
}
