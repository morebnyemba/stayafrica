'use client';

import { useEffect, useState } from 'react';
import { useSearchParams } from 'next/navigation';
import { adminApi } from '@/lib/admin-api';
import { IdentityVerification, VerificationStats } from '@/types/admin-types';
import { CheckCircle, XCircle, Clock, AlertCircle, Eye } from 'lucide-react';
import toast from 'react-hot-toast';
import VerificationModal from '@/components/admin/VerificationModal';

export default function IdentityVerificationManagement() {
  const searchParams = useSearchParams();
  const userIdFilter = searchParams.get('user');
  
  const [verifications, setVerifications] = useState<IdentityVerification[]>([]);
  const [loading, setLoading] = useState(true);
  const [page, setPage] = useState(1);
  const [totalCount, setTotalCount] = useState(0);
  const [stats, setStats] = useState<VerificationStats | null>(null);
  const [selectedVerification, setSelectedVerification] = useState<IdentityVerification | null>(null);
  const [showModal, setShowModal] = useState(false);
  const ITEMS_PER_PAGE = 15;

  useEffect(() => {
    loadVerifications();
    loadStats();
  }, [page, userIdFilter]);

  const loadVerifications = async () => {
    try {
      setLoading(true);
      const data = await adminApi.getPendingVerifications({ 
        page,
        per_page: ITEMS_PER_PAGE,
      });
      setVerifications(data.results || []);
      setTotalCount(data.count || 0);
    } catch (err: any) {
      const errorMsg = err?.response?.data?.detail || 'Failed to load verifications';
      toast.error(errorMsg);
      console.error('Verifications load error:', err);
    } finally {
      setLoading(false);
    }
  };

  const loadStats = async () => {
    try {
      const statsData = await adminApi.getVerificationStats();
      setStats(statsData);
    } catch (err: any) {
      console.error('Stats load error:', err);
    }
  };

  const handleViewDetails = async (id: string | number) => {
    try {
      const verification = await adminApi.getVerificationById(id);
      setSelectedVerification(verification);
      setShowModal(true);
    } catch (err: any) {
      const errorMsg = err?.response?.data?.detail || 'Failed to load verification details';
      toast.error(errorMsg);
    }
  };

  // Auto-open modal if user filter is present and there's a verification for that user
  useEffect(() => {
    if (userIdFilter && verifications.length > 0) {
      const userVerification = verifications.find(v => v.user.id.toString() === userIdFilter);
      if (userVerification && !showModal) {
        handleViewDetails(userVerification.id);
      }
    }
  }, [verifications, userIdFilter, showModal]);

  const handleApproveVerification = async (notes: string) => {
    if (!selectedVerification) return;
    try {
      setLoading(true);
      await adminApi.approveVerification(selectedVerification.id, notes);
      toast.success('Verification approved successfully');
      setShowModal(false);
      setSelectedVerification(null);
      loadVerifications();
      loadStats();
    } catch (err: any) {
      const errorMsg = err?.response?.data?.detail || 'Failed to approve verification';
      toast.error(errorMsg);
      console.error('Approval error:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleRejectVerification = async (reason: string, notes: string) => {
    if (!selectedVerification) return;
    try {
      setLoading(true);
      await adminApi.rejectVerification(selectedVerification.id, reason, notes);
      toast.success('Verification rejected');
      setShowModal(false);
      setSelectedVerification(null);
      loadVerifications();
      loadStats();
    } catch (err: any) {
      const errorMsg = err?.response?.data?.detail || 'Failed to reject verification';
      toast.error(errorMsg);
      console.error('Rejection error:', err);
    } finally {
      setLoading(false);
    }
  };

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'approved':
        return 'text-green-600 bg-green-50';
      case 'rejected':
        return 'text-red-600 bg-red-50';
      case 'pending':
        return 'text-yellow-600 bg-yellow-50';
      case 'under_review':
        return 'text-secondary-600 dark:text-secondary-400 bg-blue-50';
      case 'expired':
        return 'text-primary-500 dark:text-sand-400 bg-sand-50 dark:bg-primary-900';
      default:
        return 'text-primary-500 dark:text-sand-400 bg-sand-50 dark:bg-primary-900';
    }
  };

  const getStatusIcon = (status: string) => {
    switch (status) {
      case 'approved':
        return <CheckCircle className="w-4 h-4" />;
      case 'rejected':
        return <XCircle className="w-4 h-4" />;
      case 'pending':
        return <Clock className="w-4 h-4" />;
      case 'under_review':
        return <AlertCircle className="w-4 h-4" />;
      case 'expired':
        return <AlertCircle className="w-4 h-4" />;
      default:
        return <Clock className="w-4 h-4" />;
    }
  };

  const formatDate = (dateString: string | undefined) => {
    if (!dateString) return '-';
    return new Date(dateString).toLocaleDateString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric',
    });
  };

  return (
    <div className="min-h-screen bg-sand-50 dark:bg-primary-900 p-6">
      <div className="max-w-7xl mx-auto">
        {/* Header */}
        <div className="mb-8">
          <h1 className="text-3xl font-bold text-primary-900 dark:text-sand-50 mb-2">Identity Verification</h1>
          <p className="text-primary-500 dark:text-sand-400">
            {userIdFilter 
              ? `Review identity verification for user ${userIdFilter}`
              : 'Review and approve user identity verifications (KYC)'
            }
          </p>
        </div>

        {/* Statistics Cards */}
        {stats && (
          <div className="grid grid-cols-1 md:grid-cols-5 gap-4 mb-8">
            <div className="bg-white rounded-lg shadow p-4 border-l-4 border-blue-500">
              <p className="text-sm text-primary-500 dark:text-sand-400 mb-1">Pending Review</p>
              <p className="text-2xl font-bold text-secondary-600 dark:text-secondary-400">{stats.pending_review}</p>
            </div>
            <div className="bg-white rounded-lg shadow p-4 border-l-4 border-yellow-500">
              <p className="text-sm text-primary-500 dark:text-sand-400 mb-1">Total Submissions</p>
              <p className="text-2xl font-bold text-yellow-600">{stats.total_verifications}</p>
            </div>
            <div className="bg-white rounded-lg shadow p-4 border-l-4 border-green-500">
              <p className="text-sm text-primary-500 dark:text-sand-400 mb-1">Approved</p>
              <p className="text-2xl font-bold text-green-600">{stats.by_status.approved || 0}</p>
            </div>
            <div className="bg-white rounded-lg shadow p-4 border-l-4 border-red-500">
              <p className="text-sm text-primary-500 dark:text-sand-400 mb-1">Rejected</p>
              <p className="text-2xl font-bold text-red-600">{stats.by_status.rejected || 0}</p>
            </div>
            <div className="bg-white rounded-lg shadow p-4 border-l-4 border-purple-500">
              <p className="text-sm text-primary-500 dark:text-sand-400 mb-1">Approval Rate</p>
              <p className="text-2xl font-bold text-purple-600">{stats.approval_rate}%</p>
            </div>
          </div>
        )}

        {/* Verifications Table */}
        <div className="bg-white rounded-lg shadow overflow-hidden">
          <div className="overflow-x-auto">
            <table className="w-full">
              <thead className="bg-primary-100 dark:bg-primary-800 border-b border-sand-200/50 dark:border-primary-700/50">
                <tr>
                  <th className="px-6 py-3 text-left text-xs font-medium text-primary-700 dark:text-sand-200 uppercase tracking-wider">User</th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-primary-700 dark:text-sand-200 uppercase tracking-wider">Document Type</th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-primary-700 dark:text-sand-200 uppercase tracking-wider">Status</th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-primary-700 dark:text-sand-200 uppercase tracking-wider">Submitted</th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-primary-700 dark:text-sand-200 uppercase tracking-wider">Actions</th>
                </tr>
              </thead>
              <tbody className="divide-y divide-primary-200 dark:divide-primary-700">
                {loading ? (
                  <tr>
                    <td colSpan={5} className="px-6 py-4 text-center text-primary-400 dark:text-sand-500">
                      Loading...
                    </td>
                  </tr>
                ) : (verifications.filter(v => !userIdFilter || v.user.id.toString() === userIdFilter)).length === 0 ? (
                  <tr>
                    <td colSpan={5} className="px-6 py-4 text-center text-primary-400 dark:text-sand-500">
                      {userIdFilter ? 'No verifications found for this user' : 'No verifications found'}
                    </td>
                  </tr>
                ) : (
                  (verifications.filter(v => !userIdFilter || v.user.id.toString() === userIdFilter)).map((verification) => (
                    <tr key={verification.id} className="hover:bg-sand-50 dark:hover:bg-primary-800 transition">
                      <td className="px-6 py-4 whitespace-nowrap">
                        <div>
                          <p className="text-sm font-medium text-primary-900 dark:text-sand-50">
                            {verification.user.first_name} {verification.user.last_name}
                          </p>
                          <p className="text-sm text-primary-400 dark:text-sand-500">{verification.user.email}</p>
                        </div>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-primary-700 dark:text-sand-200 capitalize">
                        {verification.document_type.replace('_', ' ')}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap">
                        <span className={`inline-flex items-center gap-2 px-3 py-1 rounded-full text-xs font-medium ${getStatusColor(verification.status)}`}>
                          {getStatusIcon(verification.status)}
                          {verification.status.replace('_', ' ')}
                        </span>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-primary-400 dark:text-sand-500">
                        {formatDate(verification.submitted_at)}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm">
                        <button
                          onClick={() => handleViewDetails(verification.id)}
                          className="inline-flex items-center gap-1 text-secondary-600 dark:text-secondary-400 hover:text-secondary-700 transition"
                        >
                          <Eye className="w-4 h-4" />
                          View
                        </button>
                      </td>
                    </tr>
                  ))
                )}
              </tbody>
            </table>
          </div>

          {/* Pagination */}
          {totalCount > ITEMS_PER_PAGE && (
            <div className="px-6 py-4 border-t border-sand-200/50 dark:border-primary-700/50 flex items-center justify-between">
              <p className="text-sm text-primary-500 dark:text-sand-400">
                Showing {(page - 1) * ITEMS_PER_PAGE + 1} to {Math.min(page * ITEMS_PER_PAGE, totalCount)} of {totalCount}
              </p>
              <div className="flex gap-2">
                <button
                  onClick={() => setPage(Math.max(1, page - 1))}
                  disabled={page === 1}
                  className="px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg text-sm font-medium text-primary-700 dark:text-sand-200 hover:bg-sand-50 dark:hover:bg-primary-800 disabled:opacity-50 disabled:cursor-not-allowed transition"
                >
                  Previous
                </button>
                <span className="px-4 py-2 text-sm text-primary-500 dark:text-sand-400">Page {page}</span>
                <button
                  onClick={() => setPage(page + 1)}
                  disabled={page * ITEMS_PER_PAGE >= totalCount}
                  className="px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg text-sm font-medium text-primary-700 dark:text-sand-200 hover:bg-sand-50 dark:hover:bg-primary-800 disabled:opacity-50 disabled:cursor-not-allowed transition"
                >
                  Next
                </button>
              </div>
            </div>
          )}
        </div>
      </div>

      {/* Verification Detail Modal */}
      {showModal && selectedVerification && (
        <VerificationModal
          verification={selectedVerification}
          onClose={() => {
            setShowModal(false);
            setSelectedVerification(null);
          }}
          onApprove={handleApproveVerification}
          onReject={handleRejectVerification}
          loading={loading}
        />
      )}
    </div>
  );
}
