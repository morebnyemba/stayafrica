'use client';

import { useEffect, useState } from 'react';
import { adminApi } from '@/lib/admin-api';
import { Payment } from '@/types';
import { Eye, RotateCcw, Check, X } from 'lucide-react';
import toast from 'react-hot-toast';
import { ConfirmActionModal } from '@/components/common/confirm-action-modal';

type PaymentAction = 'success' | 'failed';

export default function PaymentsManagement() {
  const [payments, setPayments] = useState<Payment[]>([]);
  const [loading, setLoading] = useState(true);
  const [statusFilter, setStatusFilter] = useState<string>('');
  const [page, setPage] = useState(1);
  const [totalCount, setTotalCount] = useState(0);
  const [showRefundDialog, setShowRefundDialog] = useState(false);
  const [refundPaymentId, setRefundPaymentId] = useState<string | null>(null);
  const [refundAmount, setRefundAmount] = useState('');
  const [pendingAction, setPendingAction] = useState<{ id: string; action: PaymentAction } | null>(null);
  const ITEMS_PER_PAGE = 20;

  useEffect(() => {
    loadPayments();
  }, [page, statusFilter]);

  const loadPayments = async () => {
    try {
      setLoading(true);
      const data = await adminApi.getPayments({
        page,
        status: statusFilter || undefined,
        per_page: ITEMS_PER_PAGE,
      });
      setPayments(data.results || []);
      setTotalCount(data.count || 0);
    } catch (err: any) {
      const errorMsg = err?.response?.data?.detail || 'Failed to load payments';
      toast.error(errorMsg);
      console.error('Payments load error:', err);
    } finally {
      setLoading(false);
    }
  };

  const getStatusColor = (status: string) => {
    const colors: Record<string, string> = {
      initiated: 'bg-blue-100 text-blue-800',
      success: 'bg-green-100 text-green-800',
      failed: 'bg-red-100 text-red-800',
      pending: 'bg-yellow-100 text-yellow-800',
    };
    return colors[status] || 'bg-primary-100 text-primary-800';
  };

  const getProviderBadge = (provider: string) => {
    const colors: Record<string, string> = {
      paynow: 'bg-purple-100 text-purple-800',
      payfast: 'bg-blue-100 text-blue-800',
      stripe: 'bg-indigo-100 text-indigo-800',
      ozow: 'bg-[#F4F1EA] text-[#122F26]',
      cash_on_arrival: 'bg-primary-100 text-primary-800',
    };
    return colors[provider] || 'bg-primary-100 text-primary-800';
  };

  const handleRefund = (paymentId: string, amount: number) => {
    setRefundPaymentId(paymentId);
    setRefundAmount(amount.toString());
    setShowRefundDialog(true);
  };

  const confirmRefund = async () => {
    if (!refundPaymentId) return;
    try {
      await adminApi.refundPayment(refundPaymentId, parseFloat(refundAmount));
      toast.success('Refund processed successfully');
      setShowRefundDialog(false);
      setRefundPaymentId(null);
      loadPayments();
    } catch (err: any) {
      toast.error(err?.response?.data?.detail || 'Failed to process refund');
      console.error(err);
    }
  };

  const handleMarkSuccess = async (id: string) => {
    setPendingAction({ id, action: 'success' });
  };

  const handleMarkFailed = async (id: string) => {
    setPendingAction({ id, action: 'failed' });
  };

  const confirmPaymentAction = async () => {
    if (!pendingAction) return;
    try {
      if (pendingAction.action === 'success') {
        await adminApi.markPaymentSuccess(pendingAction.id);
        toast.success('Payment marked as successful');
      } else {
        await adminApi.markPaymentFailed(pendingAction.id);
        toast.success('Payment marked as failed');
      }
      setPendingAction(null);
      loadPayments();
    } catch (err: any) {
      toast.error(
        err?.response?.data?.detail
          || (pendingAction.action === 'success' ? 'Failed to mark success' : 'Failed to mark failed')
      );
    }
  };

  const paymentActionConfig = pendingAction?.action === 'success'
    ? {
        title: 'Mark Payment Successful',
        message: 'Manually mark this payment as successful?',
        confirmText: 'Mark Successful',
        variant: 'primary' as const,
      }
    : {
        title: 'Mark Payment Failed',
        message: 'Manually mark this payment as failed?',
        confirmText: 'Mark Failed',
        variant: 'danger' as const,
      };

  const isPaymentActionLoading = false;

  const closePendingAction = () => {
    setPendingAction(null);
  };

  const handleRefundDialogClose = () => {
    setShowRefundDialog(false);
    setRefundPaymentId(null);
  };

  const confirmRefundAndClose = async () => {
    await confirmRefund();
  };

  const renderPaymentActionModal = pendingAction ? (
    <ConfirmActionModal
      isOpen={Boolean(pendingAction)}
      title={paymentActionConfig.title}
      message={paymentActionConfig.message}
      confirmText={paymentActionConfig.confirmText}
      variant={paymentActionConfig.variant}
      isLoading={isPaymentActionLoading}
      onCancel={closePendingAction}
      onConfirm={confirmPaymentAction}
    />
  ) : null;

  const renderRefundDialog = showRefundDialog ? (
        <div className="fixed inset-0 z-50 flex items-center justify-center bg-black bg-opacity-50">
          <div className="bg-white rounded-lg shadow-xl p-6 w-full max-w-md">
            <h3 className="text-lg font-semibold text-[#122F26] mb-4">Process Refund</h3>
            <div className="mb-4">
              <label className="block text-sm font-medium text-primary-700 mb-2">
                Refund Amount ($)
              </label>
              <input
                type="number"
                step="0.01"
                min="0.01"
                value={refundAmount}
                onChange={(e) => setRefundAmount(e.target.value)}
                className="w-full px-4 py-2 border border-[#3A5C50] rounded-lg focus:ring-2 focus:ring-[#D9B168] focus:border-transparent"
              />
              <p className="text-xs text-primary-400 mt-1">Enter partial or full amount to refund</p>
            </div>
            <div className="flex justify-end space-x-3">
              <button
                onClick={handleRefundDialogClose}
                className="px-4 py-2 border border-primary-300 text-primary-700 rounded-lg hover:bg-sand-50"
              >
                Cancel
              </button>
              <button
                onClick={confirmRefundAndClose}
                className="px-4 py-2 bg-red-600 text-white rounded-lg hover:bg-red-700"
              >
                Process Refund
              </button>
            </div>
          </div>
        </div>
      ) : null;

  const totalRevenue = payments
    .filter(p => p.status === 'success')
    .reduce((sum, p) => sum + p.amount, 0);

  return (
    <div className="p-8">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-[#122F26]">Payment Management</h1>
        <p className="text-[#3A5C50] mt-2">Monitor payment transactions and payouts</p>
      </div>

      {/* Filters */}
      <div className="bg-white rounded-lg shadow p-6 mb-6">
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
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
              <option value="initiated">Initiated</option>
              <option value="success">Success</option>
              <option value="failed">Failed</option>
              <option value="pending">Pending</option>
            </select>
          </div>
        </div>
      </div>

      {/* Stats Cards */}
      <div className="grid grid-cols-1 md:grid-cols-4 gap-4 mb-6">
        <div className="bg-white rounded-lg shadow p-4">
          <p className="text-sm text-[#3A5C50]">Total Transactions</p>
          <p className="text-2xl font-bold text-[#122F26]">{totalCount}</p>
        </div>
        <div className="bg-white rounded-lg shadow p-4">
          <p className="text-sm text-[#3A5C50]">Successful</p>
          <p className="text-2xl font-bold text-green-600">
            {payments.filter(p => p.status === 'success').length}
          </p>
        </div>
        <div className="bg-white rounded-lg shadow p-4">
          <p className="text-sm text-[#3A5C50]">Failed</p>
          <p className="text-2xl font-bold text-red-600">
            {payments.filter(p => p.status === 'failed').length}
          </p>
        </div>
        <div className="bg-white rounded-lg shadow p-4">
          <p className="text-sm text-[#3A5C50]">Total Revenue</p>
          <p className="text-2xl font-bold text-green-600">
            ${totalRevenue.toLocaleString()}
          </p>
        </div>
      </div>

      {/* Payments Table */}
      <div className="bg-white rounded-lg shadow overflow-hidden">
        {loading ? (
          <div className="flex items-center justify-center h-64">
            <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-[#D9B168]"></div>
          </div>
        ) : (
          <>
            <div className="overflow-x-auto">
              <table className="min-w-full divide-y divide-primary-200">
                <thead className="bg-sand-50">
                  <tr>
                    <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Transaction ID
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Gateway Ref
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Provider
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Amount
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Status
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Date
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Actions
                    </th>
                  </tr>
                </thead>
                <tbody className="bg-white divide-y divide-primary-200">
                  {payments.map((payment) => (
                    <tr key={payment.id} className="hover:bg-sand-50">
                      <td className="px-6 py-4 whitespace-nowrap">
                        <div className="text-sm font-medium text-[#122F26]">
                          {payment.id}
                        </div>
                        <div className="text-sm text-[#3A5C50]">
                          Booking: {payment.booking_id}
                        </div>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-[#122F26]">
                        {payment.gateway_ref || 'N/A'}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap">
                        <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${getProviderBadge(payment.provider)}`}>
                          {payment.provider}
                        </span>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-[#122F26]">
                        ${payment.amount.toLocaleString()}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap">
                        <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${getStatusColor(payment.status)}`}>
                          {payment.status}
                        </span>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50]">
                        {new Date(payment.created_at).toLocaleString()}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm font-medium">
                        <div className="flex items-center space-x-2">
                          <button className="text-[#D9B168] hover:text-[#c9a158]" title="View details">
                            <Eye className="w-5 h-5" />
                          </button>
                          {payment.status === 'success' && (
                            <button
                              onClick={() => handleRefund(payment.id, payment.amount)}
                              className="text-red-600 hover:text-red-900"
                              title="Refund payment"
                            >
                              <RotateCcw className="w-5 h-5" />
                            </button>
                          )}
                          {payment.status !== 'success' && payment.status !== 'failed' && (
                            <>
                              <button
                                onClick={() => handleMarkSuccess(payment.id)}
                                className="text-green-600 hover:text-green-900"
                                title="Mark as Success"
                              >
                                <Check className="w-5 h-5" />
                              </button>
                              <button
                                onClick={() => handleMarkFailed(payment.id)}
                                className="text-orange-600 hover:text-orange-900"
                                title="Mark as Failed"
                              >
                                <X className="w-5 h-5" />
                              </button>
                            </>
                          )}
                        </div>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>

            {/* Pagination */}
            <div className="bg-sand-50 px-6 py-4 flex items-center justify-between border-t">
              <div className="text-sm text-[#122F26]">
                Showing {(page - 1) * ITEMS_PER_PAGE + 1} to {Math.min(page * ITEMS_PER_PAGE, totalCount)} of {totalCount} payments
              </div>
              <div className="flex space-x-2">
                <button
                  onClick={() => setPage(p => Math.max(1, p - 1))}
                  disabled={page === 1}
                  className="px-4 py-2 border border-[#3A5C50] rounded-lg text-sm font-medium text-[#122F26] hover:bg-sand-50 disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  Previous
                </button>
                <button
                  onClick={() => setPage(p => p + 1)}
                  disabled={page * ITEMS_PER_PAGE >= totalCount}
                  className="px-4 py-2 border border-[#3A5C50] rounded-lg text-sm font-medium text-[#122F26] hover:bg-sand-50 disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  Next
                </button>
              </div>
            </div>
          </>
        )}
      </div>

      {/* Refund Dialog */}
      {renderRefundDialog}
      {renderPaymentActionModal}
    </div>
  );
}
