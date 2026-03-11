'use client';

import { useEffect, useState } from 'react';
import { adminApi } from '@/lib/admin-api';
import { Search, Loader, CheckCircle, XCircle, Clock } from 'lucide-react';
import toast from 'react-hot-toast';

export default function WithdrawalsManagement() {
    const [withdrawals, setWithdrawals] = useState<any[]>([]);
    const [loading, setLoading] = useState(true);
    const [search, setSearch] = useState('');
    const [statusFilter, setStatusFilter] = useState<string>('');
    const [page, setPage] = useState(1);
    const [totalCount, setTotalCount] = useState(0);

    const [showConfirmDialog, setShowConfirmDialog] = useState(false);
    const [confirmAction, setConfirmAction] = useState<{ type: string; id: string; requireNotes: boolean } | null>(null);
    const [adminNotes, setAdminNotes] = useState('');
    const ITEMS_PER_PAGE = 20;

    useEffect(() => {
        loadWithdrawals();
    }, [page, statusFilter, search]);

    const loadWithdrawals = async () => {
        try {
            setLoading(true);
            const data = await adminApi.getWithdrawals({
                page,
                status: statusFilter || undefined,
                search: search.trim() || undefined,
                per_page: ITEMS_PER_PAGE,
            });
            setWithdrawals(data.results || []);
            setTotalCount(data.count || 0);
        } catch (err: any) {
            const errorMsg = err?.response?.data?.detail || 'Failed to load withdrawals';
            toast.error(errorMsg);
            console.error('Withdrawals load error:', err);
        } finally {
            setLoading(false);
        }
    };

    const handleSearch = () => {
        setPage(1);
    };

    const handleActionClick = (type: string, id: string, requireNotes = false) => {
        setConfirmAction({ type, id, requireNotes });
        setAdminNotes('');
        setShowConfirmDialog(true);
    };

    const handleConfirm = async () => {
        if (!confirmAction) return;

        try {
            switch (confirmAction.type) {
                case 'processing':
                    await adminApi.markWithdrawalProcessing(confirmAction.id);
                    toast.success('Withdrawal marked as processing');
                    break;
                case 'completed':
                    await adminApi.markWithdrawalCompleted(confirmAction.id, adminNotes);
                    toast.success('Withdrawal completed successfully');
                    break;
                case 'failed':
                    await adminApi.markWithdrawalFailed(confirmAction.id, adminNotes);
                    toast.success('Withdrawal marked as failed');
                    break;
            }
            setShowConfirmDialog(false);
            loadWithdrawals();
        } catch (err: any) {
            toast.error(err?.response?.data?.error || `Failed to mark ${confirmAction.type}`);
            console.error(err);
        }
    };

    const getStatusBadge = (status: string) => {
        const badges: Record<string, { color: string; text: string }> = {
            pending: { color: 'bg-yellow-100 text-yellow-800', text: 'Pending' },
            processing: { color: 'bg-blue-100 text-blue-800', text: 'Processing' },
            completed: { color: 'bg-green-100 text-green-800', text: 'Completed' },
            failed: { color: 'bg-red-100 text-red-800', text: 'Failed' },
        };
        return badges[status] || { color: 'bg-primary-100 text-primary-800', text: status };
    };

    return (
        <div className="p-8">
            <div className="mb-8">
                <h1 className="text-3xl font-bold text-[#122F26]">Withdrawals Management</h1>
                <p className="text-[#3A5C50] mt-2">Process host withdrawal requests</p>
            </div>

            {/* Filters and Search */}
            <div className="bg-white rounded-lg shadow p-6 mb-6">
                <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                    <div className="md:col-span-2">
                        <label className="block text-sm font-medium text-[#122F26] mb-2">
                            Search Reference or User
                        </label>
                        <div className="flex space-x-2">
                            <div className="flex-1 relative">
                                <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-[#3A5C50] w-5 h-5" />
                                <input
                                    type="text"
                                    value={search}
                                    onChange={(e) => setSearch(e.target.value)}
                                    onKeyPress={(e) => e.key === 'Enter' && handleSearch()}
                                    placeholder="Search by reference, email or user..."
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
                            <option value="processing">Processing</option>
                            <option value="completed">Completed</option>
                            <option value="failed">Failed</option>
                        </select>
                    </div>
                </div>
            </div>

            {/* Withdrawals Table */}
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
                                            Reference
                                        </th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                                            User / Wallet
                                        </th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                                            Amount
                                        </th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                                            Details
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
                                    {withdrawals.map((item) => {
                                        const statusBadge = getStatusBadge(item.status);
                                        const userEmail = item.wallet?.user?.email || `User #${item.wallet?.user_id}`;
                                        return (
                                            <tr key={item.id} className="hover:bg-sand-50">
                                                <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-[#122F26]">
                                                    {item.reference}
                                                </td>
                                                <td className="px-6 py-4 whitespace-nowrap">
                                                    <div className="text-sm font-medium text-[#122F26]">{userEmail}</div>
                                                    <div className="text-sm text-[#3A5C50]">Wallet #{item.wallet?.id}</div>
                                                </td>
                                                <td className="px-6 py-4 whitespace-nowrap">
                                                    <div className="text-sm font-medium text-[#122F26]">
                                                        {item.amount?.toLocaleString()} {item.currency || 'USD'}
                                                    </div>
                                                </td>
                                                <td className="px-6 py-4 text-sm text-[#3A5C50] max-w-xs truncate">
                                                    <div>Bank: {item.bank_account?.bank_name}</div>
                                                    <div>Acct: {item.bank_account?.account_number}</div>
                                                    {item.admin_notes && (
                                                        <div className="mt-1 text-xs italic text-gray-500">Note: {item.admin_notes}</div>
                                                    )}
                                                </td>
                                                <td className="px-6 py-4 whitespace-nowrap">
                                                    <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${statusBadge.color}`}>
                                                        {statusBadge.text}
                                                    </span>
                                                </td>
                                                <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50]">
                                                    {new Date(item.created_at).toLocaleString()}
                                                </td>
                                                <td className="px-6 py-4 whitespace-nowrap text-sm font-medium">
                                                    <div className="flex items-center space-x-2">
                                                        {item.status === 'pending' && (
                                                            <button
                                                                onClick={() => handleActionClick('processing', item.id)}
                                                                className="text-blue-600 hover:text-blue-900"
                                                                title="Mark as Processing"
                                                            >
                                                                <Loader className="w-5 h-5" />
                                                            </button>
                                                        )}
                                                        {(item.status === 'pending' || item.status === 'processing') && (
                                                            <>
                                                                <button
                                                                    onClick={() => handleActionClick('completed', item.id, true)}
                                                                    className="text-green-600 hover:text-green-900"
                                                                    title="Mark as Completed"
                                                                >
                                                                    <CheckCircle className="w-5 h-5" />
                                                                </button>
                                                                <button
                                                                    onClick={() => handleActionClick('failed', item.id, true)}
                                                                    className="text-red-600 hover:text-red-900"
                                                                    title="Mark as Failed"
                                                                >
                                                                    <XCircle className="w-5 h-5" />
                                                                </button>
                                                            </>
                                                        )}
                                                        {item.status === 'completed' && <Clock className="w-4 h-4 text-gray-400" />}
                                                        {item.status === 'failed' && <Clock className="w-4 h-4 text-gray-400" />}
                                                    </div>
                                                </td>
                                            </tr>
                                        );
                                    })}
                                    {withdrawals.length === 0 && (
                                        <tr>
                                            <td colSpan={7} className="px-6 py-8 text-center text-[#3A5C50]">
                                                No withdrawals found matching your filters.
                                            </td>
                                        </tr>
                                    )}
                                </tbody>
                            </table>
                        </div>

                        {/* Pagination */}
                        {totalCount > 0 && (
                            <div className="bg-sand-50 px-6 py-4 flex items-center justify-between border-t">
                                <div className="text-sm text-[#122F26]">
                                    Showing {(page - 1) * ITEMS_PER_PAGE + 1} to {Math.min(page * ITEMS_PER_PAGE, totalCount)} of {totalCount} requests
                                </div>
                                <div className="flex space-x-2">
                                    <button
                                        onClick={() => setPage(p => Math.max(1, p - 1))}
                                        disabled={page === 1}
                                        className="px-4 py-2 border border-[#3A5C50] rounded-lg text-sm font-medium text-[#122F26] hover:bg-sand-50 disabled:opacity-50 disabled:cursor-not-allowed hidden sm:block"
                                    >
                                        Previous
                                    </button>
                                    <button
                                        onClick={() => setPage(p => p + 1)}
                                        disabled={page * ITEMS_PER_PAGE >= totalCount}
                                        className="px-4 py-2 border border-[#3A5C50] rounded-lg text-sm font-medium text-[#122F26] hover:bg-sand-50 disabled:opacity-50 disabled:cursor-not-allowed hidden sm:block"
                                    >
                                        Next
                                    </button>
                                </div>
                            </div>
                        )}
                    </>
                )}
            </div>

            {/* Confirm Action Dialog */}
            {showConfirmDialog && confirmAction && (
                <div className="fixed inset-0 z-50 flex items-center justify-center bg-black bg-opacity-50">
                    <div className="bg-white rounded-lg shadow-xl p-6 w-full max-w-md">
                        <h3 className="text-lg font-semibold text-[#122F26] mb-4">
                            {confirmAction.type === 'processing' && 'Mark as Processing'}
                            {confirmAction.type === 'completed' && 'Complete Withdrawal'}
                            {confirmAction.type === 'failed' && 'Fail Withdrawal'}
                        </h3>

                        <p className="text-sm text-[#3A5C50] mb-4">
                            {confirmAction.type === 'processing' && 'Changing status to Processing indicates that you are actively transferring the funds.'}
                            {confirmAction.type === 'completed' && 'Confirm that the funds have been successfully transferred to the user\'s bank account.'}
                            {confirmAction.type === 'failed' && 'This will cancel the withdrawal and refund the amount back to the user\'s wallet.'}
                        </p>

                        {confirmAction.requireNotes && (
                            <div className="mb-4">
                                <label className="block text-sm font-medium text-[#122F26] mb-2">
                                    Admin Notes {confirmAction.type === 'failed' && <span className="text-red-500">*</span>}
                                </label>
                                <textarea
                                    value={adminNotes}
                                    onChange={(e) => setAdminNotes(e.target.value)}
                                    className="w-full px-4 py-2 border border-[#3A5C50] rounded-lg focus:ring-2 focus:ring-[#D9B168]"
                                    rows={3}
                                    placeholder={confirmAction.type === 'failed' ? 'Reason for failure (Required)' : 'Optional notes (e.g. Transaction Ref)'}
                                    required={confirmAction.type === 'failed'}
                                />
                            </div>
                        )}

                        <div className="flex justify-end space-x-3 mt-6">
                            <button
                                onClick={() => setShowConfirmDialog(false)}
                                className="px-4 py-2 border border-[#3A5C50] text-[#122F26] rounded-lg hover:bg-sand-50"
                            >
                                Cancel
                            </button>
                            <button
                                onClick={handleConfirm}
                                disabled={confirmAction.type === 'failed' && !adminNotes.trim()}
                                className={`px-4 py-2 rounded-lg text-white ${confirmAction.type === 'processing' ? 'bg-blue-600 hover:bg-blue-700' :
                                    confirmAction.type === 'completed' ? 'bg-green-600 hover:bg-green-700' :
                                        'bg-red-600 hover:bg-red-700'
                                    } disabled:opacity-50`}
                            >
                                Confirm
                            </button>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
