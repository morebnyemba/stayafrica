'use client';

import { useEffect, useState } from 'react';
import { adminApi } from '@/lib/admin-api';
import { Search, CheckCircle, Star } from 'lucide-react';
import toast from 'react-hot-toast';
import ConfirmDialog from '@/components/admin/ConfirmDialog';

export default function BankAccountsManagement() {
    const [bankAccounts, setBankAccounts] = useState<any[]>([]);
    const [loading, setLoading] = useState(true);
    const [search, setSearch] = useState('');
    const [page, setPage] = useState(1);
    const [totalCount, setTotalCount] = useState(0);

    const [showConfirmDialog, setShowConfirmDialog] = useState(false);
    const [confirmAction, setConfirmAction] = useState<{ id: string } | null>(null);
    const ITEMS_PER_PAGE = 20;

    useEffect(() => {
        loadBankAccounts();
    }, [page, search]);

    const loadBankAccounts = async () => {
        try {
            setLoading(true);
            const data = await adminApi.getBankAccounts({
                page,
                search: search.trim() || undefined,
                per_page: ITEMS_PER_PAGE,
            });
            setBankAccounts(data.results || []);
            setTotalCount(data.count || 0);
        } catch (err: any) {
            const errorMsg = err?.response?.data?.detail || 'Failed to load bank accounts';
            toast.error(errorMsg);
            console.error('Bank accounts load error:', err);
        } finally {
            setLoading(false);
        }
    };

    const handleSearch = () => {
        setPage(1);
    };

    const handleActionClick = (id: string) => {
        setConfirmAction({ id });
        setShowConfirmDialog(true);
    };

    const handleConfirm = async () => {
        if (!confirmAction) return;

        try {
            await adminApi.markBankAccountPrimary(confirmAction.id);
            toast.success('Bank account marked as primary');
            setShowConfirmDialog(false);
            loadBankAccounts();
        } catch (err: any) {
            toast.error(err?.response?.data?.error || `Failed to mark bank account as primary`);
            console.error(err);
        }
    };

    return (
        <div className="p-8">
            <div className="mb-8">
                <h1 className="text-3xl font-bold text-[#122F26]">Bank Accounts Management</h1>
                <p className="text-[#3A5C50] mt-2">View and manage user bank accounts for payouts</p>
            </div>

            {/* Filters and Search */}
            <div className="bg-white rounded-lg shadow p-6 mb-6">
                <div className="w-full">
                    <label className="block text-sm font-medium text-[#122F26] mb-2">
                        Search Bank Accounts
                    </label>
                    <div className="flex space-x-2">
                        <div className="flex-1 relative">
                            <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-[#3A5C50] w-5 h-5" />
                            <input
                                type="text"
                                value={search}
                                onChange={(e) => setSearch(e.target.value)}
                                onKeyPress={(e) => e.key === 'Enter' && handleSearch()}
                                placeholder="Search by account number, bank name, or user reference..."
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
            </div>

            {/* Bank Accounts Table */}
            <div className="bg-white rounded-lg shadow overflow-hidden">
                {loading ? (
                    <div className="flex items-center justify-center h-64">
                        <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-[#D9B168]"></div>
                    </div>
                ) : (
                    <>
                        <div className="overflow-x-auto">
                            <table className="min-w-full divide-y divide-primary-200 dark:divide-primary-700">
                                <thead className="bg-sand-50 dark:bg-primary-900">
                                    <tr>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                                            User
                                        </th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                                            Bank Name
                                        </th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                                            Account Number
                                        </th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                                            Account Name
                                        </th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                                            Status
                                        </th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                                            Actions
                                        </th>
                                    </tr>
                                </thead>
                                <tbody className="bg-white divide-y divide-primary-200 dark:divide-primary-700">
                                    {bankAccounts.map((item) => {
                                        const userEmail = item.user?.email || `User #${item.user_id}`;
                                        return (
                                            <tr key={item.id} className="hover:bg-sand-50 dark:hover:bg-primary-800">
                                                <td className="px-6 py-4 whitespace-nowrap">
                                                    <div className="text-sm font-medium text-[#122F26]">{userEmail}</div>
                                                </td>
                                                <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-[#122F26]">
                                                    {item.bank_name}
                                                </td>
                                                <td className="px-6 py-4 whitespace-nowrap">
                                                    <div className="text-sm text-[#122F26]">
                                                        {item.account_number}
                                                    </div>
                                                </td>
                                                <td className="px-6 py-4 text-sm text-[#3A5C50] max-w-xs truncate">
                                                    {item.account_name}
                                                </td>
                                                <td className="px-6 py-4 whitespace-nowrap">
                                                    {item.is_primary ? (
                                                        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
                                                            Primary
                                                        </span>
                                                    ) : (
                                                        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-gray-100 text-gray-800">
                                                            Secondary
                                                        </span>
                                                    )}
                                                </td>
                                                <td className="px-6 py-4 whitespace-nowrap text-sm font-medium">
                                                    <div className="flex items-center space-x-2">
                                                        {!item.is_primary && (
                                                            <button
                                                                onClick={() => handleActionClick(item.id)}
                                                                className="text-blue-600 hover:text-blue-900"
                                                                title="Mark as Primary"
                                                            >
                                                                <Star className="w-5 h-5 border border-blue-600 rounded-full p-0.5" />
                                                            </button>
                                                        )}
                                                        {item.is_primary && (
                                                            <CheckCircle className="w-5 h-5 text-green-500" title="Primary Account" />
                                                        )}
                                                    </div>
                                                </td>
                                            </tr>
                                        );
                                    })}
                                    {bankAccounts.length === 0 && (
                                        <tr>
                                            <td colSpan={6} className="px-6 py-8 text-center text-[#3A5C50]">
                                                No bank accounts found matching your filters.
                                            </td>
                                        </tr>
                                    )}
                                </tbody>
                            </table>
                        </div>

                        {/* Pagination */}
                        {totalCount > 0 && (
                            <div className="bg-sand-50 dark:bg-primary-900 px-6 py-4 flex items-center justify-between border-t">
                                <div className="text-sm text-[#122F26]">
                                    Showing {(page - 1) * ITEMS_PER_PAGE + 1} to {Math.min(page * ITEMS_PER_PAGE, totalCount)} of {totalCount} accounts
                                </div>
                                <div className="flex space-x-2">
                                    <button
                                        onClick={() => setPage(p => Math.max(1, p - 1))}
                                        disabled={page === 1}
                                        className="px-4 py-2 border border-[#3A5C50] rounded-lg text-sm font-medium text-[#122F26] hover:bg-sand-50 dark:hover:bg-primary-800 disabled:opacity-50 disabled:cursor-not-allowed hidden sm:block"
                                    >
                                        Previous
                                    </button>
                                    <button
                                        onClick={() => setPage(p => p + 1)}
                                        disabled={page * ITEMS_PER_PAGE >= totalCount}
                                        className="px-4 py-2 border border-[#3A5C50] rounded-lg text-sm font-medium text-[#122F26] hover:bg-sand-50 dark:hover:bg-primary-800 disabled:opacity-50 disabled:cursor-not-allowed hidden sm:block"
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
                            Mark as Primary Account
                        </h3>

                        <p className="text-sm text-[#3A5C50] mb-4">
                            Are you sure you want to set this as the primary bank account for this user? All future withdrawals will default to this account.
                        </p>

                        <div className="flex justify-end space-x-3 mt-6">
                            <button
                                onClick={() => setShowConfirmDialog(false)}
                                className="px-4 py-2 border border-[#3A5C50] text-[#122F26] rounded-lg hover:bg-sand-50"
                            >
                                Cancel
                            </button>
                            <button
                                onClick={handleConfirm}
                                className="px-4 py-2 rounded-lg text-white bg-blue-600 hover:bg-blue-700"
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
