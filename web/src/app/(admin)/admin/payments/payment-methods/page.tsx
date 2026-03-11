'use client';

import { useEffect, useState } from 'react';
import { adminApi } from '@/lib/admin-api';
import { Search, CreditCard, Smartphone, CheckCircle, Star } from 'lucide-react';
import toast from 'react-hot-toast';

export default function PaymentMethodsManagement() {
    const [methods, setMethods] = useState<any[]>([]);
    const [loading, setLoading] = useState(true);
    const [search, setSearch] = useState('');
    const [page, setPage] = useState(1);
    const [totalCount, setTotalCount] = useState(0);

    const [showConfirmDialog, setShowConfirmDialog] = useState(false);
    const [confirmAction, setConfirmAction] = useState<{ id: string } | null>(null);
    const ITEMS_PER_PAGE = 20;

    useEffect(() => {
        loadMethods();
    }, [page, search]);

    const loadMethods = async () => {
        try {
            setLoading(true);
            const data = await adminApi.getPaymentMethods({
                page,
                search: search.trim() || undefined,
                per_page: ITEMS_PER_PAGE,
            });
            setMethods(data.results || []);
            setTotalCount(data.count || 0);
        } catch (err: any) {
            const errorMsg = err?.response?.data?.detail || 'Failed to load payment methods';
            toast.error(errorMsg);
            console.error('Payment methods load error:', err);
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
            await adminApi.markPaymentMethodDefault(confirmAction.id);
            toast.success('Payment method set as default');
            setShowConfirmDialog(false);
            loadMethods();
        } catch (err: any) {
            toast.error(err?.response?.data?.error || `Failed to set payment method as default`);
            console.error(err);
        }
    };

    const getMethodIcon = (type: string) => {
        switch (type.toLowerCase()) {
            case 'card':
                return <CreditCard className="w-5 h-5 text-blue-500" />;
            case 'mobile':
            case 'mobile_money':
                return <Smartphone className="w-5 h-5 text-green-500" />;
            default:
                return <CreditCard className="w-5 h-5 text-gray-500" />;
        }
    };

    return (
        <div className="p-8">
            <div className="mb-8">
                <h1 className="text-3xl font-bold text-[#122F26]">Payment Methods Management</h1>
                <p className="text-[#3A5C50] mt-2">View and manage user payment methods</p>
            </div>

            {/* Filters and Search */}
            <div className="bg-white rounded-lg shadow p-6 mb-6">
                <div className="w-full">
                    <label className="block text-sm font-medium text-[#122F26] mb-2">
                        Search Payment Methods
                    </label>
                    <div className="flex space-x-2">
                        <div className="flex-1 relative">
                            <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-[#3A5C50] w-5 h-5" />
                            <input
                                type="text"
                                value={search}
                                onChange={(e) => setSearch(e.target.value)}
                                onKeyPress={(e) => e.key === 'Enter' && handleSearch()}
                                placeholder="Search by user or provider..."
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

            {/* Payment Methods Table */}
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
                                            Type
                                        </th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                                            Provider
                                        </th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                                            Details
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
                                    {methods.map((item) => {
                                        const userEmail = item.user?.email || `User #${item.user_id}`;
                                        return (
                                            <tr key={item.id} className="hover:bg-sand-50 dark:hover:bg-primary-800">
                                                <td className="px-6 py-4 whitespace-nowrap">
                                                    <div className="text-sm font-medium text-[#122F26]">{userEmail}</div>
                                                </td>
                                                <td className="px-6 py-4 whitespace-nowrap">
                                                    <div className="flex items-center space-x-2">
                                                        {getMethodIcon(item.method_type)}
                                                        <span className="text-sm text-[#122F26] capitalize">{item.method_type}</span>
                                                    </div>
                                                </td>
                                                <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-[#122F26] capitalize">
                                                    {item.provider}
                                                </td>
                                                <td className="px-6 py-4 text-sm text-[#3A5C50] max-w-xs truncate">
                                                    {item.method_type === 'card' && item.last_four ? `**** **** **** ${item.last_four}` : item.name}
                                                </td>
                                                <td className="px-6 py-4 whitespace-nowrap">
                                                    {item.is_default ? (
                                                        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
                                                            Default
                                                        </span>
                                                    ) : (
                                                        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-gray-100 text-gray-800">
                                                            Secondary
                                                        </span>
                                                    )}
                                                    {item.is_verified && (
                                                        <span className="ml-2 inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-blue-100 text-blue-800">
                                                            Verified
                                                        </span>
                                                    )}
                                                </td>
                                                <td className="px-6 py-4 whitespace-nowrap text-sm font-medium">
                                                    <div className="flex items-center space-x-2">
                                                        {!item.is_default && (
                                                            <button
                                                                onClick={() => handleActionClick(item.id)}
                                                                className="text-blue-600 hover:text-blue-900"
                                                                title="Set as Default"
                                                            >
                                                                <Star className="w-5 h-5 border border-blue-600 rounded-full p-0.5" />
                                                            </button>
                                                        )}
                                                        {item.is_default && (
                                                            <CheckCircle className="w-5 h-5 text-green-500" />
                                                        )}
                                                    </div>
                                                </td>
                                            </tr>
                                        );
                                    })}
                                    {methods.length === 0 && (
                                        <tr>
                                            <td colSpan={6} className="px-6 py-8 text-center text-[#3A5C50]">
                                                No payment methods found matching your filters.
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
                                    Showing {(page - 1) * ITEMS_PER_PAGE + 1} to {Math.min(page * ITEMS_PER_PAGE, totalCount)} of {totalCount} methods
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
                            Set Default Payment Method
                        </h3>

                        <p className="text-sm text-[#3A5C50] mb-4">
                            Are you sure you want to set this as the default payment method for this user? Future bookings will use this method by default.
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
