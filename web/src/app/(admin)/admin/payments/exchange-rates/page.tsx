'use client';

import { useEffect, useState } from 'react';
import { adminApi } from '@/lib/admin-api';
import { Search, Plus, Edit2, Trash2, CheckCircle, XCircle } from 'lucide-react';
import toast from 'react-hot-toast';
import ConfirmDialog from '@/components/admin/ConfirmDialog';

export default function ExchangeRatesManagement() {
    const [rates, setRates] = useState<any[]>([]);
    const [loading, setLoading] = useState(true);
    const [search, setSearch] = useState('');
    const [page, setPage] = useState(1);
    const [totalCount, setTotalCount] = useState(0);
    const ITEMS_PER_PAGE = 20;

    const [showConfirm, setShowConfirm] = useState(false);
    const [confirmId, setConfirmId] = useState<string | null>(null);

    const [showModal, setShowModal] = useState(false);
    const [editingItem, setEditingItem] = useState<any>(null);

    useEffect(() => {
        loadRates();
    }, [page, search]);

    const loadRates = async () => {
        try {
            setLoading(true);
            const data = await adminApi.getExchangeRates({ page, search: search.trim() || undefined, per_page: ITEMS_PER_PAGE });
            setRates(data.results || []);
            setTotalCount(data.count || 0);
        } catch (err: any) {
            toast.error(err?.response?.data?.detail || 'Failed to load exchange rates');
        } finally {
            setLoading(false);
        }
    };

    const handleDelete = async () => {
        if (!confirmId) return;
        try {
            await adminApi.deleteExchangeRate(confirmId);
            toast.success('Exchange rate deleted');
            loadRates();
            setShowConfirm(false);
        } catch (err: any) {
            toast.error('Failed to delete exchange rate');
        }
    };

    const saveRate = async (e: React.FormEvent<HTMLFormElement>) => {
        e.preventDefault();
        const formData = new FormData(e.currentTarget);
        const data = {
            from_currency: formData.get('from_currency'),
            to_currency: formData.get('to_currency'),
            rate: formData.get('rate'),
            is_active: formData.get('is_active') === 'true',
        };

        try {
            if (editingItem) {
                await adminApi.updateExchangeRate(editingItem.id, data);
                toast.success('Exchange rate updated');
            } else {
                await adminApi.createExchangeRate(data);
                toast.success('Exchange rate created');
            }
            setShowModal(false);
            loadRates();
        } catch (err: any) {
            toast.error(err?.response?.data?.detail || 'Failed to save exchange rate');
        }
    };

    return (
        <div className="p-8">
            <div className="mb-8 flex justify-between items-center">
                <div>
                    <h1 className="text-3xl font-bold text-[#122F26]">Currency Exchange Rates</h1>
                    <p className="text-[#3A5C50] mt-2">Manage multi-currency exchange rates</p>
                </div>
                <button
                    onClick={() => {
                        setEditingItem(null);
                        setShowModal(true);
                    }}
                    className="flex items-center space-x-2 px-4 py-2 bg-[#D9B168] text-[#122F26] rounded-lg hover:bg-[#c9a158] transition-colors font-medium"
                >
                    <Plus className="w-5 h-5" />
                    <span>Add Rate</span>
                </button>
            </div>

            <div className="bg-white rounded-lg shadow p-6 mb-6">
                <div className="relative">
                    <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-[#3A5C50] w-5 h-5" />
                    <input
                        type="text"
                        value={search}
                        onChange={(e) => setSearch(e.target.value)}
                        onKeyPress={(e) => e.key === 'Enter' && loadRates()}
                        placeholder="Search by currency code (e.g., USD)..."
                        className="w-full pl-10 pr-4 py-2 border border-[#3A5C50] rounded-lg focus:ring-2 focus:ring-[#D9B168]"
                    />
                </div>
            </div>

            <div className="bg-white rounded-lg shadow overflow-hidden">
                {loading ? (
                    <div className="flex items-center justify-center h-64">
                        <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-[#D9B168]"></div>
                    </div>
                ) : (
                    <table className="min-w-full divide-y divide-primary-200">
                        <thead className="bg-sand-50">
                            <tr>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">Pair</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">Rate</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">Last Updated</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">Status</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">Actions</th>
                            </tr>
                        </thead>
                        <tbody className="divide-y divide-primary-200">
                            {rates.map(rate => (
                                <tr key={rate.id} className="hover:bg-sand-50">
                                    <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-[#122F26]">
                                        {rate.from_currency} → {rate.to_currency}
                                    </td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-[#122F26]">
                                        {rate.rate}
                                    </td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50]">
                                        {new Date(rate.last_updated).toLocaleString()}
                                    </td>
                                    <td className="px-6 py-4 whitespace-nowrap">
                                        {rate.is_active ? (
                                            <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
                                                Active
                                            </span>
                                        ) : (
                                            <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-gray-100 text-gray-800">
                                                Inactive
                                            </span>
                                        )}
                                    </td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm font-medium flex space-x-3">
                                        <button
                                            onClick={() => {
                                                setEditingItem(rate);
                                                setShowModal(true);
                                            }}
                                            className="text-blue-600 hover:text-blue-900"
                                        >
                                            <Edit2 className="w-5 h-5" />
                                        </button>
                                        <button
                                            onClick={() => {
                                                setConfirmId(rate.id);
                                                setShowConfirm(true);
                                            }}
                                            className="text-red-600 hover:text-red-900"
                                        >
                                            <Trash2 className="w-5 h-5" />
                                        </button>
                                    </td>
                                </tr>
                            ))}
                            {rates.length === 0 && (
                                <tr>
                                    <td colSpan={5} className="px-6 py-8 text-center text-[#3A5C50]">
                                        No exchange rates found.
                                    </td>
                                </tr>
                            )}
                        </tbody>
                    </table>
                )}
            </div>

            {/* Modal */}
            {showModal && (
                <div className="fixed inset-0 z-50 flex items-center justify-center bg-black bg-opacity-50">
                    <div className="bg-white rounded-lg shadow-xl p-6 w-full max-w-md">
                        <h3 className="text-lg font-semibold text-[#122F26] mb-4">
                            {editingItem ? 'Edit Exchange Rate' : 'Add Exchange Rate'}
                        </h3>
                        <form onSubmit={saveRate} className="space-y-4">
                            <div className="flex space-x-4">
                                <div className="flex-1">
                                    <label className="block text-sm font-medium text-[#122F26] mb-1">From Currency</label>
                                    <input
                                        required
                                        type="text"
                                        name="from_currency"
                                        maxLength={3}
                                        defaultValue={editingItem?.from_currency || ''}
                                        placeholder="USD"
                                        className="w-full px-4 py-2 border rounded-lg uppercase"
                                    />
                                </div>
                                <div className="flex-1">
                                    <label className="block text-sm font-medium text-[#122F26] mb-1">To Currency</label>
                                    <input
                                        required
                                        type="text"
                                        name="to_currency"
                                        maxLength={3}
                                        defaultValue={editingItem?.to_currency || ''}
                                        placeholder="EUR"
                                        className="w-full px-4 py-2 border rounded-lg uppercase"
                                    />
                                </div>
                            </div>
                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-1">Rate</label>
                                <input
                                    required
                                    type="number"
                                    step="0.000001"
                                    name="rate"
                                    defaultValue={editingItem?.rate || ''}
                                    placeholder="0.920550"
                                    className="w-full px-4 py-2 border rounded-lg"
                                />
                            </div>
                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-1">Status</label>
                                <select
                                    name="is_active"
                                    defaultValue={editingItem ? String(editingItem.is_active) : 'true'}
                                    className="w-full px-4 py-2 border rounded-lg"
                                >
                                    <option value="true">Active</option>
                                    <option value="false">Inactive</option>
                                </select>
                            </div>

                            <div className="flex justify-end space-x-3 mt-6">
                                <button
                                    type="button"
                                    onClick={() => setShowModal(false)}
                                    className="px-4 py-2 border text-[#122F26] rounded-lg hover:bg-sand-50"
                                >
                                    Cancel
                                </button>
                                <button
                                    type="submit"
                                    className="px-4 py-2 bg-[#D9B168] text-[#122F26] rounded-lg font-medium hover:bg-[#c9a158]"
                                >
                                    Save
                                </button>
                            </div>
                        </form>
                    </div>
                </div>
            )}

            <ConfirmDialog
                isOpen={showConfirm}
                title="Delete Exchange Rate"
                message="Are you sure you want to delete this exchange rate? This could affect live multi-currency conversions."
                onConfirm={handleDelete}
                onClose={() => setShowConfirm(false)}
                variant="danger"
                confirmText="Delete"
            />
        </div>
    );
}
