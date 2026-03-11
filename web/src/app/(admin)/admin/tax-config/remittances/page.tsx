'use client';

import { useEffect, useState } from 'react';
import { adminApi } from '@/lib/admin-api';
import { Search, CheckCircle, XCircle } from 'lucide-react';
import toast from 'react-hot-toast';
import ConfirmDialog from '@/components/admin/ConfirmDialog';

type Tab = 'remittances' | 'rates' | 'exemptions' | 'booking-taxes' | 'jurisdictions';

export default function TaxConfigurationManagement() {
    const [activeTab, setActiveTab] = useState<Tab>('remittances');

    const [data, setData] = useState<any[]>([]);
    const [loading, setLoading] = useState(false);


    const [search, setSearch] = useState('');
    const [page, setPage] = useState(1);
    const ITEMS_PER_PAGE = 20;

    const [showConfirm, setShowConfirm] = useState(false);
    const [confirmAction, setConfirmAction] = useState<{ action: string, id: string, extra?: string } | null>(null);

    useEffect(() => {
        loadData();
    }, [activeTab, page, search]);

    const loadData = async () => {
        try {
            setLoading(true);
            const params = { page, search: search.trim() || undefined, per_page: ITEMS_PER_PAGE };
            let res;

            switch (activeTab) {
                case 'remittances':
                    res = await adminApi.getTaxRemittances(params);
                    break;
                case 'rates':
                    res = await adminApi.getTaxRates(params);
                    break;
                case 'exemptions':
                    res = await adminApi.getTaxExemptions(params);
                    break;
                case 'booking-taxes':
                    res = await adminApi.getBookingTaxes(params);
                    break;
                case 'jurisdictions':
                    res = await adminApi.getTaxJurisdictions(params);
                    break;
            }

            setData(res?.results || []);
        } catch (err: any) {
            toast.error('Failed to load data');
        } finally {
            setLoading(false);
        }
    };

    const handleAction = async () => {
        if (!confirmAction) return;

        try {
            if (confirmAction.action === 'mark_remitted') {
                await adminApi.markTaxRemitted(confirmAction.id, { reference: confirmAction.extra || 'Admin Override' });
                toast.success('Marked as remitted');
            } else if (confirmAction.action === 'mark_pending') {
                await adminApi.markTaxPending(confirmAction.id, {});
                toast.success('Marked as pending');
            }
            setShowConfirm(false);
            loadData();
        } catch (err: any) {
            toast.error(err?.response?.data?.error || 'Action failed');
        }
    };

    const RequestConfirm = (action: string, id: string) => {
        setConfirmAction({ action, id });
        setShowConfirm(true);
    };

    return (
        <div className="p-8">
            <div className="mb-8">
                <h1 className="text-3xl font-bold text-[#122F26]">Tax Configuration</h1>
                <p className="text-[#3A5C50] mt-2">Manage tax jurisdictions, rates, remittances, and exemptions</p>
            </div>

            <div className="flex space-x-4 mb-6 border-b border-primary-200 overflow-x-auto">
                {(['remittances', 'jurisdictions', 'rates', 'exemptions', 'booking-taxes'] as Tab[]).map((tab) => (
                    <button
                        key={tab}
                        className={`pb-4 px-4 text-sm font-medium transition-colors border-b-2 whitespace-nowrap ${activeTab === tab
                            ? 'border-[#D9B168] text-[#122F26]'
                            : 'border-transparent text-[#3A5C50] hover:text-[#122F26]'
                            }`}
                        onClick={() => { setActiveTab(tab); setPage(1); }}
                    >
                        {tab.split('-').map(w => w.charAt(0).toUpperCase() + w.slice(1)).join(' ')}
                    </button>
                ))}
            </div>

            <div className="bg-white rounded-lg shadow p-6 mb-6">
                <div className="relative">
                    <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-[#3A5C50] w-5 h-5" />
                    <input
                        type="text"
                        value={search}
                        onChange={(e) => setSearch(e.target.value)}
                        onKeyPress={(e) => e.key === 'Enter' && loadData()}
                        placeholder={`Search ${activeTab.replace('-', ' ')}...`}
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
                    <div className="overflow-x-auto">
                        {activeTab === 'remittances' && (
                            <table className="min-w-full divide-y divide-primary-200">
                                <thead className="bg-sand-50">
                                    <tr>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Jurisdiction</th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Period</th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Amount</th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Status</th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Actions</th>
                                    </tr>
                                </thead>
                                <tbody className="divide-y divide-primary-200">
                                    {data.map(item => (
                                        <tr key={item.id} className="hover:bg-sand-50">
                                            <td className="px-6 py-4 text-sm font-medium text-[#122F26]">Jur #{item.jurisdiction}</td>
                                            <td className="px-6 py-4 text-sm text-[#3A5C50]">{item.period_start} to {item.period_end}</td>
                                            <td className="px-6 py-4 text-sm font-medium text-[#122F26]">${item.total_amount_collected}</td>
                                            <td className="px-6 py-4">
                                                <span className={`inline-flex px-2 py-1 rounded-full text-xs font-medium ${item.status === 'remitted' ? 'bg-green-100 text-green-800' : 'bg-yellow-100 text-yellow-800'}`}>
                                                    {item.status.toUpperCase()}
                                                </span>
                                            </td>
                                            <td className="px-6 py-4 text-sm flex space-x-2">
                                                {item.status === 'pending' ? (
                                                    <button onClick={() => RequestConfirm('mark_remitted', item.id)} className="text-blue-600 hover:text-blue-900 font-medium">Mark Remitted</button>
                                                ) : (
                                                    <button onClick={() => RequestConfirm('mark_pending', item.id)} className="text-gray-600 hover:text-gray-900 font-medium">Mark Pending</button>
                                                )}
                                            </td>
                                        </tr>
                                    ))}
                                </tbody>
                            </table>
                        )}

                        {activeTab === 'jurisdictions' && (
                            <table className="min-w-full divide-y divide-primary-200">
                                <thead className="bg-sand-50">
                                    <tr>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Name</th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Country / Scope</th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Authorities</th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Status</th>
                                    </tr>
                                </thead>
                                <tbody className="divide-y divide-primary-200">
                                    {data.map(item => (
                                        <tr key={item.id} className="hover:bg-sand-50">
                                            <td className="px-6 py-4 text-sm font-medium text-[#122F26]">{item.name}</td>
                                            <td className="px-6 py-4 text-sm text-[#3A5C50]">{item.country} ({item.scope})</td>
                                            <td className="px-6 py-4 text-sm text-[#3A5C50]">{item.tax_authority_name}</td>
                                            <td className="px-6 py-4">
                                                {item.is_active ? <CheckCircle className="w-5 h-5 text-green-500" /> : <XCircle className="w-5 h-5 text-red-500" />}
                                            </td>
                                        </tr>
                                    ))}
                                </tbody>
                            </table>
                        )}

                        {activeTab === 'rates' && (
                            <table className="min-w-full divide-y divide-primary-200">
                                <thead className="bg-sand-50">
                                    <tr>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Jurisdiction</th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Tax Type</th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Rate</th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Status</th>
                                    </tr>
                                </thead>
                                <tbody className="divide-y divide-primary-200">
                                    {data.map(item => (
                                        <tr key={item.id} className="hover:bg-sand-50">
                                            <td className="px-6 py-4 text-sm font-medium text-[#122F26]">Jur #{item.jurisdiction}</td>
                                            <td className="px-6 py-4 text-sm text-[#3A5C50] capitalize">{item.tax_type}</td>
                                            <td className="px-6 py-4 text-sm text-[#122F26] font-medium">{item.rate_percentage}%</td>
                                            <td className="px-6 py-4">
                                                {item.is_active ? <CheckCircle className="w-5 h-5 text-green-500" /> : <XCircle className="w-5 h-5 text-red-500" />}
                                            </td>
                                        </tr>
                                    ))}
                                </tbody>
                            </table>
                        )}

                        {(activeTab === 'exemptions' || activeTab === 'booking-taxes') && (
                            <table className="min-w-full divide-y divide-primary-200">
                                <thead className="bg-sand-50">
                                    <tr>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">ID</th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Created At</th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Details</th>
                                    </tr>
                                </thead>
                                <tbody className="divide-y divide-primary-200">
                                    {data.map(item => (
                                        <tr key={item.id} className="hover:bg-sand-50">
                                            <td className="px-6 py-4 text-sm font-medium text-[#122F26]">#{item.id}</td>
                                            <td className="px-6 py-4 text-sm text-[#3A5C50]">{new Date(item.created_at || item.issued_at || Date.now()).toLocaleDateString()}</td>
                                            <td className="px-6 py-4 text-sm text-[#3A5C50]">
                                                <pre className="text-xs bg-gray-50 p-2 rounded">{JSON.stringify(item, null, 2).substring(0, 100)}...</pre>
                                            </td>
                                        </tr>
                                    ))}
                                </tbody>
                            </table>
                        )}

                        {data.length === 0 && (
                            <div className="px-6 py-8 text-center text-[#3A5C50]">
                                No records found matching your filters.
                            </div>
                        )}
                    </div>
                )}
            </div>

            <ConfirmDialog
                isOpen={showConfirm}
                title="Confirm Action"
                message="Are you sure you want to proceed with this action?"
                onConfirm={handleAction}
                onClose={() => setShowConfirm(false)}
                variant="danger"
                confirmText="Confirm"
            />
        </div>
    );
}
