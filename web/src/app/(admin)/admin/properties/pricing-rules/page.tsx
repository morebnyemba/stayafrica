'use client';

import { useEffect, useState } from 'react';
import { adminApi } from '@/lib/admin-api';
import { Search, Plus, Edit2, Trash2, CheckCircle, XCircle } from 'lucide-react';
import toast from 'react-hot-toast';
import ConfirmDialog from '@/components/admin/ConfirmDialog';

export default function PricingRulesManagement() {
    const [activeTab, setActiveTab] = useState<'rules' | 'fees'>('rules');

    // Rules state
    const [rules, setRules] = useState<any[]>([]);
    const [rulesLoading, setRulesLoading] = useState(false);
    const [rulesTotal, setRulesTotal] = useState(0);

    // Fees state
    const [fees, setFees] = useState<any[]>([]);
    const [feesLoading, setFeesLoading] = useState(false);
    const [feesTotal, setFeesTotal] = useState(0);

    // Shared state
    const [search, setSearch] = useState('');
    const [page, setPage] = useState(1);
    const ITEMS_PER_PAGE = 20;

    // Dialog state
    const [showConfirm, setShowConfirm] = useState(false);
    const [confirmAction, setConfirmAction] = useState<{ type: 'deleteRule' | 'deleteFee'; id: string } | null>(null);

    // Modal State
    const [showRuleModal, setShowRuleModal] = useState(false);
    const [showFeeModal, setShowFeeModal] = useState(false);
    const [editingItem, setEditingItem] = useState<any>(null);

    useEffect(() => {
        if (activeTab === 'rules') {
            loadRules();
        } else {
            loadFees();
        }
    }, [activeTab, page, search]);

    const loadRules = async () => {
        try {
            setRulesLoading(true);
            const data = await adminApi.getPricingRules({ page, search: search.trim() || undefined, per_page: ITEMS_PER_PAGE });
            setRules(data.results || []);
            setRulesTotal(data.count || 0);
        } catch (err: any) {
            toast.error(err?.response?.data?.detail || 'Failed to load pricing rules');
        } finally {
            setRulesLoading(false);
        }
    };

    const loadFees = async () => {
        try {
            setFeesLoading(true);
            const data = await adminApi.getPropertyFees({ page, search: search.trim() || undefined, per_page: ITEMS_PER_PAGE });
            setFees(data.results || []);
            setFeesTotal(data.count || 0);
        } catch (err: any) {
            toast.error(err?.response?.data?.detail || 'Failed to load property fees');
        } finally {
            setFeesLoading(false);
        }
    };

    const handleDelete = async () => {
        if (!confirmAction) return;
        try {
            if (confirmAction.type === 'deleteRule') {
                await adminApi.deletePricingRule(confirmAction.id);
                toast.success('Pricing rule deleted');
                loadRules();
            } else {
                await adminApi.deletePropertyFee(confirmAction.id);
                toast.success('Property fee deleted');
                loadFees();
            }
            setShowConfirm(false);
        } catch (err: any) {
            toast.error('Failed to delete item');
        }
    };

    const saveRule = async (e: React.FormEvent<HTMLFormElement>) => {
        e.preventDefault();
        const formData = new FormData(e.currentTarget);
        const data = {
            property: formData.get('property'),
            name: formData.get('name'),
            rule_type: formData.get('rule_type'),
            adjustment_type: formData.get('adjustment_type'),
            adjustment_value: formData.get('adjustment_value'),
            is_active: formData.get('is_active') === 'true',
            priority: parseInt(formData.get('priority') as string) || 0,
            start_date: formData.get('start_date') || null,
            end_date: formData.get('end_date') || null,
            min_nights: formData.get('min_nights') ? parseInt(formData.get('min_nights') as string) : null,
            max_nights: formData.get('max_nights') ? parseInt(formData.get('max_nights') as string) : null,
        };

        try {
            if (editingItem) {
                await adminApi.updatePricingRule(editingItem.id, data);
                toast.success('Rule updated');
            } else {
                await adminApi.createPricingRule(data);
                toast.success('Rule created');
            }
            setShowRuleModal(false);
            loadRules();
        } catch (err: any) {
            toast.error(err?.response?.data?.detail || 'Failed to save rule');
        }
    };

    const saveFee = async (e: React.FormEvent<HTMLFormElement>) => {
        e.preventDefault();
        const formData = new FormData(e.currentTarget);
        const data = {
            property: formData.get('property'),
            name: formData.get('name'),
            fee_type: formData.get('fee_type'),
            amount: formData.get('amount'),
            charge_type: formData.get('charge_type'),
            is_active: formData.get('is_active') === 'true',
            is_mandatory: formData.get('is_mandatory') === 'true',
            applies_after_guests: formData.get('applies_after_guests') ? parseInt(formData.get('applies_after_guests') as string) : null,
        };

        try {
            if (editingItem) {
                await adminApi.updatePropertyFee(editingItem.id, data);
                toast.success('Fee updated');
            } else {
                await adminApi.createPropertyFee(data);
                toast.success('Fee created');
            }
            setShowFeeModal(false);
            loadFees();
        } catch (err: any) {
            toast.error(err?.response?.data?.detail || 'Failed to save fee');
        }
    };

    return (
        <div className="p-8">
            <div className="mb-8 flex justify-between items-center">
                <div>
                    <h1 className="text-3xl font-bold text-[#122F26]">Pricing & Fees</h1>
                    <p className="text-[#3A5C50] mt-2">Manage property pricing rules and dynamic fees</p>
                </div>
                <button
                    onClick={() => {
                        setEditingItem(null);
                        activeTab === 'rules' ? setShowRuleModal(true) : setShowFeeModal(true);
                    }}
                    className="flex items-center space-x-2 px-4 py-2 bg-[#D9B168] text-[#122F26] rounded-lg hover:bg-[#c9a158] transition-colors font-medium"
                >
                    <Plus className="w-5 h-5" />
                    <span>Add {activeTab === 'rules' ? 'Rule' : 'Fee'}</span>
                </button>
            </div>

            <div className="flex space-x-4 mb-6 border-b border-primary-200">
                <button
                    className={`pb-4 px-4 text-sm font-medium transition-colors border-b-2 ${activeTab === 'rules'
                            ? 'border-[#D9B168] text-[#122F26]'
                            : 'border-transparent text-[#3A5C50] hover:text-[#122F26]'
                        }`}
                    onClick={() => { setActiveTab('rules'); setPage(1); }}
                >
                    Pricing Rules
                </button>
                <button
                    className={`pb-4 px-4 text-sm font-medium transition-colors border-b-2 ${activeTab === 'fees'
                            ? 'border-[#D9B168] text-[#122F26]'
                            : 'border-transparent text-[#3A5C50] hover:text-[#122F26]'
                        }`}
                    onClick={() => { setActiveTab('fees'); setPage(1); }}
                >
                    Property Fees
                </button>
            </div>

            <div className="bg-white rounded-lg shadow p-6 mb-6">
                <div className="relative">
                    <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-[#3A5C50] w-5 h-5" />
                    <input
                        type="text"
                        value={search}
                        onChange={(e) => setSearch(e.target.value)}
                        onKeyPress={(e) => e.key === 'Enter' && (activeTab === 'rules' ? loadRules() : loadFees())}
                        placeholder={`Search ${activeTab}...`}
                        className="w-full pl-10 pr-4 py-2 border border-[#3A5C50] rounded-lg focus:ring-2 focus:ring-[#D9B168]"
                    />
                </div>
            </div>

            <div className="bg-white rounded-lg shadow overflow-hidden">
                {activeTab === 'rules' ? (
                    <table className="min-w-full divide-y divide-primary-200">
                        <thead className="bg-sand-50">
                            <tr>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Property</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Rule Details</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Adjustment</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Status</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Actions</th>
                            </tr>
                        </thead>
                        <tbody className="divide-y divide-primary-200">
                            {rules.map(rule => (
                                <tr key={rule.id} className="hover:bg-sand-50">
                                    <td className="px-6 py-4 text-sm font-medium text-[#122F26]">Prop #{rule.property}</td>
                                    <td className="px-6 py-4 text-sm text-[#3A5C50]">
                                        <div className="font-medium text-[#122F26]">{rule.name}</div>
                                        <div className="text-xs uppercase">{rule.rule_type}</div>
                                    </td>
                                    <td className="px-6 py-4 text-sm text-[#122F26] font-medium">
                                        {rule.adjustment_type === 'percentage' ? `${rule.adjustment_value}%` : `$${rule.adjustment_value}`}
                                    </td>
                                    <td className="px-6 py-4">
                                        {rule.is_active ? <CheckCircle className="w-5 h-5 text-green-500" /> : <XCircle className="w-5 h-5 text-red-500" />}
                                    </td>
                                    <td className="px-6 py-4 flex space-x-2">
                                        <button onClick={() => { setEditingItem(rule); setShowRuleModal(true); }} className="text-blue-600 hover:text-blue-900"><Edit2 className="w-5 h-5" /></button>
                                        <button onClick={() => { setConfirmAction({ type: 'deleteRule', id: rule.id }); setShowConfirm(true); }} className="text-red-600 hover:text-red-900"><Trash2 className="w-5 h-5" /></button>
                                    </td>
                                </tr>
                            ))}
                            {rules.length === 0 && <tr><td colSpan={5} className="px-6 py-8 text-center text-[#3A5C50]">No rules found</td></tr>}
                        </tbody>
                    </table>
                ) : (
                    <table className="min-w-full divide-y divide-primary-200">
                        <thead className="bg-sand-50">
                            <tr>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Property</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Fee Details</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Amount</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Status</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Actions</th>
                            </tr>
                        </thead>
                        <tbody className="divide-y divide-primary-200">
                            {fees.map(fee => (
                                <tr key={fee.id} className="hover:bg-sand-50">
                                    <td className="px-6 py-4 text-sm font-medium text-[#122F26]">Prop #{fee.property}</td>
                                    <td className="px-6 py-4 text-sm text-[#3A5C50]">
                                        <div className="font-medium text-[#122F26]">{fee.name}</div>
                                        <div className="text-xs uppercase">{fee.fee_type} ({fee.charge_type})</div>
                                    </td>
                                    <td className="px-6 py-4 text-sm text-[#122F26] font-medium">${fee.amount}</td>
                                    <td className="px-6 py-4">
                                        {fee.is_active ? <CheckCircle className="w-5 h-5 text-green-500" /> : <XCircle className="w-5 h-5 text-red-500" />}
                                    </td>
                                    <td className="px-6 py-4 flex space-x-2">
                                        <button onClick={() => { setEditingItem(fee); setShowFeeModal(true); }} className="text-blue-600 hover:text-blue-900"><Edit2 className="w-5 h-5" /></button>
                                        <button onClick={() => { setConfirmAction({ type: 'deleteFee', id: fee.id }); setShowConfirm(true); }} className="text-red-600 hover:text-red-900"><Trash2 className="w-5 h-5" /></button>
                                    </td>
                                </tr>
                            ))}
                            {fees.length === 0 && <tr><td colSpan={5} className="px-6 py-8 text-center text-[#3A5C50]">No fees found</td></tr>}
                        </tbody>
                    </table>
                )}
            </div>

            {/* Basic Rule Modal */}
            {showRuleModal && (
                <div className="fixed inset-0 z-50 flex items-center justify-center bg-black bg-opacity-50 mt-10">
                    <div className="bg-white rounded-lg shadow-xl p-6 w-full max-w-2xl max-h-[90vh] overflow-y-auto">
                        <h3 className="text-lg font-semibold text-[#122F26] mb-4">{editingItem ? 'Edit Rule' : 'Add Rule'}</h3>
                        <form onSubmit={saveRule} className="grid grid-cols-2 gap-4">
                            <div className="col-span-2">
                                <label className="block text-sm font-medium text-[#122F26] mb-1">Property ID</label>
                                <input required type="text" name="property" defaultValue={editingItem?.property || ''} className="w-full px-4 py-2 border rounded-lg" />
                            </div>
                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-1">Name</label>
                                <input required type="text" name="name" defaultValue={editingItem?.name || ''} className="w-full px-4 py-2 border rounded-lg" />
                            </div>
                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-1">Rule Type</label>
                                <select name="rule_type" defaultValue={editingItem?.rule_type || 'seasonal'} className="w-full px-4 py-2 border rounded-lg">
                                    <option value="seasonal">Seasonal</option>
                                    <option value="weekend">Weekend</option>
                                    <option value="length_discount">Length Discount</option>
                                    <option value="early_bird">Early Bird</option>
                                    <option value="last_minute">Last Minute</option>
                                </select>
                            </div>
                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-1">Adjustment Type</label>
                                <select name="adjustment_type" defaultValue={editingItem?.adjustment_type || 'percentage'} className="w-full px-4 py-2 border rounded-lg">
                                    <option value="percentage">Percentage (%)</option>
                                    <option value="fixed">Fixed Amount</option>
                                </select>
                            </div>
                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-1">Adjustment Value</label>
                                <input required type="number" step="0.01" name="adjustment_value" defaultValue={editingItem?.adjustment_value || ''} className="w-full px-4 py-2 border rounded-lg" />
                            </div>
                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-1">Start Date</label>
                                <input type="date" name="start_date" defaultValue={editingItem?.start_date || ''} className="w-full px-4 py-2 border rounded-lg" />
                            </div>
                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-1">End Date</label>
                                <input type="date" name="end_date" defaultValue={editingItem?.end_date || ''} className="w-full px-4 py-2 border rounded-lg" />
                            </div>
                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-1">Is Active</label>
                                <select name="is_active" defaultValue={editingItem ? String(editingItem.is_active) : 'true'} className="w-full px-4 py-2 border rounded-lg">
                                    <option value="true">Yes</option>
                                    <option value="false">No</option>
                                </select>
                            </div>
                            <div className="col-span-2 flex justify-end space-x-3 mt-4">
                                <button type="button" onClick={() => setShowRuleModal(false)} className="px-4 py-2 border text-[#122F26] rounded-lg">Cancel</button>
                                <button type="submit" className="px-4 py-2 bg-[#D9B168] text-[#122F26] rounded-lg">Save</button>
                            </div>
                        </form>
                    </div>
                </div>
            )}

            {/* Basic Fee Modal */}
            {showFeeModal && (
                <div className="fixed inset-0 z-50 flex items-center justify-center bg-black bg-opacity-50 mt-10">
                    <div className="bg-white rounded-lg shadow-xl p-6 w-full max-w-2xl max-h-[90vh] overflow-y-auto">
                        <h3 className="text-lg font-semibold text-[#122F26] mb-4">{editingItem ? 'Edit Fee' : 'Add Fee'}</h3>
                        <form onSubmit={saveFee} className="grid grid-cols-2 gap-4">
                            <div className="col-span-2">
                                <label className="block text-sm font-medium text-[#122F26] mb-1">Property ID</label>
                                <input required type="text" name="property" defaultValue={editingItem?.property || ''} className="w-full px-4 py-2 border rounded-lg" />
                            </div>
                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-1">Name</label>
                                <input required type="text" name="name" defaultValue={editingItem?.name || ''} className="w-full px-4 py-2 border rounded-lg" />
                            </div>
                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-1">Fee Type</label>
                                <select name="fee_type" defaultValue={editingItem?.fee_type || 'cleaning'} className="w-full px-4 py-2 border rounded-lg">
                                    <option value="cleaning">Cleaning</option>
                                    <option value="service">Service</option>
                                    <option value="pet">Pet</option>
                                    <option value="extra_guest">Extra Guest</option>
                                    <option value="resort">Resort</option>
                                    <option value="parking">Parking</option>
                                    <option value="linen">Linen</option>
                                </select>
                            </div>
                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-1">Charge Type</label>
                                <select name="charge_type" defaultValue={editingItem?.charge_type || 'per_booking'} className="w-full px-4 py-2 border rounded-lg">
                                    <option value="per_booking">Per Booking</option>
                                    <option value="per_night">Per Night</option>
                                    <option value="per_guest">Per Guest</option>
                                </select>
                            </div>
                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-1">Amount</label>
                                <input required type="number" step="0.01" name="amount" defaultValue={editingItem?.amount || ''} className="w-full px-4 py-2 border rounded-lg" />
                            </div>
                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-1">Is Active</label>
                                <select name="is_active" defaultValue={editingItem ? String(editingItem.is_active) : 'true'} className="w-full px-4 py-2 border rounded-lg">
                                    <option value="true">Yes</option>
                                    <option value="false">No</option>
                                </select>
                            </div>
                            <div className="col-span-2 flex justify-end space-x-3 mt-4">
                                <button type="button" onClick={() => setShowFeeModal(false)} className="px-4 py-2 border text-[#122F26] rounded-lg">Cancel</button>
                                <button type="submit" className="px-4 py-2 bg-[#D9B168] text-[#122F26] rounded-lg">Save</button>
                            </div>
                        </form>
                    </div>
                </div>
            )}

            <ConfirmDialog
                isOpen={showConfirm}
                title="Delete Item"
                message="Are you sure you want to delete this? This action cannot be undone."
                onConfirm={handleDelete}
                onClose={() => setShowConfirm(false)}
                variant="danger"
                confirmText="Delete"
            />
        </div>
    );
}
