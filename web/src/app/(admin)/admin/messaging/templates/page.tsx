'use client';

import { useEffect, useState } from 'react';
import { adminApi } from '@/lib/admin-api';
import { Search, Plus, Edit2, Trash2, CheckCircle, XCircle } from 'lucide-react';
import toast from 'react-hot-toast';
import ConfirmDialog from '@/components/admin/ConfirmDialog';

export default function MessageTemplatesManagement() {
    const [templates, setTemplates] = useState<any[]>([]);
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
        loadTemplates();
    }, [page, search]);

    const loadTemplates = async () => {
        try {
            setLoading(true);
            const data = await adminApi.getMessageTemplates({ page, search: search.trim() || undefined, per_page: ITEMS_PER_PAGE });
            setTemplates(data.results || []);
            setTotalCount(data.count || 0);
        } catch (err: any) {
            toast.error('Failed to load templates');
        } finally {
            setLoading(false);
        }
    };

    const handleDelete = async () => {
        if (!confirmId) return;
        try {
            await adminApi.deleteMessageTemplate(confirmId);
            toast.success('Template deleted');
            loadTemplates();
            setShowConfirm(false);
        } catch (err: any) {
            toast.error('Failed to delete template');
        }
    };

    const toggleStatus = async (template: any) => {
        try {
            if (template.is_active) {
                await adminApi.deactivateMessageTemplate(template.id);
                toast.success(`Deactivated ${template.name}`);
            } else {
                await adminApi.activateMessageTemplate(template.id);
                toast.success(`Activated ${template.name}`);
            }
            loadTemplates();
        } catch (err: any) {
            toast.error('Failed to update status');
        }
    };

    const saveTemplate = async (e: React.FormEvent<HTMLFormElement>) => {
        e.preventDefault();
        const formData = new FormData(e.currentTarget);
        const data = {
            name: formData.get('name'),
            template_type: formData.get('template_type'),
            subject: formData.get('subject'),
            body: formData.get('body'),
            is_active: formData.get('is_active') === 'true',
        };

        try {
            if (editingItem) {
                await adminApi.updateMessageTemplate(editingItem.id, data);
                toast.success('Template updated');
            } else {
                await adminApi.createMessageTemplate(data);
                toast.success('Template created');
            }
            setShowModal(false);
            loadTemplates();
        } catch (err: any) {
            toast.error(err?.response?.data?.detail || 'Failed to save template');
        }
    };

    return (
        <div className="p-8">
            <div className="mb-8 flex justify-between items-center">
                <div>
                    <h1 className="text-3xl font-bold text-[#122F26]">Message Templates</h1>
                    <p className="text-[#3A5C50] mt-2">Manage automated booking/inquiry messaging presets</p>
                </div>
                <button
                    onClick={() => {
                        setEditingItem(null);
                        setShowModal(true);
                    }}
                    className="flex items-center space-x-2 px-4 py-2 bg-[#D9B168] text-[#122F26] rounded-lg hover:bg-[#c9a158] transition-colors font-medium"
                >
                    <Plus className="w-5 h-5" />
                    <span>Add Template</span>
                </button>
            </div>

            <div className="bg-white rounded-lg shadow p-6 mb-6">
                <div className="relative">
                    <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-[#3A5C50] w-5 h-5" />
                    <input
                        type="text"
                        value={search}
                        onChange={(e) => setSearch(e.target.value)}
                        onKeyPress={(e) => e.key === 'Enter' && loadTemplates()}
                        placeholder="Search template name or subject..."
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
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">Name / Type</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">Subject</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">Preview</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">Status</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">Actions</th>
                            </tr>
                        </thead>
                        <tbody className="divide-y divide-primary-200">
                            {templates.map(tpl => (
                                <tr key={tpl.id} className="hover:bg-sand-50 transition-colors">
                                    <td className="px-6 py-4 whitespace-nowrap">
                                        <div className="font-medium text-[#122F26]">{tpl.name}</div>
                                        <div className="text-xs text-[#3A5C50] capitalize">{tpl.template_type.replace('_', ' ')}</div>
                                    </td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-[#122F26] max-w-[200px] truncate" title={tpl.subject}>
                                        {tpl.subject || <span className="text-gray-400 italic">No Subject</span>}
                                    </td>
                                    <td className="px-6 py-4 text-sm text-[#3A5C50] max-w-[300px] truncate" title={tpl.body}>
                                        {tpl.body}
                                    </td>
                                    <td className="px-6 py-4 whitespace-nowrap">
                                        <button
                                            onClick={() => toggleStatus(tpl)}
                                            className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium transition-colors ${tpl.is_active ? 'bg-green-100 text-green-800 hover:bg-green-200' : 'bg-gray-100 text-gray-800 hover:bg-gray-200'
                                                }`}
                                        >
                                            {tpl.is_active ? 'Active' : 'Inactive'}
                                        </button>
                                    </td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm font-medium flex space-x-3">
                                        <button
                                            onClick={() => {
                                                setEditingItem(tpl);
                                                setShowModal(true);
                                            }}
                                            className="text-blue-600 hover:text-blue-900 transition-colors"
                                        >
                                            <Edit2 className="w-5 h-5" />
                                        </button>
                                        <button
                                            onClick={() => {
                                                setConfirmId(tpl.id);
                                                setShowConfirm(true);
                                            }}
                                            className="text-red-600 hover:text-red-900 transition-colors"
                                        >
                                            <Trash2 className="w-5 h-5" />
                                        </button>
                                    </td>
                                </tr>
                            ))}
                            {templates.length === 0 && (
                                <tr>
                                    <td colSpan={5} className="px-6 py-8 text-center text-[#3A5C50]">
                                        No message templates found.
                                    </td>
                                </tr>
                            )}
                        </tbody>
                    </table>
                )}
            </div>

            {showModal && (
                <div className="fixed inset-0 z-50 flex items-center justify-center bg-black bg-opacity-50">
                    <div className="bg-white rounded-lg shadow-xl p-6 w-full max-w-2xl max-h-[90vh] overflow-y-auto">
                        <h3 className="text-xl font-bold text-[#122F26] mb-6 border-b pb-2 border-primary-100">
                            {editingItem ? 'Edit Template' : 'Add Template'}
                        </h3>
                        <form onSubmit={saveTemplate} className="space-y-5">
                            <div className="grid grid-cols-2 gap-4">
                                <div>
                                    <label className="block text-sm font-medium text-[#122F26] mb-1">Name</label>
                                    <input
                                        required
                                        type="text"
                                        name="name"
                                        defaultValue={editingItem?.name || ''}
                                        className="w-full px-4 py-2 border border-primary-200 rounded-lg focus:ring-2 focus:ring-[#D9B168]"
                                    />
                                </div>
                                <div>
                                    <label className="block text-sm font-medium text-[#122F26] mb-1">Template Type</label>
                                    <select
                                        name="template_type"
                                        defaultValue={editingItem?.template_type || 'booking_inquiry'}
                                        className="w-full px-4 py-2 border border-primary-200 rounded-lg focus:ring-2 focus:ring-[#D9B168]"
                                    >
                                        <option value="booking_inquiry">Booking Inquiry</option>
                                        <option value="booking_confirmation">Booking Confirmation</option>
                                        <option value="cancellation">Cancellation</option>
                                        <option value="review_request">Review Request</option>
                                        <option value="welcome">Welcome</option>
                                        <option value="custom">Custom</option>
                                    </select>
                                </div>
                            </div>

                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-1">Subject (Optional)</label>
                                <input
                                    type="text"
                                    name="subject"
                                    defaultValue={editingItem?.subject || ''}
                                    className="w-full px-4 py-2 border border-primary-200 rounded-lg focus:ring-2 focus:ring-[#D9B168]"
                                />
                            </div>

                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-1">Body Text</label>
                                <p className="text-xs text-gray-500 mb-2">Available Variables: {'{guest_name}'}, {'{host_name}'}, {'{property_title}'}, {'{booking_id}'}</p>
                                <textarea
                                    required
                                    name="body"
                                    rows={6}
                                    defaultValue={editingItem?.body || ''}
                                    className="w-full px-4 py-2 border border-primary-200 rounded-lg focus:ring-2 focus:ring-[#D9B168]"
                                />
                            </div>

                            <div className="flex items-center">
                                <input
                                    type="checkbox"
                                    name="is_active"
                                    value="true"
                                    defaultChecked={editingItem ? editingItem.is_active : true}
                                    id="is_active_check"
                                    className="w-4 h-4 text-[#D9B168] bg-gray-100 border-gray-300 rounded focus:ring-[#D9B168]"
                                />
                                <label htmlFor="is_active_check" className="ml-2 text-sm font-medium text-[#122F26]">
                                    Active (Ready for Use)
                                </label>
                            </div>

                            <div className="flex justify-end space-x-3 pt-4 border-t border-primary-100">
                                <button
                                    type="button"
                                    onClick={() => setShowModal(false)}
                                    className="px-4 py-2 border border-primary-200 text-[#122F26] rounded-lg hover:bg-sand-50 transition-colors font-medium"
                                >
                                    Cancel
                                </button>
                                <button
                                    type="submit"
                                    className="px-4 py-2 bg-[#D9B168] text-[#122F26] rounded-lg font-medium hover:bg-[#c9a158] transition-colors"
                                >
                                    Save Template
                                </button>
                            </div>
                        </form>
                    </div>
                </div>
            )}

            <ConfirmDialog
                isOpen={showConfirm}
                title="Delete Template"
                message="Are you sure you want to delete this template? Any automated messages relying on it will fail."
                onConfirm={handleDelete}
                onClose={() => setShowConfirm(false)}
                variant="danger"
                confirmText="Delete"
            />
        </div>
    );
}
