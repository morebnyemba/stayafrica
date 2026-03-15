'use client';

import { useState, useEffect } from 'react';
import { adminApi } from '@/lib/admin-api';
import { Amenity } from '@/types';
import toast from 'react-hot-toast';
import { Plus, Edit2, Trash2, X, Save, Search } from 'lucide-react';
import { ConfirmActionModal } from '@/components/common/confirm-action-modal';

export default function AmenitiesManagement() {
    const [amenities, setAmenities] = useState<Amenity[]>([]);
    const [loading, setLoading] = useState(true);
    const [search, setSearch] = useState('');

    // Modal state
    const [isModalOpen, setIsModalOpen] = useState(false);
    const [editingAmenity, setEditingAmenity] = useState<Amenity | null>(null);
    const [formData, setFormData] = useState({ name: '', icon: '' });
    const [saving, setSaving] = useState(false);
    const [pendingDeleteAmenity, setPendingDeleteAmenity] = useState<{ id: string; name: string } | null>(null);

    useEffect(() => {
        loadAmenities();
    }, [search]);

    const loadAmenities = async () => {
        try {
            setLoading(true);
            const data = await adminApi.getAmenities({ search: search.trim() || undefined });
            setAmenities(data.results || []);
        } catch (err: any) {
            toast.error('Failed to load amenities');
            console.error(err);
        } finally {
            setLoading(false);
        }
    };

    const handleOpenModal = (amenity?: Amenity) => {
        if (amenity) {
            setEditingAmenity(amenity);
            setFormData({ name: amenity.name, icon: amenity.icon || '' });
        } else {
            setEditingAmenity(null);
            setFormData({ name: '', icon: '' });
        }
        setIsModalOpen(true);
    };

    const handleCloseModal = () => {
        setIsModalOpen(false);
        setEditingAmenity(null);
        setFormData({ name: '', icon: '' });
    };

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!formData.name.trim()) {
            toast.error('Name is required');
            return;
        }

        try {
            setSaving(true);
            if (editingAmenity) {
                await adminApi.updateAmenity(editingAmenity.id, formData);
                toast.success('Amenity updated successfully');
            } else {
                await adminApi.createAmenity(formData);
                toast.success('Amenity created successfully');
            }
            handleCloseModal();
            loadAmenities();
        } catch (err: any) {
            toast.error(err?.response?.data?.name?.[0] || err?.response?.data?.detail || 'Failed to save amenity');
        } finally {
            setSaving(false);
        }
    };

    const handleDelete = async (id: string, name: string) => {
        setPendingDeleteAmenity({ id, name });
    };

    const confirmDelete = async () => {
        if (!pendingDeleteAmenity) return;
        try {
            await adminApi.deleteAmenity(pendingDeleteAmenity.id);
            toast.success('Amenity deleted successfully');
            setPendingDeleteAmenity(null);
            loadAmenities();
        } catch (err: any) {
            toast.error('Failed to delete amenity');
            console.error(err);
        }
    };

    return (
        <div className="p-8">
            <div className="mb-8 flex justify-between items-center">
                <div>
                    <h1 className="text-3xl font-bold text-[#122F26]">Amenities Management</h1>
                    <p className="text-[#3A5C50] mt-2">Manage available property amenities globally</p>
                </div>
                <button
                    onClick={() => handleOpenModal()}
                    className="flex items-center gap-2 px-4 py-2 bg-[#D9B168] text-[#122F26] rounded-lg hover:bg-[#c9a158] transition-colors"
                >
                    <Plus className="w-5 h-5" /> Add New Amenity
                </button>
            </div>

            <div className="bg-white rounded-lg shadow p-6 mb-6">
                <div className="relative max-w-md">
                    <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-primary-300 w-5 h-5" />
                    <input
                        type="text"
                        value={search}
                        onChange={(e) => setSearch(e.target.value)}
                        placeholder="Search amenities..."
                        className="w-full pl-10 pr-4 py-2 border border-[#3A5C50] rounded-lg focus:ring-2 focus:ring-[#D9B168]"
                    />
                </div>
            </div>

            <div className="bg-white rounded-lg shadow overflow-hidden">
                {loading ? (
                    <div className="flex items-center justify-center p-8">
                        <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-[#D9B168]"></div>
                    </div>
                ) : amenities.length === 0 ? (
                    <div className="p-8 text-center text-[#3A5C50]">
                        No amenities found.
                    </div>
                ) : (
                    <table className="min-w-full divide-y divide-primary-200 border-collapse">
                        <thead className="bg-[#F4F1EA]">
                            <tr>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Name</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Icon Name</th>
                                <th className="px-6 py-3 text-right text-xs font-medium text-[#122F26] uppercase">Actions</th>
                            </tr>
                        </thead>
                        <tbody className="divide-y divide-primary-200 bg-white">
                            {amenities.map((amenity) => (
                                <tr key={amenity.id} className="hover:bg-sand-50 transition">
                                    <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-[#122F26]">
                                        {amenity.name}
                                    </td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50]">
                                        {amenity.icon || '-'}
                                    </td>
                                    <td className="px-6 py-4 whitespace-nowrap text-right text-sm font-medium">
                                        <button
                                            onClick={() => handleOpenModal(amenity)}
                                            className="text-[#D9B168] hover:text-[#c9a158] border-r border-[#3A5C50] pr-3 mr-3"
                                        >
                                            <Edit2 className="w-4 h-4" />
                                        </button>
                                        <button
                                            onClick={() => handleDelete(amenity.id, amenity.name)}
                                            className="text-red-500 hover:text-red-700"
                                        >
                                            <Trash2 className="w-4 h-4" />
                                        </button>
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                )}
            </div>

            {isModalOpen && (
                <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/50 backdrop-blur-sm">
                    <div className="bg-white rounded-xl shadow-xl w-full max-w-md overflow-hidden">
                        <div className="flex justify-between items-center p-6 border-b">
                            <h2 className="text-xl font-bold text-[#122F26]">
                                {editingAmenity ? 'Edit Amenity' : 'Add Amenity'}
                            </h2>
                            <button onClick={handleCloseModal} className="text-gray-500 hover:text-gray-700">
                                <X className="w-5 h-5" />
                            </button>
                        </div>

                        <form onSubmit={handleSubmit} className="p-6 space-y-4">
                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-1">
                                    Name <span className="text-red-500">*</span>
                                </label>
                                <input
                                    type="text"
                                    required
                                    value={formData.name}
                                    onChange={(e) => setFormData({ ...formData, name: e.target.value })}
                                    className="w-full p-2 border border-sand-300 rounded focus:ring-2 focus:ring-[#D9B168]"
                                    placeholder="e.g. WiFi, Pool"
                                />
                            </div>

                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-1">
                                    Icon Name (optional)
                                </label>
                                <input
                                    type="text"
                                    value={formData.icon}
                                    onChange={(e) => setFormData({ ...formData, icon: e.target.value })}
                                    className="w-full p-2 border border-sand-300 rounded focus:ring-2 focus:ring-[#D9B168]"
                                    placeholder="lucide-react icon name e.g. Wifi"
                                />
                                <p className="text-xs text-gray-500 mt-1">Leave empty if no icon is available</p>
                            </div>

                            <div className="pt-4 flex justify-end gap-3">
                                <button
                                    type="button"
                                    onClick={handleCloseModal}
                                    className="px-4 py-2 border border-[#3A5C50] text-[#122F26] rounded-lg hover:bg-sand-50"
                                    disabled={saving}
                                >
                                    Cancel
                                </button>
                                <button
                                    type="submit"
                                    disabled={saving}
                                    className="px-4 py-2 bg-[#D9B168] text-white rounded-lg hover:bg-[#c9a158] flex items-center gap-2"
                                >
                                    <Save className="w-4 h-4" />
                                    {saving ? 'Saving...' : 'Save'}
                                </button>
                            </div>
                        </form>
                    </div>
                </div>
            )}
            <ConfirmActionModal
                isOpen={Boolean(pendingDeleteAmenity)}
                title="Delete Amenity"
                message={pendingDeleteAmenity ? `Are you sure you want to delete the amenity "${pendingDeleteAmenity.name}"?` : ''}
                confirmText="Delete"
                variant="danger"
                onCancel={() => setPendingDeleteAmenity(null)}
                onConfirm={confirmDelete}
            />
        </div>
    );
}
