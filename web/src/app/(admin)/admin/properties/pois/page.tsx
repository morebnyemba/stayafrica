'use client';

import { useState, useEffect } from 'react';
import { adminApi } from '@/lib/admin-api';
import toast from 'react-hot-toast';
import { Plus, Search, MapPin, Tag } from 'lucide-react';
import { POICategory, PointOfInterest } from '@/types/admin-types';
import { ConfirmActionModal } from '@/components/common/confirm-action-modal';

export default function POIManagement() {
    const [activeTab, setActiveTab] = useState<'categories' | 'pois'>('categories');
    const [categories, setCategories] = useState<POICategory[]>([]);
    const [pois, setPois] = useState<PointOfInterest[]>([]);
    const [loading, setLoading] = useState(true);

    // Search & Pagination
    const [search, setSearch] = useState('');
    const [page, setPage] = useState(1);
    const [totalCount, setTotalCount] = useState(0);
    const ITEMS_PER_PAGE = 20;

    // Forms
    const [isCategoryModalOpen, setIsCategoryModalOpen] = useState(false);
    const [selectedCategory, setSelectedCategory] = useState<Partial<POICategory> | null>(null);
    const [pendingDeleteCategoryId, setPendingDeleteCategoryId] = useState<number | null>(null);

    const [isPOIModalOpen, setIsPOIModalOpen] = useState(false);
    const [selectedPOI, setSelectedPOI] = useState<Partial<PointOfInterest> | null>(null);
    const [pendingDeletePOIId, setPendingDeletePOIId] = useState<string | null>(null);

    useEffect(() => {
        const timer = setTimeout(() => {
            if (activeTab === 'categories') {
                loadCategories();
            } else {
                loadPOIs();
            }
        }, 500);
        return () => clearTimeout(timer);
    }, [activeTab, search, page]);

    const loadCategories = async () => {
        try {
            setLoading(true);
            const data = await adminApi.getPOICategories({ search: search.trim() });
            setCategories(data.results || []);
        } catch (err: any) {
            toast.error('Failed to load POI Categories');
            console.error(err);
        } finally {
            setLoading(false);
        }
    };

    const loadPOIs = async () => {
        try {
            setLoading(true);
            const data = await adminApi.getPOIs({ search: search.trim(), page, per_page: ITEMS_PER_PAGE });
            setPois(data.results || []);
            setTotalCount(data.count || 0);
        } catch (err: any) {
            toast.error('Failed to load POIs');
            console.error(err);
        } finally {
            setLoading(false);
        }
    };

    const handleSaveCategory = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!selectedCategory?.name) {
            toast.error('Category Name is required');
            return;
        }

        try {
            if (selectedCategory.id) {
                await adminApi.updatePOICategory(selectedCategory.id, selectedCategory);
                toast.success('Category updated successfully');
            } else {
                await adminApi.createPOICategory(selectedCategory);
                toast.success('Category created successfully');
            }
            setIsCategoryModalOpen(false);
            setSelectedCategory(null);
            loadCategories();
        } catch (err: any) {
            toast.error(err.response?.data?.message || 'Failed to save category');
            console.error(err);
        }
    };

    const handleDeleteCategory = async (id: number) => {
        setPendingDeleteCategoryId(id);
    };

    const confirmDeleteCategory = async () => {
        if (!pendingDeleteCategoryId) return;
        try {
            await adminApi.deletePOICategory(pendingDeleteCategoryId);
            toast.success('Category deleted');
            setPendingDeleteCategoryId(null);
            loadCategories();
        } catch (err: any) {
            toast.error('Failed to delete category');
        }
    };

    const handleSavePOI = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!selectedPOI?.name || !selectedPOI?.city || !selectedPOI?.country || !selectedPOI?.poi_type) {
            toast.error('Please fill all required fields (Name, City, Country, Type)');
            return;
        }

        try {
            if (selectedPOI.id) {
                await adminApi.updatePOI(selectedPOI.id as string, selectedPOI);
                toast.success('POI updated successfully');
            } else {
                await adminApi.createPOI({
                    ...selectedPOI,
                    location: { type: 'Point', coordinates: [0, 0] } // Mock default location if omitted
                });
                toast.success('POI created successfully');
            }
            setIsPOIModalOpen(false);
            setSelectedPOI(null);
            loadPOIs();
        } catch (err: any) {
            toast.error(err.response?.data?.message || 'Failed to save POI');
            console.error(err);
        }
    };

    const handleDeletePOI = async (id: string) => {
        setPendingDeletePOIId(id);
    };

    const confirmDeletePOI = async () => {
        if (!pendingDeletePOIId) return;
        try {
            await adminApi.deletePOI(pendingDeletePOIId);
            toast.success('POI deleted');
            setPendingDeletePOIId(null);
            loadPOIs();
        } catch (err: any) {
            toast.error('Failed to delete POI');
        }
    };

    return (
        <div className="p-8">
            <div className="mb-8 flex justify-between items-center">
                <div>
                    <h1 className="text-3xl font-bold text-[#122F26]">Points of Interest (POIs)</h1>
                    <p className="text-[#3A5C50] mt-2">Manage geographical points of interest and categories</p>
                </div>

                <div className="flex flex-col gap-4 items-end">
                    <div className="flex bg-sand-100 p-1 rounded-lg">
                        <button
                            onClick={() => { setActiveTab('categories'); setPage(1); setSearch(''); }}
                            className={`flex items-center gap-2 px-6 py-2 rounded-md font-medium transition ${activeTab === 'categories' ? 'bg-white text-[#122F26] shadow-sm' : 'text-[#3A5C50] hover:text-[#122F26]'
                                }`}
                        >
                            <Tag className="w-4 h-4" /> Categories
                        </button>
                        <button
                            onClick={() => { setActiveTab('pois'); setPage(1); setSearch(''); }}
                            className={`flex items-center gap-2 px-6 py-2 rounded-md font-medium transition ${activeTab === 'pois' ? 'bg-white text-[#122F26] shadow-sm' : 'text-[#3A5C50] hover:text-[#122F26]'
                                }`}
                        >
                            <MapPin className="w-4 h-4" /> Points of Interest
                        </button>
                    </div>

                    <button
                        onClick={() => {
                            if (activeTab === 'categories') {
                                setSelectedCategory({ display_order: 0, color: '#3A5C50' });
                                setIsCategoryModalOpen(true);
                            } else {
                                setSelectedPOI({ poi_type: 'restaurant', is_active: true });
                                setIsPOIModalOpen(true);
                            }
                        }}
                        className="flex items-center gap-2 px-4 py-2 bg-[#122F26] text-white rounded-lg hover:bg-[#3A5C50] transition"
                    >
                        <Plus className="w-4 h-4" />
                        Add {activeTab === 'categories' ? 'Category' : 'POI'}
                    </button>
                </div>
            </div>

            <div className="bg-white rounded-lg shadow p-6 mb-6">
                <div className="relative max-w-md">
                    <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-primary-300 w-5 h-5" />
                    <input
                        type="text"
                        value={search}
                        onChange={(e) => { setSearch(e.target.value); setPage(1); }}
                        placeholder={`Search ${activeTab}...`}
                        className="w-full pl-10 pr-4 py-2 border border-[#3A5C50] rounded-lg focus:ring-2 focus:ring-[#D9B168]"
                    />
                </div>
            </div>

            <div className="bg-white rounded-lg shadow overflow-hidden">
                {loading ? (
                    <div className="flex items-center justify-center p-12">
                        <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-[#D9B168]"></div>
                    </div>
                ) : activeTab === 'categories' ? (
                    <div className="overflow-x-auto">
                        <table className="min-w-full divide-y divide-primary-200">
                            <thead className="bg-[#F4F1EA]">
                                <tr>
                                    <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Name</th>
                                    <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Icon</th>
                                    <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Color</th>
                                    <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Display Order</th>
                                    <th className="px-6 py-3 text-right text-xs font-medium text-[#122F26] uppercase">Actions</th>
                                </tr>
                            </thead>
                            <tbody className="divide-y divide-primary-200 bg-white">
                                {categories.map((category) => (
                                    <tr key={category.id} className="hover:bg-sand-50 transition">
                                        <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-[#122F26]">
                                            {category.name}
                                        </td>
                                        <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50]">{category.icon}</td>
                                        <td className="px-6 py-4 whitespace-nowrap">
                                            <div className="flex items-center gap-2">
                                                <div className="w-6 h-6 rounded-full border border-gray-300" style={{ backgroundColor: category.color || '#3A5C50' }} />
                                                <span className="text-sm text-[#3A5C50]">{category.color}</span>
                                            </div>
                                        </td>
                                        <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50]">{category.display_order}</td>
                                        <td className="px-6 py-4 whitespace-nowrap text-right text-sm">
                                            <button
                                                onClick={() => { setSelectedCategory(category); setIsCategoryModalOpen(true); }}
                                                className="text-[#D9B168] hover:text-[#c49b50] mr-4"
                                            >
                                                Edit
                                            </button>
                                            <button
                                                onClick={() => handleDeleteCategory(category.id)}
                                                className="text-red-500 hover:text-red-700"
                                            >
                                                Delete
                                            </button>
                                        </td>
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    </div>
                ) : (
                    <div className="overflow-x-auto">
                        <table className="min-w-full divide-y divide-primary-200">
                            <thead className="bg-[#F4F1EA]">
                                <tr>
                                    <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Name</th>
                                    <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Type</th>
                                    <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">City / Country</th>
                                    <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Status</th>
                                    <th className="px-6 py-3 text-right text-xs font-medium text-[#122F26] uppercase">Actions</th>
                                </tr>
                            </thead>
                            <tbody className="divide-y divide-primary-200 bg-white">
                                {pois.map((poi) => (
                                    <tr key={poi.id} className="hover:bg-sand-50 transition">
                                        <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-[#122F26]">
                                            {poi.name}
                                        </td>
                                        <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50] capitalize">{poi.poi_type}</td>
                                        <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50]">
                                            {poi.city}, {poi.country}
                                        </td>
                                        <td className="px-6 py-4 whitespace-nowrap">
                                            <span className={`px-2 py-1 inline-flex text-xs leading-5 font-semibold rounded-full ${poi.is_active ? 'bg-green-100 text-green-800' : 'bg-red-100 text-red-800'}`}>
                                                {poi.is_active ? 'Active' : 'Inactive'}
                                            </span>
                                        </td>
                                        <td className="px-6 py-4 whitespace-nowrap text-right text-sm">
                                            <button
                                                onClick={() => { setSelectedPOI(poi); setIsPOIModalOpen(true); }}
                                                className="text-[#D9B168] hover:text-[#c49b50] mr-4"
                                            >
                                                Edit
                                            </button>
                                            <button
                                                onClick={() => handleDeletePOI(poi.id)}
                                                className="text-red-500 hover:text-red-700"
                                            >
                                                Delete
                                            </button>
                                        </td>
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    </div>
                )}

                {/* POI Pagination */}
                {!loading && activeTab === 'pois' && totalCount > 0 && (
                    <div className="bg-sand-50 px-6 py-4 border-t flex items-center justify-between">
                        <div className="text-sm text-[#122F26]">
                            Showing {(page - 1) * ITEMS_PER_PAGE + 1} to {Math.min(page * ITEMS_PER_PAGE, totalCount)} of {totalCount} entries
                        </div>
                        <div className="flex space-x-2">
                            <button
                                onClick={() => setPage(p => Math.max(1, p - 1))}
                                disabled={page === 1}
                                className="px-4 py-2 border border-[#3A5C50] rounded-lg text-sm font-medium text-[#122F26] hover:bg-white disabled:opacity-50 disabled:cursor-not-allowed bg-transparent transition"
                            >
                                Previous
                            </button>
                            <button
                                onClick={() => setPage(p => p + 1)}
                                disabled={page * ITEMS_PER_PAGE >= totalCount}
                                className="px-4 py-2 border border-[#3A5C50] rounded-lg text-sm font-medium text-[#122F26] hover:bg-white disabled:opacity-50 disabled:cursor-not-allowed bg-transparent transition"
                            >
                                Next
                            </button>
                        </div>
                    </div>
                )}
            </div>

            {/* Category Modal */}
            {isCategoryModalOpen && (
                <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
                    <div className="bg-white rounded-lg p-6 max-w-md w-full">
                        <h2 className="text-xl font-bold mb-4 text-[#122F26]">
                            {selectedCategory?.id ? 'Edit Category' : 'Create Category'}
                        </h2>
                        <form onSubmit={handleSaveCategory} className="space-y-4">
                            <div>
                                <label className="block text-sm font-medium text-[#3A5C50]">Name *</label>
                                <input
                                    type="text"
                                    required
                                    value={selectedCategory?.name || ''}
                                    onChange={(e) => setSelectedCategory(prev => ({ ...prev, name: e.target.value }))}
                                    className="mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-[#D9B168] focus:ring-[#D9B168]"
                                />
                            </div>
                            <div className="grid grid-cols-2 gap-4">
                                <div>
                                    <label className="block text-sm font-medium text-[#3A5C50]">Icon</label>
                                    <input
                                        type="text"
                                        value={selectedCategory?.icon || ''}
                                        onChange={(e) => setSelectedCategory(prev => ({ ...prev, icon: e.target.value }))}
                                        className="mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-[#D9B168] focus:ring-[#D9B168]"
                                    />
                                </div>
                                <div>
                                    <label className="block text-sm font-medium text-[#3A5C50]">Color</label>
                                    <input
                                        type="color"
                                        value={selectedCategory?.color || '#3A5C50'}
                                        onChange={(e) => setSelectedCategory(prev => ({ ...prev, color: e.target.value }))}
                                        className="mt-1 block w-full h-[38px] rounded-md border-gray-300 shadow-sm focus:border-[#D9B168] focus:ring-[#D9B168]"
                                    />
                                </div>
                            </div>
                            <div>
                                <label className="block text-sm font-medium text-[#3A5C50]">Description</label>
                                <textarea
                                    value={selectedCategory?.description || ''}
                                    onChange={(e) => setSelectedCategory(prev => ({ ...prev, description: e.target.value }))}
                                    className="mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-[#D9B168] focus:ring-[#D9B168]"
                                />
                            </div>
                            <div>
                                <label className="block text-sm font-medium text-[#3A5C50]">Display Order</label>
                                <input
                                    type="number"
                                    value={selectedCategory?.display_order || 0}
                                    onChange={(e) => setSelectedCategory(prev => ({ ...prev, display_order: parseInt(e.target.value) }))}
                                    className="mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-[#D9B168] focus:ring-[#D9B168]"
                                />
                            </div>
                            <div className="flex justify-end gap-2 mt-6">
                                <button
                                    type="button"
                                    onClick={() => setIsCategoryModalOpen(false)}
                                    className="px-4 py-2 border border-gray-300 rounded-md text-gray-700 hover:bg-gray-50"
                                >
                                    Cancel
                                </button>
                                <button
                                    type="submit"
                                    className="px-4 py-2 bg-[#D9B168] text-white rounded-md hover:bg-[#c49b50]"
                                >
                                    Save
                                </button>
                            </div>
                        </form>
                    </div>
                </div>
            )}

            {/* POI Modal */}
            {isPOIModalOpen && (
                <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50 overflow-y-auto">
                    <div className="bg-white rounded-lg p-6 max-w-2xl w-full my-8">
                        <h2 className="text-xl font-bold mb-4 text-[#122F26]">
                            {selectedPOI?.id ? 'Edit POI' : 'Create POI'}
                        </h2>
                        <form onSubmit={handleSavePOI} className="space-y-4">
                            <div className="grid grid-cols-2 gap-4">
                                <div>
                                    <label className="block text-sm font-medium text-[#3A5C50]">Name *</label>
                                    <input
                                        type="text"
                                        required
                                        value={selectedPOI?.name || ''}
                                        onChange={(e) => setSelectedPOI(prev => ({ ...prev, name: e.target.value }))}
                                        className="mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-[#D9B168] focus:ring-[#D9B168]"
                                    />
                                </div>
                                <div>
                                    <label className="block text-sm font-medium text-[#3A5C50]">Type *</label>
                                    <select
                                        required
                                        value={selectedPOI?.poi_type || ''}
                                        onChange={(e) => setSelectedPOI(prev => ({ ...prev, poi_type: e.target.value }))}
                                        className="mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-[#D9B168] focus:ring-[#D9B168]"
                                    >
                                        <option value="">Select a type...</option>
                                        <option value="restaurant">Restaurant</option>
                                        <option value="cafe">Cafe</option>
                                        <option value="park">Park</option>
                                        <option value="attraction">Attraction</option>
                                        <option value="shopping">Shopping</option>
                                        <option value="hospital">Hospital</option>
                                        <option value="beach">Beach</option>
                                        <option value="museum">Museum</option>
                                        <option value="other">Other</option>
                                    </select>
                                </div>
                            </div>

                            <div className="grid grid-cols-2 gap-4">
                                <div>
                                    <label className="block text-sm font-medium text-[#3A5C50]">City *</label>
                                    <input
                                        type="text"
                                        required
                                        value={selectedPOI?.city || ''}
                                        onChange={(e) => setSelectedPOI(prev => ({ ...prev, city: e.target.value }))}
                                        className="mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-[#D9B168] focus:ring-[#D9B168]"
                                    />
                                </div>
                                <div>
                                    <label className="block text-sm font-medium text-[#3A5C50]">Country *</label>
                                    <input
                                        type="text"
                                        required
                                        value={selectedPOI?.country || ''}
                                        onChange={(e) => setSelectedPOI(prev => ({ ...prev, country: e.target.value }))}
                                        className="mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-[#D9B168] focus:ring-[#D9B168]"
                                    />
                                </div>
                            </div>

                            <div>
                                <label className="block text-sm font-medium text-[#3A5C50]">Address</label>
                                <input
                                    type="text"
                                    value={selectedPOI?.address || ''}
                                    onChange={(e) => setSelectedPOI(prev => ({ ...prev, address: e.target.value }))}
                                    className="mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-[#D9B168] focus:ring-[#D9B168]"
                                />
                            </div>

                            <div>
                                <label className="block text-sm font-medium text-[#3A5C50]">Description</label>
                                <textarea
                                    rows={3}
                                    value={selectedPOI?.description || ''}
                                    onChange={(e) => setSelectedPOI(prev => ({ ...prev, description: e.target.value }))}
                                    className="mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-[#D9B168] focus:ring-[#D9B168]"
                                />
                            </div>

                            <div className="grid grid-cols-2 gap-4">
                                <div>
                                    <label className="block text-sm font-medium text-[#3A5C50]">Phone</label>
                                    <input
                                        type="text"
                                        value={selectedPOI?.phone || ''}
                                        onChange={(e) => setSelectedPOI(prev => ({ ...prev, phone: e.target.value }))}
                                        className="mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-[#D9B168] focus:ring-[#D9B168]"
                                    />
                                </div>
                                <div>
                                    <label className="block text-sm font-medium text-[#3A5C50]">Website</label>
                                    <input
                                        type="url"
                                        value={selectedPOI?.website || ''}
                                        onChange={(e) => setSelectedPOI(prev => ({ ...prev, website: e.target.value }))}
                                        className="mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-[#D9B168] focus:ring-[#D9B168]"
                                    />
                                </div>
                            </div>

                            <div className="flex items-center gap-2 mt-4">
                                <input
                                    type="checkbox"
                                    id="poi_active"
                                    checked={selectedPOI?.is_active ?? true}
                                    onChange={(e) => setSelectedPOI(prev => ({ ...prev, is_active: e.target.checked }))}
                                    className="rounded border-gray-300 text-[#D9B168] focus:ring-[#D9B168]"
                                />
                                <label htmlFor="poi_active" className="text-sm font-medium text-[#3A5C50]">
                                    Active (Visible on platform)
                                </label>
                            </div>

                            <div className="flex justify-end gap-2 mt-6">
                                <button
                                    type="button"
                                    onClick={() => setIsPOIModalOpen(false)}
                                    className="px-4 py-2 border border-gray-300 rounded-md text-gray-700 hover:bg-gray-50"
                                >
                                    Cancel
                                </button>
                                <button
                                    type="submit"
                                    className="px-4 py-2 bg-[#D9B168] text-white rounded-md hover:bg-[#c49b50]"
                                >
                                    Save
                                </button>
                            </div>
                        </form>
                    </div>
                </div>
            )}
            <ConfirmActionModal
                isOpen={Boolean(pendingDeleteCategoryId)}
                title="Delete Category"
                message="Are you sure you want to delete this category? Features and POIs attached to it might be affected."
                confirmText="Delete Category"
                variant="danger"
                onCancel={() => setPendingDeleteCategoryId(null)}
                onConfirm={confirmDeleteCategory}
            />
            <ConfirmActionModal
                isOpen={Boolean(pendingDeletePOIId)}
                title="Delete POI"
                message="Are you sure you want to delete this POI?"
                confirmText="Delete POI"
                variant="danger"
                onCancel={() => setPendingDeletePOIId(null)}
                onConfirm={confirmDeletePOI}
            />
        </div>
    );
}
