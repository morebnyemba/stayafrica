'use client';

import { useState, useEffect } from 'react';
import { adminApi } from '@/lib/admin-api';
import toast from 'react-hot-toast';
import { Search, Heart } from 'lucide-react';

export default function WishlistsManagement() {
    const [savedItems, setSavedItems] = useState<any[]>([]);
    const [loading, setLoading] = useState(true);
    const [search, setSearch] = useState('');
    const [page, setPage] = useState(1);
    const [totalCount, setTotalCount] = useState(0);
    const ITEMS_PER_PAGE = 20;

    useEffect(() => {
        // Basic debounce for search
        const timer = setTimeout(() => {
            loadSavedProperties();
        }, 500);
        return () => clearTimeout(timer);
    }, [search, page]);

    const loadSavedProperties = async () => {
        try {
            setLoading(true);
            const data = await adminApi.getAllSavedProperties({
                search: search.trim() || undefined,
                page,
                per_page: ITEMS_PER_PAGE
            });
            setSavedItems(data.results || []);
            setTotalCount(data.count || 0);
        } catch (err: any) {
            toast.error('Failed to load saved properties');
            console.error(err);
        } finally {
            setLoading(false);
        }
    };

    return (
        <div className="p-8">
            <div className="mb-8">
                <h1 className="text-3xl font-bold text-[#122F26]">Saved Properties (Wishlists)</h1>
                <p className="text-[#3A5C50] mt-2">Track user interest and wishlisted properties across the platform</p>
            </div>

            <div className="bg-white rounded-lg shadow p-6 mb-6">
                <div className="relative max-w-md">
                    <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-primary-300 w-5 h-5" />
                    <input
                        type="text"
                        value={search}
                        onChange={(e) => { setSearch(e.target.value); setPage(1); }}
                        placeholder="Search by user email or property title..."
                        className="w-full pl-10 pr-4 py-2 border border-[#3A5C50] rounded-lg focus:ring-2 focus:ring-[#D9B168]"
                    />
                </div>
            </div>

            <div className="bg-white rounded-lg shadow overflow-hidden">
                {loading ? (
                    <div className="flex items-center justify-center p-12">
                        <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-[#D9B168]"></div>
                    </div>
                ) : savedItems.length === 0 ? (
                    <div className="p-12 text-center flex flex-col items-center text-[#3A5C50]">
                        <Heart className="w-12 h-12 mb-4 text-sand-300" />
                        <p>No wishlisted properties found.</p>
                    </div>
                ) : (
                    <div className="overflow-x-auto">
                        <table className="min-w-full divide-y divide-primary-200 border-collapse">
                            <thead className="bg-[#F4F1EA]">
                                <tr>
                                    <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Date Saved</th>
                                    <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">User Details</th>
                                    <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Property Title</th>
                                    <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Property Location</th>
                                </tr>
                            </thead>
                            <tbody className="divide-y divide-primary-200 bg-white">
                                {savedItems.map((item) => (
                                    <tr key={item.id} className="hover:bg-sand-50 transition">
                                        <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50]">
                                            {new Date(item.created_at).toLocaleDateString()}
                                        </td>
                                        <td className="px-6 py-4 whitespace-nowrap">
                                            <div className="text-sm font-medium text-[#122F26]">
                                                {item.user_name || 'User'}
                                            </div>
                                            <div className="text-sm text-[#3A5C50]">
                                                {item.user_email}
                                            </div>
                                        </td>
                                        <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-[#122F26]">
                                            {item.property?.title || `Property ID ${item.property_id}`}
                                        </td>
                                        <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50]">
                                            {item.property ? `${item.property.city}, ${item.property.country}` : 'N/A'}
                                        </td>
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    </div>
                )}

                {/* Pagination */}
                {!loading && totalCount > 0 && (
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
        </div>
    );
}
