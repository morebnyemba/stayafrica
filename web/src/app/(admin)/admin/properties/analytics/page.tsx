'use client';

import { useState, useEffect } from 'react';
import { adminApi } from '@/lib/admin-api';
import toast from 'react-hot-toast';
import { BarChart3, TrendingUp, Building, Users } from 'lucide-react';

export default function AnalyticsDashboard() {
    const [propertyAnalytics, setPropertyAnalytics] = useState<any[]>([]);
    const [hostAnalytics, setHostAnalytics] = useState<any[]>([]);
    const [loading, setLoading] = useState(true);

    const [activeTab, setActiveTab] = useState<'property' | 'host'>('property');
    const [page, setPage] = useState(1);
    const [totalCount, setTotalCount] = useState(0);
    const ITEMS_PER_PAGE = 20;

    useEffect(() => {
        loadData();
    }, [activeTab, page]);

    const loadData = async () => {
        try {
            setLoading(true);
            if (activeTab === 'property') {
                const data = await adminApi.getGlobalPropertyAnalytics({ page, per_page: ITEMS_PER_PAGE });
                setPropertyAnalytics(data.results || []);
                setTotalCount(data.count || 0);
            } else {
                const data = await adminApi.getGlobalHostAnalytics({ page, per_page: ITEMS_PER_PAGE });
                setHostAnalytics(data.results || []);
                setTotalCount(data.count || 0);
            }
        } catch (err: any) {
            toast.error('Failed to load analytics data');
            console.error(err);
        } finally {
            setLoading(false);
        }
    };

    return (
        <div className="p-8">
            <div className="mb-8 flex justify-between items-center">
                <div>
                    <h1 className="text-3xl font-bold text-[#122F26]">Platform Analytics</h1>
                    <p className="text-[#3A5C50] mt-2">Comprehensive overview of Property and Host performances</p>
                </div>
                <div className="flex bg-sand-100 p-1 rounded-lg">
                    <button
                        onClick={() => { setActiveTab('property'); setPage(1); }}
                        className={`flex items-center gap-2 px-6 py-2 rounded-md font-medium transition ${activeTab === 'property'
                                ? 'bg-white text-[#122F26] shadow-sm'
                                : 'text-[#3A5C50] hover:text-[#122F26]'
                            }`}
                    >
                        <Building className="w-4 h-4" /> Property Details
                    </button>
                    <button
                        onClick={() => { setActiveTab('host'); setPage(1); }}
                        className={`flex items-center gap-2 px-6 py-2 rounded-md font-medium transition ${activeTab === 'host'
                                ? 'bg-white text-[#122F26] shadow-sm'
                                : 'text-[#3A5C50] hover:text-[#122F26]'
                            }`}
                    >
                        <Users className="w-4 h-4" /> Host Highlights
                    </button>
                </div>
            </div>

            <div className="bg-white rounded-lg shadow min-h-[500px] flex flex-col">
                {loading ? (
                    <div className="flex-1 flex items-center justify-center p-12">
                        <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-[#D9B168]"></div>
                    </div>
                ) : activeTab === 'property' ? (
                    <div className="flex-1 overflow-x-auto">
                        {propertyAnalytics.length === 0 ? (
                            <div className="p-12 text-center flex flex-col items-center justify-center h-full text-[#3A5C50]">
                                <BarChart3 className="w-12 h-12 mb-4 text-sand-300" />
                                <p>No property analytics found.</p>
                            </div>
                        ) : (
                            <table className="min-w-full divide-y divide-primary-200">
                                <thead className="bg-[#F4F1EA]">
                                    <tr>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Date</th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Property</th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Views</th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Bookings</th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Revenue (USD)</th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Occupancy</th>
                                    </tr>
                                </thead>
                                <tbody className="divide-y divide-primary-200 bg-white">
                                    {propertyAnalytics.map((item) => (
                                        <tr key={item.id} className="hover:bg-sand-50 transition">
                                            <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50]">
                                                {new Date(item.date).toLocaleDateString()}
                                            </td>
                                            <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-[#122F26]">
                                                {item.property_title || `Prop ID ${item.property}`}
                                            </td>
                                            <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50]">
                                                {item.views_count}
                                            </td>
                                            <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50]">
                                                {item.bookings_count}
                                            </td>
                                            <td className="px-6 py-4 whitespace-nowrap text-sm font-bold text-green-700">
                                                ${parseFloat(item.total_revenue || '0').toFixed(2)}
                                            </td>
                                            <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50]">
                                                {parseFloat(item.occupancy_rate || '0').toFixed(1)}%
                                            </td>
                                        </tr>
                                    ))}
                                </tbody>
                            </table>
                        )}
                    </div>
                ) : (
                    <div className="flex-1 overflow-x-auto">
                        {hostAnalytics.length === 0 ? (
                            <div className="p-12 text-center flex flex-col items-center justify-center h-full text-[#3A5C50]">
                                <TrendingUp className="w-12 h-12 mb-4 text-sand-300" />
                                <p>No host analytics found.</p>
                            </div>
                        ) : (
                            <table className="min-w-full divide-y divide-primary-200">
                                <thead className="bg-[#F4F1EA]">
                                    <tr>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Period Range</th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Host Info</th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Bookings</th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Cancellations</th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Revenue (USD)</th>
                                        <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Avg Rating</th>
                                    </tr>
                                </thead>
                                <tbody className="divide-y divide-primary-200 bg-white">
                                    {hostAnalytics.map((item) => (
                                        <tr key={item.id} className="hover:bg-sand-50 transition">
                                            <td className="px-6 py-4 whitespace-nowrap">
                                                <div className="text-sm font-medium text-[#122F26] capitalize">{item.period}</div>
                                                <div className="text-xs text-[#3A5C50]">
                                                    {item.start_date} to {item.end_date}
                                                </div>
                                            </td>
                                            <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50]">
                                                {item.host_name || `Host ID ${item.host}`}
                                            </td>
                                            <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50]">
                                                {item.total_bookings}
                                            </td>
                                            <td className="px-6 py-4 whitespace-nowrap text-sm text-red-600">
                                                {item.cancellations}
                                            </td>
                                            <td className="px-6 py-4 whitespace-nowrap text-sm font-bold text-green-700">
                                                ${parseFloat(item.total_revenue || '0').toFixed(2)}
                                            </td>
                                            <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50] flex items-center">
                                                <span className="text-yellow-500 mr-1">★</span>
                                                {parseFloat(item.avg_rating || '0').toFixed(1)}
                                            </td>
                                        </tr>
                                    ))}
                                </tbody>
                            </table>
                        )}
                    </div>
                )}

                {/* Pagination */}
                {!loading && totalCount > 0 && (
                    <div className="bg-sand-50 px-6 py-4 border-t flex items-center justify-between mt-auto">
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
