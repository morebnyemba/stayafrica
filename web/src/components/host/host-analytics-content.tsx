'use client';

import { useState, useEffect, useCallback } from 'react';
import { apiClient } from '@/services/api-client';
import { BarChart3, TrendingUp, TrendingDown, Star, Eye, Calendar, DollarSign, ArrowUpRight, ArrowDownRight, Minus } from 'lucide-react';

interface Benchmark {
    property_id: string;
    property_title: string;
    occupancy_rate: number;
    avg_nightly_rate: number;
    avg_rating: number;
    total_reviews: number;
    total_bookings: number;
    revenue: number;
    avg_occupancy_market: number;
    avg_rate_market: number;
    avg_rating_market: number;
}

function TrendIndicator({ value, benchmark, suffix = '' }: { value: number; benchmark: number; suffix?: string }) {
    const diff = value - benchmark;
    const pct = benchmark > 0 ? Math.round((diff / benchmark) * 100) : 0;

    if (Math.abs(pct) < 2) {
        return (
            <span className="flex items-center gap-1 text-xs text-primary-400 dark:text-sand-500">
                <Minus className="w-3 h-3" /> On par
            </span>
        );
    }

    return pct > 0 ? (
        <span className="flex items-center gap-1 text-xs text-green-600 dark:text-green-400">
            <ArrowUpRight className="w-3 h-3" /> {Math.abs(pct)}% above{suffix}
        </span>
    ) : (
        <span className="flex items-center gap-1 text-xs text-red-500 dark:text-red-400">
            <ArrowDownRight className="w-3 h-3" /> {Math.abs(pct)}% below{suffix}
        </span>
    );
}

export function HostAnalyticsContent() {
    const [benchmarks, setBenchmarks] = useState<Benchmark[]>([]);
    const [loading, setLoading] = useState(true);
    const [period, setPeriod] = useState<string>('month');

    const fetchBenchmarks = useCallback(async () => {
        try {
            setLoading(true);
            const res = await apiClient.get(`/analytics/benchmarks/?period=${period}`);
            const data = res.data?.results || res.data || [];
            setBenchmarks(data);
        } catch (error) {
            console.error('Failed to fetch benchmarks:', error);
        } finally {
            setLoading(false);
        }
    }, [period]);

    useEffect(() => {
        fetchBenchmarks();
    }, [fetchBenchmarks]);

    // Overall averages
    const avgOccupancy = benchmarks.length > 0
        ? Math.round(benchmarks.reduce((sum, b) => sum + (b.occupancy_rate || 0), 0) / benchmarks.length)
        : 0;
    const avgRate = benchmarks.length > 0
        ? Math.round(benchmarks.reduce((sum, b) => sum + (b.avg_nightly_rate || 0), 0) / benchmarks.length)
        : 0;
    const avgRating = benchmarks.length > 0
        ? (benchmarks.reduce((sum, b) => sum + (b.avg_rating || 0), 0) / benchmarks.length).toFixed(1)
        : '0.0';
    const totalRevenue = benchmarks.reduce((sum, b) => sum + (b.revenue || 0), 0);
    const marketOccupancy = benchmarks.length > 0
        ? Math.round(benchmarks.reduce((sum, b) => sum + (b.avg_occupancy_market || 0), 0) / benchmarks.length)
        : 0;

    return (
        <div className="max-w-6xl mx-auto px-4 py-8">
            {/* Header */}
            <div className="flex flex-col sm:flex-row items-start sm:items-center justify-between gap-4 mb-6">
                <div>
                    <h1 className="text-2xl font-bold text-primary-900 dark:text-sand-100">Performance Analytics</h1>
                    <p className="text-sm text-primary-500 dark:text-sand-400 mt-1">
                        Compare your properties against market benchmarks
                    </p>
                </div>
                <select
                    value={period}
                    onChange={e => setPeriod(e.target.value)}
                    className="px-4 py-2 rounded-lg border border-sand-300 dark:border-primary-600 bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-100 text-sm focus:outline-none focus:ring-2 focus:ring-accent-500"
                >
                    <option value="week">This Week</option>
                    <option value="month">This Month</option>
                    <option value="quarter">This Quarter</option>
                    <option value="year">This Year</option>
                </select>
            </div>

            {/* Summary Cards */}
            <div className="grid grid-cols-2 lg:grid-cols-4 gap-4 mb-8">
                {[
                    { label: 'Avg Occupancy', value: `${avgOccupancy}%`, icon: Calendar, benchmark: marketOccupancy, suffix: ' market' },
                    { label: 'Avg Nightly Rate', value: `$${avgRate}`, icon: DollarSign },
                    { label: 'Avg Rating', value: avgRating, icon: Star },
                    { label: 'Total Revenue', value: `$${totalRevenue.toLocaleString()}`, icon: TrendingUp },
                ].map((card, i) => {
                    const CardIcon = card.icon;
                    return (
                        <div key={i} className="bg-white dark:bg-primary-800/40 rounded-xl p-5 shadow-sm border border-sand-200/50 dark:border-primary-700/50">
                            <div className="flex items-center gap-2 mb-3">
                                <div className="w-8 h-8 rounded-lg bg-accent-100 dark:bg-accent-900/30 flex items-center justify-center">
                                    <CardIcon className="w-4 h-4 text-accent-600 dark:text-accent-400" />
                                </div>
                                <span className="text-xs text-primary-500 dark:text-sand-400">{card.label}</span>
                            </div>
                            <p className="text-xl font-bold text-primary-900 dark:text-sand-100">{card.value}</p>
                            {card.benchmark !== undefined && (
                                <div className="mt-1">
                                    <TrendIndicator value={avgOccupancy} benchmark={card.benchmark} suffix={card.suffix} />
                                </div>
                            )}
                        </div>
                    );
                })}
            </div>

            {/* Property Benchmarks Table */}
            <div className="bg-white dark:bg-primary-800/40 rounded-2xl shadow-sm border border-sand-200/50 dark:border-primary-700/50 overflow-hidden">
                <div className="p-6 border-b border-sand-200 dark:border-primary-700">
                    <h2 className="text-lg font-semibold text-primary-900 dark:text-sand-100 flex items-center gap-2">
                        <BarChart3 className="w-5 h-5 text-accent-600 dark:text-accent-400" />
                        Property Benchmarks
                    </h2>
                </div>

                {loading ? (
                    <div className="p-8 space-y-4">
                        {[...Array(3)].map((_, i) => (
                            <div key={i} className="animate-pulse flex gap-4">
                                <div className="h-4 w-1/4 rounded bg-sand-200 dark:bg-primary-700" />
                                <div className="h-4 w-1/6 rounded bg-sand-200 dark:bg-primary-700" />
                                <div className="h-4 w-1/6 rounded bg-sand-200 dark:bg-primary-700" />
                                <div className="h-4 w-1/6 rounded bg-sand-200 dark:bg-primary-700" />
                            </div>
                        ))}
                    </div>
                ) : benchmarks.length === 0 ? (
                    <div className="text-center py-16">
                        <BarChart3 className="w-10 h-10 mx-auto text-sand-300 dark:text-primary-600 mb-3" />
                        <p className="text-sm text-primary-500 dark:text-sand-400">
                            No benchmark data available yet. Data will appear after your properties receive bookings.
                        </p>
                    </div>
                ) : (
                    <div className="overflow-x-auto">
                        <table className="w-full text-sm">
                            <thead>
                                <tr className="border-b border-sand-200 dark:border-primary-700">
                                    <th className="text-left px-6 py-3 text-xs font-medium text-primary-500 dark:text-sand-400 uppercase tracking-wider">Property</th>
                                    <th className="text-center px-4 py-3 text-xs font-medium text-primary-500 dark:text-sand-400 uppercase tracking-wider">Occupancy</th>
                                    <th className="text-center px-4 py-3 text-xs font-medium text-primary-500 dark:text-sand-400 uppercase tracking-wider">Avg Rate</th>
                                    <th className="text-center px-4 py-3 text-xs font-medium text-primary-500 dark:text-sand-400 uppercase tracking-wider">Rating</th>
                                    <th className="text-center px-4 py-3 text-xs font-medium text-primary-500 dark:text-sand-400 uppercase tracking-wider">Bookings</th>
                                    <th className="text-right px-6 py-3 text-xs font-medium text-primary-500 dark:text-sand-400 uppercase tracking-wider">Revenue</th>
                                </tr>
                            </thead>
                            <tbody className="divide-y divide-sand-100 dark:divide-primary-700">
                                {benchmarks.map(benchmark => (
                                    <tr key={benchmark.property_id} className="hover:bg-sand-50 dark:hover:bg-primary-800/60 transition-colors">
                                        <td className="px-6 py-4">
                                            <p className="font-medium text-primary-900 dark:text-sand-100 truncate max-w-[200px]">
                                                {benchmark.property_title}
                                            </p>
                                        </td>
                                        <td className="px-4 py-4 text-center">
                                            <span className="text-primary-900 dark:text-sand-100">{benchmark.occupancy_rate || 0}%</span>
                                            <div className="mt-0.5">
                                                <TrendIndicator value={benchmark.occupancy_rate || 0} benchmark={benchmark.avg_occupancy_market || 0} />
                                            </div>
                                        </td>
                                        <td className="px-4 py-4 text-center">
                                            <span className="text-primary-900 dark:text-sand-100">${benchmark.avg_nightly_rate || 0}</span>
                                            <div className="mt-0.5">
                                                <TrendIndicator value={benchmark.avg_nightly_rate || 0} benchmark={benchmark.avg_rate_market || 0} />
                                            </div>
                                        </td>
                                        <td className="px-4 py-4 text-center">
                                            <span className="flex items-center justify-center gap-1 text-primary-900 dark:text-sand-100">
                                                <Star className="w-3 h-3 text-yellow-500 fill-yellow-500" />
                                                {(benchmark.avg_rating || 0).toFixed(1)}
                                            </span>
                                            <div className="mt-0.5">
                                                <TrendIndicator value={benchmark.avg_rating || 0} benchmark={benchmark.avg_rating_market || 0} />
                                            </div>
                                        </td>
                                        <td className="px-4 py-4 text-center text-primary-900 dark:text-sand-100">
                                            {benchmark.total_bookings || 0}
                                        </td>
                                        <td className="px-6 py-4 text-right font-medium text-primary-900 dark:text-sand-100">
                                            ${(benchmark.revenue || 0).toLocaleString()}
                                        </td>
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    </div>
                )}
            </div>
        </div>
    );
}
