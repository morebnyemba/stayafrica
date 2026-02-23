import React, { useState, useEffect, useCallback } from 'react';
import {
    View,
    Text,
    ScrollView,
    TouchableOpacity,
    ActivityIndicator,
} from 'react-native';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { router } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { apiClient } from '@/services/api-client';

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

function TrendBadge({ value, benchmark }: { value: number; benchmark: number }) {
    if (!benchmark) return null;
    const diff = value - benchmark;
    const pct = Math.round((diff / benchmark) * 100);

    if (Math.abs(pct) < 2) {
        return (
            <View className="flex-row items-center bg-gray-100 px-2 py-0.5 rounded-full">
                <Ionicons name="remove" size={10} color="#6b7280" />
                <Text className="text-xs text-gray-500 ml-0.5">On par</Text>
            </View>
        );
    }

    return pct > 0 ? (
        <View className="flex-row items-center bg-emerald-100 px-2 py-0.5 rounded-full">
            <Ionicons name="trending-up" size={10} color="#059669" />
            <Text className="text-xs text-emerald-700 ml-0.5">+{pct}%</Text>
        </View>
    ) : (
        <View className="flex-row items-center bg-red-100 px-2 py-0.5 rounded-full">
            <Ionicons name="trending-down" size={10} color="#dc2626" />
            <Text className="text-xs text-red-600 ml-0.5">{pct}%</Text>
        </View>
    );
}

export default function HostAnalyticsScreen() {
    const insets = useSafeAreaInsets();
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

    const periods = [
        { key: 'week', label: 'Week' },
        { key: 'month', label: 'Month' },
        { key: 'quarter', label: 'Quarter' },
        { key: 'year', label: 'Year' },
    ];

    return (
        <SafeAreaView className="flex-1 bg-sand-100">
            <ScrollView className="flex-1" showsVerticalScrollIndicator={false}>
                {/* Header */}
                <LinearGradient
                    colors={['#122F26', '#1d392f', '#2d4a40']}
                    start={{ x: 0, y: 0 }}
                    end={{ x: 1, y: 1 }}
                    className="px-4 pb-8"
                    style={{ paddingTop: insets.top + 12 }}
                >
                    <View className="flex-row items-center mb-2">
                        <TouchableOpacity
                            onPress={() => router.back()}
                            className="w-10 h-10 rounded-xl items-center justify-center mr-3"
                            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
                        >
                            <Ionicons name="arrow-back" size={24} color="#fff" />
                        </TouchableOpacity>
                        <Text className="text-3xl font-black text-white tracking-tight">Analytics</Text>
                    </View>
                    <Text className="text-sand-200 text-sm ml-12">
                        Performance vs. market benchmarks
                    </Text>
                </LinearGradient>

                {/* Period Selector */}
                <View className="px-4 -mt-4">
                    <View className="flex-row bg-white rounded-xl p-1 border border-gray-200">
                        {periods.map(p => (
                            <TouchableOpacity
                                key={p.key}
                                onPress={() => setPeriod(p.key)}
                                className={`flex-1 py-2 rounded-lg items-center ${period === p.key ? 'bg-emerald-600' : ''
                                    }`}
                            >
                                <Text className={`text-sm font-medium ${period === p.key ? 'text-white' : 'text-gray-500'
                                    }`}>
                                    {p.label}
                                </Text>
                            </TouchableOpacity>
                        ))}
                    </View>
                </View>

                {/* Summary Cards */}
                <View className="px-4 mt-4 flex-row flex-wrap">
                    {[
                        { label: 'Occupancy', value: `${avgOccupancy}%`, icon: 'calendar' as const, color: '#059669' },
                        { label: 'Avg Rate', value: `$${avgRate}`, icon: 'cash' as const, color: '#8b5cf6' },
                        { label: 'Rating', value: avgRating, icon: 'star' as const, color: '#f59e0b' },
                        { label: 'Revenue', value: `$${totalRevenue.toLocaleString()}`, icon: 'trending-up' as const, color: '#3b82f6' },
                    ].map((card, i) => (
                        <View key={i} className="w-1/2 p-1">
                            <View
                                className="bg-white rounded-2xl p-4"
                                style={{
                                    shadowColor: '#122F26',
                                    shadowOffset: { width: 0, height: 2 },
                                    shadowOpacity: 0.05,
                                    shadowRadius: 4,
                                    elevation: 2,
                                }}
                            >
                                <View
                                    className="w-10 h-10 rounded-full items-center justify-center mb-3"
                                    style={{ backgroundColor: `${card.color}15` }}
                                >
                                    <Ionicons name={card.icon} size={20} color={card.color} />
                                </View>
                                <Text className="text-xs text-gray-500">{card.label}</Text>
                                <Text className="text-xl font-bold text-gray-900 mt-0.5">{card.value}</Text>
                            </View>
                        </View>
                    ))}
                </View>

                {/* Property Benchmarks */}
                <View className="px-4 mt-6 mb-8">
                    <Text className="text-lg font-bold text-gray-900 mb-3">Property Benchmarks</Text>

                    {loading ? (
                        <View className="items-center py-12">
                            <ActivityIndicator size="large" color="#059669" />
                            <Text className="text-sm text-gray-500 mt-3">Loading benchmarks...</Text>
                        </View>
                    ) : benchmarks.length === 0 ? (
                        <View className="items-center py-12 bg-white rounded-2xl">
                            <Ionicons name="bar-chart-outline" size={48} color="#d1d5db" />
                            <Text className="text-sm text-gray-500 mt-3 text-center px-6">
                                No benchmark data yet. Data appears after your properties receive bookings.
                            </Text>
                        </View>
                    ) : (
                        benchmarks.map(benchmark => (
                            <View
                                key={benchmark.property_id}
                                className="bg-white rounded-2xl p-4 mb-3"
                                style={{
                                    shadowColor: '#122F26',
                                    shadowOffset: { width: 0, height: 2 },
                                    shadowOpacity: 0.05,
                                    shadowRadius: 4,
                                    elevation: 2,
                                }}
                            >
                                <Text className="text-base font-semibold text-gray-900 mb-3" numberOfLines={1}>
                                    {benchmark.property_title}
                                </Text>

                                <View className="flex-row justify-between mb-2">
                                    <View className="items-center flex-1">
                                        <Text className="text-xs text-gray-500">Occupancy</Text>
                                        <Text className="text-base font-bold text-gray-900">{benchmark.occupancy_rate || 0}%</Text>
                                        <TrendBadge value={benchmark.occupancy_rate || 0} benchmark={benchmark.avg_occupancy_market || 0} />
                                    </View>
                                    <View className="items-center flex-1">
                                        <Text className="text-xs text-gray-500">Avg Rate</Text>
                                        <Text className="text-base font-bold text-gray-900">${benchmark.avg_nightly_rate || 0}</Text>
                                        <TrendBadge value={benchmark.avg_nightly_rate || 0} benchmark={benchmark.avg_rate_market || 0} />
                                    </View>
                                    <View className="items-center flex-1">
                                        <Text className="text-xs text-gray-500">Rating</Text>
                                        <View className="flex-row items-center">
                                            <Ionicons name="star" size={12} color="#f59e0b" />
                                            <Text className="text-base font-bold text-gray-900 ml-1">{(benchmark.avg_rating || 0).toFixed(1)}</Text>
                                        </View>
                                        <TrendBadge value={benchmark.avg_rating || 0} benchmark={benchmark.avg_rating_market || 0} />
                                    </View>
                                </View>

                                <View className="flex-row justify-between mt-2 pt-2 border-t border-gray-100">
                                    <Text className="text-xs text-gray-500">
                                        {benchmark.total_bookings || 0} bookings · {benchmark.total_reviews || 0} reviews
                                    </Text>
                                    <Text className="text-sm font-bold text-emerald-600">
                                        ${(benchmark.revenue || 0).toLocaleString()}
                                    </Text>
                                </View>
                            </View>
                        ))
                    )}
                </View>
            </ScrollView>
        </SafeAreaView>
    );
}
