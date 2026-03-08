'use client';

import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import type {
  AnalyticsDashboardData,
  TimePeriod,
  RevenueDataPoint,
  OccupancyDataPoint,
  ProjectionDataPoint,
  PropertyPerformance,
  BenchmarkData,
  ProjectionParams,
  FilterOptions,
} from '@/types/analytics-types';

/**
 * Hook for fetching analytics dashboard data
 */
export function useAnalyticsDashboard(filters?: FilterOptions) {
  return useQuery<AnalyticsDashboardData>({
    queryKey: ['analytics', 'dashboard', filters],
    queryFn: async () => {
      const params = new URLSearchParams();
      if (filters?.period) params.append('period', filters.period);
      if (filters?.property_id) params.append('property_id', filters.property_id);
      if (filters?.start_date) params.append('start_date', filters.start_date);
      if (filters?.end_date) params.append('end_date', filters.end_date);

      const response = await apiClient.get(`/analytics/host/dashboard_full/?${params}`);
      const data = response.data;

      // Normalize response with safe defaults
      return {
        summary: {
          total_revenue: data?.summary?.total_revenue ?? 0,
          average_occupancy: data?.summary?.average_occupancy ?? 0,
          total_bookings: data?.summary?.total_bookings ?? 0,
          average_rating: data?.summary?.average_rating ?? 0,
          revenue_change: data?.summary?.revenue_change ?? 0,
          occupancy_change: data?.summary?.occupancy_change ?? 0,
          bookings_change: data?.summary?.bookings_change ?? 0,
          rating_change: data?.summary?.rating_change ?? 0,
          period: data?.summary?.period ?? (filters?.period || 'monthly'),
        },
        revenue_chart: Array.isArray(data?.revenue_chart) ? data.revenue_chart : [],
        occupancy_chart: Array.isArray(data?.occupancy_chart) ? data.occupancy_chart : [],
        booking_timeline: Array.isArray(data?.booking_timeline) ? data.booking_timeline : [],
        property_performance: Array.isArray(data?.property_performance) ? data.property_performance : [],
        insights: Array.isArray(data?.insights) ? data.insights : [],
        period: data?.period ?? (filters?.period || 'monthly'),
        date_range: data?.date_range ?? { start_date: '', end_date: '' },
      } as AnalyticsDashboardData;
    },
    staleTime: 5 * 60 * 1000,
    retry: 2,
    retryDelay: (attemptIndex) => Math.min(1000 * 2 ** attemptIndex, 10000),
  });
}

/**
 * Hook for fetching revenue chart data
 */
export function useRevenueChart(period: TimePeriod = 'monthly', propertyId?: string) {
  return useQuery<RevenueDataPoint[]>({
    queryKey: ['analytics', 'revenue', period, propertyId],
    queryFn: async () => {
      const params = new URLSearchParams();
      params.append('period', period);
      if (propertyId) params.append('property_id', propertyId);

      const response = await apiClient.get(`/analytics/host/revenue_chart_v2/?${params}`);
      return Array.isArray(response.data) ? response.data : [];
    },
    staleTime: 5 * 60 * 1000,
    retry: 2,
    retryDelay: (attemptIndex) => Math.min(1000 * 2 ** attemptIndex, 10000),
  });
}

/**
 * Hook for fetching occupancy trend data
 */
export function useOccupancyTrend(period: TimePeriod = 'monthly', propertyId?: string) {
  return useQuery<OccupancyDataPoint[]>({
    queryKey: ['analytics', 'occupancy', period, propertyId],
    queryFn: async () => {
      const params = new URLSearchParams();
      params.append('period', period);
      if (propertyId) params.append('property_id', propertyId);

      const response = await apiClient.get(`/analytics/host/occupancy_trend_v2/?${params}`);
      return Array.isArray(response.data) ? response.data : [];
    },
    staleTime: 5 * 60 * 1000,
    retry: 2,
    retryDelay: (attemptIndex) => Math.min(1000 * 2 ** attemptIndex, 10000),
  });
}

/**
 * Hook for generating revenue projections
 */
export function useGenerateProjections() {
  const queryClient = useQueryClient();

  return useMutation<ProjectionDataPoint[], Error, ProjectionParams>({
    mutationFn: async (params) => {
      const response = await apiClient.post('/analytics/host/generate_projections/', params);
      return response.data;
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['analytics', 'projections'] });
    },
  });
}

/**
 * Hook for fetching property performance data
 */
export function usePropertyPerformance(period: TimePeriod = 'monthly') {
  return useQuery<PropertyPerformance[]>({
    queryKey: ['analytics', 'performance', period],
    queryFn: async () => {
      const params = new URLSearchParams();
      params.append('period', period);

      const response = await apiClient.get(`/analytics/properties/?${params}`);
      // DRF ReadOnlyModelViewSet returns paginated {count, results} or plain array
      const raw = response.data?.results ?? response.data;
      if (!Array.isArray(raw)) return [];

      // Normalize PropertyAnalytics → PropertyPerformance
      return raw.map((item: any) => ({
        property_id: String(item.property_id ?? item.property ?? item.id ?? ''),
        property_name: item.property_name ?? 'Unknown Property',
        property_image: item.property_image ?? undefined,
        revenue: parseFloat(item.total_revenue ?? item.revenue ?? 0),
        occupancy_rate: parseFloat(item.occupancy_rate ?? 0),
        bookings: item.bookings_count ?? item.bookings ?? 0,
        average_rating: parseFloat(item.average_rating ?? item.avg_rating ?? 0),
        revenue_per_night: parseFloat(item.avg_nightly_rate ?? item.revenue_per_night ?? 0),
        days_available: item.nights_available ?? item.days_available ?? 30,
      })) as PropertyPerformance[];
    },
    staleTime: 5 * 60 * 1000,
    retry: 2,
    retryDelay: (attemptIndex) => Math.min(1000 * 2 ** attemptIndex, 10000),
  });
}

/**
 * Hook for fetching benchmark data
 */
export function useBenchmarks(propertyType?: string, location?: string) {
  return useQuery<BenchmarkData[]>({
    queryKey: ['analytics', 'benchmarks', propertyType, location],
    queryFn: async () => {
      const params = new URLSearchParams();
      if (propertyType) params.append('property_type', propertyType);
      if (location) params.append('location', location);

      const response = await apiClient.get(`/analytics/benchmarks/?${params}`);
      // DRF ReadOnlyModelViewSet returns paginated {count, results} or plain array
      const raw = response.data?.results ?? response.data;
      if (!Array.isArray(raw) || raw.length === 0) return [];

      // Normalize PerformanceBenchmark → BenchmarkData
      const firstBenchmark = raw[0];
      return [
        {
          metric: 'Occupancy Rate',
          your_value: 0, // needs host-specific data
          market_average: parseFloat(firstBenchmark.avg_occupancy_rate ?? 0),
          percentile: 50,
          top_10_percent: parseFloat(firstBenchmark.occupancy_90th_percentile ?? 0),
          unit: '%',
        },
        {
          metric: 'Nightly Rate',
          your_value: 0,
          market_average: parseFloat(firstBenchmark.avg_nightly_rate ?? 0),
          percentile: 50,
          top_10_percent: parseFloat(firstBenchmark.rate_90th_percentile ?? 0),
          unit: '$',
        },
        {
          metric: 'Revenue/Available Night',
          your_value: 0,
          market_average: parseFloat(firstBenchmark.avg_revenue_per_available_night ?? 0),
          percentile: 50,
          top_10_percent: 0,
          unit: '$',
        },
        {
          metric: 'Avg Length of Stay',
          your_value: 0,
          market_average: parseFloat(firstBenchmark.avg_length_of_stay ?? 0),
          percentile: 50,
          top_10_percent: 0,
          unit: 'days',
        },
      ] as BenchmarkData[];
    },
    staleTime: 30 * 60 * 1000, // 30 minutes (benchmarks change less frequently)
    retry: 2,
    retryDelay: (attemptIndex) => Math.min(1000 * 2 ** attemptIndex, 10000),
  });
}

/**
 * Hook for exporting analytics data
 */
export function useExportAnalytics() {
  return useMutation({
    mutationFn: async ({ format, data }: { format: 'csv' | 'pdf'; data: any }) => {
      const response = await apiClient.post(
        `/analytics/export/`,
        { format, data },
        { responseType: 'blob' }
      );
      return response.data;
    },
  });
}
