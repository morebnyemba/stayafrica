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

      const response = await apiClient.get(`/analytics/host/dashboard/?${params}`);
      return response.data;
    },
    staleTime: 5 * 60 * 1000, // 5 minutes
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

      const response = await apiClient.get(`/analytics/host/revenue_chart/?${params}`);
      return response.data;
    },
    staleTime: 5 * 60 * 1000,
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

      const response = await apiClient.get(`/analytics/host/occupancy_trend/?${params}`);
      return response.data;
    },
    staleTime: 5 * 60 * 1000,
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
      return response.data;
    },
    staleTime: 5 * 60 * 1000,
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
      return response.data;
    },
    staleTime: 30 * 60 * 1000, // 30 minutes (benchmarks change less frequently)
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
