'use client';

import React, { useState } from 'react';
import { BarChart3, RefreshCw, AlertCircle } from 'lucide-react';
import { useAuth } from '@/store/auth-store';
import { useAnalyticsDashboard } from '@/hooks/useAnalytics';
import { SummaryCards } from './SummaryCards';
import { RevenueChart } from './RevenueChart';
import { OccupancyTrendChart } from './OccupancyTrendChart';
import { BookingTimelineChart } from './BookingTimelineChart';
import { RevenueProjectionChart } from './RevenueProjectionChart';
import { PropertyPerformanceTable } from './PropertyPerformanceTable';
import { BenchmarkComparison } from './BenchmarkComparison';
import { InsightsPanel } from './InsightsPanel';
import { DateRangePicker } from './DateRangePicker';
import { ExportButton } from './ExportButton';
import type { TimePeriod, FilterOptions } from '@/types/analytics-types';

interface AnalyticsDashboardProps {
  className?: string;
}

export const AnalyticsDashboard: React.FC<AnalyticsDashboardProps> = ({ className = '' }) => {
  const { isAuthenticated, user, isLoading: authLoading } = useAuth();
  const [filters, setFilters] = useState<FilterOptions>({
    period: 'monthly',
    property_id: undefined,
    start_date: undefined,
    end_date: undefined,
  });

  const { data, isLoading, error, refetch } = useAnalyticsDashboard(filters);

  const handlePeriodChange = (period: TimePeriod) => {
    setFilters((prev) => ({ ...prev, period }));
  };

  const handleDateRangeChange = (startDate: Date | null, endDate: Date | null) => {
    setFilters((prev) => ({
      ...prev,
      start_date: startDate ? startDate.toISOString().split('T')[0] : undefined,
      end_date: endDate ? endDate.toISOString().split('T')[0] : undefined,
    }));
  };

  const periods: { value: TimePeriod; label: string }[] = [
    { value: 'daily', label: 'Daily' },
    { value: 'weekly', label: 'Weekly' },
    { value: 'monthly', label: 'Monthly' },
    { value: 'yearly', label: 'Yearly' },
  ];

  // Show loading state while checking authentication
  if (authLoading) {
    return (
      <div className={`min-h-screen bg-gray-50 dark:bg-gray-900 p-6 ${className}`}>
        <div className="max-w-7xl mx-auto">
          <div className="bg-white dark:bg-gray-800 rounded-lg p-12 text-center">
            <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto mb-4"></div>
            <p className="text-gray-600 dark:text-gray-400">Loading analytics...</p>
          </div>
        </div>
      </div>
    );
  }

  // Show message if not authenticated or not a host
  if (!isAuthenticated || user?.role !== 'host') {
    return (
      <div className={`min-h-screen bg-gray-50 dark:bg-gray-900 p-6 ${className}`}>
        <div className="max-w-7xl mx-auto">
          <div className="bg-white dark:bg-gray-800 rounded-lg p-12 text-center">
            <div className="text-yellow-600 dark:text-yellow-400 mb-4">
              <AlertCircle className="h-16 w-16 mx-auto mb-4 opacity-50" />
              <h3 className="text-xl font-semibold mb-2">Access Required</h3>
              <p className="text-gray-600 dark:text-gray-400">
                Please log in as a host to view analytics.
              </p>
            </div>
          </div>
        </div>
      </div>
    );
  }

  if (error) {
    return (
      <div className={`min-h-screen bg-gray-50 dark:bg-gray-900 p-6 ${className}`}>
        <div className="max-w-7xl mx-auto">
          <div className="bg-white dark:bg-gray-800 rounded-lg p-12 text-center">
            <div className="text-red-600 dark:text-red-400 mb-4">
              <BarChart3 className="h-16 w-16 mx-auto mb-4 opacity-50" />
              <h3 className="text-xl font-semibold mb-2">Failed to Load Analytics</h3>
              <p className="text-gray-600 dark:text-gray-400">
                {error.message || 'An error occurred while loading analytics data'}
              </p>
            </div>
            <button
              onClick={() => refetch()}
              className="mt-4 px-6 py-2 bg-blue-600 hover:bg-blue-700 text-white rounded-lg transition-colors"
            >
              Try Again
            </button>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className={`min-h-screen bg-gray-50 dark:bg-gray-900 p-6 ${className}`}>
      <div className="max-w-7xl mx-auto space-y-6">
        {/* Header */}
        <div className="bg-white dark:bg-gray-800 rounded-lg p-6 shadow-sm border border-gray-200 dark:border-gray-700">
          <div className="flex flex-col lg:flex-row lg:items-center lg:justify-between gap-4">
            <div>
              <h1 className="text-3xl font-bold text-gray-900 dark:text-white mb-2">
                Analytics Dashboard
              </h1>
              <p className="text-gray-600 dark:text-gray-400">
                Comprehensive insights into your property performance
              </p>
            </div>

            {/* Controls */}
            <div className="flex flex-wrap items-center gap-3">
              {/* Period Selector */}
              <div className="flex space-x-1 bg-gray-100 dark:bg-gray-700 rounded-lg p-1">
                {periods.map((p) => (
                  <button
                    key={p.value}
                    onClick={() => handlePeriodChange(p.value)}
                    className={`px-3 py-1.5 text-sm font-medium rounded-md transition-colors ${
                      filters.period === p.value
                        ? 'bg-white dark:bg-gray-600 text-gray-900 dark:text-white shadow-sm'
                        : 'text-gray-600 dark:text-gray-300 hover:text-gray-900 dark:hover:text-white'
                    }`}
                  >
                    {p.label}
                  </button>
                ))}
              </div>

              {/* Date Range Picker */}
              <DateRangePicker
                startDate={filters.start_date ? new Date(filters.start_date) : undefined}
                endDate={filters.end_date ? new Date(filters.end_date) : undefined}
                onDateChange={handleDateRangeChange}
              />

              {/* Refresh */}
              <button
                onClick={() => refetch()}
                disabled={isLoading}
                className="p-2 bg-gray-100 dark:bg-gray-700 hover:bg-gray-200 dark:hover:bg-gray-600 rounded-lg transition-colors"
                title="Refresh data"
              >
                <RefreshCw className={`h-5 w-5 text-gray-600 dark:text-gray-400 ${isLoading ? 'animate-spin' : ''}`} />
              </button>

              {/* Export */}
              <ExportButton data={data} dataType="dashboard" />
            </div>
          </div>
        </div>

        {/* Summary Cards */}
        <SummaryCards summary={data?.summary} isLoading={isLoading} />

        {/* Revenue and Occupancy Charts */}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
          <RevenueChart period={filters.period} propertyId={filters.property_id} />
          <OccupancyTrendChart period={filters.period} propertyId={filters.property_id} />
        </div>

        {/* Booking Timeline */}
        <BookingTimelineChart
          data={data?.booking_timeline}
          period={filters.period}
          isLoading={isLoading}
        />

        {/* Revenue Projections */}
        <RevenueProjectionChart propertyId={filters.property_id} />

        {/* Property Performance Table */}
        <PropertyPerformanceTable period={filters.period} />

        {/* Insights and Benchmarks */}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
          <InsightsPanel insights={data?.insights} isLoading={isLoading} />
          <BenchmarkComparison />
        </div>

        {/* Footer */}
        <div className="text-center text-sm text-gray-500 dark:text-gray-400 py-4">
          <p>Analytics data updates every 5 minutes</p>
        </div>
      </div>
    </div>
  );
};
