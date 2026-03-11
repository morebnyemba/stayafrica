'use client';

import React, { useState } from 'react';
import dynamic from 'next/dynamic';
import { BarChart3, RefreshCw, AlertCircle } from 'lucide-react';
import { useAuth } from '@/store/auth-store';
import { useAnalyticsDashboard } from '@/hooks/useAnalytics';
import { SummaryCards } from './SummaryCards';
import { PropertyPerformanceTable } from './PropertyPerformanceTable';
import { BenchmarkComparison } from './BenchmarkComparison';
import { InsightsPanel } from './InsightsPanel';
import { ExportButton } from './ExportButton';
import type { TimePeriod, FilterOptions } from '@/types/analytics-types';

// Dynamic imports for components depending on browser-only libs (recharts, date-fns)
const RevenueChart = dynamic(() => import('./RevenueChart').then(m => ({ default: m.RevenueChart })), { ssr: false });
const OccupancyTrendChart = dynamic(() => import('./OccupancyTrendChart').then(m => ({ default: m.OccupancyTrendChart })), { ssr: false });
const BookingTimelineChart = dynamic(() => import('./BookingTimelineChart').then(m => ({ default: m.BookingTimelineChart })), { ssr: false });
const RevenueProjectionChart = dynamic(() => import('./RevenueProjectionChart').then(m => ({ default: m.RevenueProjectionChart })), { ssr: false });
const DateRangePicker = dynamic(() => import('./DateRangePicker').then(m => ({ default: m.DateRangePicker })), { ssr: false });

// Error boundary to gracefully handle import/render failures
class ChartErrorBoundary extends React.Component<
  { children: React.ReactNode; name?: string },
  { hasError: boolean }
> {
  constructor(props: { children: React.ReactNode; name?: string }) {
    super(props);
    this.state = { hasError: false };
  }
  static getDerivedStateFromError() {
    return { hasError: true };
  }
  render() {
    if (this.state.hasError) {
      return (
        <div className="bg-white rounded-xl p-6 border border-sand-200/50 text-center">
          <BarChart3 className="h-10 w-10 mx-auto mb-2 text-primary-400" />
          <p className="text-primary-500 text-sm">
            {this.props.name ?? 'Chart'} could not be loaded. Please refresh the page.
          </p>
        </div>
      );
    }
    return this.props.children;
  }
}

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
      <div className={`p-6 ${className}`}>
        <div className="max-w-7xl mx-auto">
          <div className="bg-white rounded-xl p-12 text-center">
            <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-secondary-600 mx-auto mb-4"></div>
            <p className="text-primary-500">Loading analytics...</p>
          </div>
        </div>
      </div>
    );
  }

  // Show message if not authenticated or not a host
  if (!isAuthenticated || user?.role !== 'host') {
    return (
      <div className={`p-6 ${className}`}>
        <div className="max-w-7xl mx-auto">
          <div className="bg-white rounded-xl p-12 text-center">
            <div className="text-yellow-600 mb-4">
              <AlertCircle className="h-16 w-16 mx-auto mb-4 opacity-50" />
              <h3 className="text-xl font-semibold mb-2">Access Required</h3>
              <p className="text-primary-500">
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
      <div className={`p-6 ${className}`}>
        <div className="max-w-7xl mx-auto">
          <div className="bg-white rounded-xl p-12 text-center">
            <div className="text-red-600 mb-4">
              <BarChart3 className="h-16 w-16 mx-auto mb-4 opacity-50" />
              <h3 className="text-xl font-semibold mb-2">Failed to Load Analytics</h3>
              <p className="text-primary-500">
                {error.message || 'An error occurred while loading analytics data'}
              </p>
            </div>
            <button
              onClick={() => refetch()}
              className="mt-4 px-6 py-2 bg-secondary-600 hover:bg-secondary-700 text-white rounded-lg transition-colors"
            >
              Try Again
            </button>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className={`p-6 ${className}`}>
      <div className="max-w-7xl mx-auto space-y-6">
        {/* Header */}
        <div className="bg-white rounded-xl p-6 shadow-sm border border-sand-200/50">
          <div className="flex flex-col lg:flex-row lg:items-center lg:justify-between gap-4">
            <div>
              <h1 className="text-3xl font-bold text-primary-900 mb-2">
                Analytics Dashboard
              </h1>
              <p className="text-primary-500">
                Comprehensive insights into your property performance
              </p>
            </div>

            {/* Controls */}
            <div className="flex flex-wrap items-center gap-3">
              {/* Period Selector */}
              <div className="flex space-x-1 bg-primary-100 rounded-lg p-1">
                {periods.map((p) => (
                  <button
                    key={p.value}
                    onClick={() => handlePeriodChange(p.value)}
                    className={`px-3 py-1.5 text-sm font-medium rounded-md transition-colors ${
                      filters.period === p.value
                        ? 'bg-white text-primary-900 shadow-sm'
                        : 'text-primary-500 hover:text-primary-900'
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
                className="p-2 bg-primary-100 hover:bg-primary-200 rounded-lg transition-colors"
                title="Refresh data"
              >
                <RefreshCw className={`h-5 w-5 text-primary-500 ${isLoading ? 'animate-spin' : ''}`} />
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
          <ChartErrorBoundary name="Revenue Chart"><RevenueChart period={filters.period} propertyId={filters.property_id} /></ChartErrorBoundary>
          <ChartErrorBoundary name="Occupancy Chart"><OccupancyTrendChart period={filters.period} propertyId={filters.property_id} /></ChartErrorBoundary>
        </div>

        {/* Booking Timeline */}
        <ChartErrorBoundary name="Booking Timeline">
          <BookingTimelineChart
            data={data?.booking_timeline}
            period={filters.period}
            isLoading={isLoading}
          />
        </ChartErrorBoundary>

        {/* Revenue Projections */}
        <ChartErrorBoundary name="Revenue Projections"><RevenueProjectionChart propertyId={filters.property_id} /></ChartErrorBoundary>

        {/* Property Performance Table */}
        <PropertyPerformanceTable period={filters.period} />

        {/* Insights and Benchmarks */}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
          <InsightsPanel insights={data?.insights} isLoading={isLoading} />
          <BenchmarkComparison />
        </div>

        {/* Footer */}
        <div className="text-center text-sm text-primary-400 py-4">
          <p>Analytics data updates every 5 minutes</p>
        </div>
      </div>
    </div>
  );
};
