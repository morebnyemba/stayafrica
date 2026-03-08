/**
 * Analytics Components
 * Export all analytics dashboard components
 *
 * NOTE: Chart components (RevenueChart, OccupancyTrendChart,
 * BookingTimelineChart, RevenueProjectionChart) are NOT exported here
 * because they depend on recharts (browser-only). They are loaded
 * via next/dynamic in AnalyticsDashboard.tsx with ssr: false.
 */

// Main Dashboard
export { AnalyticsDashboard } from './AnalyticsDashboard';

// Summary Components
export { SummaryCards } from './SummaryCards';
export { MetricCard } from './MetricCard';

// Table and Comparison
export { PropertyPerformanceTable } from './PropertyPerformanceTable';
export { BenchmarkComparison } from './BenchmarkComparison';

// Insights and Utilities
export { InsightsPanel } from './InsightsPanel';
export { ExportButton } from './ExportButton';

// Supporting Components
export { ChartLegend } from './ChartLegend';
export { PerformanceIndicator } from './PerformanceIndicator';

