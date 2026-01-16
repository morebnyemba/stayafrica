/**
 * Analytics Types
 * TypeScript types for analytics dashboard and charts
 */

export type TimePeriod = 'daily' | 'weekly' | 'monthly' | 'yearly';

export interface DateRange {
  start_date: string; // ISO date string
  end_date: string; // ISO date string
}

export interface DashboardSummary {
  total_revenue: number;
  average_occupancy: number;
  total_bookings: number;
  average_rating: number;
  revenue_change: number; // Percentage change
  occupancy_change: number;
  bookings_change: number;
  rating_change: number;
  period: TimePeriod;
}

export interface RevenueDataPoint {
  date: string; // ISO date string
  revenue: number;
  bookings: number;
  label?: string; // Formatted date for display
}

export interface OccupancyDataPoint {
  date: string;
  occupancy_rate: number;
  booked_nights: number;
  available_nights: number;
}

export interface BookingDataPoint {
  date: string;
  count: number;
  confirmed: number;
  pending: number;
  cancelled: number;
}

export interface ProjectionDataPoint {
  date: string;
  actual_revenue?: number;
  projected_revenue: number;
  confidence_lower: number;
  confidence_upper: number;
}

export interface PropertyPerformance {
  property_id: string;
  property_name: string;
  property_image?: string;
  revenue: number;
  occupancy_rate: number;
  bookings: number;
  average_rating: number;
  revenue_per_night: number;
  days_available: number;
}

export interface BenchmarkData {
  metric: string;
  your_value: number;
  market_average: number;
  percentile: number; // 0-100
  top_10_percent: number;
  unit?: string; // e.g., "$", "%", "days"
}

export interface Insight {
  id: string;
  type: 'success' | 'warning' | 'info' | 'danger';
  title: string;
  description: string;
  recommendation?: string;
  action_url?: string;
  action_label?: string;
  metric?: string;
  value?: number | string;
}

export interface AnalyticsDashboardData {
  summary: DashboardSummary;
  revenue_chart: RevenueDataPoint[];
  occupancy_chart: OccupancyDataPoint[];
  booking_timeline: BookingDataPoint[];
  property_performance: PropertyPerformance[];
  insights: Insight[];
  period: TimePeriod;
  date_range: DateRange;
}

export interface ProjectionParams {
  property_id?: string;
  days_ahead?: number;
  period?: TimePeriod;
}

export interface ExportFormat {
  format: 'csv' | 'pdf';
  filename?: string;
  data_type: 'dashboard' | 'revenue' | 'occupancy' | 'bookings' | 'performance';
}

export interface ChartTooltipPayload {
  value: number;
  name: string;
  dataKey: string;
  color: string;
  payload: any;
}

export interface ChartConfig {
  showGrid?: boolean;
  showLegend?: boolean;
  showTooltip?: boolean;
  animationDuration?: number;
  height?: number;
  responsive?: boolean;
}

export interface MetricCardData {
  title: string;
  value: number | string;
  change?: number;
  trend?: 'up' | 'down' | 'neutral';
  format?: 'currency' | 'percentage' | 'number' | 'rating';
  icon?: string;
  subtitle?: string;
  loading?: boolean;
}

export interface PerformanceIndicatorData {
  label: string;
  value: number;
  target?: number;
  status: 'excellent' | 'good' | 'average' | 'poor';
  change?: number;
}

export interface FilterOptions {
  period: TimePeriod;
  property_id?: string;
  start_date?: string;
  end_date?: string;
}
