'use client';

import React from 'react';
import { AreaChart, Area, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer } from 'recharts';
import { Calendar, TrendingUp, TrendingDown } from 'lucide-react';
import { useOccupancyTrend } from '@/hooks/useAnalytics';
import type { TimePeriod, ChartConfig } from '@/types/analytics-types';

interface OccupancyTrendChartProps {
  period?: TimePeriod;
  propertyId?: string;
  config?: ChartConfig;
  className?: string;
}

export const OccupancyTrendChart: React.FC<OccupancyTrendChartProps> = ({
  period = 'monthly',
  propertyId,
  config = {},
  className = '',
}) => {
  const { data, isLoading, error } = useOccupancyTrend(period, propertyId);

  const {
    showGrid = true,
    showTooltip = true,
    animationDuration = 500,
    height = 350,
  } = config;

  const formatPercentage = (value: number) => `${value.toFixed(1)}%`;

  const formatDate = (dateStr: string) => {
    const date = new Date(dateStr);
    switch (period) {
      case 'daily':
        return date.toLocaleDateString('en-US', { month: 'short', day: 'numeric' });
      case 'weekly':
        return `Week ${date.toLocaleDateString('en-US', { month: 'short', day: 'numeric' })}`;
      case 'monthly':
        return date.toLocaleDateString('en-US', { month: 'short', year: 'numeric' });
      case 'yearly':
        return date.getFullYear().toString();
      default:
        return dateStr;
    }
  };

  const calculateAverageOccupancy = () => {
    if (!data || data.length === 0) return 0;
    const sum = data.reduce((acc, item) => acc + item.occupancy_rate, 0);
    return sum / data.length;
  };

  const calculateTrend = () => {
    if (!data || data.length < 2) return null;
    const recent = data[data.length - 1].occupancy_rate;
    const previous = data[data.length - 2].occupancy_rate;
    const change = recent - previous;
    return { change, isPositive: change >= 0 };
  };

  const avgOccupancy = calculateAverageOccupancy();
  const trend = calculateTrend();

  const CustomTooltip = ({ active, payload }: any) => {
    if (active && payload && payload.length) {
      const data = payload[0].payload;
      return (
        <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-lg border border-gray-200 dark:border-gray-700">
          <p className="text-sm font-medium text-gray-900 dark:text-white mb-2">
            {formatDate(data.date)}
          </p>
          <p className="text-sm text-gray-600 dark:text-gray-300">
            Occupancy: <span className="font-semibold text-blue-600">{formatPercentage(data.occupancy_rate)}</span>
          </p>
          <p className="text-sm text-gray-600 dark:text-gray-300">
            Booked: <span className="font-semibold">{data.booked_nights} nights</span>
          </p>
          <p className="text-sm text-gray-600 dark:text-gray-300">
            Available: <span className="font-semibold">{data.available_nights} nights</span>
          </p>
        </div>
      );
    }
    return null;
  };

  if (error) {
    return (
      <div className={`bg-white dark:bg-gray-800 rounded-lg p-6 ${className}`}>
        <div className="text-center text-red-600 dark:text-red-400">
          <p>Failed to load occupancy data</p>
        </div>
      </div>
    );
  }

  return (
    <div className={`bg-white dark:bg-gray-800 rounded-lg p-6 ${className}`}>
      {/* Header */}
      <div className="flex items-center justify-between mb-6">
        <div className="flex items-center space-x-3">
          <div className="p-2 bg-blue-100 dark:bg-blue-900/30 rounded-lg">
            <Calendar className="h-5 w-5 text-blue-600 dark:text-blue-400" />
          </div>
          <div>
            <h3 className="text-lg font-semibold text-gray-900 dark:text-white">Occupancy Trend</h3>
            <div className="flex items-center space-x-3 text-sm">
              <span className="text-gray-600 dark:text-gray-400">
                Average: <span className="font-semibold text-blue-600">{formatPercentage(avgOccupancy)}</span>
              </span>
              {trend && (
                <div className="flex items-center space-x-1">
                  {trend.isPositive ? (
                    <TrendingUp className="h-4 w-4 text-green-600" />
                  ) : (
                    <TrendingDown className="h-4 w-4 text-red-600" />
                  )}
                  <span className={trend.isPositive ? 'text-green-600' : 'text-red-600'}>
                    {Math.abs(trend.change).toFixed(1)}%
                  </span>
                </div>
              )}
            </div>
          </div>
        </div>
      </div>

      {/* Chart */}
      {isLoading ? (
        <div className="flex items-center justify-center" style={{ height }}>
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600"></div>
        </div>
      ) : !data || data.length === 0 ? (
        <div className="flex items-center justify-center" style={{ height }}>
          <div className="text-center text-gray-500 dark:text-gray-400">
            <Calendar className="h-12 w-12 mx-auto mb-2 opacity-50" />
            <p>No occupancy data available for this period</p>
          </div>
        </div>
      ) : (
        <ResponsiveContainer width="100%" height={height}>
          <AreaChart data={data} margin={{ top: 5, right: 30, left: 20, bottom: 5 }}>
            <defs>
              <linearGradient id="colorOccupancy" x1="0" y1="0" x2="0" y2="1">
                <stop offset="5%" stopColor="#3b82f6" stopOpacity={0.3} />
                <stop offset="95%" stopColor="#3b82f6" stopOpacity={0} />
              </linearGradient>
            </defs>
            {showGrid && <CartesianGrid strokeDasharray="3 3" stroke="#e5e7eb" />}
            <XAxis
              dataKey="date"
              tickFormatter={formatDate}
              stroke="#6b7280"
              style={{ fontSize: '12px' }}
            />
            <YAxis
              tickFormatter={formatPercentage}
              stroke="#6b7280"
              style={{ fontSize: '12px' }}
              domain={[0, 100]}
            />
            {showTooltip && <Tooltip content={<CustomTooltip />} />}
            <Area
              type="monotone"
              dataKey="occupancy_rate"
              stroke="#3b82f6"
              strokeWidth={2}
              fill="url(#colorOccupancy)"
              animationDuration={animationDuration}
            />
          </AreaChart>
        </ResponsiveContainer>
      )}
    </div>
  );
};
