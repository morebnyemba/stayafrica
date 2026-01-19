'use client';

import React from 'react';
import { BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer } from 'recharts';
import { CalendarCheck } from 'lucide-react';
import type { BookingDataPoint, TimePeriod, ChartConfig } from '@/types/analytics-types';

interface BookingTimelineChartProps {
  data?: BookingDataPoint[];
  period?: TimePeriod;
  config?: ChartConfig;
  className?: string;
  isLoading?: boolean;
}

export const BookingTimelineChart: React.FC<BookingTimelineChartProps> = ({
  data = [],
  period = 'monthly',
  config = {},
  className = '',
  isLoading = false,
}) => {
  const {
    showGrid = true,
    showLegend = true,
    showTooltip = true,
    animationDuration = 500,
    height = 350,
  } = config;

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

  const totalBookings = data.reduce((sum, item) => sum + (item?.count ?? 0), 0);
  const totalConfirmed = data.reduce((sum, item) => sum + (item?.confirmed ?? 0), 0);
  const confirmationRate = totalBookings > 0 ? (totalConfirmed / totalBookings) * 100 : 0;

  const CustomTooltip = ({ active, payload, label }: any) => {
    if (active && payload && payload.length) {
      const data = payload[0]?.payload;
      if (!data) return null;
      return (
        <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-lg border border-gray-200 dark:border-gray-700">
          <p className="text-sm font-medium text-gray-900 dark:text-white mb-2">
            {formatDate(label || '')}
          </p>
          <div className="space-y-1">
            <p className="text-sm text-gray-600 dark:text-gray-300">
              Total: <span className="font-semibold">{data.count ?? 0}</span>
            </p>
            <p className="text-sm text-green-600">
              Confirmed: <span className="font-semibold">{data.confirmed ?? 0}</span>
            </p>
            <p className="text-sm text-yellow-600">
              Pending: <span className="font-semibold">{data.pending ?? 0}</span>
            </p>
            <p className="text-sm text-red-600">
              Cancelled: <span className="font-semibold">{data.cancelled ?? 0}</span>
            </p>
          </div>
        </div>
      );
    }
    return null;
  };

  return (
    <div className={`bg-white dark:bg-gray-800 rounded-lg p-6 ${className}`}>
      {/* Header */}
      <div className="flex items-center justify-between mb-6">
        <div className="flex items-center space-x-3">
          <div className="p-2 bg-purple-100 dark:bg-purple-900/30 rounded-lg">
            <CalendarCheck className="h-5 w-5 text-purple-600 dark:text-purple-400" />
          </div>
          <div>
            <h3 className="text-lg font-semibold text-gray-900 dark:text-white">Booking Timeline</h3>
            <div className="flex items-center space-x-3 text-sm text-gray-600 dark:text-gray-400">
              <span>
                Total: <span className="font-semibold text-gray-900 dark:text-white">{totalBookings}</span>
              </span>
              <span>•</span>
              <span>
                Confirmation Rate: <span className="font-semibold text-green-600">{confirmationRate.toFixed(1)}%</span>
              </span>
            </div>
          </div>
        </div>
      </div>

      {/* Chart */}
      {isLoading ? (
        <div className="flex items-center justify-center" style={{ height }}>
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-purple-600"></div>
        </div>
      ) : !data || data.length === 0 ? (
        <div className="flex items-center justify-center" style={{ height }}>
          <div className="text-center text-gray-500 dark:text-gray-400">
            <CalendarCheck className="h-12 w-12 mx-auto mb-2 opacity-50" />
            <p>No booking data available for this period</p>
          </div>
        </div>
      ) : (
        <ResponsiveContainer width="100%" height={height}>
          <BarChart data={data} margin={{ top: 5, right: 30, left: 20, bottom: 5 }}>
            {showGrid && <CartesianGrid strokeDasharray="3 3" stroke="#e5e7eb" />}
            <XAxis
              dataKey="date"
              tickFormatter={formatDate}
              stroke="#6b7280"
              style={{ fontSize: '12px' }}
            />
            <YAxis
              stroke="#6b7280"
              style={{ fontSize: '12px' }}
            />
            {showTooltip && <Tooltip content={<CustomTooltip />} />}
            {showLegend && (
              <Legend
                wrapperStyle={{ paddingTop: '20px' }}
              />
            )}
            <Bar
              dataKey="confirmed"
              stackId="a"
              fill="#10b981"
              animationDuration={animationDuration}
              name="Confirmed"
              radius={[0, 0, 0, 0]}
            />
            <Bar
              dataKey="pending"
              stackId="a"
              fill="#f59e0b"
              animationDuration={animationDuration}
              name="Pending"
              radius={[0, 0, 0, 0]}
            />
            <Bar
              dataKey="cancelled"
              stackId="a"
              fill="#ef4444"
              animationDuration={animationDuration}
              name="Cancelled"
              radius={[4, 4, 0, 0]}
            />
          </BarChart>
        </ResponsiveContainer>
      )}
    </div>
  );
};
