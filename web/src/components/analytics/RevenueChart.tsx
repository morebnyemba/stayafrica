'use client';

import React, { useState } from 'react';
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer } from 'recharts';
import { TrendingUp, TrendingDown, DollarSign } from 'lucide-react';
import { useRevenueChart } from '@/hooks/useAnalytics';
import type { TimePeriod, ChartConfig } from '@/types/analytics-types';

interface RevenueChartProps {
  period?: TimePeriod;
  propertyId?: string;
  config?: ChartConfig;
  className?: string;
}

export const RevenueChart: React.FC<RevenueChartProps> = ({
  period = 'monthly',
  propertyId,
  config = {},
  className = '',
}) => {
  const [selectedPeriod, setSelectedPeriod] = useState<TimePeriod>(period);
  const { data, isLoading, error } = useRevenueChart(selectedPeriod, propertyId);

  const {
    showGrid = true,
    showLegend = true,
    showTooltip = true,
    animationDuration = 500,
    height = 400,
  } = config;

  const periods: { value: TimePeriod; label: string }[] = [
    { value: 'daily', label: 'Daily' },
    { value: 'weekly', label: 'Weekly' },
    { value: 'monthly', label: 'Monthly' },
    { value: 'yearly', label: 'Yearly' },
  ];

  const formatCurrency = (value: number) => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
      minimumFractionDigits: 0,
      maximumFractionDigits: 0,
    }).format(value);
  };

  const formatDate = (dateStr: string) => {
    const date = new Date(dateStr);
    switch (selectedPeriod) {
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

  const calculateTrend = () => {
    if (!data || data.length < 2) return null;
    const recent = data[data.length - 1].revenue;
    const previous = data[data.length - 2].revenue;
    const change = ((recent - previous) / previous) * 100;
    return { change, isPositive: change >= 0 };
  };

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
            Revenue: <span className="font-semibold text-green-600">{formatCurrency(data.revenue)}</span>
          </p>
          <p className="text-sm text-gray-600 dark:text-gray-300">
            Bookings: <span className="font-semibold">{data.bookings}</span>
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
          <p>Failed to load revenue data</p>
        </div>
      </div>
    );
  }

  return (
    <div className={`bg-white dark:bg-gray-800 rounded-lg p-6 ${className}`}>
      {/* Header */}
      <div className="flex items-center justify-between mb-6">
        <div className="flex items-center space-x-3">
          <div className="p-2 bg-green-100 dark:bg-green-900/30 rounded-lg">
            <DollarSign className="h-5 w-5 text-green-600 dark:text-green-400" />
          </div>
          <div>
            <h3 className="text-lg font-semibold text-gray-900 dark:text-white">Revenue Overview</h3>
            {trend && (
              <div className="flex items-center space-x-1 text-sm">
                {trend.isPositive ? (
                  <TrendingUp className="h-4 w-4 text-green-600" />
                ) : (
                  <TrendingDown className="h-4 w-4 text-red-600" />
                )}
                <span className={trend.isPositive ? 'text-green-600' : 'text-red-600'}>
                  {Math.abs(trend.change).toFixed(1)}% vs previous period
                </span>
              </div>
            )}
          </div>
        </div>

        {/* Period Selector */}
        <div className="flex space-x-1 bg-gray-100 dark:bg-gray-700 rounded-lg p-1">
          {periods.map((p) => (
            <button
              key={p.value}
              onClick={() => setSelectedPeriod(p.value)}
              className={`px-3 py-1.5 text-sm font-medium rounded-md transition-colors ${
                selectedPeriod === p.value
                  ? 'bg-white dark:bg-gray-600 text-gray-900 dark:text-white shadow-sm'
                  : 'text-gray-600 dark:text-gray-300 hover:text-gray-900 dark:hover:text-white'
              }`}
            >
              {p.label}
            </button>
          ))}
        </div>
      </div>

      {/* Chart */}
      {isLoading ? (
        <div className="flex items-center justify-center" style={{ height }}>
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-green-600"></div>
        </div>
      ) : !data || data.length === 0 ? (
        <div className="flex items-center justify-center" style={{ height }}>
          <div className="text-center text-gray-500 dark:text-gray-400">
            <DollarSign className="h-12 w-12 mx-auto mb-2 opacity-50" />
            <p>No revenue data available for this period</p>
          </div>
        </div>
      ) : (
        <ResponsiveContainer width="100%" height={height}>
          <LineChart data={data} margin={{ top: 5, right: 30, left: 20, bottom: 5 }}>
            {showGrid && <CartesianGrid strokeDasharray="3 3" stroke="#e5e7eb" />}
            <XAxis
              dataKey="date"
              tickFormatter={formatDate}
              stroke="#6b7280"
              style={{ fontSize: '12px' }}
            />
            <YAxis
              tickFormatter={formatCurrency}
              stroke="#6b7280"
              style={{ fontSize: '12px' }}
            />
            {showTooltip && <Tooltip content={<CustomTooltip />} />}
            {showLegend && (
              <Legend
                wrapperStyle={{ paddingTop: '20px' }}
                iconType="line"
              />
            )}
            <Line
              type="monotone"
              dataKey="revenue"
              stroke="#10b981"
              strokeWidth={3}
              dot={{ fill: '#10b981', r: 4 }}
              activeDot={{ r: 6 }}
              animationDuration={animationDuration}
              name="Revenue"
            />
          </LineChart>
        </ResponsiveContainer>
      )}
    </div>
  );
};
