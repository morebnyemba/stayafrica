'use client';

import React, { useState } from 'react';
import { Line, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer, Area, ComposedChart } from 'recharts';
import { TrendingUp, RefreshCw } from 'lucide-react';
import { useGenerateProjections } from '@/hooks/useAnalytics';
import type { ProjectionDataPoint, ChartConfig } from '@/types/analytics-types';

interface RevenueProjectionChartProps {
  propertyId?: string;
  daysAhead?: number;
  config?: ChartConfig;
  className?: string;
}

export const RevenueProjectionChart: React.FC<RevenueProjectionChartProps> = ({
  propertyId,
  daysAhead = 30,
  config = {},
  className = '',
}) => {
  const [projectionData, setProjectionData] = useState<ProjectionDataPoint[]>([]);
  const [error, setError] = useState<string | null>(null);
  const { mutate: generateProjections, isPending } = useGenerateProjections();

  const {
    showGrid = true,
    showLegend = true,
    showTooltip = true,
    animationDuration = 500,
    height = 400,
  } = config;

  const handleGenerateProjections = React.useCallback(() => {
    setError(null);
    generateProjections(
      { property_id: propertyId, days_ahead: daysAhead },
      {
        onSuccess: (data) => {
          setProjectionData(Array.isArray(data) ? data : []);
        },
        onError: (err) => {
          setError(err?.message || 'Failed to generate projections');
        },
      }
    );
  }, [propertyId, daysAhead, generateProjections]);

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
    return date.toLocaleDateString('en-US', { month: 'short', day: 'numeric' });
  };

  const CustomTooltip = ({ active, payload }: any) => {
    if (active && payload && payload.length) {
      const data = payload[0]?.payload;
      if (!data) return null;
      return (
        <div className="bg-white p-4 rounded-lg shadow-lg border border-sand-200">
          <p className="text-sm font-medium text-primary-900 mb-2">
            {formatDate(data.date || '')}
          </p>
          {data.actual_revenue != null && (
            <p className="text-sm text-primary-500">
              Actual: <span className="font-semibold text-blue-600">{formatCurrency(data.actual_revenue)}</span>
            </p>
          )}
          <p className="text-sm text-primary-500">
            Projected: <span className="font-semibold text-green-600">{formatCurrency(data.projected_revenue ?? 0)}</span>
          </p>
          <p className="text-sm text-primary-500 text-xs mt-1">
            Range: {formatCurrency(data.confidence_lower ?? 0)} - {formatCurrency(data.confidence_upper ?? 0)}
          </p>
        </div>
      );
    }
    return null;
  };

  return (
    <div className={`bg-white rounded-xl p-6 ${className}`}>
      {/* Header */}
      <div className="flex items-center justify-between mb-6">
        <div className="flex items-center space-x-3">
          <div className="p-2 bg-green-100 rounded-lg">
            <TrendingUp className="h-5 w-5 text-green-600" />
          </div>
          <div>
            <h3 className="text-lg font-semibold text-primary-900">Revenue Projections</h3>
            <p className="text-sm text-primary-500">
              Next {daysAhead} days forecast with confidence intervals
            </p>
          </div>
        </div>

        <button
          onClick={handleGenerateProjections}
          disabled={isPending}
          className="flex items-center space-x-2 px-4 py-2 bg-green-600 hover:bg-green-700 disabled:bg-primary-300 text-white rounded-lg transition-colors"
        >
          <RefreshCw className={`h-4 w-4 ${isPending ? 'animate-spin' : ''}`} />
          <span>{isPending ? 'Generating...' : 'Refresh'}</span>
        </button>
      </div>

      {/* Chart */}
      {isPending ? (
        <div className="flex items-center justify-center" style={{ height }}>
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-green-600"></div>
        </div>
      ) : error ? (
        <div className="flex items-center justify-center" style={{ height }}>
          <div className="text-center text-red-500">
            <TrendingUp className="h-12 w-12 mx-auto mb-2 opacity-50" />
            <p className="mb-2">{error}</p>
            <button
              onClick={handleGenerateProjections}
              className="text-sm underline hover:no-underline"
            >
              Try again
            </button>
          </div>
        </div>
      ) : !projectionData || projectionData.length === 0 ? (
        <div className="flex items-center justify-center" style={{ height }}>
          <div className="text-center text-primary-400">
            <TrendingUp className="h-12 w-12 mx-auto mb-2 opacity-50" />
            <p>Click &quot;Refresh&quot; to generate projections</p>
          </div>
        </div>
      ) : (
        <ResponsiveContainer width="100%" height={height}>
          <ComposedChart data={projectionData} margin={{ top: 5, right: 30, left: 20, bottom: 5 }}>
            <defs>
              <linearGradient id="colorConfidence" x1="0" y1="0" x2="0" y2="1">
                <stop offset="5%" stopColor="#10b981" stopOpacity={0.1} />
                <stop offset="95%" stopColor="#10b981" stopOpacity={0.05} />
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
              tickFormatter={formatCurrency}
              stroke="#6b7280"
              style={{ fontSize: '12px' }}
            />
            {showTooltip && <Tooltip content={<CustomTooltip />} />}
            {showLegend && (
              <Legend
                wrapperStyle={{ paddingTop: '20px' }}
              />
            )}
            {/* Confidence interval */}
            <Area
              type="monotone"
              dataKey="confidence_upper"
              stroke="none"
              fill="url(#colorConfidence)"
              animationDuration={animationDuration}
              name="Confidence Range"
            />
            <Area
              type="monotone"
              dataKey="confidence_lower"
              stroke="none"
              fill="#ffffff"
              animationDuration={animationDuration}
            />
            {/* Actual revenue */}
            <Line
              type="monotone"
              dataKey="actual_revenue"
              stroke="#3b82f6"
              strokeWidth={2}
              dot={{ fill: '#3b82f6', r: 3 }}
              animationDuration={animationDuration}
              name="Actual Revenue"
              connectNulls
            />
            {/* Projected revenue */}
            <Line
              type="monotone"
              dataKey="projected_revenue"
              stroke="#10b981"
              strokeWidth={3}
              strokeDasharray="5 5"
              dot={{ fill: '#10b981', r: 4 }}
              activeDot={{ r: 6 }}
              animationDuration={animationDuration}
              name="Projected Revenue"
            />
          </ComposedChart>
        </ResponsiveContainer>
      )}

      {/* Legend Info */}
      {projectionData.length > 0 && (
        <div className="mt-4 flex items-center justify-center space-x-6 text-sm text-primary-500">
          <div className="flex items-center space-x-2">
            <div className="w-8 h-0.5 bg-blue-600"></div>
            <span>Actual</span>
          </div>
          <div className="flex items-center space-x-2">
            <svg width="32" height="2" className="overflow-visible">
              <line x1="0" y1="1" x2="32" y2="1" stroke="#10b981" strokeWidth="2" strokeDasharray="5,5" />
            </svg>
            <span>Projected</span>
          </div>
          <div className="flex items-center space-x-2">
            <div className="w-8 h-4 bg-green-100"></div>
            <span>Confidence Range</span>
          </div>
        </div>
      )}
    </div>
  );
};
