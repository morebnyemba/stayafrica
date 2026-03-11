'use client';

import React from 'react';
import { TrendingUp, TrendingDown, Minus } from 'lucide-react';
import type { PerformanceIndicatorData } from '@/types/analytics-types';

interface PerformanceIndicatorProps {
  data: PerformanceIndicatorData;
  className?: string;
}

export const PerformanceIndicator: React.FC<PerformanceIndicatorProps> = ({
  data,
  className = '',
}) => {
  const { label, value, target, status, change } = data;

  const getStatusColor = () => {
    switch (status) {
      case 'excellent':
        return 'text-green-600 bg-green-100';
      case 'good':
        return 'text-blue-600 bg-blue-100';
      case 'average':
        return 'text-yellow-600 bg-yellow-100';
      case 'poor':
        return 'text-red-600 bg-red-100';
      default:
        return 'text-primary-500 bg-primary-100';
    }
  };

  const getStatusLabel = () => {
    return status.charAt(0).toUpperCase() + status.slice(1);
  };

  const getProgress = () => {
    if (!target) return 100;
    return Math.min((value / target) * 100, 100);
  };

  const getTrendIcon = () => {
    if (!change) return <Minus className="h-4 w-4" />;
    if (change > 0) return <TrendingUp className="h-4 w-4" />;
    return <TrendingDown className="h-4 w-4" />;
  };

  const getTrendColor = () => {
    if (!change) return 'text-primary-500';
    if (change > 0) return 'text-green-600';
    return 'text-red-600';
  };

  return (
    <div className={`bg-white rounded-xl p-4 border border-sand-200/50 ${className}`}>
      {/* Header */}
      <div className="flex items-center justify-between mb-3">
        <h4 className="text-sm font-medium text-primary-600">
          {label}
        </h4>
        <span className={`px-2 py-1 rounded-full text-xs font-semibold ${getStatusColor()}`}>
          {getStatusLabel()}
        </span>
      </div>

      {/* Value */}
      <div className="flex items-baseline space-x-2 mb-3">
        <span className="text-2xl font-bold text-primary-900">
          {value.toFixed(1)}
        </span>
        {target && (
          <span className="text-sm text-primary-500">
            / {target}
          </span>
        )}
        {change !== undefined && (
          <div className={`flex items-center space-x-1 text-sm font-medium ${getTrendColor()}`}>
            {getTrendIcon()}
            <span>{Math.abs(change).toFixed(1)}%</span>
          </div>
        )}
      </div>

      {/* Progress Bar */}
      {target && (
        <div className="w-full bg-primary-200 rounded-full h-2">
          <div
            className={`h-2 rounded-full transition-all ${
              status === 'excellent' ? 'bg-green-600' :
              status === 'good' ? 'bg-blue-600' :
              status === 'average' ? 'bg-yellow-600' : 'bg-red-600'
            }`}
            style={{ width: `${getProgress()}%` }}
          ></div>
        </div>
      )}
    </div>
  );
};
