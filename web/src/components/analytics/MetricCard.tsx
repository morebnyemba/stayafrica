'use client';

import React from 'react';
import { TrendingUp, TrendingDown, Minus, DollarSign, Calendar, Star, LucideIcon } from 'lucide-react';
import { CalendarCheck } from 'lucide-react';
import type { MetricCardData } from '@/types/analytics-types';

// Small icon map instead of importing all ~1400 icons
const ICON_MAP: Record<string, LucideIcon> = {
  DollarSign,
  Calendar,
  CalendarCheck,
  Star,
  TrendingUp,
  TrendingDown,
};

interface MetricCardProps {
  data: MetricCardData;
  className?: string;
}

export const MetricCard: React.FC<MetricCardProps> = ({ data, className = '' }) => {
  const {
    title,
    value,
    change,
    trend = 'neutral',
    format = 'number',
    icon,
    subtitle,
    loading = false,
  } = data;

  const formatValue = (val: number | string) => {
    if (typeof val === 'string') return val;

    switch (format) {
      case 'currency':
        return new Intl.NumberFormat('en-US', {
          style: 'currency',
          currency: 'USD',
          minimumFractionDigits: 0,
          maximumFractionDigits: 0,
        }).format(val);
      case 'percentage':
        return `${val.toFixed(1)}%`;
      case 'rating':
        return val.toFixed(2);
      default:
        return new Intl.NumberFormat('en-US').format(val);
    }
  };

  const getTrendIcon = () => {
    switch (trend) {
      case 'up':
        return <TrendingUp className="h-4 w-4" />;
      case 'down':
        return <TrendingDown className="h-4 w-4" />;
      default:
        return <Minus className="h-4 w-4" />;
    }
  };

  const getTrendColor = () => {
    switch (trend) {
      case 'up':
        return 'text-green-600';
      case 'down':
        return 'text-red-600';
      default:
        return 'text-primary-400';
    }
  };

  const getIconComponent = () => {
    if (!icon) return null;
    const IconComponent = ICON_MAP[icon];
    return IconComponent ? <IconComponent className="h-5 w-5" /> : null;
  };

  if (loading) {
    return (
      <div className={`bg-white rounded-lg p-6 shadow-sm border border-sand-200/50 ${className}`}>
        <div className="animate-pulse">
          <div className="h-4 bg-primary-200 rounded w-24 mb-4"></div>
          <div className="h-8 bg-primary-200 rounded w-32 mb-2"></div>
          <div className="h-3 bg-primary-200 rounded w-16"></div>
        </div>
      </div>
    );
  }

  return (
    <div className={`bg-white rounded-lg p-6 shadow-sm border border-sand-200/50 transition-all hover:shadow-md ${className}`}>
      {/* Header */}
      <div className="flex items-center justify-between mb-4">
        <h3 className="text-sm font-medium text-primary-500">
          {title}
        </h3>
        {icon && (
          <div className="p-2 bg-primary-100 rounded-lg text-primary-500">
            {getIconComponent()}
          </div>
        )}
      </div>

      {/* Value */}
      <div className="mb-2">
        <p className="text-3xl font-bold text-primary-900">
          {formatValue(value)}
        </p>
      </div>

      {/* Trend or Subtitle */}
      <div className="flex items-center justify-between">
        {change !== undefined && (
          <div className={`flex items-center space-x-1 text-sm font-medium ${getTrendColor()}`}>
            {getTrendIcon()}
            <span>
              {Math.abs(change).toFixed(1)}%
            </span>
          </div>
        )}
        {subtitle && (
          <p className="text-xs text-primary-400">
            {subtitle}
          </p>
        )}
      </div>
    </div>
  );
};
