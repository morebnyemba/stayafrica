'use client';

import React from 'react';
import { MetricCard } from './MetricCard';
import type { DashboardSummary, MetricCardData } from '@/types/analytics-types';

interface SummaryCardsProps {
  summary?: DashboardSummary;
  isLoading?: boolean;
  className?: string;
}

export const SummaryCards: React.FC<SummaryCardsProps> = ({
  summary,
  isLoading = false,
  className = '',
}) => {
  const getMetricCards = (): MetricCardData[] => {
    if (!summary) {
      return [
        { title: 'Total Revenue', value: 0, format: 'currency', icon: 'DollarSign', loading: true },
        { title: 'Average Occupancy', value: 0, format: 'percentage', icon: 'Calendar', loading: true },
        { title: 'Total Bookings', value: 0, format: 'number', icon: 'CalendarCheck', loading: true },
        { title: 'Average Rating', value: 0, format: 'rating', icon: 'Star', loading: true },
      ];
    }

    const getTrend = (change: number) => {
      if (change > 0) return 'up';
      if (change < 0) return 'down';
      return 'neutral';
    };

    return [
      {
        title: 'Total Revenue',
        value: summary.total_revenue,
        change: summary.revenue_change,
        trend: getTrend(summary.revenue_change),
        format: 'currency',
        icon: 'DollarSign',
        subtitle: `vs previous ${summary.period}`,
        loading: isLoading,
      },
      {
        title: 'Average Occupancy',
        value: summary.average_occupancy,
        change: summary.occupancy_change,
        trend: getTrend(summary.occupancy_change),
        format: 'percentage',
        icon: 'Calendar',
        subtitle: `vs previous ${summary.period}`,
        loading: isLoading,
      },
      {
        title: 'Total Bookings',
        value: summary.total_bookings,
        change: summary.bookings_change,
        trend: getTrend(summary.bookings_change),
        format: 'number',
        icon: 'CalendarCheck',
        subtitle: `vs previous ${summary.period}`,
        loading: isLoading,
      },
      {
        title: 'Average Rating',
        value: summary.average_rating,
        change: summary.rating_change,
        trend: getTrend(summary.rating_change),
        format: 'rating',
        icon: 'Star',
        subtitle: `vs previous ${summary.period}`,
        loading: isLoading,
      },
    ];
  };

  const cards = getMetricCards();

  return (
    <div className={`grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 ${className}`}>
      {cards.map((card, index) => (
        <MetricCard key={index} data={card} />
      ))}
    </div>
  );
};
