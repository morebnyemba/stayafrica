'use client';

import { useAuth } from '@/context/auth-context';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { ProtectedRoute } from '@/components/auth/protected-route';
import { useState } from 'react';
import {
  DollarSign,
  TrendingUp,
  TrendingDown,
  Calendar,
  Download,
  Filter,
  CreditCard,
  Clock,
  CheckCircle,
} from 'lucide-react';

type Period = 'week' | 'month' | 'year';

export function HostEarningsContent() {
  const { user, isAuthenticated } = useAuth();
  const [period, setPeriod] = useState<Period>('month');
  const [showFilters, setShowFilters] = useState(false);

  // Fetch earnings data
  const { data: earningsData, isLoading: loadingEarnings } = useQuery({
    queryKey: ['host', 'earnings', period],
    queryFn: async () => {
      const response = await apiClient.getHostEarnings(period);
      return response.data;
    },
    enabled: isAuthenticated && user?.role === 'host',
  });

  // Fetch analytics for summary
  const { data: analytics } = useQuery({
    queryKey: ['host', 'analytics'],
    queryFn: async () => {
      const response = await apiClient.getHostAnalytics();
      return response.data;
    },
    enabled: isAuthenticated && user?.role === 'host',
  });

  const earnings = earningsData?.earnings || [];
  
  // Calculate totals and trends
  const totalEarnings = earnings.reduce((sum: number, e: any) => sum + parseFloat(e.total || 0), 0);
  
  // Get trend (compare last 2 periods)
  const lastPeriod = earnings[earnings.length - 1];
  const prevPeriod = earnings[earnings.length - 2];
  const trend = lastPeriod && prevPeriod 
    ? ((parseFloat(lastPeriod.total) - parseFloat(prevPeriod.total)) / parseFloat(prevPeriod.total)) * 100
    : 0;

  const summaryCards = [
    {
      title: 'Total Earnings',
      value: `$${analytics?.total_earnings?.toFixed(2) || '0.00'}`,
      icon: DollarSign,
      color: 'text-green-600 dark:text-green-400',
      bgColor: 'bg-green-100 dark:bg-green-900/30',
      description: 'All-time earnings',
    },
    {
      title: 'This Period',
      value: `$${lastPeriod?.total || '0.00'}`,
      icon: Calendar,
      color: 'text-blue-600 dark:text-blue-400',
      bgColor: 'bg-blue-100 dark:bg-blue-900/30',
      description: `${period} earnings`,
      trend: trend !== 0 ? trend : null,
    },
    {
      title: 'Pending Payout',
      value: `$${analytics?.pending_earnings?.toFixed(2) || '0.00'}`,
      icon: Clock,
      color: 'text-yellow-600 dark:text-yellow-400',
      bgColor: 'bg-yellow-100 dark:bg-yellow-900/30',
      description: 'Awaiting payout',
    },
    {
      title: 'Completed Bookings',
      value: analytics?.completed_bookings || 0,
      icon: CheckCircle,
      color: 'text-purple-600 dark:text-purple-400',
      bgColor: 'bg-purple-100 dark:bg-purple-900/30',
      description: 'Paid bookings',
    },
  ];

  return (
    <ProtectedRoute>
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          {/* Header */}
          <header className="mb-8">
            <div className="flex flex-col sm:flex-row sm:items-center sm:justify-between gap-4">
              <div>
                <h1 className="text-2xl sm:text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-2">
                  Earnings Dashboard
                </h1>
                <p className="text-base sm:text-lg text-primary-600 dark:text-sand-300">
                  Track your revenue and financial performance
                </p>
              </div>
              
              <div className="flex items-center gap-3">
                <button
                  onClick={() => setShowFilters(!showFilters)}
                  className="btn-secondary px-4 py-2 flex items-center gap-2"
                  aria-label="Toggle filters"
                >
                  <Filter className="w-4 h-4" />
                  <span className="hidden sm:inline">Filters</span>
                </button>
                <button
                  className="btn-primary px-4 py-2 flex items-center gap-2"
                  aria-label="Download earnings report"
                >
                  <Download className="w-4 h-4" />
                  <span className="hidden sm:inline">Export</span>
                </button>
              </div>
            </div>
          </header>

          {/* Period Selector */}
          <div className="mb-6">
            <div className="inline-flex rounded-lg border border-primary-200 dark:border-primary-700 bg-white dark:bg-primary-800 p-1">
              {(['week', 'month', 'year'] as Period[]).map((p) => (
                <button
                  key={p}
                  onClick={() => setPeriod(p)}
                  className={`px-4 sm:px-6 py-2 rounded-md font-medium transition-colors ${
                    period === p
                      ? 'bg-secondary-500 text-white'
                      : 'text-primary-700 dark:text-sand-300 hover:bg-primary-50 dark:hover:bg-primary-700'
                  }`}
                  aria-label={`View ${p} earnings`}
                >
                  {p.charAt(0).toUpperCase() + p.slice(1)}
                </button>
              ))}
            </div>
          </div>

          {/* Summary Cards */}
          <section aria-labelledby="earnings-summary-heading" className="mb-8">
            <h2 id="earnings-summary-heading" className="sr-only">Earnings Summary</h2>
            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 sm:gap-6">
              {summaryCards.map((card, index) => (
                <div
                  key={index}
                  className="card p-4 sm:p-6 hover:shadow-lg transition-shadow"
                >
                  <div className="flex items-start justify-between mb-3">
                    <div className={`p-3 rounded-lg ${card.bgColor}`}>
                      <card.icon className={`w-5 h-5 sm:w-6 sm:h-6 ${card.color}`} />
                    </div>
                    {card.trend !== null && card.trend !== undefined && (
                      <div className={`flex items-center gap-1 text-xs sm:text-sm font-semibold ${
                        card.trend > 0 ? 'text-green-600 dark:text-green-400' : 'text-red-600 dark:text-red-400'
                      }`}>
                        {card.trend > 0 ? (
                          <TrendingUp className="w-4 h-4" aria-hidden="true" />
                        ) : (
                          <TrendingDown className="w-4 h-4" aria-hidden="true" />
                        )}
                        {Math.abs(card.trend).toFixed(1)}%
                      </div>
                    )}
                  </div>
                  <h3 className="text-xs sm:text-sm font-medium text-primary-600 dark:text-sand-400 mb-1">
                    {card.title}
                  </h3>
                  <p className="text-xl sm:text-2xl font-bold text-primary-900 dark:text-sand-50 mb-1">
                    {card.value}
                  </p>
                  <p className="text-xs text-primary-500 dark:text-sand-500">
                    {card.description}
                  </p>
                </div>
              ))}
            </div>
          </section>

          {/* Earnings Timeline */}
          <section aria-labelledby="earnings-timeline-heading" className="card p-4 sm:p-6 mb-8">
            <h2 id="earnings-timeline-heading" className="text-xl sm:text-2xl font-bold text-primary-900 dark:text-sand-50 mb-6">
              Earnings Timeline
            </h2>
            
            {loadingEarnings ? (
              <div className="space-y-4" aria-busy="true" aria-live="polite">
                {[1, 2, 3].map((i) => (
                  <div key={i} className="animate-pulse h-16 bg-primary-200 dark:bg-primary-700 rounded-lg" />
                ))}
              </div>
            ) : earnings.length > 0 ? (
              <div className="space-y-4">
                {earnings.map((earning: any, index: number) => {
                  const amount = parseFloat(earning.total || 0);
                  const bookingsCount = earning.bookings || 0;
                  
                  return (
                    <article
                      key={index}
                      className="flex flex-col sm:flex-row sm:items-center justify-between p-4 border border-primary-100 dark:border-primary-700 rounded-lg hover:bg-primary-50 dark:hover:bg-primary-800 transition-colors"
                      aria-label={`Earnings for ${earning.period}`}
                    >
                      <div className="flex-1 mb-3 sm:mb-0">
                        <div className="flex items-center gap-3 mb-2">
                          <Calendar className="w-5 h-5 text-secondary-600 dark:text-secondary-400" aria-hidden="true" />
                          <h3 className="font-semibold text-primary-900 dark:text-sand-50">
                            {earning.period}
                          </h3>
                        </div>
                        <div className="flex items-center gap-4 text-sm text-primary-600 dark:text-sand-300">
                          <span>{bookingsCount} booking{bookingsCount !== 1 ? 's' : ''}</span>
                          <span>•</span>
                          <span>Avg: ${bookingsCount > 0 ? (amount / bookingsCount).toFixed(2) : '0.00'}</span>
                        </div>
                      </div>
                      
                      <div className="flex items-center gap-4">
                        {/* Visual bar */}
                        <div className="hidden sm:block w-32 h-2 bg-primary-200 dark:bg-primary-700 rounded-full overflow-hidden">
                          <div
                            className="h-full bg-secondary-500"
                            style={{
                              width: `${Math.min((amount / totalEarnings) * 100, 100)}%`,
                            }}
                            aria-label={`${((amount / totalEarnings) * 100).toFixed(0)}% of total`}
                          />
                        </div>
                        
                        <div className="text-right">
                          <p className="text-xl sm:text-2xl font-bold text-green-600 dark:text-green-400">
                            ${amount.toFixed(2)}
                          </p>
                          <p className="text-xs text-primary-500 dark:text-sand-500">
                            Net earnings
                          </p>
                        </div>
                      </div>
                    </article>
                  );
                })}
              </div>
            ) : (
              <div className="text-center py-12">
                <CreditCard className="w-12 h-12 text-primary-300 dark:text-primary-700 mx-auto mb-4" />
                <h3 className="text-lg font-semibold text-primary-900 dark:text-sand-50 mb-2">
                  No Earnings Yet
                </h3>
                <p className="text-primary-600 dark:text-sand-300">
                  Start receiving bookings to see your earnings here.
                </p>
              </div>
            )}
          </section>

          {/* Payout Information */}
          <section aria-labelledby="payout-info-heading" className="card p-4 sm:p-6">
            <h2 id="payout-info-heading" className="text-xl sm:text-2xl font-bold text-primary-900 dark:text-sand-50 mb-4">
              Payout Information
            </h2>
            
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
              <div className="space-y-3">
                <h3 className="font-semibold text-primary-900 dark:text-sand-50 mb-2">
                  How Payouts Work
                </h3>
                <ul className="space-y-2 text-sm text-primary-600 dark:text-sand-300">
                  <li className="flex items-start gap-2">
                    <CheckCircle className="w-4 h-4 text-green-500 mt-0.5 flex-shrink-0" />
                    <span>Earnings are calculated after platform commission (15%)</span>
                  </li>
                  <li className="flex items-start gap-2">
                    <CheckCircle className="w-4 h-4 text-green-500 mt-0.5 flex-shrink-0" />
                    <span>Payouts are processed within 24 hours of booking completion</span>
                  </li>
                  <li className="flex items-start gap-2">
                    <CheckCircle className="w-4 h-4 text-green-500 mt-0.5 flex-shrink-0" />
                    <span>You can track pending and completed payouts in this dashboard</span>
                  </li>
                </ul>
              </div>
              
              <div className="p-4 bg-secondary-50 dark:bg-secondary-900/20 rounded-lg border border-secondary-200 dark:border-secondary-800">
                <h3 className="font-semibold text-primary-900 dark:text-sand-50 mb-3">
                  Next Payout
                </h3>
                <div className="space-y-2">
                  <div className="flex justify-between text-sm">
                    <span className="text-primary-600 dark:text-sand-300">Amount:</span>
                    <span className="font-semibold text-primary-900 dark:text-sand-50">
                      ${analytics?.pending_earnings?.toFixed(2) || '0.00'}
                    </span>
                  </div>
                  <div className="flex justify-between text-sm">
                    <span className="text-primary-600 dark:text-sand-300">Status:</span>
                    <span className="font-semibold text-yellow-600 dark:text-yellow-400">
                      Processing
                    </span>
                  </div>
                  <div className="flex justify-between text-sm">
                    <span className="text-primary-600 dark:text-sand-300">Expected:</span>
                    <span className="font-semibold text-primary-900 dark:text-sand-50">
                      Within 24 hours
                    </span>
                  </div>
                </div>
              </div>
            </div>
          </section>
        </div>
      </div>
    </ProtectedRoute>
  );
}
