'use client';

import React from 'react';
import { TrendingUp, Award, BarChart3 } from 'lucide-react';
import { useBenchmarks } from '@/hooks/useAnalytics';

interface BenchmarkComparisonProps {
  propertyType?: string;
  location?: string;
  className?: string;
}

export const BenchmarkComparison: React.FC<BenchmarkComparisonProps> = ({
  propertyType,
  location,
  className = '',
}) => {
  const { data, isLoading, error } = useBenchmarks(propertyType, location);

  const getPercentileColor = (percentile: number) => {
    if (percentile >= 90) return 'text-green-600 bg-green-100 dark:bg-green-900/30';
    if (percentile >= 75) return 'text-blue-600 bg-blue-100 dark:bg-blue-900/30';
    if (percentile >= 50) return 'text-yellow-600 bg-yellow-100 dark:bg-yellow-900/30';
    return 'text-red-600 bg-red-100 dark:bg-red-900/30';
  };

  const getPercentileLabel = (percentile: number) => {
    if (percentile >= 90) return 'Excellent';
    if (percentile >= 75) return 'Above Average';
    if (percentile >= 50) return 'Average';
    return 'Below Average';
  };

  const formatValue = (value: number, unit?: string) => {
    if (unit === '$') {
      return new Intl.NumberFormat('en-US', {
        style: 'currency',
        currency: 'USD',
        minimumFractionDigits: 0,
        maximumFractionDigits: 0,
      }).format(value);
    }
    if (unit === '%') {
      return `${value.toFixed(1)}%`;
    }
    return value.toLocaleString();
  };

  if (error) {
    return (
      <div className={`bg-white dark:bg-primary-800/40 rounded-lg p-6 ${className}`}>
        <div className="text-center text-red-600 dark:text-red-400">
          <p>Failed to load benchmark data</p>
        </div>
      </div>
    );
  }

  return (
    <div className={`bg-white dark:bg-primary-800/40 rounded-lg shadow-sm border border-sand-200/50 dark:border-primary-700/50 ${className}`}>
      {/* Header */}
      <div className="px-6 py-4 border-b border-sand-200/50 dark:border-primary-700/50">
        <div className="flex items-center space-x-3">
          <div className="p-2 bg-purple-100 dark:bg-purple-900/30 rounded-lg">
            <BarChart3 className="h-5 w-5 text-purple-600 dark:text-purple-400" />
          </div>
          <div>
            <h3 className="text-lg font-semibold text-primary-900 dark:text-sand-50">Market Benchmarks</h3>
            <p className="text-sm text-primary-500 dark:text-sand-400">
              Compare your performance to market averages
            </p>
          </div>
        </div>
      </div>

      {/* Content */}
      <div className="p-6">
        {isLoading ? (
          <div className="flex items-center justify-center py-12">
            <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-purple-600"></div>
          </div>
        ) : !data || data.length === 0 ? (
          <div className="flex flex-col items-center justify-center py-12 text-primary-400 dark:text-sand-500">
            <BarChart3 className="h-12 w-12 mb-3 opacity-50" />
            <p>No benchmark data available</p>
            <p className="text-sm mt-1">Try adjusting filters</p>
          </div>
        ) : (
          <div className="space-y-6">
            {data.map((benchmark, index) => {
              const yourValue = benchmark?.your_value ?? 0;
              const marketAvg = benchmark?.market_average ?? 1;
              const percentile = benchmark?.percentile ?? 0;
              const top10 = benchmark?.top_10_percent ?? 0;
              const unit = benchmark?.unit;
              const metric = benchmark?.metric ?? 'Metric';
              
              return (
              <div key={index} className="space-y-3">
                {/* Metric Header */}
                <div className="flex items-center justify-between">
                  <div>
                    <h4 className="text-sm font-medium text-primary-900 dark:text-sand-50">
                      {metric}
                    </h4>
                    <div className="flex items-center space-x-2 mt-1">
                      <span className="text-2xl font-bold text-primary-900 dark:text-sand-50">
                        {formatValue(yourValue, unit)}
                      </span>
                      <span className={`px-2 py-1 rounded-full text-xs font-semibold ${getPercentileColor(percentile)}`}>
                        {getPercentileLabel(percentile)}
                      </span>
                    </div>
                  </div>
                  <div className="text-right">
                    <p className="text-xs text-primary-400 dark:text-sand-500">Your Percentile</p>
                    <div className="flex items-center space-x-1">
                      <Award className="h-5 w-5 text-yellow-500" />
                      <span className="text-xl font-bold text-primary-900 dark:text-sand-50">
                        {percentile.toFixed(0)}
                      </span>
                      <span className="text-sm text-primary-500 dark:text-sand-400">%ile</span>
                    </div>
                  </div>
                </div>

                {/* Progress Bar */}
                <div className="relative pt-1">
                  <div className="flex mb-2 items-center justify-between text-xs">
                    <div className="text-primary-500 dark:text-sand-400">
                      Market Avg: {formatValue(marketAvg, unit)}
                    </div>
                    <div className="text-primary-500 dark:text-sand-400">
                      Top 10%: {formatValue(top10, unit)}
                    </div>
                  </div>
                  <div className="overflow-hidden h-3 text-xs flex rounded-full bg-primary-200 dark:bg-primary-700">
                    <div
                      className={`shadow-none flex flex-col text-center whitespace-nowrap text-white justify-center transition-all ${
                        percentile >= 90 ? 'bg-green-600' :
                        percentile >= 75 ? 'bg-blue-600' :
                        percentile >= 50 ? 'bg-yellow-600' : 'bg-red-600'
                      }`}
                      style={{ width: `${percentile}%` }}
                    ></div>
                  </div>
                  {/* Markers */}
                  <div className="relative mt-1">
                    <div className="flex items-center justify-between text-xs">
                      <span className="text-primary-400 dark:text-primary-600">0</span>
                      <span className="text-primary-400 dark:text-primary-600">50</span>
                      <span className="text-primary-400 dark:text-primary-600">100</span>
                    </div>
                  </div>
                </div>

                {/* Comparison */}
                <div className="flex items-center justify-between text-sm pt-2">
                  <div className="text-primary-500 dark:text-sand-400">
                    vs Market Average:
                  </div>
                  <div className="flex items-center space-x-1">
                    {yourValue > marketAvg ? (
                      <>
                        <TrendingUp className="h-4 w-4 text-green-600" />
                        <span className="font-semibold text-green-600">
                          +{marketAvg > 0 ? (((yourValue - marketAvg) / marketAvg) * 100).toFixed(1) : '0.0'}%
                        </span>
                      </>
                    ) : (
                      <>
                        <TrendingUp className="h-4 w-4 text-red-600 rotate-180" />
                        <span className="font-semibold text-red-600">
                          {marketAvg > 0 ? (((yourValue - marketAvg) / marketAvg) * 100).toFixed(1) : '0.0'}%
                        </span>
                      </>
                    )}
                  </div>
                </div>
              </div>
            )})}
          </div>
        )}
      </div>
    </div>
  );
};
