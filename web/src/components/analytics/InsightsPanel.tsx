'use client';

import React from 'react';
import { Lightbulb, AlertTriangle, CheckCircle, Info, ArrowRight } from 'lucide-react';
import type { Insight } from '@/types/analytics-types';

interface InsightsPanelProps {
  insights?: Insight[];
  isLoading?: boolean;
  className?: string;
}

export const InsightsPanel: React.FC<InsightsPanelProps> = ({
  insights = [],
  isLoading = false,
  className = '',
}) => {
  const getInsightIcon = (type: Insight['type']) => {
    switch (type) {
      case 'success':
        return <CheckCircle className="h-5 w-5 text-green-600" />;
      case 'warning':
        return <AlertTriangle className="h-5 w-5 text-yellow-600" />;
      case 'danger':
        return <AlertTriangle className="h-5 w-5 text-red-600" />;
      default:
        return <Info className="h-5 w-5 text-blue-600" />;
    }
  };

  const getInsightStyles = (type: Insight['type']) => {
    switch (type) {
      case 'success':
        return {
          bg: 'bg-green-50 dark:bg-green-900/20',
          border: 'border-green-200 dark:border-green-800',
          title: 'text-green-900 dark:text-green-100',
          text: 'text-green-700 dark:text-green-300',
        };
      case 'warning':
        return {
          bg: 'bg-yellow-50 dark:bg-yellow-900/20',
          border: 'border-yellow-200 dark:border-yellow-800',
          title: 'text-yellow-900 dark:text-yellow-100',
          text: 'text-yellow-700 dark:text-yellow-300',
        };
      case 'danger':
        return {
          bg: 'bg-red-50 dark:bg-red-900/20',
          border: 'border-red-200 dark:border-red-800',
          title: 'text-red-900 dark:text-red-100',
          text: 'text-red-700 dark:text-red-300',
        };
      default:
        return {
          bg: 'bg-blue-50 dark:bg-blue-900/20',
          border: 'border-blue-200 dark:border-blue-800',
          title: 'text-blue-900 dark:text-blue-100',
          text: 'text-blue-700 dark:text-blue-300',
        };
    }
  };

  if (isLoading) {
    return (
      <div className={`bg-white dark:bg-gray-800 rounded-lg shadow-sm border border-gray-200 dark:border-gray-700 ${className}`}>
        <div className="px-6 py-4 border-b border-gray-200 dark:border-gray-700">
          <div className="flex items-center space-x-3">
            <div className="p-2 bg-yellow-100 dark:bg-yellow-900/30 rounded-lg">
              <Lightbulb className="h-5 w-5 text-yellow-600 dark:text-yellow-400" />
            </div>
            <h3 className="text-lg font-semibold text-gray-900 dark:text-white">Insights & Recommendations</h3>
          </div>
        </div>
        <div className="p-6">
          <div className="space-y-4">
            {[1, 2, 3].map((i) => (
              <div key={i} className="animate-pulse">
                <div className="h-4 bg-gray-200 dark:bg-gray-700 rounded w-3/4 mb-2"></div>
                <div className="h-3 bg-gray-200 dark:bg-gray-700 rounded w-full"></div>
              </div>
            ))}
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className={`bg-white dark:bg-gray-800 rounded-lg shadow-sm border border-gray-200 dark:border-gray-700 ${className}`}>
      {/* Header */}
      <div className="px-6 py-4 border-b border-gray-200 dark:border-gray-700">
        <div className="flex items-center space-x-3">
          <div className="p-2 bg-yellow-100 dark:bg-yellow-900/30 rounded-lg">
            <Lightbulb className="h-5 w-5 text-yellow-600 dark:text-yellow-400" />
          </div>
          <div>
            <h3 className="text-lg font-semibold text-gray-900 dark:text-white">Insights & Recommendations</h3>
            <p className="text-sm text-gray-600 dark:text-gray-400">
              AI-powered insights to improve your performance
            </p>
          </div>
        </div>
      </div>

      {/* Content */}
      <div className="p-6">
        {!insights || insights.length === 0 ? (
          <div className="flex flex-col items-center justify-center py-12 text-gray-500 dark:text-gray-400">
            <Lightbulb className="h-12 w-12 mb-3 opacity-50" />
            <p>No insights available at this time</p>
            <p className="text-sm mt-1">Check back after more data is collected</p>
          </div>
        ) : (
          <div className="space-y-4">
            {insights.map((insight) => {
              const styles = getInsightStyles(insight.type);
              return (
                <div
                  key={insight.id}
                  className={`rounded-lg border p-4 transition-all hover:shadow-md ${styles.bg} ${styles.border}`}
                >
                  <div className="flex items-start space-x-3">
                    <div className="flex-shrink-0 mt-0.5">
                      {getInsightIcon(insight.type)}
                    </div>
                    <div className="flex-1 min-w-0">
                      {/* Title */}
                      <h4 className={`text-sm font-semibold mb-1 ${styles.title}`}>
                        {insight.title}
                      </h4>

                      {/* Description */}
                      <p className={`text-sm mb-2 ${styles.text}`}>
                        {insight.description}
                      </p>

                      {/* Metric */}
                      {insight.metric && insight.value !== undefined && (
                        <div className={`text-xs mb-2 ${styles.text}`}>
                          <span className="font-medium">{insight.metric}:</span>{' '}
                          <span className="font-semibold">{insight.value}</span>
                        </div>
                      )}

                      {/* Recommendation */}
                      {insight.recommendation && (
                        <div className={`mt-3 p-3 rounded-md bg-white/50 dark:bg-black/20`}>
                          <p className={`text-sm font-medium mb-1 flex items-center gap-1.5 ${styles.title}`}>
                            <Lightbulb className="h-4 w-4" /> Recommendation
                          </p>
                          <p className={`text-sm ${styles.text}`}>
                            {insight.recommendation}
                          </p>
                        </div>
                      )}

                      {/* Action */}
                      {insight.action_url && insight.action_label && (
                        <div className="mt-3">
                          <a
                            href={insight.action_url}
                            className={`inline-flex items-center space-x-1 text-sm font-medium hover:underline ${styles.title}`}
                          >
                            <span>{insight.action_label}</span>
                            <ArrowRight className="h-4 w-4" />
                          </a>
                        </div>
                      )}
                    </div>
                  </div>
                </div>
              );
            })}
          </div>
        )}
      </div>
    </div>
  );
};
