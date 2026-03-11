'use client';

import React, { useState } from 'react';
import { ChevronUp, ChevronDown, ArrowUpDown, Home } from 'lucide-react';
import { usePropertyPerformance } from '@/hooks/useAnalytics';
import type { PropertyPerformance, TimePeriod } from '@/types/analytics-types';

interface PropertyPerformanceTableProps {
  period?: TimePeriod;
  className?: string;
}

type SortKey = keyof PropertyPerformance;
type SortDirection = 'asc' | 'desc';

export const PropertyPerformanceTable: React.FC<PropertyPerformanceTableProps> = ({
  period = 'monthly',
  className = '',
}) => {
  const { data, isLoading, error } = usePropertyPerformance(period);
  const [sortKey, setSortKey] = useState<SortKey>('revenue');
  const [sortDirection, setSortDirection] = useState<SortDirection>('desc');

  const handleSort = (key: SortKey) => {
    if (sortKey === key) {
      setSortDirection(sortDirection === 'asc' ? 'desc' : 'asc');
    } else {
      setSortKey(key);
      setSortDirection('desc');
    }
  };

  const getSortIcon = (key: SortKey) => {
    if (sortKey !== key) {
      return <ArrowUpDown className="h-4 w-4 text-primary-400" />;
    }
    return sortDirection === 'asc' ? (
      <ChevronUp className="h-4 w-4 text-secondary-600" />
    ) : (
      <ChevronDown className="h-4 w-4 text-secondary-600" />
    );
  };

  const sortedData = React.useMemo(() => {
    if (!data) return [];
    
    return [...data].sort((a, b) => {
      const aVal = a[sortKey];
      const bVal = b[sortKey];
      
      if (typeof aVal === 'string' && typeof bVal === 'string') {
        return sortDirection === 'asc' 
          ? aVal.localeCompare(bVal)
          : bVal.localeCompare(aVal);
      }
      
      const aNum = Number(aVal);
      const bNum = Number(bVal);
      return sortDirection === 'asc' ? aNum - bNum : bNum - aNum;
    });
  }, [data, sortKey, sortDirection]);

  const formatCurrency = (value: number) => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
      minimumFractionDigits: 0,
      maximumFractionDigits: 0,
    }).format(value);
  };

  const formatPercentage = (value: number) => `${value.toFixed(1)}%`;

  if (error) {
    return (
      <div className={`bg-white rounded-lg p-6 ${className}`}>
        <div className="text-center text-red-600">
          <p>Failed to load property performance data</p>
        </div>
      </div>
    );
  }

  return (
    <div className={`bg-white rounded-lg shadow-sm border border-sand-200/50 ${className}`}>
      {/* Header */}
      <div className="px-6 py-4 border-b border-sand-200/50">
        <div className="flex items-center space-x-3">
          <div className="p-2 bg-blue-100 rounded-lg">
            <Home className="h-5 w-5 text-secondary-600" />
          </div>
          <div>
            <h3 className="text-lg font-semibold text-primary-900">Property Performance</h3>
            <p className="text-sm text-primary-500">
              Detailed performance metrics for each property
            </p>
          </div>
        </div>
      </div>

      {/* Table */}
      <div className="overflow-x-auto">
        {isLoading ? (
          <div className="flex items-center justify-center py-12">
            <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-secondary-600"></div>
          </div>
        ) : !sortedData || sortedData.length === 0 ? (
          <div className="flex flex-col items-center justify-center py-12 text-primary-500">
            <Home className="h-12 w-12 mb-3 opacity-50" />
            <p>No properties to display</p>
          </div>
        ) : (
          <table className="w-full">
            <thead className="bg-sand-50">
              <tr>
                <th className="px-6 py-3 text-left text-xs font-medium text-primary-500 uppercase tracking-wider">
                  Property
                </th>
                <th
                  className="px-6 py-3 text-left text-xs font-medium text-primary-500 uppercase tracking-wider cursor-pointer hover:bg-primary-100"
                  onClick={() => handleSort('revenue')}
                >
                  <div className="flex items-center space-x-1">
                    <span>Revenue</span>
                    {getSortIcon('revenue')}
                  </div>
                </th>
                <th
                  className="px-6 py-3 text-left text-xs font-medium text-primary-500 uppercase tracking-wider cursor-pointer hover:bg-primary-100"
                  onClick={() => handleSort('occupancy_rate')}
                >
                  <div className="flex items-center space-x-1">
                    <span>Occupancy</span>
                    {getSortIcon('occupancy_rate')}
                  </div>
                </th>
                <th
                  className="px-6 py-3 text-left text-xs font-medium text-primary-500 uppercase tracking-wider cursor-pointer hover:bg-primary-100"
                  onClick={() => handleSort('bookings')}
                >
                  <div className="flex items-center space-x-1">
                    <span>Bookings</span>
                    {getSortIcon('bookings')}
                  </div>
                </th>
                <th
                  className="px-6 py-3 text-left text-xs font-medium text-primary-500 uppercase tracking-wider cursor-pointer hover:bg-primary-100"
                  onClick={() => handleSort('average_rating')}
                >
                  <div className="flex items-center space-x-1">
                    <span>Rating</span>
                    {getSortIcon('average_rating')}
                  </div>
                </th>
                <th
                  className="px-6 py-3 text-left text-xs font-medium text-primary-500 uppercase tracking-wider cursor-pointer hover:bg-primary-100"
                  onClick={() => handleSort('revenue_per_night')}
                >
                  <div className="flex items-center space-x-1">
                    <span>Per Night</span>
                    {getSortIcon('revenue_per_night')}
                  </div>
                </th>
              </tr>
            </thead>
            <tbody className="bg-white divide-y divide-sand-200/50">
              {sortedData.map((property) => (
                <tr
                  key={property.property_id}
                  className="hover:bg-sand-50 transition-colors"
                >
                  <td className="px-6 py-4 whitespace-nowrap">
                    <div className="flex items-center">
                      {property.property_image ? (
                        <img
                          src={property.property_image}
                          alt={property.property_name}
                          className="h-10 w-10 rounded-lg object-cover mr-3"
                        />
                      ) : (
                        <div className="h-10 w-10 rounded-lg bg-primary-200 flex items-center justify-center mr-3">
                          <Home className="h-5 w-5 text-primary-400" />
                        </div>
                      )}
                      <div>
                        <p className="text-sm font-medium text-primary-900">
                          {property.property_name}
                        </p>
                        <p className="text-xs text-primary-500">
                          {property.days_available ?? 0} days available
                        </p>
                      </div>
                    </div>
                  </td>
                  <td className="px-6 py-4 whitespace-nowrap">
                    <span className="text-sm font-semibold text-green-600">
                      {formatCurrency(property.revenue ?? 0)}
                    </span>
                  </td>
                  <td className="px-6 py-4 whitespace-nowrap">
                    <div className="flex items-center">
                      <div className="w-16 bg-primary-200 rounded-full h-2 mr-2">
                        <div
                          className="bg-secondary-600 h-2 rounded-full"
                          style={{ width: `${Math.min(Math.max(property.occupancy_rate ?? 0, 0), 100)}%` }}
                        ></div>
                      </div>
                      <span className="text-sm text-primary-900">
                        {formatPercentage(property.occupancy_rate ?? 0)}
                      </span>
                    </div>
                  </td>
                  <td className="px-6 py-4 whitespace-nowrap text-sm text-primary-900">
                    {property.bookings ?? 0}
                  </td>
                  <td className="px-6 py-4 whitespace-nowrap">
                    <div className="flex items-center">
                      <span className="text-sm text-primary-900">
                        {(property.average_rating ?? 0).toFixed(2)}
                      </span>
                      <span className="text-yellow-400 ml-1">★</span>
                    </div>
                  </td>
                  <td className="px-6 py-4 whitespace-nowrap text-sm text-primary-900">
                    {formatCurrency(property.revenue_per_night ?? 0)}
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        )}
      </div>
    </div>
  );
};
