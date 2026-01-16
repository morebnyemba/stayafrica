'use client';

import React from 'react';

interface ChartLegendProps {
  items: Array<{
    label: string;
    color: string;
    value?: string | number;
  }>;
  className?: string;
}

export const ChartLegend: React.FC<ChartLegendProps> = ({ items, className = '' }) => {
  return (
    <div className={`flex flex-wrap items-center gap-4 ${className}`}>
      {items.map((item, index) => (
        <div key={index} className="flex items-center space-x-2">
          <div
            className="w-3 h-3 rounded-full"
            style={{ backgroundColor: item.color }}
          ></div>
          <span className="text-sm text-gray-600 dark:text-gray-400">
            {item.label}
            {item.value && (
              <span className="font-semibold text-gray-900 dark:text-white ml-1">
                {item.value}
              </span>
            )}
          </span>
        </div>
      ))}
    </div>
  );
};
