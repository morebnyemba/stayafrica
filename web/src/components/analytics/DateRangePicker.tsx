'use client';

import React, { useState } from 'react';
import { Calendar, X } from 'lucide-react';
import { format } from 'date-fns';

interface DateRangePickerProps {
  startDate?: Date;
  endDate?: Date;
  onDateChange: (startDate: Date | null, endDate: Date | null) => void;
  className?: string;
}

export const DateRangePicker: React.FC<DateRangePickerProps> = ({
  startDate,
  endDate,
  onDateChange,
  className = '',
}) => {
  const [isOpen, setIsOpen] = useState(false);
  const [localStartDate, setLocalStartDate] = useState<string>(
    startDate ? format(startDate, 'yyyy-MM-dd') : ''
  );
  const [localEndDate, setLocalEndDate] = useState<string>(
    endDate ? format(endDate, 'yyyy-MM-dd') : ''
  );

  // Sync local state with props when they change
  React.useEffect(() => {
    if (startDate) {
      setLocalStartDate(format(startDate, 'yyyy-MM-dd'));
    }
    if (endDate) {
      setLocalEndDate(format(endDate, 'yyyy-MM-dd'));
    }
  }, [startDate, endDate]);

  const handleApply = () => {
    const start = localStartDate ? new Date(localStartDate) : null;
    const end = localEndDate ? new Date(localEndDate) : null;
    onDateChange(start, end);
    setIsOpen(false);
  };

  const handleClear = () => {
    setLocalStartDate('');
    setLocalEndDate('');
    onDateChange(null, null);
    setIsOpen(false);
  };

  const formatDateRange = () => {
    if (!startDate || !endDate) return 'Select date range';
    return `${format(startDate, 'MMM d, yyyy')} - ${format(endDate, 'MMM d, yyyy')}`;
  };

  return (
    <div className={`relative ${className}`}>
      {/* Trigger Button */}
      <button
        onClick={() => setIsOpen(!isOpen)}
        className="flex items-center space-x-2 px-4 py-2 bg-white dark:bg-primary-800 border border-primary-300 dark:border-primary-600 rounded-lg hover:bg-sand-50 dark:hover:bg-primary-700 transition-colors"
      >
        <Calendar className="h-4 w-4 text-primary-500 dark:text-sand-400" />
        <span className="text-sm text-primary-700 dark:text-sand-200">
          {formatDateRange()}
        </span>
      </button>

      {/* Dropdown */}
      {isOpen && (
        <>
          {/* Backdrop */}
          <div
            className="fixed inset-0 z-40"
            onClick={() => setIsOpen(false)}
          ></div>

          {/* Picker Panel */}
          <div className="absolute right-0 mt-2 z-50 w-80 bg-white dark:bg-primary-800 rounded-lg shadow-xl border border-primary-200 dark:border-primary-700 p-4">
            {/* Header */}
            <div className="flex items-center justify-between mb-4">
              <h3 className="text-sm font-semibold text-primary-900 dark:text-sand-50">
                Select Date Range
              </h3>
              <button
                onClick={() => setIsOpen(false)}
                className="p-1 hover:bg-primary-100 dark:hover:bg-primary-700 rounded"
              >
                <X className="h-4 w-4 text-primary-500 dark:text-sand-400" />
              </button>
            </div>

            {/* Date Inputs */}
            <div className="space-y-4">
              <div>
                <label className="block text-xs font-medium text-primary-700 dark:text-sand-200 mb-1">
                  Start Date
                </label>
                <input
                  type="date"
                  value={localStartDate}
                  onChange={(e) => setLocalStartDate(e.target.value)}
                  max={localEndDate || undefined}
                  className="w-full px-3 py-2 border border-primary-300 dark:border-primary-600 rounded-lg text-sm bg-white dark:bg-primary-700 text-primary-900 dark:text-sand-50 focus:ring-2 focus:ring-secondary-500 focus:border-transparent"
                />
              </div>

              <div>
                <label className="block text-xs font-medium text-primary-700 dark:text-sand-200 mb-1">
                  End Date
                </label>
                <input
                  type="date"
                  value={localEndDate}
                  onChange={(e) => setLocalEndDate(e.target.value)}
                  min={localStartDate || undefined}
                  className="w-full px-3 py-2 border border-primary-300 dark:border-primary-600 rounded-lg text-sm bg-white dark:bg-primary-700 text-primary-900 dark:text-sand-50 focus:ring-2 focus:ring-secondary-500 focus:border-transparent"
                />
              </div>
            </div>

            {/* Quick Ranges */}
            <div className="mt-4 pt-4 border-t border-primary-200 dark:border-primary-700">
              <p className="text-xs font-medium text-primary-700 dark:text-sand-200 mb-2">
                Quick Ranges
              </p>
              <div className="grid grid-cols-2 gap-2">
                {[
                  { label: 'Last 7 days', days: 7 },
                  { label: 'Last 30 days', days: 30 },
                  { label: 'Last 90 days', days: 90 },
                  { label: 'Last year', days: 365 },
                ].map(({ label, days }) => (
                  <button
                    key={label}
                    onClick={() => {
                      const end = new Date();
                      const start = new Date();
                      start.setDate(start.getDate() - days);
                      setLocalStartDate(format(start, 'yyyy-MM-dd'));
                      setLocalEndDate(format(end, 'yyyy-MM-dd'));
                    }}
                    className="px-3 py-1.5 text-xs bg-primary-100 dark:bg-primary-800 hover:bg-primary-200 dark:hover:bg-primary-700 rounded text-primary-700 dark:text-sand-200 transition-colors"
                  >
                    {label}
                  </button>
                ))}
              </div>
            </div>

            {/* Actions */}
            <div className="mt-4 pt-4 border-t border-primary-200 dark:border-primary-700 flex items-center justify-end space-x-2">
              <button
                onClick={handleClear}
                className="px-3 py-1.5 text-sm text-primary-700 dark:text-sand-200 hover:bg-primary-100 dark:hover:bg-primary-700 rounded transition-colors"
              >
                Clear
              </button>
              <button
                onClick={handleApply}
                disabled={!localStartDate || !localEndDate}
                className="px-3 py-1.5 text-sm bg-secondary-600 hover:bg-secondary-700 disabled:bg-primary-300 text-white rounded transition-colors"
              >
                Apply
              </button>
            </div>
          </div>
        </>
      )}
    </div>
  );
};
