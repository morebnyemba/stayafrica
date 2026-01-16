'use client';

import React, { useState } from 'react';
import { Calendar, Minus, Plus } from 'lucide-react';

export type FlexibilityType = 'exact' | 'flexible_days' | 'weekends' | 'month';

interface FlexibleDateSearchPanelProps {
  checkIn: Date;
  checkOut: Date;
  onSearch: (flexibility: FlexibilityType, days?: number) => void;
  className?: string;
}

export default function FlexibleDateSearchPanel({
  checkIn,
  checkOut,
  onSearch,
  className = '',
}: FlexibleDateSearchPanelProps) {
  const [flexibility, setFlexibility] = useState<FlexibilityType>('exact');
  const [flexibleDays, setFlexibleDays] = useState(3);

  const handleSearch = () => {
    onSearch(flexibility, flexibility === 'flexible_days' ? flexibleDays : undefined);
  };

  return (
    <div className={`bg-white rounded-xl shadow-lg p-6 ${className}`}>
      <div className="flex items-center gap-2 mb-6">
        <Calendar className="w-5 h-5 text-primary-600" />
        <h3 className="text-lg font-semibold text-gray-900">Date Flexibility</h3>
      </div>

      {/* Flexibility Options */}
      <div className="space-y-3 mb-6">
        <label className="flex items-start gap-3 p-4 border-2 rounded-lg cursor-pointer hover:bg-gray-50 transition">
          <input
            type="radio"
            name="flexibility"
            value="exact"
            checked={flexibility === 'exact'}
            onChange={(e) => setFlexibility(e.target.value as FlexibilityType)}
            className="mt-0.5 w-4 h-4 text-primary-600"
          />
          <div className="flex-1">
            <div className="font-medium text-gray-900">Exact dates</div>
            <div className="text-sm text-gray-600 mt-1">
              {checkIn.toLocaleDateString()} - {checkOut.toLocaleDateString()}
            </div>
          </div>
        </label>

        <label className="flex items-start gap-3 p-4 border-2 rounded-lg cursor-pointer hover:bg-gray-50 transition">
          <input
            type="radio"
            name="flexibility"
            value="flexible_days"
            checked={flexibility === 'flexible_days'}
            onChange={(e) => setFlexibility(e.target.value as FlexibilityType)}
            className="mt-0.5 w-4 h-4 text-primary-600"
          />
          <div className="flex-1">
            <div className="font-medium text-gray-900">Flexible days</div>
            <div className="text-sm text-gray-600 mt-1">
              Shift dates by ±{flexibleDays} days to find better prices
            </div>
            {flexibility === 'flexible_days' && (
              <div className="mt-3 flex items-center gap-3">
                <button
                  type="button"
                  onClick={() => setFlexibleDays(Math.max(1, flexibleDays - 1))}
                  className="p-2 rounded-lg bg-gray-100 hover:bg-gray-200 transition"
                >
                  <Minus className="w-4 h-4" />
                </button>
                <span className="font-semibold text-gray-900 min-w-[60px] text-center">
                  ±{flexibleDays} days
                </span>
                <button
                  type="button"
                  onClick={() => setFlexibleDays(Math.min(7, flexibleDays + 1))}
                  className="p-2 rounded-lg bg-gray-100 hover:bg-gray-200 transition"
                >
                  <Plus className="w-4 h-4" />
                </button>
              </div>
            )}
          </div>
        </label>

        <label className="flex items-start gap-3 p-4 border-2 rounded-lg cursor-pointer hover:bg-gray-50 transition">
          <input
            type="radio"
            name="flexibility"
            value="weekends"
            checked={flexibility === 'weekends'}
            onChange={(e) => setFlexibility(e.target.value as FlexibilityType)}
            className="mt-0.5 w-4 h-4 text-primary-600"
          />
          <div className="flex-1">
            <div className="font-medium text-gray-900">Any weekend</div>
            <div className="text-sm text-gray-600 mt-1">
              Show properties available for weekend stays
            </div>
          </div>
        </label>

        <label className="flex items-start gap-3 p-4 border-2 rounded-lg cursor-pointer hover:bg-gray-50 transition">
          <input
            type="radio"
            name="flexibility"
            value="month"
            checked={flexibility === 'month'}
            onChange={(e) => setFlexibility(e.target.value as FlexibilityType)}
            className="mt-0.5 w-4 h-4 text-primary-600"
          />
          <div className="flex-1">
            <div className="font-medium text-gray-900">Entire month</div>
            <div className="text-sm text-gray-600 mt-1">
              Show all available dates in {checkIn.toLocaleString('default', { month: 'long' })}
            </div>
          </div>
        </label>
      </div>

      {/* Search Button */}
      <button
        onClick={handleSearch}
        className="w-full py-3 bg-primary-600 text-white font-semibold rounded-lg hover:bg-primary-700 transition"
      >
        Search with flexibility
      </button>

      {/* Info */}
      <p className="text-xs text-gray-500 mt-4 text-center">
        Flexible dates can help you find better prices and more options
      </p>
    </div>
  );
}
