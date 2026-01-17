'use client';

import { useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { ChevronLeft, ChevronRight, TrendingUp, TrendingDown, Minus } from 'lucide-react';
import pricingApi from '@/services/pricing-api';
import { PricingCalendarDay } from '@/types/pricing-types';

interface PricingCalendarProps {
  propertyId: string;
}

export default function PricingCalendar({ propertyId }: PricingCalendarProps) {
  const [currentDate, setCurrentDate] = useState(new Date());
  const month = currentDate.getMonth() + 1;
  const year = currentDate.getFullYear();

  const { data: calendar, isLoading } = useQuery({
    queryKey: ['pricing-calendar', propertyId, month, year],
    queryFn: () => pricingApi.getPricingCalendar(propertyId, month, year),
  });

  const navigateMonth = (direction: 'prev' | 'next') => {
    const newDate = new Date(currentDate);
    if (direction === 'prev') {
      newDate.setMonth(newDate.getMonth() - 1);
    } else {
      newDate.setMonth(newDate.getMonth() + 1);
    }
    setCurrentDate(newDate);
  };

  const getPriceChangeIcon = (day: PricingCalendarDay) => {
    const change = ((day.dynamic_price - day.base_price) / day.base_price) * 100;
    if (change > 5) return <TrendingUp className="w-3 h-3 text-green-600" />;
    if (change < -5) return <TrendingDown className="w-3 h-3 text-red-600" />;
    return <Minus className="w-3 h-3 text-gray-400" />;
  };

  const getPriceChangeColor = (day: PricingCalendarDay) => {
    const change = ((day.dynamic_price - day.base_price) / day.base_price) * 100;
    if (change > 5) return 'text-green-700 bg-green-50 border-green-200';
    if (change < -5) return 'text-red-700 bg-red-50 border-red-200';
    return 'text-gray-700 bg-gray-50 border-gray-200';
  };

  if (isLoading) {
    return (
      <div className="bg-white rounded-lg shadow p-6">
        <div className="animate-pulse space-y-4">
          <div className="h-8 bg-gray-200 rounded w-1/3"></div>
          <div className="h-64 bg-gray-200 rounded"></div>
        </div>
      </div>
    );
  }

  if (!calendar) return null;

  return (
    <div className="bg-white rounded-lg shadow p-6">
      <div className="flex items-center justify-between mb-6">
        <h3 className="text-xl font-semibold">
          {currentDate.toLocaleString('default', { month: 'long', year: 'numeric' })}
        </h3>
        <div className="flex gap-2">
          <button
            onClick={() => navigateMonth('prev')}
            className="p-2 hover:bg-gray-100 rounded-lg transition"
          >
            <ChevronLeft className="w-5 h-5" />
          </button>
          <button
            onClick={() => navigateMonth('next')}
            className="p-2 hover:bg-gray-100 rounded-lg transition"
          >
            <ChevronRight className="w-5 h-5" />
          </button>
        </div>
      </div>

      {/* Price Summary */}
      <div className="grid grid-cols-3 gap-4 mb-6">
        <div className="bg-blue-50 rounded-lg p-4">
          <p className="text-sm text-blue-600 font-medium">Min Price</p>
          <p className="text-2xl font-bold text-blue-700">${calendar.min_price}</p>
        </div>
        <div className="bg-purple-50 rounded-lg p-4">
          <p className="text-sm text-purple-600 font-medium">Avg Price</p>
          <p className="text-2xl font-bold text-purple-700">${calendar.avg_price}</p>
        </div>
        <div className="bg-green-50 rounded-lg p-4">
          <p className="text-sm text-green-600 font-medium">Max Price</p>
          <p className="text-2xl font-bold text-green-700">${calendar.max_price}</p>
        </div>
      </div>

      {/* Calendar Grid */}
      <div className="grid grid-cols-7 gap-2">
        {['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'].map((day) => (
          <div key={day} className="text-center text-sm font-medium text-gray-600 py-2">
            {day}
          </div>
        ))}
        
        {calendar.calendar.map((day: any, index: number) => {
          const date = new Date(day.date);
          const isToday = date.toDateString() === new Date().toDateString();

          return (
            <div
              key={index}
              className={`
                border rounded-lg p-3 min-h-[80px] transition
                ${day.is_available ? 'cursor-pointer hover:shadow-md' : 'opacity-50'}
                ${getPriceChangeColor(day)}
                ${isToday ? 'ring-2 ring-blue-500' : ''}
              `}
            >
              <div className="flex items-center justify-between mb-1">
                <span className="text-sm font-medium">{date.getDate()}</span>
                {getPriceChangeIcon(day)}
              </div>
              
              {day.is_available ? (
                <>
                  <p className="text-lg font-bold">${day.dynamic_price}</p>
                  {day.base_price !== day.dynamic_price && (
                    <p className="text-xs line-through text-gray-500">${day.base_price}</p>
                  )}
                  {day.applied_rules.length > 0 && (
                    <div className="mt-1">
                      {day.applied_rules.map((rule: any, idx: number) => (
                        <span
                          key={idx}
                          className="inline-block text-xs bg-white bg-opacity-50 px-1 py-0.5 rounded mr-1 mb-1"
                        >
                          {rule.substring(0, 10)}...
                        </span>
                      ))}
                    </div>
                  )}
                </>
              ) : (
                <p className="text-sm text-gray-500">Unavailable</p>
              )}
            </div>
          );
        })}
      </div>

      {/* Legend */}
      <div className="mt-6 flex flex-wrap gap-4 text-sm">
        <div className="flex items-center gap-2">
          <TrendingUp className="w-4 h-4 text-green-600" />
          <span className="text-gray-600">Price increased</span>
        </div>
        <div className="flex items-center gap-2">
          <TrendingDown className="w-4 h-4 text-red-600" />
          <span className="text-gray-600">Price decreased</span>
        </div>
        <div className="flex items-center gap-2">
          <Minus className="w-4 h-4 text-gray-400" />
          <span className="text-gray-600">Base price</span>
        </div>
      </div>
    </div>
  );
}
