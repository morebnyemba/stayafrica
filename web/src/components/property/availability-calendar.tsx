'use client';

import { useState } from 'react';
import { ChevronLeft, ChevronRight } from 'lucide-react';

interface AvailabilityCalendarProps {
  bookedDates?: string[];
  onSelectDate?: (date: string) => void;
}

export function AvailabilityCalendar({
  bookedDates = [],
  onSelectDate,
}: AvailabilityCalendarProps) {
  const [currentDate, setCurrentDate] = useState(new Date());
  const [selectedDates, setSelectedDates] = useState<string[]>([]);

  const getDaysInMonth = (date: Date) => {
    return new Date(date.getFullYear(), date.getMonth() + 1, 0).getDate();
  };

  const getFirstDayOfMonth = (date: Date) => {
    return new Date(date.getFullYear(), date.getMonth(), 1).getDay();
  };

  const monthName = currentDate.toLocaleString('default', {
    month: 'long',
    year: 'numeric',
  });

  const daysInMonth = getDaysInMonth(currentDate);
  const firstDay = getFirstDayOfMonth(currentDate);
  const days = [];

  // Empty cells for days before month starts
  for (let i = 0; i < firstDay; i++) {
    days.push(null);
  }

  // Days of month
  for (let i = 1; i <= daysInMonth; i++) {
    days.push(i);
  }

  const handlePrevMonth = () => {
    setCurrentDate(new Date(currentDate.getFullYear(), currentDate.getMonth() - 1));
  };

  const handleNextMonth = () => {
    setCurrentDate(new Date(currentDate.getFullYear(), currentDate.getMonth() + 1));
  };

  const handleDateClick = (day: number) => {
    const dateStr = `${currentDate.getFullYear()}-${String(currentDate.getMonth() + 1).padStart(2, '0')}-${String(day).padStart(2, '0')}`;
    
    if (selectedDates.includes(dateStr)) {
      setSelectedDates(selectedDates.filter((d) => d !== dateStr));
    } else {
      setSelectedDates([...selectedDates, dateStr]);
    }

    if (onSelectDate) {
      onSelectDate(dateStr);
    }
  };

  return (
    <div className="bg-white dark:bg-primary-800 p-4 rounded-lg border border-primary-200 dark:border-primary-700">
      {/* Header */}
      <div className="flex items-center justify-between mb-4">
        <button
          onClick={handlePrevMonth}
          className="p-2 hover:bg-primary-100 dark:hover:bg-primary-700 rounded transition"
        >
          <ChevronLeft className="w-5 h-5 text-primary-600 dark:text-sand-300" />
        </button>
        <h3 className="font-semibold text-primary-900 dark:text-sand-50">{monthName}</h3>
        <button
          onClick={handleNextMonth}
          className="p-2 hover:bg-primary-100 dark:hover:bg-primary-700 rounded transition"
        >
          <ChevronRight className="w-5 h-5 text-primary-600 dark:text-sand-300" />
        </button>
      </div>

      {/* Day names */}
      <div className="grid grid-cols-7 gap-1 mb-2">
        {['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'].map((day) => (
          <div key={day} className="text-center text-xs font-semibold text-primary-600 dark:text-sand-400">
            {day}
          </div>
        ))}
      </div>

      {/* Days */}
      <div className="grid grid-cols-7 gap-1">
        {days.map((day, index) => {
          if (day === null) {
            return (
              <div
                key={`empty-${index}`}
                className="aspect-square"
              />
            );
          }

          const dateStr = `${currentDate.getFullYear()}-${String(currentDate.getMonth() + 1).padStart(2, '0')}-${String(day).padStart(2, '0')}`;
          const isBooked = bookedDates.includes(dateStr);
          const isSelected = selectedDates.includes(dateStr);

          return (
            <button
              key={day}
              onClick={() => !isBooked && handleDateClick(day)}
              disabled={isBooked}
              className={`aspect-square rounded flex items-center justify-center text-sm font-medium transition ${
                isBooked
                  ? 'bg-red-100 dark:bg-red-900/30 text-red-600 dark:text-red-400 cursor-not-allowed'
                  : isSelected
                    ? 'bg-secondary-600 dark:bg-secondary-700 text-white'
                    : 'bg-primary-50 dark:bg-primary-700 text-primary-900 dark:text-sand-100 hover:bg-primary-100 dark:hover:bg-primary-600'
              }`}
            >
              {day}
            </button>
          );
        })}
      </div>

      {/* Legend */}
      <div className="mt-4 flex items-center justify-between text-xs text-primary-600 dark:text-sand-400">
        <div className="flex items-center space-x-2">
          <div className="w-3 h-3 bg-secondary-600 rounded" />
          <span>Selected</span>
        </div>
        <div className="flex items-center space-x-2">
          <div className="w-3 h-3 bg-red-100 dark:bg-red-900/30 rounded" />
          <span>Booked</span>
        </div>
      </div>
    </div>
  );
}
