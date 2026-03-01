/**
 * Booking Panel Component
 * Features: Date picker, guest selector, price breakdown, cancellation policy, trust badges
 * Accessibility: ARIA labels, proper form associations, keyboard navigation
 */
'use client';

import React, { useState, useId, useCallback, useMemo } from 'react';
import { Card, CardHeader, CardBody, CardFooter } from '@/components/ui/Card';
import { Button } from '@/components/ui/Button';
import { Badge } from '@/components/ui/Badge';
import { AlertCircle, Check, Users, Calendar } from 'lucide-react';
import { format, differenceInDays, addDays } from 'date-fns';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import DatePicker from 'react-datepicker';
import 'react-datepicker/dist/react-datepicker.css';

interface BookingPanelProps {
  propertyId: string;
  pricePerNight: number;
  maxGuests: number;
  minStay?: number;
  cancellationPolicy?: string;
  hostVerified?: boolean;
  hostRating?: number;
  onBook?: (details: { propertyId: string; checkIn: Date; checkOut: Date }, guests: number) => void;
  isLoading?: boolean;
}

export const BookingPanel: React.FC<BookingPanelProps> = ({
  propertyId,
  pricePerNight,
  maxGuests,
  minStay = 1,
  cancellationPolicy = 'Flexible',
  hostVerified = false,
  hostRating = 4.8,
  onBook,
  isLoading = false,
}) => {
  const [checkInDate, setCheckInDate] = useState<Date | null>(null);
  const [checkOutDate, setCheckOutDate] = useState<Date | null>(null);
  const [guestCount, setGuestCount] = useState(1);
  const [showCancellationInfo, setShowCancellationInfo] = useState(false);

  // Generate unique IDs for form accessibility
  const checkInId = useId();
  const checkOutId = useId();
  const guestsId = useId();
  const minStayErrorId = useId();

  // Fetch unavailable dates from backend
  const { data: unavailableDatesData } = useQuery({
    queryKey: ['unavailable-dates', propertyId],
    queryFn: async () => {
      const response = await apiClient.getUnavailableDates(propertyId);
      return response.data;
    },
  });
  const unavailableDates: Set<string> = new Set(unavailableDatesData?.unavailable_dates || []);

  // Check if a date range overlaps with any unavailable dates
  const hasUnavailableDatesInRange = useCallback((start: Date, end: Date) => {
    let current = new Date(start);
    while (current < end) {
      if (unavailableDates.has(format(current, 'yyyy-MM-dd'))) return true;
      current = addDays(current, 1);
    }
    return false;
  }, [unavailableDates]);

  // Convert unavailable date strings to Date objects for react-datepicker
  const excludedDates = useMemo(() => {
    return [...unavailableDates].map(dateStr => {
      const [year, month, day] = dateStr.split('-').map(Number);
      return new Date(year, month - 1, day);
    });
  }, [unavailableDates]);

  // Day class name for red highlighting of booked dates
  const getDayClassName = useCallback((date: Date) => {
    const dateStr = format(date, 'yyyy-MM-dd');
    return unavailableDates.has(dateStr) ? 'booked-date' : '';
  }, [unavailableDates]);

  // Calculate pricing
  const nights =
    checkInDate && checkOutDate ? differenceInDays(checkOutDate, checkInDate) : 0;
  const subtotal = nights > 0 ? nights * pricePerNight : 0;
  const serviceFee = subtotal > 0 ? Math.round(subtotal * 0.1) : 0; // 10% service fee
  const tax = subtotal > 0 ? Math.round((subtotal + serviceFee) * 0.08) : 0; // 8% tax
  const total = subtotal + serviceFee + tax;

  // Validation state
  const hasSelectedDates = checkInDate && checkOutDate;
  const meetsMinStay = nights >= minStay;
  const showMinStayError = hasSelectedDates && !meetsMinStay;
  const hasDateConflict = hasSelectedDates && hasUnavailableDatesInRange(checkInDate!, checkOutDate!);

  const handleBook = () => {
    if (checkInDate && checkOutDate && meetsMinStay && !hasDateConflict) {
      onBook?.({ propertyId, checkIn: checkInDate, checkOut: checkOutDate }, guestCount);
    }
  };

  const isValidBooking = hasSelectedDates && meetsMinStay && guestCount > 0 && !hasDateConflict;

  // Dynamic button text based on state
  const getButtonText = () => {
    if (hasDateConflict) return 'Selected dates are unavailable';
    if (isValidBooking) return 'Book Now';
    if (!checkInDate) return 'Select check-in date';
    if (!checkOutDate) return 'Select check-out date';
    if (!meetsMinStay) return `Minimum ${minStay} night${minStay > 1 ? 's' : ''} required`;
    return 'Select Dates';
  };

  return (
    <Card variant="elevated" className="sticky top-24" role="region" aria-label="Booking details">
      <CardHeader className="border-b border-neutral-200 dark:border-neutral-700">
        <div className="flex items-baseline gap-2">
          <span className="text-3xl font-bold text-neutral-900 dark:text-neutral-100">${pricePerNight}</span>
          <span className="text-sm text-neutral-600 dark:text-neutral-400">per night</span>
        </div>
      </CardHeader>

      <CardBody className="space-y-4">
        {/* Check-in & Check-out */}
        <div className="space-y-2">
          <label htmlFor={checkInId} className="block text-sm font-medium text-neutral-700 dark:text-neutral-300">
            Check-in
          </label>
          <div className="relative">
            <Calendar className="absolute left-3 top-1/2 -translate-y-1/2 h-4 w-4 text-neutral-400 dark:text-neutral-500 z-10 pointer-events-none" aria-hidden="true" />
            <DatePicker
              id={checkInId}
              selected={checkInDate}
              onChange={(date: Date | null) => {
                setCheckInDate(date);
                if (checkOutDate && date && date >= checkOutDate) {
                  setCheckOutDate(null);
                }
              }}
              minDate={new Date()}
              excludeDates={excludedDates}
              dayClassName={getDayClassName}
              dateFormat="MMM dd, yyyy"
              placeholderText="Add date"
              className="w-full h-10 rounded-lg border-2 border-neutral-300 dark:border-neutral-600 bg-white dark:bg-primary-800 pl-10 pr-4 py-2 text-sm text-neutral-900 dark:text-neutral-100 placeholder:text-neutral-400 focus:outline-none focus:ring-2 focus:ring-primary-500 focus:ring-offset-2"
              aria-describedby={showMinStayError ? minStayErrorId : undefined}
            />
          </div>
        </div>

        <div className="space-y-2">
          <label htmlFor={checkOutId} className="block text-sm font-medium text-neutral-700 dark:text-neutral-300">
            Check-out
          </label>
          <div className="relative">
            <Calendar className="absolute left-3 top-1/2 -translate-y-1/2 h-4 w-4 text-neutral-400 dark:text-neutral-500 z-10 pointer-events-none" aria-hidden="true" />
            <DatePicker
              id={checkOutId}
              selected={checkOutDate}
              onChange={(date: Date | null) => {
                setCheckOutDate(date);
              }}
              minDate={checkInDate ? addDays(checkInDate, 1) : new Date()}
              excludeDates={excludedDates}
              dayClassName={getDayClassName}
              dateFormat="MMM dd, yyyy"
              placeholderText="Add date"
              disabled={!checkInDate}
              className={`w-full h-10 rounded-lg border-2 border-neutral-300 dark:border-neutral-600 bg-white dark:bg-primary-800 pl-10 pr-4 py-2 text-sm text-neutral-900 dark:text-neutral-100 placeholder:text-neutral-400 focus:outline-none focus:ring-2 focus:ring-primary-500 focus:ring-offset-2 ${!checkInDate ? 'opacity-50 cursor-not-allowed' : ''}`}
              aria-invalid={showMinStayError ? "true" : undefined}
              aria-describedby={showMinStayError ? minStayErrorId : undefined}
            />
          </div>
          {showMinStayError && (
            <p id={minStayErrorId} className="text-sm text-red-600 dark:text-red-400" role="alert">
              Minimum stay is {minStay} night{minStay > 1 ? 's' : ''}. Please select at least {minStay} night{minStay > 1 ? 's' : ''}.
            </p>
          )}
          {hasDateConflict && (
            <p className="text-sm text-red-600 dark:text-red-400" role="alert">
              Some dates in your selection are already booked. Please choose different dates.
            </p>
          )}
          {unavailableDates.size > 0 && (
            <div className="flex items-center gap-2 text-xs text-neutral-500 dark:text-neutral-400">
              <span className="inline-block w-3 h-3 rounded-full bg-red-100 border border-red-300" />
              <span>Already booked</span>
            </div>
          )}
        </div>

        {/* Guest Count */}
        <div className="space-y-2">
          <label htmlFor={guestsId} className="block text-sm font-medium text-neutral-700 dark:text-neutral-300">
            Guests
          </label>
          <div className="flex items-center gap-2 border-2 border-neutral-300 dark:border-neutral-600 rounded-lg p-2 bg-white dark:bg-neutral-800">
            <Users className="h-5 w-5 text-neutral-400 dark:text-neutral-500" aria-hidden="true" />
            <select
              id={guestsId}
              value={guestCount}
              onChange={(e) => setGuestCount(parseInt(e.target.value))}
              className="flex-1 bg-transparent outline-none text-sm text-neutral-900 dark:text-neutral-100"
              aria-label={`Number of guests, currently ${guestCount}`}
            >
              {Array.from({ length: maxGuests }, (_, i) => i + 1).map((num) => (
                <option key={num} value={num}>
                  {num} {num === 1 ? 'guest' : 'guests'}
                </option>
              ))}
            </select>
          </div>
        </div>

        {/* Price Breakdown */}
        {nights > 0 && (
          <div className="space-y-3 border-t border-neutral-200 dark:border-neutral-700 pt-4" role="region" aria-label="Price breakdown">
            <div className="flex justify-between text-sm">
              <span className="text-neutral-600 dark:text-neutral-400">
                ${pricePerNight} × {nights} night{nights !== 1 ? 's' : ''}
              </span>
              <span className="font-medium text-neutral-900 dark:text-neutral-100">${subtotal}</span>
            </div>

            <div className="flex justify-between text-sm">
              <span className="text-neutral-600 dark:text-neutral-400">Service fee</span>
              <span className="font-medium text-neutral-900 dark:text-neutral-100">${serviceFee}</span>
            </div>

            <div className="flex justify-between text-sm">
              <span className="text-neutral-600 dark:text-neutral-400">Tax</span>
              <span className="font-medium text-neutral-900 dark:text-neutral-100">${tax}</span>
            </div>

            <div className="flex justify-between font-semibold border-t border-neutral-200 dark:border-neutral-700 pt-3">
              <span className="text-neutral-900 dark:text-neutral-100">Total</span>
              <span className="text-xl text-primary-600 dark:text-primary-400">${total}</span>
            </div>
          </div>
        )}
      </CardBody>

      {/* CTA Button */}
      <CardFooter>
        <Button
          fullWidth
          onClick={handleBook}
          disabled={!isValidBooking}
          isLoading={isLoading}
          aria-label={isValidBooking ? `Book now for $${total} total` : getButtonText()}
        >
          {getButtonText()}
        </Button>
      </CardFooter>

      {/* Trust Indicators */}
      <CardBody className="border-t border-neutral-200 dark:border-neutral-700 space-y-2">
        {hostVerified && (
          <div className="flex items-center gap-2 text-sm">
            <Check className="h-4 w-4 text-success-500" aria-hidden="true" />
            <span className="text-neutral-700 dark:text-neutral-300">Verified host</span>
          </div>
        )}

        {hostRating && (
          <div className="flex items-center gap-2 text-sm">
            <span className="text-neutral-700 dark:text-neutral-300" aria-label={`Host rating ${hostRating} out of 5 stars`}>
              ★ {hostRating}
            </span>
            <span className="text-neutral-600 dark:text-neutral-400">(Host rating)</span>
          </div>
        )}

        <button
          onClick={() => setShowCancellationInfo(!showCancellationInfo)}
          className="flex items-center gap-2 text-sm text-neutral-600 dark:text-neutral-400 hover:text-neutral-900 dark:hover:text-neutral-200 transition-colors"
          aria-expanded={showCancellationInfo}
          aria-controls="cancellation-info"
        >
          <AlertCircle className="h-4 w-4" aria-hidden="true" />
          <span>{cancellationPolicy} cancellation</span>
        </button>

        {showCancellationInfo && (
          <div id="cancellation-info" className="mt-2 p-3 bg-neutral-50 dark:bg-neutral-800 rounded-lg text-xs text-neutral-700 dark:text-neutral-300">
            <p className="font-medium mb-1">Cancellation Policy</p>
            <p>
              {cancellationPolicy === 'Flexible'
                ? 'Full refund if cancelled at least 5 days before check-in.'
                : cancellationPolicy === 'Moderate'
                ? 'Full refund if cancelled at least 7 days before check-in.'
                : 'Full refund if cancelled at least 30 days before check-in.'}
            </p>
          </div>
        )}
      </CardBody>

      {/* Minimum Stay Notice */}
      {minStay > 1 && (
        <CardBody className="border-t border-neutral-200 dark:border-neutral-700">
          <Badge variant="warning" size="sm">
            Minimum {minStay} night{minStay !== 1 ? 's' : ''} stay required
          </Badge>
        </CardBody>
      )}
    </Card>
  );
};
