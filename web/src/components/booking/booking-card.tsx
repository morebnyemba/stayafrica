'use client';

import { useState, useMemo } from 'react';
import { useRouter } from 'next/navigation';
import { useAuth } from '@/store/auth-store';
import { useFeeConfiguration, useTaxEstimate, calculateBookingCost } from '@/hooks/use-fees';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { pricingApi } from '@/services/pricing-api';
import { Star, AlertCircle, Calendar, TrendingDown } from 'lucide-react';
import { toast } from 'react-hot-toast';
import { format, differenceInDays, addDays } from 'date-fns';
import DatePicker, { registerLocale } from 'react-datepicker';
import { enGB } from 'date-fns/locale';
import 'react-datepicker/dist/react-datepicker.css';

registerLocale('en-GB', enGB);

interface BookingCardProps {
  property?: {
    id: string;
    price_per_night: number;
    currency: string;
    average_rating?: number;
    booked_dates?: string[];
    cleaning_fee?: number;
    country?: string;
  };
}

export function BookingCard({ property }: BookingCardProps) {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const [checkInDate, setCheckInDate] = useState<Date | null>(null);
  const [checkOutDate, setCheckOutDate] = useState<Date | null>(null);
  const [guests, setGuests] = useState(1);

  const { data: feeConfig } = useFeeConfiguration();
  const { data: taxEstimate } = useTaxEstimate(property?.country);

  const { data: unavailableDatesData } = useQuery({
    queryKey: ['unavailable-dates', property?.id],
    queryFn: async () => {
      const response = await apiClient.getUnavailableDates(property!.id);
      return response.data;
    },
    enabled: !!property?.id,
  });
  const unavailableDates: Set<string> = useMemo(
    () => new Set(unavailableDatesData?.unavailable_dates || []),
    [unavailableDatesData]
  );

  const excludedDates = useMemo(() => {
    return [...unavailableDates].map(dateStr => {
      const [year, month, day] = dateStr.split('-').map(Number);
      return new Date(year, month - 1, day);
    });
  }, [unavailableDates]);

  const getDayClassName = (date: Date) => {
    const dateStr = format(date, 'yyyy-MM-dd');
    return unavailableDates.has(dateStr) ? 'booked-date' : '';
  };

  const hasUnavailableDatesInRange = (start: Date, end: Date) => {
    let current = new Date(start);
    while (current < end) {
      if (unavailableDates.has(format(current, 'yyyy-MM-dd'))) return true;
      current = addDays(current, 1);
    }
    return false;
  };

  if (!property) {
    return null;
  }

  const nights = checkInDate && checkOutDate ? differenceInDays(checkOutDate, checkInDate) : 0;
  const checkIn = checkInDate ? format(checkInDate, 'yyyy-MM-dd') : '';
  const checkOut = checkOutDate ? format(checkOutDate, 'yyyy-MM-dd') : '';

  // Fetch dynamic pricing from backend when dates are selected
  const { data: dynamicPricing, isLoading: dynamicLoading } = useQuery({
    queryKey: ['dynamic-pricing', property.id, checkIn, checkOut, guests],
    queryFn: () => pricingApi.calculateBookingTotal(property.id, checkIn, checkOut, guests),
    enabled: !!checkIn && !!checkOut && nights > 0,
    retry: false,
  });

  // Static fallback
  const staticCosts = feeConfig && nights > 0
    ? calculateBookingCost(property.price_per_night, nights, feeConfig, property.cleaning_fee, taxEstimate || null)
    : null;

  // Use dynamic pricing if available, otherwise fall back to static
  const hasDynamic = !!dynamicPricing && !dynamicLoading;
  const appliedRules = dynamicPricing?.applied_rules || [];
  const adjustedPerNight = dynamicPricing?.adjusted_price_per_night;
  const hasDiscount = adjustedPerNight && adjustedPerNight < property.price_per_night;

  const displayNightly = hasDynamic ? dynamicPricing.nightly_total : (staticCosts?.basePrice || 0);
  const displayServiceFee = hasDynamic ? dynamicPricing.service_fee : (staticCosts?.serviceFee || 0);
  const displayCleaningFee = hasDynamic ? (dynamicPricing.cleaning_fee || 0) : (staticCosts?.cleaningFee || 0);
  const displayTaxes = hasDynamic ? (dynamicPricing.taxes || 0) : (staticCosts?.taxes || 0);
  const displayTotal = hasDynamic ? dynamicPricing.grand_total : (staticCosts?.total || 0);

  // Detailed breakdowns
  const taxBreakdown: { name: string; type?: string; rate?: number; amount: number }[] =
    hasDynamic && dynamicPricing.tax_breakdown?.length
      ? dynamicPricing.tax_breakdown
      : staticCosts?.individualTaxes?.filter((t: { name: string; amount: number }) => t.amount > 0) || [];
  const feeBreakdown: { name: string; type?: string; amount: number }[] =
    hasDynamic && dynamicPricing.fee_breakdown?.length ? dynamicPricing.fee_breakdown : [];

  const hasDateConflict = checkInDate && checkOutDate && hasUnavailableDatesInRange(checkInDate, checkOutDate);

  const handleBooking = () => {
    if (!checkInDate || !checkOutDate) {
      toast.error('Please select check-in and check-out dates');
      return;
    }
    if (nights <= 0) {
      toast.error('Check-out date must be after check-in date');
      return;
    }
    if (hasDateConflict) {
      toast.error('Some dates in your selection are already booked');
      return;
    }

    const confirmUrl = `/booking/confirm?propertyId=${property.id}&checkIn=${checkIn}&checkOut=${checkOut}&guests=${guests}`;
    if (!isAuthenticated) {
      toast.error('Please log in to book');
      router.push(`/login?redirect=${encodeURIComponent(confirmUrl)}`);
      return;
    }
    router.push(confirmUrl);
  };

  return (
    <div className="bg-white rounded-lg border border-primary-200 p-6 sticky top-24 h-fit">
      {/* Price */}
      <div className="mb-6">
        <div className="flex items-baseline space-x-2">
          {hasDiscount ? (
            <>
              <span className="text-3xl font-bold text-green-700">
                {property.currency} {adjustedPerNight.toFixed(0)}
              </span>
              <span className="text-lg text-primary-400 line-through">
                {property.currency} {property.price_per_night}
              </span>
              <span className="text-primary-600">per night</span>
            </>
          ) : (
            <>
              <span className="text-3xl font-bold text-primary-900">
                {property.currency} {property.price_per_night}
              </span>
              <span className="text-primary-600">per night</span>
            </>
          )}
        </div>
        {appliedRules.length > 0 && (
          <div className="flex items-center gap-1 mt-1">
            <TrendingDown className="w-3.5 h-3.5 text-green-600" />
            <span className="text-xs text-green-700 font-medium">
              {appliedRules.map((r: any) => r.name).join(', ')} applied
            </span>
          </div>
        )}
      </div>

      {/* Rating */}
      {property.average_rating !== undefined && property.average_rating > 0 && (
        <div className="flex items-center space-x-1 mb-6 pb-6 border-b border-primary-200">
          <Star className="w-4 h-4 fill-secondary-500 text-secondary-500" />
          <span className="font-semibold text-primary-900">
            {property.average_rating.toFixed(1)}
          </span>
        </div>
      )}

      {/* Date inputs */}
      <div className="mb-6 pb-6 border-b border-primary-200 space-y-3">
        <div>
          <label className="block text-sm font-medium text-primary-900 mb-2">Check-in</label>
          <div className="relative">
            <Calendar className="absolute left-3 top-1/2 -translate-y-1/2 h-4 w-4 text-primary-400 z-10 pointer-events-none" />
            <DatePicker
              selected={checkInDate}
              onChange={(date: Date | null) => {
                setCheckInDate(date);
                if (checkOutDate && date && date >= checkOutDate) setCheckOutDate(null);
              }}
              minDate={new Date()}
              excludeDates={excludedDates}
              dayClassName={getDayClassName}
              locale="en-GB"
              dateFormat="dd-MM-yyyy"
              placeholderText="Add date"
              className="w-full px-3 py-2 pl-10 border border-primary-200 rounded-lg bg-sand-50 text-primary-900 focus:ring-2 focus:ring-secondary-500 focus:border-transparent text-sm"
            />
          </div>
        </div>
        <div>
          <label className="block text-sm font-medium text-primary-900 mb-2">Check-out</label>
          <div className="relative">
            <Calendar className="absolute left-3 top-1/2 -translate-y-1/2 h-4 w-4 text-primary-400 z-10 pointer-events-none" />
            <DatePicker
              selected={checkOutDate}
              onChange={(date: Date | null) => setCheckOutDate(date)}
              minDate={checkInDate ? addDays(checkInDate, 1) : new Date()}
              excludeDates={excludedDates}
              dayClassName={getDayClassName}
              locale="en-GB"
              dateFormat="dd-MM-yyyy"
              placeholderText="Add date"
              disabled={!checkInDate}
              className={`w-full px-3 py-2 pl-10 border border-primary-200 rounded-lg bg-sand-50 text-primary-900 focus:ring-2 focus:ring-secondary-500 focus:border-transparent text-sm ${!checkInDate ? 'opacity-50 cursor-not-allowed' : ''}`}
            />
          </div>
        </div>
        {hasDateConflict && (
          <p className="text-sm text-red-600">Some dates in your selection are already booked. Please choose different dates.</p>
        )}
        {unavailableDates.size > 0 && (
          <div className="flex items-center gap-2 text-xs text-primary-500">
            <span className="inline-block w-3 h-3 rounded-full bg-red-100 border border-red-300" />
            <span>Already booked</span>
          </div>
        )}
        <div>
          <label className="block text-sm font-medium text-primary-900 mb-2">Guests</label>
          <select
            value={guests}
            onChange={(e) => setGuests(parseInt(e.target.value))}
            className="w-full px-3 py-2 border border-primary-200 rounded-lg bg-sand-50 text-primary-900 focus:ring-2 focus:ring-secondary-500 focus:border-transparent"
          >
            {[1, 2, 3, 4, 5, 6, 7, 8].map((num) => (
              <option key={num} value={num}>{num} {num === 1 ? 'guest' : 'guests'}</option>
            ))}
          </select>
        </div>
      </div>

      {/* Cost breakdown */}
      {nights > 0 && (staticCosts || hasDynamic) && (
        <div className="mb-6 pb-6 border-b border-primary-200 space-y-2 text-sm">
          {dynamicLoading ? (
            <div className="animate-pulse space-y-2">
              <div className="h-4 bg-primary-100 rounded w-3/4" />
              <div className="h-4 bg-primary-100 rounded w-1/2" />
            </div>
          ) : (
            <>
              <div className="flex justify-between text-primary-700">
                <span>
                  {hasDiscount
                    ? <>{property.currency} <span className="line-through text-primary-400">{property.price_per_night}</span> {adjustedPerNight.toFixed(2)} × {nights} {nights === 1 ? 'night' : 'nights'}</>
                    : <>{property.currency} {property.price_per_night} × {nights} {nights === 1 ? 'night' : 'nights'}</>}
                </span>
                <span>{property.currency} {displayNightly.toFixed(2)}</span>
              </div>
              {displayServiceFee > 0 && (
                <div className="flex justify-between text-primary-700">
                  <span>Service fee</span>
                  <span>{property.currency} {displayServiceFee.toFixed(2)}</span>
                </div>
              )}
              {/* Individual fees from dynamic pricing */}
              {feeBreakdown.length > 0 ? (
                feeBreakdown.map((fee: any, idx: number) => (
                  <div key={`fee-${idx}`} className="flex justify-between text-primary-700">
                    <span>{fee.name}</span>
                    <span>{property.currency} {Number(fee.amount).toFixed(2)}</span>
                  </div>
                ))
              ) : displayCleaningFee > 0 ? (
                <div className="flex justify-between text-primary-700">
                  <span>Cleaning fee</span>
                  <span>{property.currency} {displayCleaningFee.toFixed(2)}</span>
                </div>
              ) : null}
              {/* Individual taxes */}
              {taxBreakdown.length > 0 ? (
                taxBreakdown.map((tax: any, idx: number) => (
                  <div key={`tax-${idx}`} className="flex justify-between text-primary-700">
                    <span>{tax.name}{tax.rate ? ` (${tax.rate}%)` : ''}</span>
                    <span>{property.currency} {Number(tax.amount).toFixed(2)}</span>
                  </div>
                ))
              ) : displayTaxes > 0 ? (
                <div className="flex justify-between text-primary-700">
                  <span>Taxes</span>
                  <span>{property.currency} {displayTaxes.toFixed(2)}</span>
                </div>
              ) : null}
              {appliedRules.length > 0 && (
                <div className="pt-1 text-xs text-green-700 font-medium">
                  💡 {appliedRules.map((r: any) => r.name).join(', ')}
                </div>
              )}
              <div className="flex justify-between font-semibold text-primary-900 pt-2">
                <span>Total</span>
                <span>{property.currency} {displayTotal.toFixed(2)}</span>
              </div>
            </>
          )}
        </div>
      )}

      {/* Booking button */}
      <button
        onClick={handleBooking}
        className="w-full bg-secondary-600 hover:bg-secondary-700 text-white font-medium py-3 px-4 rounded-lg transition mb-4"
      >
        Reserve
      </button>

      <div className="flex items-start space-x-2 text-xs text-primary-600">
        <AlertCircle className="w-4 h-4 flex-shrink-0 mt-0.5" />
        <p>You won&apos;t be charged yet</p>
      </div>
    </div>
  );
}
