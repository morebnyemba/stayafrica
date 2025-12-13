'use client';

import { useState } from 'react';
import { useRouter } from 'next/navigation';
import { useAuth } from '@/context/auth-context';
import { useFeeConfiguration, calculateBookingCost } from '@/hooks/use-fees';
import { Star, AlertCircle } from 'lucide-react';
import { toast } from 'react-hot-toast';

interface BookingCardProps {
  property?: {
    id: string;
    price_per_night: number;
    currency: string;
    average_rating?: number;
    booked_dates?: string[];
    cleaning_fee?: number;
  };
}

export function BookingCard({ property }: BookingCardProps) {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const [checkIn, setCheckIn] = useState<string>('');
  const [checkOut, setCheckOut] = useState<string>('');
  const [guests, setGuests] = useState(1);

  // Fetch fee configuration
  const { data: feeConfig } = useFeeConfiguration();

  if (!property) {
    return null;
  }

  const nights = checkIn && checkOut ? Math.ceil((new Date(checkOut).getTime() - new Date(checkIn).getTime()) / (1000 * 60 * 60 * 24)) : 0;
  
  // Calculate costs using the fee configuration
  const costs = feeConfig && nights > 0
    ? calculateBookingCost(property.price_per_night, nights, feeConfig, property.cleaning_fee)
    : { basePrice: 0, serviceFee: 0, commissionFee: 0, commissionRate: 0, cleaningFee: 0, total: 0 };

  const handleBooking = () => {
    if (!isAuthenticated) {
      toast.error('Please log in to book');
      router.push('/login');
      return;
    }

    if (!checkIn || !checkOut) {
      toast.error('Please select check-in and check-out dates');
      return;
    }

    if (nights <= 0) {
      toast.error('Check-out date must be after check-in date');
      return;
    }

    // Navigate to booking confirmation page
    router.push(
      `/booking/confirm?propertyId=${property.id}&checkIn=${checkIn}&checkOut=${checkOut}&guests=${guests}`
    );
  };

  return (
    <div className="bg-white dark:bg-primary-800 rounded-lg border border-primary-200 dark:border-primary-700 p-6 sticky top-24 h-fit">
      {/* Price */}
      <div className="mb-6">
        <div className="flex items-baseline space-x-2">
          <span className="text-3xl font-bold text-primary-900 dark:text-sand-50">
            {property.currency} {property.price_per_night}
          </span>
          <span className="text-primary-600 dark:text-sand-400">per night</span>
        </div>
      </div>

      {/* Rating */}
      {property.average_rating !== undefined && property.average_rating > 0 && (
        <div className="flex items-center space-x-1 mb-6 pb-6 border-b border-primary-200 dark:border-primary-700">
          <Star className="w-4 h-4 fill-secondary-500 text-secondary-500" />
          <span className="font-semibold text-primary-900 dark:text-sand-50">
            {property.average_rating.toFixed(1)}
          </span>
        </div>
      )}

      {/* Date inputs */}
      <div className="mb-6 pb-6 border-b border-primary-200 dark:border-primary-700 space-y-3">
        <div>
          <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
            Check-in
          </label>
          <input
            type="date"
            value={checkIn}
            onChange={(e) => setCheckIn(e.target.value)}
            className="w-full px-3 py-2 border border-primary-200 dark:border-primary-600 rounded-lg bg-sand-50 dark:bg-primary-700 text-primary-900 dark:text-sand-100 focus:ring-2 focus:ring-secondary-500 focus:border-transparent"
          />
        </div>
        <div>
          <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
            Check-out
          </label>
          <input
            type="date"
            value={checkOut}
            onChange={(e) => setCheckOut(e.target.value)}
            className="w-full px-3 py-2 border border-primary-200 dark:border-primary-600 rounded-lg bg-sand-50 dark:bg-primary-700 text-primary-900 dark:text-sand-100 focus:ring-2 focus:ring-secondary-500 focus:border-transparent"
          />
        </div>
        <div>
          <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
            Guests
          </label>
          <select
            value={guests}
            onChange={(e) => setGuests(parseInt(e.target.value))}
            className="w-full px-3 py-2 border border-primary-200 dark:border-primary-600 rounded-lg bg-sand-50 dark:bg-primary-700 text-primary-900 dark:text-sand-100 focus:ring-2 focus:ring-secondary-500 focus:border-transparent"
          >
            {[1, 2, 3, 4, 5, 6, 7, 8].map((num) => (
              <option key={num} value={num}>
                {num} {num === 1 ? 'guest' : 'guests'}
              </option>
            ))}
          </select>
        </div>
      </div>

      {/* Cost breakdown */}
      {nights > 0 && feeConfig && (
        <div className="mb-6 pb-6 border-b border-primary-200 dark:border-primary-700 space-y-2 text-sm">
          <div className="flex justify-between text-primary-700 dark:text-sand-200">
            <span>
              {property.currency} {property.price_per_night} × {nights} {nights === 1 ? 'night' : 'nights'}
            </span>
            <span>{property.currency} {costs.basePrice.toFixed(2)}</span>
          </div>
          <div className="flex justify-between text-primary-700 dark:text-sand-200">
            <span>Service fee</span>
            <span>{property.currency} {costs.serviceFee.toFixed(2)}</span>
          </div>
          <div className="flex justify-between text-primary-700 dark:text-sand-200">
            <span>Commission fee ({(costs.commissionRate * 100).toFixed(1)}%)</span>
            <span>{property.currency} {costs.commissionFee.toFixed(2)}</span>
          </div>
          {costs.cleaningFee > 0 && (
            <div className="flex justify-between text-primary-700 dark:text-sand-200">
              <span>Cleaning fee</span>
              <span>{property.currency} {costs.cleaningFee.toFixed(2)}</span>
            </div>
          )}
          <div className="flex justify-between font-semibold text-primary-900 dark:text-sand-50 pt-2">
            <span>Total</span>
            <span>{property.currency} {costs.total.toFixed(2)}</span>
          </div>
        </div>
      )}

      {/* Booking button */}
      <button
        onClick={handleBooking}
        className="w-full bg-secondary-600 hover:bg-secondary-700 dark:bg-secondary-700 dark:hover:bg-secondary-600 text-white font-medium py-3 px-4 rounded-lg transition mb-4"
      >
        Reserve
      </button>

      {/* Info message */}
      <div className="flex items-start space-x-2 text-xs text-primary-600 dark:text-sand-400">
        <AlertCircle className="w-4 h-4 flex-shrink-0 mt-0.5" />
        <p>You won't be charged yet</p>
      </div>
    </div>
  );
}
