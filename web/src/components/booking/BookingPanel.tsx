/**
 * Booking Panel Component
 * Features: Date picker, guest selector, price breakdown, cancellation policy, trust badges
 */
'use client';

import React, { useState } from 'react';
import { Card, CardHeader, CardBody, CardFooter } from '@/components/ui/Card';
import { Input } from '@/components/ui/Input';
import { Button } from '@/components/ui/Button';
import { Badge } from '@/components/ui/Badge';
import { AlertCircle, Check, Users, Calendar } from 'lucide-react';
import { format, differenceInDays } from 'date-fns';

interface BookingPanelProps {
  propertyId: string;
  pricePerNight: number;
  maxGuests: number;
  minStay?: number;
  cancellationPolicy?: string;
  hostVerified?: boolean;
  hostRating?: number;
  onBook?: (dates: { checkIn: Date; checkOut: Date }, guests: number) => void;
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

  // Calculate pricing
  const nights =
    checkInDate && checkOutDate ? differenceInDays(checkOutDate, checkInDate) : 0;
  const subtotal = nights * pricePerNight;
  const serviceFee = Math.round(subtotal * 0.1); // 10% service fee
  const tax = Math.round((subtotal + serviceFee) * 0.08); // 8% tax
  const total = subtotal + serviceFee + tax;

  const handleBook = () => {
    if (checkInDate && checkOutDate && nights >= minStay) {
      onBook?.({ checkIn: checkInDate, checkOut: checkOutDate }, guestCount);
    }
  };

  const isValidBooking = checkInDate && checkOutDate && nights >= minStay && guestCount > 0;

  return (
    <Card variant="elevated" className="sticky top-24">
      <CardHeader className="border-b border-neutral-200">
        <div className="flex items-baseline gap-2">
          <span className="text-3xl font-bold text-neutral-900">${pricePerNight}</span>
          <span className="text-sm text-neutral-600">per night</span>
        </div>
      </CardHeader>

      <CardBody className="space-y-4">
        {/* Check-in & Check-out */}
        <div className="space-y-2">
          <label className="block text-sm font-medium text-neutral-700">Check-in</label>
          <Input
            type="date"
            value={checkInDate ? format(checkInDate, 'yyyy-MM-dd') : ''}
            onChange={(e) => setCheckInDate(e.target.value ? new Date(e.target.value) : null)}
            icon={<Calendar className="h-4 w-4" />}
            placeholder="Add date"
          />
        </div>

        <div className="space-y-2">
          <label className="block text-sm font-medium text-neutral-700">Check-out</label>
          <Input
            type="date"
            value={checkOutDate ? format(checkOutDate, 'yyyy-MM-dd') : ''}
            onChange={(e) => setCheckOutDate(e.target.value ? new Date(e.target.value) : null)}
            icon={<Calendar className="h-4 w-4" />}
            placeholder="Add date"
            disabled={!checkInDate}
          />
        </div>

        {/* Guest Count */}
        <div className="space-y-2">
          <label className="block text-sm font-medium text-neutral-700">Guests</label>
          <div className="flex items-center gap-2 border-2 border-neutral-300 rounded-lg p-2">
            <Users className="h-5 w-5 text-neutral-400" />
            <select
              value={guestCount}
              onChange={(e) => setGuestCount(parseInt(e.target.value))}
              className="flex-1 bg-transparent outline-none text-sm"
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
          <div className="space-y-3 border-t border-neutral-200 pt-4">
            <div className="flex justify-between text-sm">
              <span className="text-neutral-600">
                ${pricePerNight} × {nights} nights
              </span>
              <span className="font-medium text-neutral-900">${subtotal}</span>
            </div>

            <div className="flex justify-between text-sm">
              <span className="text-neutral-600">Service fee</span>
              <span className="font-medium text-neutral-900">${serviceFee}</span>
            </div>

            <div className="flex justify-between text-sm">
              <span className="text-neutral-600">Tax</span>
              <span className="font-medium text-neutral-900">${tax}</span>
            </div>

            <div className="flex justify-between font-semibold border-t border-neutral-200 pt-3">
              <span>Total</span>
              <span className="text-xl text-primary-600">${total}</span>
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
        >
          {isValidBooking ? 'Book Now' : 'Select Dates'}
        </Button>
      </CardFooter>

      {/* Trust Indicators */}
      <CardBody className="border-t border-neutral-200 space-y-2">
        {hostVerified && (
          <div className="flex items-center gap-2 text-sm">
            <Check className="h-4 w-4 text-success-500" />
            <span className="text-neutral-700">Verified host</span>
          </div>
        )}

        {hostRating && (
          <div className="flex items-center gap-2 text-sm">
            <span className="text-neutral-700">★ {hostRating}</span>
            <span className="text-neutral-600">(Host rating)</span>
          </div>
        )}

        <button
          onClick={() => setShowCancellationInfo(!showCancellationInfo)}
          className="flex items-center gap-2 text-sm text-neutral-600 hover:text-neutral-900"
        >
          <AlertCircle className="h-4 w-4" />
          <span>{cancellationPolicy} cancellation</span>
        </button>

        {showCancellationInfo && (
          <div className="mt-2 p-3 bg-neutral-50 rounded-lg text-xs text-neutral-700">
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
        <CardBody className="border-t border-neutral-200">
          <Badge variant="warning" size="sm">
            Minimum {minStay} night stay required
          </Badge>
        </CardBody>
      )}
    </Card>
  );
};
