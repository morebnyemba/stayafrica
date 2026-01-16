'use client';

import React from 'react';
import { useInstantBooking } from '@/hooks/useInstantBooking';
import { Zap, Loader2, CheckCircle, XCircle } from 'lucide-react';
import GuestQualificationBadge from './GuestQualificationBadge';

interface InstantBookButtonProps {
  propertyId: string;
  checkIn: string;
  checkOut: string;
  guests: number;
  onSuccess: (booking: any) => void;
  className?: string;
}

export default function InstantBookButton({
  propertyId,
  checkIn,
  checkOut,
  guests,
  onSuccess,
  className = '',
}: InstantBookButtonProps) {
  const {
    isQualified,
    isLoading: checkingQualification,
    qualificationDetails,
    bookInstantly,
    isBooking,
    bookingError,
  } = useInstantBooking(propertyId);

  const handleInstantBook = async () => {
    const booking = await bookInstantly({
      check_in: checkIn,
      check_out: checkOut,
      number_of_guests: guests,
    });

    if (booking) {
      onSuccess(booking);
    }
  };

  if (checkingQualification) {
    return (
      <button
        disabled
        className={`
          flex items-center justify-center gap-2 px-6 py-3 
          bg-gray-100 text-gray-400 font-semibold rounded-lg cursor-not-allowed
          ${className}
        `}
      >
        <Loader2 className="w-5 h-5 animate-spin" />
        Checking eligibility...
      </button>
    );
  }

  if (!isQualified) {
    return (
      <div className={className}>
        <button
          disabled
          className="w-full flex items-center justify-center gap-2 px-6 py-3 bg-gray-100 text-gray-500 font-semibold rounded-lg cursor-not-allowed"
        >
          <XCircle className="w-5 h-5" />
          Instant Book Not Available
        </button>
        {qualificationDetails && !qualificationDetails.is_qualified && (
          <div className="mt-3 p-4 bg-yellow-50 border border-yellow-200 rounded-lg">
            <p className="text-sm font-medium text-yellow-900 mb-2">Requirements not met:</p>
            <ul className="text-sm text-yellow-800 space-y-1">
              {qualificationDetails.requirements_met.id_verified === false && (
                <li>• ID verification required</li>
              )}
              {qualificationDetails.requirements_met.min_reviews === false && (
                <li>• Minimum {qualificationDetails.required_reviews} reviews required</li>
              )}
              {qualificationDetails.requirements_met.min_rating === false && (
                <li>• Minimum {qualificationDetails.required_rating} rating required</li>
              )}
              {qualificationDetails.requirements_met.completed_bookings === false && (
                <li>• At least one completed booking required</li>
              )}
              {qualificationDetails.requirements_met.payment_method === false && (
                <li>• Payment method required</li>
              )}
            </ul>
          </div>
        )}
      </div>
    );
  }

  return (
    <div className={className}>
      <button
        onClick={handleInstantBook}
        disabled={isBooking}
        className="w-full flex items-center justify-center gap-2 px-6 py-3 bg-gradient-to-r from-yellow-400 to-orange-500 text-white font-bold rounded-lg hover:from-yellow-500 hover:to-orange-600 transition shadow-lg hover:shadow-xl disabled:opacity-50 disabled:cursor-not-allowed"
      >
        {isBooking ? (
          <>
            <Loader2 className="w-5 h-5 animate-spin" />
            Processing...
          </>
        ) : (
          <>
            <Zap className="w-5 h-5" />
            Book Instantly
          </>
        )}
      </button>

      {bookingError && (
        <div className="mt-3 p-4 bg-red-50 border border-red-200 rounded-lg">
          <p className="text-sm text-red-800">{bookingError}</p>
        </div>
      )}

      {qualificationDetails && (
        <div className="mt-3">
          <GuestQualificationBadge details={qualificationDetails} />
        </div>
      )}

      <p className="text-xs text-gray-500 mt-3 text-center">
        <CheckCircle className="w-4 h-4 inline mr-1 text-green-600" />
        Confirmed immediately, no waiting for host approval
      </p>
    </div>
  );
}
