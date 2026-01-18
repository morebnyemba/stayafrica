'use client';

import { CheckCircle, ShieldCheck, Star, CreditCard, Home } from 'lucide-react';

interface QualificationDetails {
  is_qualified: boolean;
  requirements_met: {
    id_verified: boolean;
    min_reviews: boolean;
    min_rating: boolean;
    completed_bookings: boolean;
    payment_method: boolean;
  };
  guest_stats: {
    review_count: number;
    average_rating: number;
    completed_bookings_count: number;
  };
}

interface GuestQualificationBadgeProps {
  details: QualificationDetails;
  size?: 'sm' | 'md' | 'lg';
}

export default function GuestQualificationBadge({ 
  details, 
  size = 'md' 
}: GuestQualificationBadgeProps) {
  if (!details.is_qualified) {
    return null;
  }

  const iconSize = size === 'sm' ? 'w-4 h-4' : size === 'md' ? 'w-5 h-5' : 'w-6 h-6';
  const textSize = size === 'sm' ? 'text-xs' : size === 'md' ? 'text-sm' : 'text-base';
  const badgeSize = size === 'sm' ? 'px-2 py-1' : size === 'md' ? 'px-3 py-2' : 'px-4 py-3';

  const qualifications = [
    {
      met: details.requirements_met.id_verified,
      icon: <ShieldCheck className={iconSize} />,
      label: 'ID Verified',
    },
    {
      met: details.requirements_met.min_reviews && details.guest_stats.review_count > 0,
      icon: <Star className={iconSize} />,
      label: `${details.guest_stats.review_count} Reviews`,
    },
    {
      met: details.requirements_met.completed_bookings,
      icon: <Home className={iconSize} />,
      label: `${details.guest_stats.completed_bookings_count} Stays`,
    },
    {
      met: details.requirements_met.payment_method,
      icon: <CreditCard className={iconSize} />,
      label: 'Payment Verified',
    },
  ];

  return (
    <div className={`bg-green-50 border border-green-200 rounded-lg ${badgeSize}`}>
      <div className="flex items-center gap-2 mb-2">
        <CheckCircle className={`${iconSize} text-green-600`} />
        <span className={`font-semibold text-green-900 ${textSize}`}>
          Qualified for Instant Booking
        </span>
      </div>
      
      <div className={`flex flex-wrap gap-2 ${size === 'sm' ? 'mt-1' : 'mt-2'}`}>
        {qualifications.filter(q => q.met).map((qual, index) => (
          <div
            key={index}
            className={`flex items-center gap-1 bg-white px-2 py-1 rounded-full border border-green-300 ${textSize}`}
          >
            <span className="text-green-600">{qual.icon}</span>
            <span className="text-green-800 font-medium">{qual.label}</span>
          </div>
        ))}
      </div>

      {details.guest_stats.average_rating > 0 && (
        <div className={`flex items-center gap-1 mt-2 ${textSize} text-green-700`}>
          <Star className={`${iconSize} fill-yellow-400 text-yellow-400`} />
          <span className="font-semibold">{details.guest_stats.average_rating.toFixed(1)}</span>
          <span className="text-green-600">average rating</span>
        </div>
      )}
    </div>
  );
}
