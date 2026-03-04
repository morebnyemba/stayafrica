'use client';

import { useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { TrendingUp, TrendingDown, Info } from 'lucide-react';
import pricingApi from '@/services/pricing-api';
import PriceBreakdownModal from './PriceBreakdownModal';
import PricingRuleIndicator from './PricingRuleIndicator';

interface DynamicPricingDisplayProps {
  propertyId: string;
  checkIn: string;
  checkOut: string;
  basePrice: number;
  guests?: number;
  className?: string;
}

export default function DynamicPricingDisplay({
  propertyId,
  checkIn,
  checkOut,
  basePrice,
  guests = 1,
  className = '',
}: DynamicPricingDisplayProps) {
  const [showBreakdown, setShowBreakdown] = useState(false);

  const { data: pricing, isLoading, error } = useQuery({
    queryKey: ['dynamic-pricing', propertyId, checkIn, checkOut, guests],
    queryFn: () => pricingApi.calculateBookingTotal(propertyId, checkIn, checkOut, guests),
    enabled: !!checkIn && !!checkOut,
  });

  if (isLoading) {
    return (
      <div className={`animate-pulse ${className}`}>
        <div className="h-8 bg-primary-200 dark:bg-primary-700 rounded w-32"></div>
        <div className="h-4 bg-primary-200 dark:bg-primary-700 rounded w-24 mt-2"></div>
      </div>
    );
  }

  if (error || !pricing) {
    return (
      <div className={className}>
        <p className="text-2xl font-bold">${basePrice}</p>
        <p className="text-sm text-primary-500 dark:text-sand-400">per night</p>
      </div>
    );
  }

  const priceChange = ((pricing.adjusted_price - pricing.base_price) / pricing.base_price) * 100;
  const hasDiscount = priceChange < 0;

  return (
    <div className={`space-y-2 ${className}`}>
      <div className="flex items-baseline gap-3">
        <div className="flex items-center gap-2">
          <span className="text-3xl font-bold text-primary-900 dark:text-sand-50">
            ${pricing.adjusted_price.toFixed(0)}
          </span>
          {Math.abs(priceChange) > 1 && (
            <span className={`flex items-center gap-1 text-sm font-medium ${
              hasDiscount ? 'text-green-600' : 'text-orange-600'
            }`}>
              {hasDiscount ? <TrendingDown className="w-4 h-4" /> : <TrendingUp className="w-4 h-4" />}
              {Math.abs(priceChange).toFixed(0)}%
            </span>
          )}
        </div>
        {Math.abs(priceChange) > 1 && (
          <span className="text-lg text-primary-400 dark:text-sand-500 line-through">
            ${pricing.base_price.toFixed(0)}
          </span>
        )}
      </div>

      <div className="flex items-center gap-2 text-sm text-primary-500 dark:text-sand-400">
        <span>per night</span>
        {pricing.applied_rules.length > 0 && (
          <>
            <span>•</span>
            <button
              onClick={() => setShowBreakdown(true)}
              className="flex items-center gap-1 text-primary-600 hover:text-primary-700 font-medium"
            >
              <Info className="w-4 h-4" />
              Price details
            </button>
          </>
        )}
      </div>

      {pricing.applied_rules.length > 0 && (
        <div className="flex flex-wrap gap-2 mt-3">
          {pricing.applied_rules.slice(0, 2).map((rule: any, index: number) => (
            <PricingRuleIndicator key={index} rule={rule} />
          ))}
          {pricing.applied_rules.length > 2 && (
            <button
              onClick={() => setShowBreakdown(true)}
              className="px-2 py-1 text-xs font-medium text-primary-500 dark:text-sand-400 bg-primary-100 dark:bg-primary-800 rounded-full hover:bg-primary-200 dark:hover:bg-primary-700 transition"
            >
              +{pricing.applied_rules.length - 2} more
            </button>
          )}
        </div>
      )}

      {showBreakdown && (
        <PriceBreakdownModal
          pricing={pricing}
          checkIn={checkIn}
          checkOut={checkOut}
          onClose={() => setShowBreakdown(false)}
        />
      )}
    </div>
  );
}
