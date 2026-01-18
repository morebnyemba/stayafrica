'use client';

import { X, Calendar, TrendingUp, TrendingDown, DollarSign, Receipt } from 'lucide-react';
import { DynamicPricing } from '@/types/pricing-types';

interface PriceBreakdownModalProps {
  pricing: DynamicPricing;
  checkIn: string;
  checkOut: string;
  onClose: () => void;
}

export default function PriceBreakdownModal({
  pricing,
  checkIn,
  checkOut,
  onClose,
}: PriceBreakdownModalProps) {
  const nights = Math.ceil(
    (new Date(checkOut).getTime() - new Date(checkIn).getTime()) / (1000 * 60 * 60 * 24)
  );

  const baseTotal = pricing.base_price * nights;
  const adjustedTotal = pricing.adjusted_price * nights;

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/50 p-4">
      <div className="bg-white rounded-xl shadow-2xl max-w-2xl w-full max-h-[90vh] overflow-y-auto">
        {/* Header */}
        <div className="sticky top-0 bg-white border-b px-6 py-4 flex items-center justify-between">
          <h2 className="text-2xl font-bold text-gray-900">Price Breakdown</h2>
          <button
            onClick={onClose}
            className="p-2 hover:bg-gray-100 rounded-full transition"
          >
            <X className="w-5 h-5" />
          </button>
        </div>

        <div className="p-6 space-y-6">
          {/* Stay Details */}
          <div className="bg-blue-50 rounded-lg p-4">
            <div className="flex items-center gap-2 text-blue-900 mb-2">
              <Calendar className="w-5 h-5" />
              <span className="font-semibold">Stay Details</span>
            </div>
            <div className="text-sm text-blue-800">
              <p>{new Date(checkIn).toLocaleDateString()} - {new Date(checkOut).toLocaleDateString()}</p>
              <p className="mt-1">{nights} {nights === 1 ? 'night' : 'nights'}</p>
            </div>
          </div>

          {/* Base Price */}
          <div>
            <div className="flex items-center gap-2 mb-3">
              <DollarSign className="w-5 h-5 text-gray-600" />
              <h3 className="font-semibold text-gray-900">Base Price</h3>
            </div>
            <div className="bg-gray-50 rounded-lg p-4">
              <div className="flex justify-between items-center">
                <span className="text-gray-700">
                  ${pricing.base_price.toFixed(2)} × {nights} {nights === 1 ? 'night' : 'nights'}
                </span>
                <span className="font-semibold text-gray-900">
                  ${baseTotal.toFixed(2)}
                </span>
              </div>
            </div>
          </div>

          {/* Applied Rules */}
          {pricing.applied_rules.length > 0 && (
            <div>
              <div className="flex items-center gap-2 mb-3">
                <TrendingUp className="w-5 h-5 text-primary-600" />
                <h3 className="font-semibold text-gray-900">Dynamic Pricing Adjustments</h3>
              </div>
              <div className="space-y-2">
                {pricing.applied_rules.map((rule, index) => {
                  const isDiscount = rule.adjusted_amount < rule.original_amount;
                  const difference = rule.adjusted_amount - rule.original_amount;
                  return (
                    <div
                      key={index}
                      className={`rounded-lg p-4 ${
                        isDiscount ? 'bg-green-50 border border-green-200' : 'bg-orange-50 border border-orange-200'
                      }`}
                    >
                      <div className="flex items-start justify-between">
                        <div>
                          <div className="flex items-center gap-2">
                            {isDiscount ? (
                              <TrendingDown className="w-4 h-4 text-green-600" />
                            ) : (
                              <TrendingUp className="w-4 h-4 text-orange-600" />
                            )}
                            <span className={`font-medium ${
                              isDiscount ? 'text-green-900' : 'text-orange-900'
                            }`}>
                              {rule.rule_name}
                            </span>
                          </div>
                          <p className={`text-sm mt-1 ${
                            isDiscount ? 'text-green-700' : 'text-orange-700'
                          }`}>
                            {rule.adjustment_type === 'percentage'
                              ? `${rule.adjustment_value}% ${isDiscount ? 'discount' : 'increase'}`
                              : `$${Math.abs(rule.adjustment_value)} ${isDiscount ? 'discount' : 'surcharge'}`
                            }
                          </p>
                        </div>
                        <span className={`font-semibold ${
                          isDiscount ? 'text-green-700' : 'text-orange-700'
                        }`}>
                          {difference >= 0 ? '+' : ''}${difference.toFixed(2)}
                        </span>
                      </div>
                    </div>
                  );
                })}
              </div>
              <div className="bg-purple-50 rounded-lg p-4 mt-2 border border-purple-200">
                <div className="flex justify-between items-center">
                  <span className="font-medium text-purple-900">Adjusted Nightly Rate</span>
                  <span className="font-bold text-purple-900 text-lg">
                    ${pricing.adjusted_price.toFixed(2)}
                  </span>
                </div>
              </div>
            </div>
          )}

          {/* Fees */}
          {pricing.fees.length > 0 && (
            <div>
              <div className="flex items-center gap-2 mb-3">
                <Receipt className="w-5 h-5 text-gray-600" />
                <h3 className="font-semibold text-gray-900">Fees</h3>
              </div>
              <div className="space-y-2">
                {pricing.fees.map((fee, index) => (
                  <div key={index} className="flex justify-between items-center bg-gray-50 rounded-lg p-3">
                    <span className="text-gray-700">{fee.name}</span>
                    <span className="font-medium text-gray-900">${fee.amount.toFixed(2)}</span>
                  </div>
                ))}
              </div>
            </div>
          )}

          {/* Taxes */}
          {pricing.taxes.length > 0 && (
            <div>
              <h3 className="font-semibold text-gray-900 mb-3">Taxes</h3>
              <div className="space-y-2">
                {pricing.taxes.map((tax, index) => (
                  <div key={index} className="flex justify-between items-center bg-gray-50 rounded-lg p-3">
                    <div>
                      <span className="text-gray-700">{tax.name}</span>
                      <span className="text-sm text-gray-500 ml-2">({(tax.rate * 100).toFixed(1)}%)</span>
                    </div>
                    <span className="font-medium text-gray-900">${tax.amount.toFixed(2)}</span>
                  </div>
                ))}
              </div>
            </div>
          )}

          {/* Total */}
          <div className="border-t pt-4">
            <div className="flex justify-between items-center">
              <span className="text-xl font-bold text-gray-900">Total</span>
              <span className="text-3xl font-bold text-primary-600">
                ${pricing.total_price.toFixed(2)}
              </span>
            </div>
            {adjustedTotal !== baseTotal && (
              <p className="text-sm text-gray-600 mt-2 text-right">
                You save ${(baseTotal - adjustedTotal).toFixed(2)} with dynamic pricing
              </p>
            )}
          </div>
        </div>
      </div>
    </div>
  );
}
