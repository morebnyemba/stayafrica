'use client';

import { useQuery } from '@tanstack/react-query';
import axios from 'axios';
import { BookingTax } from '@/types/tax-types';
import { Loader2, Info } from 'lucide-react';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8000';

interface TaxBreakdownProps {
  bookingId?: string;
  propertyId: string;
  checkIn: string;
  checkOut: string;
  guests: number;
}

export const TaxBreakdown = ({ 
  bookingId, 
  propertyId, 
  checkIn, 
  checkOut, 
  guests 
}: TaxBreakdownProps) => {
  const { data: taxData, isLoading, error } = useQuery<BookingTax>({
    queryKey: ['booking-taxes', propertyId, checkIn, checkOut, guests],
    queryFn: async () => {
      const token = localStorage.getItem('access_token');
      const response = await axios.post(
        `${API_BASE_URL}/api/v1/payments/tax/booking-taxes/`,
        {
          booking_id: bookingId,
          property_id: propertyId,
          check_in: checkIn,
          check_out: checkOut,
          guests,
        },
        {
          headers: {
            Authorization: `Bearer ${token}`,
            'Content-Type': 'application/json',
          },
        }
      );
      return response.data;
    },
    enabled: !!(propertyId && checkIn && checkOut),
  });

  if (isLoading) {
    return (
      <div className="flex items-center justify-center p-4">
        <Loader2 className="h-6 w-6 animate-spin text-gray-400" />
      </div>
    );
  }

  if (error) {
    return (
      <div className="bg-red-50 border border-red-200 rounded-lg p-4">
        <p className="text-sm text-red-800">
          Failed to calculate taxes. Please try again.
        </p>
      </div>
    );
  }

  if (!taxData) {
    return null;
  }

  return (
    <div className="space-y-4">
      <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
        <div className="flex items-start gap-2">
          <Info className="h-5 w-5 text-blue-600 flex-shrink-0 mt-0.5" />
          <div>
            <h4 className="text-sm font-medium text-blue-900">Tax Information</h4>
            <p className="text-xs text-blue-800 mt-1">
              Taxes are calculated based on the property location and local regulations.
            </p>
          </div>
        </div>
      </div>

      <div className="space-y-3">
        <div className="flex justify-between text-base">
          <span className="text-gray-700">Subtotal</span>
          <span className="font-medium text-gray-900">
            ${taxData.subtotal.toFixed(2)}
          </span>
        </div>

        {taxData.taxes && taxData.taxes.length > 0 && (
          <div className="pt-3 border-t space-y-2">
            <h4 className="text-sm font-medium text-gray-900">Taxes & Fees</h4>
            
            {taxData.taxes.map((tax, index) => (
              <div key={index} className="flex justify-between text-sm">
                <div className="flex flex-col">
                  <span className="text-gray-700">
                    {tax.tax_type}
                  </span>
                  <span className="text-xs text-gray-500">
                    {tax.jurisdiction} • {(tax.rate * 100).toFixed(2)}%
                  </span>
                </div>
                <span className="text-gray-900">
                  ${tax.amount.toFixed(2)}
                </span>
              </div>
            ))}
            
            <div className="flex justify-between text-sm pt-2 border-t">
              <span className="font-medium text-gray-700">Total Taxes</span>
              <span className="font-medium text-gray-900">
                ${taxData.total_tax.toFixed(2)}
              </span>
            </div>
          </div>
        )}

        <div className="flex justify-between text-lg font-semibold pt-3 border-t-2 border-gray-300">
          <span className="text-gray-900">Total</span>
          <span className="text-gray-900">
            ${taxData.grand_total.toFixed(2)}
          </span>
        </div>
      </div>

      <div className="text-xs text-gray-500 pt-2">
        <p>
          All prices are in USD. Exchange rates and taxes may vary.
        </p>
      </div>
    </div>
  );
};
