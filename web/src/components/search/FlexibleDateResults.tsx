'use client';

import React from 'react';
import { Calendar, TrendingDown, MapPin } from 'lucide-react';
import Link from 'next/link';
import Image from 'next/image';

interface DateOption {
  check_in: string;
  check_out: string;
  properties_count: number;
  price_range: {
    min: number;
    max: number;
    avg: number;
  };
  sample_properties: Array<{
    id: string;
    name: string;
    image_url: string;
    location: string;
    price: number;
    rating?: number;
  }>;
}

interface FlexibleDateResultsProps {
  results: DateOption[];
  isLoading?: boolean;
}

export default function FlexibleDateResults({ results, isLoading }: FlexibleDateResultsProps) {
  if (isLoading) {
    return (
      <div className="space-y-4">
        {[1, 2, 3].map((i) => (
          <div key={i} className="bg-white rounded-xl shadow p-6 animate-pulse">
            <div className="h-6 bg-gray-200 rounded w-1/3 mb-4"></div>
            <div className="grid grid-cols-3 gap-4">
              <div className="h-48 bg-gray-200 rounded"></div>
              <div className="h-48 bg-gray-200 rounded"></div>
              <div className="h-48 bg-gray-200 rounded"></div>
            </div>
          </div>
        ))}
      </div>
    );
  }

  if (!results || results.length === 0) {
    return (
      <div className="bg-white rounded-xl shadow p-12 text-center">
        <Calendar className="w-16 h-16 text-gray-400 mx-auto mb-4" />
        <h3 className="text-xl font-semibold text-gray-900 mb-2">No results found</h3>
        <p className="text-gray-600">Try adjusting your search criteria or flexibility settings</p>
      </div>
    );
  }

  return (
    <div className="space-y-6">
      {results.map((option, index) => {
        const checkInDate = new Date(option.check_in);
        const checkOutDate = new Date(option.check_out);
        const nights = Math.ceil(
          (checkOutDate.getTime() - checkInDate.getTime()) / (1000 * 60 * 60 * 24)
        );
        const savings = option.price_range.max - option.price_range.min;

        return (
          <div key={index} className="bg-white rounded-xl shadow hover:shadow-lg transition">
            {/* Date Header */}
            <div className="bg-gradient-to-r from-primary-50 to-blue-50 p-6 rounded-t-xl border-b">
              <div className="flex items-center justify-between flex-wrap gap-4">
                <div className="flex items-center gap-3">
                  <Calendar className="w-5 h-5 text-primary-600" />
                  <div>
                    <div className="text-lg font-semibold text-gray-900">
                      {checkInDate.toLocaleDateString('en-US', { 
                        weekday: 'short', 
                        month: 'short', 
                        day: 'numeric' 
                      })} - {checkOutDate.toLocaleDateString('en-US', { 
                        weekday: 'short', 
                        month: 'short', 
                        day: 'numeric' 
                      })}
                    </div>
                    <div className="text-sm text-gray-600">
                      {nights} {nights === 1 ? 'night' : 'nights'}
                    </div>
                  </div>
                </div>
                <div className="text-right">
                  <div className="text-sm text-gray-600">Price range</div>
                  <div className="text-2xl font-bold text-gray-900">
                    ${option.price_range.min} - ${option.price_range.max}
                  </div>
                  {savings > 0 && (
                    <div className="flex items-center gap-1 text-green-600 text-sm font-medium mt-1">
                      <TrendingDown className="w-4 h-4" />
                      Save up to ${savings}
                    </div>
                  )}
                </div>
              </div>
            </div>

            {/* Properties Grid */}
            <div className="p-6">
              <div className="flex items-center justify-between mb-4">
                <h4 className="text-sm font-semibold text-gray-900">
                  {option.properties_count} {option.properties_count === 1 ? 'property' : 'properties'} available
                </h4>
                <Link
                  href={`/explore?check_in=${option.check_in}&check_out=${option.check_out}`}
                  className="text-sm font-medium text-primary-600 hover:text-primary-700"
                >
                  View all →
                </Link>
              </div>

              <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                {option.sample_properties.slice(0, 3).map((property) => (
                  <Link
                    key={property.id}
                    href={`/property/${property.id}?check_in=${option.check_in}&check_out=${option.check_out}`}
                    className="group"
                  >
                    <div className="relative h-48 rounded-lg overflow-hidden mb-3">
                      <Image
                        src={property.image_url || '/placeholder-property.jpg'}
                        alt={property.name}
                        fill
                        className="object-cover group-hover:scale-110 transition duration-300"
                      />
                    </div>
                    <div>
                      <h5 className="font-semibold text-gray-900 group-hover:text-primary-600 transition line-clamp-1">
                        {property.name}
                      </h5>
                      <div className="flex items-center gap-1 text-sm text-gray-600 mt-1">
                        <MapPin className="w-3 h-3" />
                        <span className="line-clamp-1">{property.location}</span>
                      </div>
                      <div className="flex items-center justify-between mt-2">
                        <span className="text-lg font-bold text-gray-900">
                          ${property.price}
                        </span>
                        {property.rating && (
                          <span className="text-sm text-gray-600">
                            ⭐ {property.rating.toFixed(1)}
                          </span>
                        )}
                      </div>
                    </div>
                  </Link>
                ))}
              </div>
            </div>
          </div>
        );
      })}
    </div>
  );
}
