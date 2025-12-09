'use client';

import { useState } from 'react';
import { useRouter } from 'next/navigation';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { MapPin, Star, Filter } from 'lucide-react';
import { PropertyListSkeleton } from './property-card-skeleton';
import { SearchFilters, type FilterOptions } from '@/components/common/search-filters';

export function ExploreContent() {
  const router = useRouter();
  const [filters, setFilters] = useState<FilterOptions>({
    priceMin: 0,
    priceMax: 1000,
    amenities: [],
    propertyType: '',
    minRating: 0,
    guests: 1,
  });

  const { data: propertiesData, isLoading, error } = useQuery({
    queryKey: ['properties', filters],
    queryFn: async () => {
      const response = await apiClient.getProperties({
        min_price: filters.priceMin,
        max_price: filters.priceMax,
        property_type: filters.propertyType,
        min_rating: filters.minRating,
        guests: filters.guests,
      });
      return response.data;
    },
  });

  const properties = propertiesData?.results || [];

  const handlePropertyClick = (propertyId: string) => {
    router.push(`/property?id=${propertyId}`);
  };

  return (
    <div className="bg-sand-100 dark:bg-primary-900 min-h-screen">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
        {/* Header */}
        <div className="mb-8">
          <h1 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-2">
            Explore Properties
          </h1>
          <p className="text-lg text-primary-600 dark:text-sand-200">
            Discover unique accommodations across Africa
          </p>
        </div>

        {/* Search and Filters */}
        <div className="mb-8 max-w-2xl">
          <SearchFilters
            onFilterChange={setFilters}
            onSearch={() => {
              // Search triggered
            }}
          />
        </div>

        {/* Properties Grid */}
        {isLoading ? (
          <PropertyListSkeleton count={9} />
        ) : error ? (
          <div className="bg-white dark:bg-primary-800 p-12 rounded-lg text-center border border-primary-200 dark:border-primary-700">
            <p className="text-primary-600 dark:text-sand-300 mb-4">
              Unable to load properties. Please try again later.
            </p>
            <p className="text-sm text-primary-500 dark:text-sand-400">
              Error: {error instanceof Error ? error.message : 'Unknown error'}
            </p>
          </div>
        ) : properties.length === 0 ? (
          <div className="bg-white dark:bg-primary-800 p-12 rounded-lg text-center border border-primary-200 dark:border-primary-700">
            <Filter className="w-16 h-16 text-primary-400 dark:text-sand-500 mx-auto mb-4" />
            <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-2">
              No Properties Found
            </h3>
            <p className="text-primary-600 dark:text-sand-300">
              Try adjusting your search filters to find more results.
            </p>
          </div>
        ) : (
          <div className="grid md:grid-cols-3 lg:grid-cols-4 gap-6">
            {properties.map((property: any) => (
              <article
                key={property.id}
                onClick={() => handlePropertyClick(property.id)}
                className="bg-white dark:bg-primary-800 rounded-lg group overflow-hidden border border-primary-100/80 dark:border-primary-700 hover:border-secondary-400 transition cursor-pointer"
              >
                <div className="relative h-56 overflow-hidden">
                  <img
                    src={
                      property.images?.[0]?.image_url ||
                      'https://images.unsplash.com/photo-1512917774080-9991f1c52e1d'
                    }
                    alt={property.title}
                    className="w-full h-full object-cover transition duration-700 group-hover:scale-105"
                  />
                  <div className="absolute inset-0 bg-gradient-to-t from-primary-900/70 via-primary-900/10 to-transparent opacity-0 group-hover:opacity-100 transition" />
                  {property.average_rating && (
                    <div className="absolute top-4 right-4 inline-flex items-center gap-1 bg-primary-900/80 backdrop-blur px-3 py-1 rounded-full text-sm font-semibold text-sand-50">
                      <Star
                        className="w-4 h-4 text-secondary-300"
                        fill="currentColor"
                      />
                      <span>{property.average_rating.toFixed(1)}</span>
                    </div>
                  )}
                </div>
                <div className="p-6 space-y-4">
                  <div>
                    <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-2 group-hover:text-secondary-600 transition">
                      {property.title}
                    </h3>
                    <div className="flex items-center gap-2 text-primary-600 dark:text-sand-300 text-sm">
                      <MapPin className="w-4 h-4" />
                      <span>
                        {property.city}, {property.country}
                      </span>
                    </div>
                  </div>
                  <div className="flex justify-between items-end">
                    <div>
                      <span className="text-2xl font-semibold text-primary-900 dark:text-sand-50">
                        ${property.price_per_night}
                      </span>
                      <span className="text-sm text-primary-500 dark:text-sand-400 ml-1 font-medium">
                        /night
                      </span>
                    </div>
                    <button
                      type="button"
                      className="bg-secondary-600 hover:bg-secondary-700 dark:bg-secondary-700 dark:hover:bg-secondary-600 text-white px-4 py-2 rounded-lg text-sm transition"
                    >
                      View details
                    </button>
                  </div>
                </div>
              </article>
            ))}
          </div>
        )}

        {/* Pagination Info */}
        {properties.length > 0 && (
          <div className="mt-8 text-center">
            <p className="text-primary-600 dark:text-sand-300">
              Showing {properties.length} properties
            </p>
          </div>
        )}
      </div>
    </div>
  );
}
