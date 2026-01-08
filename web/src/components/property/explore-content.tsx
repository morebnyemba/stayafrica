'use client';

import { useState } from 'react';
import { useRouter } from 'next/navigation';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { Filter } from 'lucide-react';
import { PropertyCard } from '@/components/property';
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
              <PropertyCard
                key={property.id}
                id={property.id}
                title={property.title}
                location={`${property.city}, ${property.country}`}
                price={property.price_per_night}
                image={property.images?.[0]?.image_url || property.main_image || 'https://images.unsplash.com/photo-1512917774080-9991f1c52e1d'}
                rating={property.average_rating || 0}
                reviewCount={property.review_count || 0}
                bedrooms={property.bedrooms || 0}
                guests={property.max_guests || 1}
                amenities={property.amenities?.slice(0, 4) || []}
                isFavorite={false}
                onClick={() => handlePropertyClick(property.id)}
                onFavorite={() => {}}
              />
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
