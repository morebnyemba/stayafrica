'use client';

import { useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { MapPin, Star, Filter, Search } from 'lucide-react';

export function ExploreContent() {
  const [searchQuery, setSearchQuery] = useState('');
  const [filters, setFilters] = useState({
    min_price: '',
    max_price: '',
    property_type: '',
  });

  const { data: propertiesData, isLoading, error } = useQuery({
    queryKey: ['properties', filters],
    queryFn: async () => {
      const response = await apiClient.getProperties({
        ...filters,
        search: searchQuery,
      });
      return response.data;
    },
  });

  const properties = propertiesData?.results || [];

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
        <div className="card p-6 mb-8">
          <div className="grid md:grid-cols-4 gap-4">
            <div className="md:col-span-2">
              <div className="relative">
                <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-primary-400 w-5 h-5" />
                <input
                  type="text"
                  placeholder="Search by location or property name..."
                  className="input-base pl-10"
                  value={searchQuery}
                  onChange={(e) => setSearchQuery(e.target.value)}
                />
              </div>
            </div>
            <div>
              <select
                className="input-base"
                value={filters.property_type}
                onChange={(e) => setFilters({ ...filters, property_type: e.target.value })}
              >
                <option value="">All Types</option>
                <option value="HOUSE">House</option>
                <option value="APARTMENT">Apartment</option>
                <option value="VILLA">Villa</option>
                <option value="COTTAGE">Cottage</option>
                <option value="LODGE">Lodge</option>
              </select>
            </div>
            <div>
              <select className="input-base">
                <option value="">Price Range</option>
                <option value="0-50">$0 - $50</option>
                <option value="51-100">$51 - $100</option>
                <option value="101-200">$101 - $200</option>
                <option value="201+">$201+</option>
              </select>
            </div>
          </div>
        </div>

        {/* Properties Grid */}
        {isLoading ? (
          <div className="grid md:grid-cols-3 gap-8">
            {[1, 2, 3, 4, 5, 6].map((i) => (
              <div key={i} className="card p-6 animate-pulse">
                <div className="h-48 bg-primary-200 dark:bg-primary-700 rounded-lg mb-4"></div>
                <div className="h-6 bg-primary-200 dark:bg-primary-700 rounded mb-3"></div>
                <div className="h-4 bg-primary-200 dark:bg-primary-700 rounded mb-2"></div>
                <div className="h-4 bg-primary-200 dark:bg-primary-700 rounded w-2/3"></div>
              </div>
            ))}
          </div>
        ) : error ? (
          <div className="card p-12 text-center">
            <p className="text-primary-600 dark:text-sand-300 mb-4">
              Unable to load properties. Please try again later.
            </p>
            <p className="text-sm text-primary-500 dark:text-sand-400">
              Error: {error instanceof Error ? error.message : 'Unknown error'}
            </p>
          </div>
        ) : properties.length === 0 ? (
          <div className="card p-12 text-center">
            <Filter className="w-16 h-16 text-primary-400 dark:text-sand-500 mx-auto mb-4" />
            <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-2">
              No Properties Found
            </h3>
            <p className="text-primary-600 dark:text-sand-300">
              Try adjusting your search filters to find more results.
            </p>
          </div>
        ) : (
          <div className="grid md:grid-cols-3 gap-8">
            {properties.map((property: any) => (
              <article
                key={property.id}
                className="card group overflow-hidden border border-primary-100/80 dark:border-primary-700 hover:border-secondary-400 transition cursor-pointer"
              >
                <div className="relative h-56 overflow-hidden">
                  <img
                    src={property.images?.[0]?.image_url || 'https://images.unsplash.com/photo-1512917774080-9991f1c52e1d'}
                    alt={property.title}
                    className="w-full h-full object-cover transition duration-700 group-hover:scale-105"
                  />
                  <div className="absolute inset-0 bg-gradient-to-t from-primary-900/70 via-primary-900/10 to-transparent opacity-0 group-hover:opacity-100 transition" />
                  {property.average_rating && (
                    <div className="absolute top-4 right-4 inline-flex items-center gap-1 bg-primary-900/80 backdrop-blur px-3 py-1 rounded-full text-sm font-semibold text-sand-50">
                      <Star className="w-4 h-4 text-secondary-300" fill="currentColor" />
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
                      <span>{property.city}, {property.country}</span>
                    </div>
                  </div>
                  <div className="flex justify-between items-end">
                    <div>
                      <span className="text-2xl font-semibold text-primary-900 dark:text-sand-50">
                        ${property.price_per_night}
                      </span>
                      <span className="text-sm text-primary-500 dark:text-sand-400 ml-1 font-medium">/night</span>
                    </div>
                    <button
                      type="button"
                      className="btn-secondary px-4 py-2 text-sm"
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
