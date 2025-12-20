'use client';

import { useState } from 'react';
import { Search, Filter, X } from 'lucide-react';

interface SearchFiltersProps {
  onFilterChange?: (filters: FilterOptions) => void;
  onSearch?: (query: string) => void;
}

export interface FilterOptions {
  priceMin?: number;
  priceMax?: number;
  amenities?: string[];
  propertyType?: string;
  minRating?: number;
  guests?: number;
  checkIn?: string;
  checkOut?: string;
}

const PROPERTY_TYPES = [
  'Entire Place',
  'Private Room',
  'Shared Room',
  'Hotel Room',
];

const AMENITIES = [
  { id: 'wifi', name: 'WiFi' },
  { id: 'pool', name: 'Pool' },
  { id: 'kitchen', name: 'Kitchen' },
  { id: 'parking', name: 'Parking' },
  { id: 'ac', name: 'Air Conditioning' },
  { id: 'gym', name: 'Gym' },
  { id: 'heating', name: 'Heating' },
  { id: 'washer', name: 'Washer' },
];

export function SearchFilters({ onFilterChange, onSearch }: SearchFiltersProps) {
  const [isOpen, setIsOpen] = useState(false);
  const [searchQuery, setSearchQuery] = useState('');
  const [filters, setFilters] = useState<FilterOptions>({
    priceMin: 0,
    priceMax: 1000,
    amenities: [],
    propertyType: '',
    minRating: 0,
    guests: 1,
  });

  const handleSearchChange = (value: string) => {
    setSearchQuery(value);
    if (onSearch) {
      onSearch(value);
    }
  };

  const handleFilterChange = (newFilters: FilterOptions) => {
    setFilters(newFilters);
    if (onFilterChange) {
      onFilterChange(newFilters);
    }
  };

  const handleAmenityToggle = (amenityId: string) => {
    const updated = filters.amenities?.includes(amenityId)
      ? filters.amenities.filter((a) => a !== amenityId)
      : [...(filters.amenities || []), amenityId];

    handleFilterChange({
      ...filters,
      amenities: updated,
    });
  };

  const handleClearFilters = () => {
    setSearchQuery('');
    setFilters({
      priceMin: 0,
      priceMax: 1000,
      amenities: [],
      propertyType: '',
      minRating: 0,
      guests: 1,
    });
    if (onFilterChange) {
      onFilterChange({
        priceMin: 0,
        priceMax: 1000,
        amenities: [],
        propertyType: '',
        minRating: 0,
        guests: 1,
      });
    }
  };

  const activeFiltersCount =
    (filters.amenities?.length || 0) +
    (filters.propertyType ? 1 : 0) +
    ((filters.minRating || 0) > 0 ? 1 : 0) +
    (filters.guests > 1 ? 1 : 0);

  return (
    <div className="space-y-4">
      {/* Search bar */}
      <div className="relative">
        <Search className="absolute left-3 top-1/2 -translate-y-1/2 w-5 h-5 text-primary-400 dark:text-sand-400" />
        <input
          type="text"
          value={searchQuery}
          onChange={(e) => handleSearchChange(e.target.value)}
          placeholder="Search by location, property name..."
          className="w-full pl-10 pr-4 py-3 border border-primary-200 dark:border-primary-600 rounded-lg bg-sand-50 dark:bg-primary-700 text-primary-900 dark:text-sand-100 placeholder-primary-400 dark:placeholder-sand-400 focus:ring-2 focus:ring-secondary-500 focus:border-transparent"
        />
      </div>

      {/* Filter toggle button */}
      <button
        onClick={() => setIsOpen(!isOpen)}
        className="w-full flex items-center justify-between px-4 py-3 bg-white dark:bg-primary-800 border border-primary-200 dark:border-primary-700 rounded-lg hover:bg-sand-50 dark:hover:bg-primary-700 transition"
      >
        <div className="flex items-center space-x-2">
          <Filter className="w-5 h-5 text-primary-600 dark:text-sand-300" />
          <span className="font-medium text-primary-900 dark:text-sand-50">Filters</span>
          {activeFiltersCount > 0 && (
            <span className="bg-secondary-600 text-white text-xs rounded-full px-2 py-1">
              {activeFiltersCount}
            </span>
          )}
        </div>
        <span className="text-primary-400">{isOpen ? '−' : '+'}</span>
      </button>

      {/* Filters panel */}
      {isOpen && (
        <div className="bg-white dark:bg-primary-800 border border-primary-200 dark:border-primary-700 rounded-lg p-6 space-y-6">
          {/* Price range */}
          <div>
            <label className="block text-sm font-semibold text-primary-900 dark:text-sand-50 mb-4">
              Price Range
            </label>
            <div className="space-y-3">
              <div className="flex items-center space-x-4">
                <input
                  type="number"
                  value={filters.priceMin || 0}
                  onChange={(e) =>
                    handleFilterChange({
                      ...filters,
                      priceMin: parseInt(e.target.value),
                    })
                  }
                  placeholder="Min"
                  className="w-1/2 px-3 py-2 border border-primary-200 dark:border-primary-600 rounded-lg bg-sand-50 dark:bg-primary-700 text-primary-900 dark:text-sand-100"
                />
                <span className="text-primary-600 dark:text-sand-400">−</span>
                <input
                  type="number"
                  value={filters.priceMax || 1000}
                  onChange={(e) =>
                    handleFilterChange({
                      ...filters,
                      priceMax: parseInt(e.target.value),
                    })
                  }
                  placeholder="Max"
                  className="w-1/2 px-3 py-2 border border-primary-200 dark:border-primary-600 rounded-lg bg-sand-50 dark:bg-primary-700 text-primary-900 dark:text-sand-100"
                />
              </div>
            </div>
          </div>

          {/* Property type */}
          <div>
            <label className="block text-sm font-semibold text-primary-900 dark:text-sand-50 mb-3">
              Property Type
            </label>
            <select
              value={filters.propertyType || ''}
              onChange={(e) =>
                handleFilterChange({
                  ...filters,
                  propertyType: e.target.value,
                })
              }
              className="w-full px-3 py-2 border border-primary-200 dark:border-primary-600 rounded-lg bg-sand-50 dark:bg-primary-700 text-primary-900 dark:text-sand-100 focus:ring-2 focus:ring-secondary-500"
            >
              <option value="">All types</option>
              {PROPERTY_TYPES.map((type) => (
                <option key={type} value={type}>
                  {type}
                </option>
              ))}
            </select>
          </div>

          {/* Amenities */}
          <div>
            <label className="block text-sm font-semibold text-primary-900 dark:text-sand-50 mb-3">
              Amenities
            </label>
            <div className="grid grid-cols-2 gap-2">
              {AMENITIES.map((amenity) => (
                <label key={amenity.id} className="flex items-center space-x-2 cursor-pointer">
                  <input
                    type="checkbox"
                    checked={filters.amenities?.includes(amenity.id) || false}
                    onChange={() => handleAmenityToggle(amenity.id)}
                    className="rounded border-primary-300 text-secondary-600 focus:ring-secondary-500"
                  />
                  <span className="text-sm text-primary-700 dark:text-sand-200">
                    {amenity.name}
                  </span>
                </label>
              ))}
            </div>
          </div>

          {/* Minimum rating */}
          <div>
            <label className="block text-sm font-semibold text-primary-900 dark:text-sand-50 mb-3">
              Minimum Rating
            </label>
            <select
              value={filters.minRating || 0}
              onChange={(e) =>
                handleFilterChange({
                  ...filters,
                  minRating: parseFloat(e.target.value),
                })
              }
              className="w-full px-3 py-2 border border-primary-200 dark:border-primary-600 rounded-lg bg-sand-50 dark:bg-primary-700 text-primary-900 dark:text-sand-100 focus:ring-2 focus:ring-secondary-500"
            >
              <option value="0">Any rating</option>
              <option value="3.5">3.5+</option>
              <option value="4">4+</option>
              <option value="4.5">4.5+</option>
            </select>
          </div>

          {/* Guests */}
          <div>
            <label className="block text-sm font-semibold text-primary-900 dark:text-sand-50 mb-3">
              Number of Guests
            </label>
            <select
              value={filters.guests || 1}
              onChange={(e) =>
                handleFilterChange({
                  ...filters,
                  guests: parseInt(e.target.value),
                })
              }
              className="w-full px-3 py-2 border border-primary-200 dark:border-primary-600 rounded-lg bg-sand-50 dark:bg-primary-700 text-primary-900 dark:text-sand-100 focus:ring-2 focus:ring-secondary-500"
            >
              {[1, 2, 3, 4, 5, 6, 7, 8].map((num) => (
                <option key={num} value={num}>
                  {num} {num === 1 ? 'guest' : 'guests'}
                </option>
              ))}
            </select>
          </div>

          {/* Clear filters */}
          {activeFiltersCount > 0 && (
            <button
              onClick={handleClearFilters}
              className="w-full flex items-center justify-center space-x-2 text-primary-600 dark:text-sand-300 hover:text-primary-900 dark:hover:text-sand-50 transition"
            >
              <X className="w-4 h-4" />
              <span>Clear all filters</span>
            </button>
          )}
        </div>
      )}
    </div>
  );
}
