'use client';

import { useMemo, useState, useEffect } from 'react';
import { useQuery } from '@tanstack/react-query';
import { useSearchParams } from 'next/navigation';
import { apiClient } from '@/services/api-client';
import { Filter, Navigation } from 'lucide-react';
import { PropertyListSkeleton } from './property-card-skeleton';
import { SearchFilters, type FilterOptions } from '@/components/common/search-filters';
import { Button } from '@/components/ui';
import toast from 'react-hot-toast';
import Link from 'next/link';
import FlexibilityToggle from '@/components/search/FlexibilityToggle';
import FlexibleDateSearchPanel, { FlexibilityType } from '@/components/search/FlexibleDateSearchPanel';
import FlexibleDateResults from '@/components/search/FlexibleDateResults';
import { useFlexibleSearch } from '@/hooks/useFlexibleSearch';

export function ExploreContent() {
  const searchParams = useSearchParams();
  
  // Initialize filters from URL parameters
  const [filters, setFilters] = useState<FilterOptions>(() => {
    return {
      priceMin: 0,
      priceMax: 1000,
      amenities: [],
      propertyType: searchParams.get('type') || '',
      minRating: 0,
      guests: parseInt(searchParams.get('guests') || '1'),
      checkIn: searchParams.get('check_in') || undefined,
      checkOut: searchParams.get('check_out') || undefined,
    };
  });
  
  const [searchQuery, setSearchQuery] = useState(searchParams.get('city') || '');
  const [userLocation, setUserLocation] = useState<{ lat: number; lng: number } | null>(null);
  const [gettingLocation, setGettingLocation] = useState(false);
  const [isFlexible, setIsFlexible] = useState(false);
  const [flexibilityType, setFlexibilityType] = useState<FlexibilityType>('exact');
  const [flexibleDays, setFlexibleDays] = useState<number | undefined>(3);

  // Get user's current location
  useEffect(() => {
    if (navigator.geolocation) {
      setGettingLocation(true);
      navigator.geolocation.getCurrentPosition(
        (position) => {
          setUserLocation({
            lat: position.coords.latitude,
            lng: position.coords.longitude,
          });
          setGettingLocation(false);
        },
        (error) => {
          console.error('Geolocation error:', error);
          setGettingLocation(false);
        },
        { enableHighAccuracy: false, timeout: 10000 }
      );
    }
  }, []);

  const { data: propertiesData, isLoading, error } = useQuery({
    queryKey: ['properties', filters, searchQuery, userLocation],
    queryFn: async () => {
      const params: any = {
        min_price: filters.priceMin,
        max_price: filters.priceMax,
        property_type: filters.propertyType,
        min_rating: filters.minRating,
        guests: filters.guests,
      };

      // Add search query
      if (searchQuery) {
        params.search = searchQuery;
      }

      // Add amenities filter (convert to comma-separated string or array)
      if (filters.amenities && filters.amenities.length > 0) {
        params.amenities = filters.amenities.join(',');
      }

      // Add check-in/check-out dates
      if (filters.checkIn) {
        params.check_in = filters.checkIn;
      }
      if (filters.checkOut) {
        params.check_out = filters.checkOut;
      }

      // If we have user location, search nearby properties
      if (userLocation) {
        params.latitude = userLocation.lat;
        params.longitude = userLocation.lng;
        params.radius_km = 100; // 100km radius
      }

      const response = await apiClient.getProperties(params);
      return response.data;
    },
  });

  const properties = propertiesData?.results || [];

  const featuredSections = useMemo(() => {
    const counts = (properties as any[]).reduce<Record<string, number>>((acc, property: any) => {
      const city = property.city?.trim();
      if (!city) return acc;
      acc[city] = (acc[city] || 0) + 1;
      return acc;
    }, {});

    const topCities = Object.entries(counts)
      .sort((a, b) => b[1] - a[1])
      .slice(0, 3)
      .map(([city]) => city);

    return topCities.map((city) => ({
      id: city,
      title: `Popular homes in ${city}`,
      city,
      data: properties.filter((property: any) => property.city === city).slice(0, 10),
    }));
  }, [properties]);

  const handleUseMyLocation = () => {
    if (!navigator.geolocation) {
      toast.error('Geolocation is not supported by your browser');
      return;
    }

    setGettingLocation(true);
    navigator.geolocation.getCurrentPosition(
      (position) => {
        setUserLocation({
          lat: position.coords.latitude,
          lng: position.coords.longitude,
        });
        toast.success('Location updated! Showing nearby properties');
        setGettingLocation(false);
      },
      (error) => {
        console.error('Geolocation error:', error);
        toast.error('Unable to get your location. Please enable location services.');
        setGettingLocation(false);
      },
      { enableHighAccuracy: true, timeout: 10000 }
    );
  };

  const handleSearch = (query: string) => {
    setSearchQuery(query);
  };

  const handleFlexibleSearch = (flexibility: FlexibilityType, days?: number) => {
    setFlexibilityType(flexibility);
    setFlexibleDays(days);
  };

  // Flexible search query
  const { data: flexibleResults, isLoading: flexibleLoading } = useFlexibleSearch({
    location: searchQuery,
    checkIn: filters.checkIn || new Date().toISOString().split('T')[0],
    checkOut: filters.checkOut || new Date(Date.now() + 7 * 24 * 60 * 60 * 1000).toISOString().split('T')[0],
    flexibility: flexibilityType,
    days: flexibleDays,
    guests: filters.guests,
  });

  return (
    <div className="bg-sand-100 dark:bg-primary-900 min-h-screen">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8 sm:py-12">
        {/* Header */}
        <div className="mb-8 animate-fade-in">
          <div className="flex items-start justify-between">
            <div>
              <h1 className="text-3xl sm:text-4xl md:text-5xl font-bold text-primary-900 dark:text-sand-50 mb-2">
                Explore Properties
              </h1>
              <p className="text-base sm:text-lg text-primary-600 dark:text-sand-200">
                {userLocation ? 'Showing properties near you' : 'Discover unique accommodations across Africa'}
              </p>
            </div>
            <div className="flex items-center gap-3">
              <FlexibilityToggle
                isFlexible={isFlexible}
                onChange={setIsFlexible}
              />
              <Button
                onClick={handleUseMyLocation}
                disabled={gettingLocation}
                variant="outline"
                size="sm"
                className="flex items-center gap-2"
              >
                <Navigation className="w-4 h-4" />
                {gettingLocation ? 'Getting location...' : userLocation ? 'Update location' : 'Use my location'}
              </Button>
            </div>
          </div>
        </div>

        {/* Search and Filters */}
        <div className="mb-8 max-w-2xl">
          <SearchFilters
            onFilterChange={setFilters}
            onSearch={handleSearch}
          />
        </div>

        {/* Flexible Date Search Panel */}
        {isFlexible && filters.checkIn && filters.checkOut && (
          <div className="mb-8 max-w-2xl">
            <FlexibleDateSearchPanel
              checkIn={new Date(filters.checkIn)}
              checkOut={new Date(filters.checkOut)}
              onSearch={handleFlexibleSearch}
            />
          </div>
        )}

        {/* Results: Flexible or Regular */}
        {isFlexible && flexibilityType !== 'exact' ? (
          <FlexibleDateResults
            results={flexibleResults?.date_options || []}
            isLoading={flexibleLoading}
          />
        ) : (
          <>
            {featuredSections.length > 0 && (
              <div className="mb-10 space-y-8">
                {featuredSections.map((section) => (
                  <div key={section.id}>
                    <div className="flex items-center justify-between mb-4">
                      <h3 className="text-lg sm:text-xl font-semibold text-primary-900 dark:text-sand-50">
                        {section.title}
                      </h3>
                      <Link
                        href={`/explore?city=${encodeURIComponent(section.city)}`}
                        className="text-sm font-semibold text-secondary-600 hover:text-secondary-700"
                      >
                        View more
                      </Link>
                    </div>
                    <div className="flex gap-4 overflow-x-auto pb-2">
                      {section.data.map((property: any) => (
                        <Link
                          key={`${section.id}-${property.id}`}
                          href={`/property/${property.id}`}
                          className="min-w-[220px] sm:min-w-[260px] group"
                        >
                          <div className="bg-white dark:bg-primary-800 rounded-xl overflow-hidden shadow-md hover:shadow-xl transition border border-primary-100 dark:border-primary-700">
                            <div className="relative h-36 sm:h-40 overflow-hidden">
                              <img
                                src={property.images?.[0]?.image_url || property.main_image || 'https://images.unsplash.com/photo-1512917774080-9991f1c52e1d'}
                                alt={property.title}
                                className="w-full h-full object-cover transition duration-500 group-hover:scale-110"
                              />
                            </div>
                            <div className="p-3">
                              <p className="text-sm font-semibold text-primary-900 dark:text-sand-50 line-clamp-1">
                                {property.title}
                              </p>
                              <p className="text-xs text-primary-600 dark:text-sand-300 line-clamp-1">
                                {property.city}, {property.country}
                              </p>
                              <div className="mt-2 text-sm font-semibold text-primary-900 dark:text-sand-50">
                                ${property.price_per_night}
                                <span className="text-xs font-normal text-primary-500 dark:text-sand-400"> / night</span>
                              </div>
                            </div>
                          </div>
                        </Link>
                      ))}
                    </div>
                  </div>
                ))}
              </div>
            )}
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
          <div className="grid grid-cols-2 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-3 sm:gap-6">
            {properties.map((property: any) => (
              <Link
                key={property.id}
                href={`/property/${property.id}`}
                className="group cursor-pointer"
              >
                <div className="bg-white dark:bg-primary-800 rounded-xl overflow-hidden shadow-md hover:shadow-2xl transition-all duration-300 border border-primary-100 dark:border-primary-700">
                  {/* Image Container - Matching home page style */}
                  <div className="relative h-48 sm:h-52 md:h-60 overflow-hidden">
                    <img
                      src={property.images?.[0]?.image_url || property.main_image || 'https://images.unsplash.com/photo-1512917774080-9991f1c52e1d'}
                      alt={property.title}
                      className="w-full h-full object-cover transition duration-500 group-hover:scale-110"
                    />
                    <div className="absolute inset-0 bg-gradient-to-t from-black/60 via-black/0 to-transparent opacity-0 group-hover:opacity-100 transition-opacity duration-300" />
                    {property.average_rating && (
                      <div className="absolute top-3 right-3 flex items-center gap-1 bg-white/98 dark:bg-primary-900/98 backdrop-blur-sm px-2.5 py-1.5 rounded-lg shadow-lg">
                        <svg className="w-3.5 h-3.5 text-secondary-500" fill="currentColor" viewBox="0 0 20 20">
                          <path d="M9.049 2.927c.3-.921 1.603-.921 1.902 0l1.07 3.292a1 1 0 00.95.69h3.462c.969 0 1.371 1.24.588 1.81l-2.8 2.034a1 1 0 00-.364 1.118l1.07 3.292c.3.921-.755 1.688-1.54 1.118l-2.8-2.034a1 1 0 00-1.175 0l-2.8 2.034c-.784.57-1.838-.197-1.539-1.118l1.07-3.292a1 1 0 00-.364-1.118L2.98 8.72c-.783-.57-.38-1.81.588-1.81h3.461a1 1 0 00.951-.69l1.07-3.292z" />
                        </svg>
                        <span className="text-sm font-bold text-primary-900 dark:text-sand-50">{property.average_rating.toFixed(1)}</span>
                      </div>
                    )}
                  </div>
                  
                  {/* Content - Matching home page style */}
                  <div className="p-4">
                    <div className="mb-2">
                      <h3 className="font-bold text-base text-primary-900 dark:text-sand-50 group-hover:text-secondary-600 dark:group-hover:text-secondary-400 transition line-clamp-1 mb-1">
                        {property.city}, {property.country}
                      </h3>
                      <p className="text-sm text-primary-600 dark:text-sand-300 line-clamp-1">
                        {property.title}
                      </p>
                    </div>
                    
                    {/* Property details */}
                    {(property.bedrooms || property.max_guests) && (
                      <div className="flex items-center gap-3 text-xs text-primary-500 dark:text-sand-400 mb-3">
                        {property.bedrooms && <span>{property.bedrooms} bed{property.bedrooms > 1 ? 's' : ''}</span>}
                        {property.bedrooms && property.max_guests && <span>•</span>}
                        {property.max_guests && <span>{property.max_guests} guest{property.max_guests > 1 ? 's' : ''}</span>}
                      </div>
                    )}
                    
                    <div className="flex items-baseline justify-between">
                      <div>
                        <span className="font-bold text-lg text-primary-900 dark:text-sand-50">
                          ${property.price_per_night}
                        </span>
                        <span className="text-sm text-primary-600 dark:text-sand-300 ml-1">/ night</span>
                      </div>
                    </div>
                  </div>
                </div>
              </Link>
            ))}
          </div>
        )}

        {/* Pagination Info */}
        {properties.length > 0 && !isFlexible && (
          <div className="mt-8 text-center">
            <p className="text-primary-600 dark:text-sand-300">
              Showing {properties.length} properties {userLocation && '(sorted by distance)'}
            </p>
          </div>
        )}
            </>
          )}
      </div>
    </div>
  );
}
