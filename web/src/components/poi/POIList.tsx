'use client';

import { useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { ChevronDown, ChevronUp, Loader2, MapPin } from 'lucide-react';
import poiApi from '@/services/poi-api';
import { POIType } from '@/types/poi-types';
import POICard from './POICard';
import POICategoryFilter from './POICategoryFilter';

interface POIListProps {
  propertyId: string;
  radiusKm?: number;
}

export default function POIList({ propertyId, radiusKm = 2 }: POIListProps) {
  const [selectedCategories, setSelectedCategories] = useState<POIType[]>([]);
  const [expandedCategories, setExpandedCategories] = useState<POIType[]>([]);

  const { data: nearbyData, isLoading, error } = useQuery({
    queryKey: ['nearby-pois', propertyId, radiusKm, selectedCategories],
    queryFn: () =>
      poiApi.getNearbyPOIs(propertyId, {
        radiusKm,
        poiTypes: selectedCategories.length > 0 ? selectedCategories : undefined,
      }),
  });

  const toggleCategory = (category: POIType) => {
    setExpandedCategories(prev =>
      prev.includes(category)
        ? prev.filter(c => c !== category)
        : [...prev, category]
    );
  };

  if (isLoading) {
    return (
      <div className="flex items-center justify-center py-12">
        <Loader2 className="w-8 h-8 animate-spin text-primary-600" />
      </div>
    );
  }

  if (error) {
    return (
      <div className="bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 rounded-lg p-6 text-center">
        <p className="text-red-800 dark:text-red-200">Failed to load nearby places</p>
      </div>
    );
  }

  if (!nearbyData || nearbyData.total_pois === 0) {
    return (
      <div className="bg-sand-50 dark:bg-primary-800 rounded-lg p-12 text-center">
        <MapPin className="w-16 h-16 text-primary-400 dark:text-sand-500 mx-auto mb-4" />
        <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-2">No places found nearby</h3>
        <p className="text-primary-600 dark:text-sand-400">Try expanding the search radius</p>
      </div>
    );
  }

  const categories = Object.entries(nearbyData.pois_by_category)
    .filter(([_, pois]) => (pois as any[]).length > 0)
    .sort((a, b) => (b[1] as any[]).length - (a[1] as any[]).length);

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex items-center justify-between flex-wrap gap-4">
        <div>
          <h3 className="text-2xl font-bold text-primary-900 dark:text-sand-50">What's Nearby</h3>
          <p className="text-primary-600 dark:text-sand-400 mt-1">
            {nearbyData.total_pois} places within {radiusKm}km
          </p>
        </div>
        <POICategoryFilter
          categories={categories.map(([type]) => type as POIType)}
          selectedCategories={selectedCategories}
          onChange={setSelectedCategories}
        />
      </div>

      {/* Category Sections */}
      <div className="space-y-4">
        {categories.map(([category, pois]) => {
          const poisArray = pois as any[];
          const isExpanded = expandedCategories.includes(category as POIType);
          const displayPois = isExpanded ? poisArray : poisArray.slice(0, 3);
          const hasMore = poisArray.length > 3;

          return (
            <div key={category} className="bg-white dark:bg-primary-800 rounded-xl shadow dark:shadow-primary-900/50 border dark:border-primary-700">
              {/* Category Header */}
              <div className="p-4 border-b bg-sand-50 dark:bg-primary-900/50 rounded-t-xl">
                <div className="flex items-center justify-between">
                  <h4 className="text-lg font-semibold text-primary-900 dark:text-sand-50 capitalize flex items-center gap-2">
                    {category.replace(/_/g, ' ')}
                    <span className="px-2 py-0.5 bg-primary-100 text-primary-700 text-sm font-medium rounded-full">
                      {poisArray.length}
                    </span>
                  </h4>
                  {hasMore && (
                    <button
                      onClick={() => toggleCategory(category as POIType)}
                      className="flex items-center gap-1 text-primary-600 hover:text-primary-700 font-medium text-sm"
                    >
                      {isExpanded ? (
                        <>
                          Show less <ChevronUp className="w-4 h-4" />
                        </>
                      ) : (
                        <>
                          Show all ({poisArray.length}) <ChevronDown className="w-4 h-4" />
                        </>
                      )}
                    </button>
                  )}
                </div>
              </div>

              {/* POIs Grid */}
              <div className="p-4">
                <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
                  {displayPois.map((poi) => (
                    <POICard key={poi.id} poi={poi} />
                  ))}
                </div>
              </div>
            </div>
          );
        })}
      </div>
    </div>
  );
}
