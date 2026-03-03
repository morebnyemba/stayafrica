'use client';

import {
  Filter,
  UtensilsCrossed,
  Coffee,
  Beer,
  FerrisWheel,
  Landmark,
  TreePine,
  Umbrella,
  ShoppingBag,
  ShoppingCart,
  Pill,
  Hospital,
  Bus,
  Train,
  Plane,
  Fuel,
  MapPin,
} from 'lucide-react';
import type { ReactNode } from 'react';
import { POIType } from '@/types/poi-types';

interface POICategoryFilterProps {
  categories: POIType[];
  selectedCategories: POIType[];
  onChange: (categories: POIType[]) => void;
}

const categoryIcons: Record<POIType, ReactNode> = {
  restaurant: <UtensilsCrossed className="h-4 w-4" />,
  cafe: <Coffee className="h-4 w-4" />,
  bar: <Beer className="h-4 w-4" />,
  attraction: <FerrisWheel className="h-4 w-4" />,
  museum: <Landmark className="h-4 w-4" />,
  park: <TreePine className="h-4 w-4" />,
  beach: <Umbrella className="h-4 w-4" />,
  shopping: <ShoppingBag className="h-4 w-4" />,
  grocery: <ShoppingCart className="h-4 w-4" />,
  pharmacy: <Pill className="h-4 w-4" />,
  hospital: <Hospital className="h-4 w-4" />,
  bus_station: <Bus className="h-4 w-4" />,
  train_station: <Train className="h-4 w-4" />,
  airport: <Plane className="h-4 w-4" />,
  gas_station: <Fuel className="h-4 w-4" />,
  other: <MapPin className="h-4 w-4" />,
};

const categoryLabels: Record<POIType, string> = {
  restaurant: 'Restaurants',
  cafe: 'Cafés',
  bar: 'Bars',
  attraction: 'Attractions',
  museum: 'Museums',
  park: 'Parks',
  beach: 'Beaches',
  shopping: 'Shopping',
  grocery: 'Groceries',
  pharmacy: 'Pharmacies',
  hospital: 'Hospitals',
  bus_station: 'Bus Stations',
  train_station: 'Train Stations',
  airport: 'Airports',
  gas_station: 'Gas Stations',
  other: 'Other',
};

export default function POICategoryFilter({
  categories,
  selectedCategories,
  onChange,
}: POICategoryFilterProps) {
  const toggleCategory = (category: POIType) => {
    if (selectedCategories.includes(category)) {
      onChange(selectedCategories.filter(c => c !== category));
    } else {
      onChange([...selectedCategories, category]);
    }
  };

  const clearAll = () => {
    onChange([]);
  };

  return (
    <div className="bg-white dark:bg-primary-800 rounded-lg border dark:border-primary-700 p-4">
      <div className="flex items-center justify-between mb-3">
        <div className="flex items-center gap-2">
          <Filter className="w-5 h-5 text-primary-600 dark:text-sand-400" />
          <span className="font-semibold text-primary-900 dark:text-sand-50">Filter by category</span>
        </div>
        {selectedCategories.length > 0 && (
          <button
            onClick={clearAll}
            className="text-sm text-primary-600 hover:text-primary-700 font-medium"
          >
            Clear all
          </button>
        )}
      </div>

      <div className="flex flex-wrap gap-2">
        {categories.map((category) => {
          const isSelected = selectedCategories.includes(category);
          return (
            <button
              key={category}
              onClick={() => toggleCategory(category)}
              className={`
                px-3 py-1.5 rounded-full text-sm font-medium transition
                ${isSelected
                  ? 'bg-primary-600 text-white'
                  : 'bg-sand-100 dark:bg-primary-700 text-primary-700 dark:text-sand-300 hover:bg-sand-200 dark:hover:bg-primary-600'
                }
              `}
            >
              <span className="flex items-center gap-1.5">{categoryIcons[category]} {categoryLabels[category] || category}</span>
            </button>
          );
        })}
      </div>

      {selectedCategories.length > 0 && (
        <p className="text-xs text-primary-600 dark:text-sand-400 mt-3">
          Showing {selectedCategories.length} {selectedCategories.length === 1 ? 'category' : 'categories'}
        </p>
      )}
    </div>
  );
}
