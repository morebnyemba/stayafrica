'use client';

import {
  Wifi,
  Tv,
  Wind,
  Waves,
  UtensilsCrossed,
  Dumbbell,
  ParkingCircle,
  Flame,
  Trash2,
  Sofa,
  Home,
  Bell,
  type LucideIcon,
} from 'lucide-react';

interface PropertyAmenitiesProps {
  amenities: Array<{ id: string; name: string }>;
}

const amenityIcons: Record<string, LucideIcon> = {
  wifi: Wifi,
  tv: Tv,
  ac: Wind,
  pool: Waves,
  kitchen: UtensilsCrossed,
  gym: Dumbbell,
  parking: ParkingCircle,
  heating: Flame,
  washer: Trash2,
  sofa: Sofa,
  garden: Home,
  doorbell: Bell,
};

export function PropertyAmenities({ amenities }: PropertyAmenitiesProps) {
  if (!amenities || amenities.length === 0) {
    return null;
  }

  return (
    <div>
      <h2 className="text-2xl font-semibold text-primary-900 dark:text-sand-50 mb-6">
        Amenities
      </h2>
      <div className="grid grid-cols-2 sm:grid-cols-3 md:grid-cols-4 gap-4">
        {amenities.map((amenity) => {
          const IconComponent =
            amenityIcons[amenity.name.toLowerCase().replace(/\s+/g, '')] || Home;

          return (
            <div
              key={amenity.id}
              className="flex items-center space-x-3 p-4 bg-white dark:bg-primary-800 rounded-lg border border-primary-200 dark:border-primary-700 hover:border-secondary-300 dark:hover:border-secondary-500 transition"
            >
              <IconComponent className="w-6 h-6 text-secondary-600 dark:text-secondary-400 flex-shrink-0" />
              <span className="text-primary-900 dark:text-sand-100 font-medium">
                {amenity.name}
              </span>
            </div>
          );
        })}
      </div>
    </div>
  );
}
