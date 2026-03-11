'use client';

import { useState } from 'react';
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
  X,
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
  const [showAll, setShowAll] = useState(false);

  if (!amenities || amenities.length === 0) {
    return null;
  }

  const displayedAmenities = amenities.slice(0, 10);
  const hasMore = amenities.length > 10;

  const AmenityItem = ({ amenity }: { amenity: { id: string; name: string } }) => {
    const IconComponent =
      amenityIcons[amenity.name.toLowerCase().replace(/\s+/g, '')] || Home;
    return (
      <div className="flex items-center gap-4 py-3">
        <IconComponent className="w-6 h-6 text-primary-700 flex-shrink-0" />
        <span className="text-[15px] text-primary-900">
          {amenity.name}
        </span>
      </div>
    );
  };

  return (
    <div>
      <h2 className="text-xl font-semibold text-primary-900 mb-4">
        What this place offers
      </h2>
      <div className="grid grid-cols-1 sm:grid-cols-2 gap-x-8">
        {displayedAmenities.map((amenity) => (
          <AmenityItem key={amenity.id} amenity={amenity} />
        ))}
      </div>
      {hasMore && (
        <button
          onClick={() => setShowAll(true)}
          className="mt-4 px-6 py-3 border border-primary-900 rounded-lg text-primary-900 font-semibold text-sm hover:bg-sand-50 transition"
        >
          Show all {amenities.length} amenities
        </button>
      )}

      {/* All Amenities Modal */}
      {showAll && (
        <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/50">
          <div className="bg-white rounded-xl w-full max-w-lg mx-4 max-h-[80vh] flex flex-col shadow-2xl">
            <div className="flex items-center justify-between px-6 py-4 border-b border-primary-200">
              <h3 className="text-lg font-semibold text-primary-900">
                What this place offers
              </h3>
              <button
                onClick={() => setShowAll(false)}
                className="p-1 rounded-full hover:bg-sand-100 transition"
                aria-label="Close"
              >
                <X className="w-5 h-5 text-primary-900" />
              </button>
            </div>
            <div className="flex-1 overflow-y-auto px-6 py-4">
              {amenities.map((amenity) => (
                <AmenityItem key={amenity.id} amenity={amenity} />
              ))}
            </div>
          </div>
        </div>
      )}
    </div>
  );
}
