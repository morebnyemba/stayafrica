'use client';

import { MapPin, Star, DollarSign, Clock, Phone, Globe, ThumbsUp, Navigation } from 'lucide-react';
import { PropertyPOI } from '@/types/poi-types';

interface POICardProps {
  poi: PropertyPOI;
}

export default function POICard({ poi }: POICardProps) {
  const { poi: poiData, distance_display, walking_time_minutes, is_recommended, host_notes } = poi;

  const getPriceLevelDisplay = (level?: number) => {
    if (!level) return null;
    return '$'.repeat(level);
  };

  const openInMaps = () => {
    const url = `https://www.google.com/maps/search/?api=1&query=${poiData.latitude},${poiData.longitude}`;
    window.open(url, '_blank');
  };

  return (
    <div className={`
      bg-white rounded-lg border-2 p-4 hover:shadow-md transition
      ${is_recommended ? 'border-green-400 bg-green-50/30' : 'border-gray-200'}
    `}>
      {/* Header */}
      <div className="flex items-start justify-between mb-3">
        <div className="flex-1 min-w-0">
          <h5 className="font-semibold text-gray-900 line-clamp-1">{poiData.name}</h5>
          <p className="text-sm text-gray-600 capitalize mt-0.5">
            {poiData.poi_type.replace(/_/g, ' ')}
          </p>
        </div>
        {is_recommended && (
          <div className="flex items-center gap-1 px-2 py-1 bg-green-100 text-green-700 rounded-full text-xs font-medium ml-2 whitespace-nowrap">
            <ThumbsUp className="w-3 h-3" />
            Recommended
          </div>
        )}
      </div>

      {/* Rating & Price */}
      {(poiData.rating || poiData.price_level) && (
        <div className="flex items-center gap-3 mb-3">
          {poiData.rating && (
            <div className="flex items-center gap-1 text-sm">
              <Star className="w-4 h-4 fill-yellow-400 text-yellow-400" />
              <span className="font-medium text-gray-900">{poiData.rating.toFixed(1)}</span>
            </div>
          )}
          {poiData.price_level && (
            <div className="flex items-center gap-1 text-sm text-gray-600">
              <DollarSign className="w-4 h-4" />
              <span>{getPriceLevelDisplay(poiData.price_level)}</span>
            </div>
          )}
        </div>
      )}

      {/* Distance Info */}
      <div className="space-y-2 mb-3">
        <div className="flex items-center gap-2 text-sm text-gray-700">
          <MapPin className="w-4 h-4 text-primary-600" />
          <span className="font-medium">{distance_display}</span>
        </div>
        {walking_time_minutes && (
          <div className="flex items-center gap-2 text-sm text-gray-600">
            <Clock className="w-4 h-4" />
            <span>{walking_time_minutes} min walk</span>
          </div>
        )}
      </div>

      {/* Host Notes */}
      {host_notes && (
        <div className="bg-blue-50 border border-blue-200 rounded-lg p-3 mb-3">
          <p className="text-sm text-blue-900 italic">"{host_notes}"</p>
        </div>
      )}

      {/* Address */}
      <p className="text-xs text-gray-600 mb-3 line-clamp-2">{poiData.address}</p>

      {/* Actions */}
      <div className="flex gap-2">
        <button
          onClick={openInMaps}
          className="flex-1 flex items-center justify-center gap-1 px-3 py-2 bg-primary-600 text-white text-sm font-medium rounded-lg hover:bg-primary-700 transition"
        >
          <Navigation className="w-4 h-4" />
          Directions
        </button>
        {poiData.website && (
          <a
            href={poiData.website}
            target="_blank"
            rel="noopener noreferrer"
            className="p-2 border border-gray-300 rounded-lg hover:bg-gray-50 transition"
            title="Visit website"
          >
            <Globe className="w-4 h-4 text-gray-600" />
          </a>
        )}
        {poiData.phone && (
          <a
            href={`tel:${poiData.phone}`}
            className="p-2 border border-gray-300 rounded-lg hover:bg-gray-50 transition"
            title="Call"
          >
            <Phone className="w-4 h-4 text-gray-600" />
          </a>
        )}
      </div>
    </div>
  );
}
