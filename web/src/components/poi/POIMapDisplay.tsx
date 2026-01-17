'use client';

import { useEffect, useRef, useState } from 'react';
import { MapPin, Navigation } from 'lucide-react';
import { PropertyPOI, POIType } from '@/types/poi-types';

interface POIMapDisplayProps {
  propertyId: string;
  propertyLocation: { latitude: number; longitude: number };
  pois: PropertyPOI[];
  onPOIClick?: (poi: PropertyPOI) => void;
}

export default function POIMapDisplay({
  propertyId: _propertyId,
  propertyLocation: _propertyLocation,
  pois,
  onPOIClick: _onPOIClick,
}: POIMapDisplayProps) {
  const mapRef = useRef<HTMLDivElement>(null);
  const [mapLoaded, setMapLoaded] = useState(false);

  const getCategoryColor = (type: POIType): string => {
    const colors: Record<POIType, string> = {
      restaurant: '#ef4444',
      cafe: '#f59e0b',
      bar: '#8b5cf6',
      attraction: '#3b82f6',
      museum: '#6366f1',
      park: '#10b981',
      beach: '#06b6d4',
      shopping: '#ec4899',
      grocery: '#84cc16',
      pharmacy: '#14b8a6',
      hospital: '#dc2626',
      bus_station: '#f97316',
      train_station: '#f97316',
      airport: '#0ea5e9',
      gas_station: '#78716c',
      other: '#6b7280',
    };
    return colors[type] || '#6b7280';
  };

  useEffect(() => {
    if (typeof window === 'undefined' || mapLoaded) return;

    const loadMapbox = () => {
      const existingScript = document.querySelector('script[src*="mapbox-gl.js"]');
      const existingLink = document.querySelector('link[href*="mapbox-gl.css"]');
      
      if (!existingScript) {
        const script = document.createElement('script');
        script.src = 'https://api.mapbox.com/mapbox-gl-js/v2.15.0/mapbox-gl.js';
        script.async = true;
        script.onload = () => setMapLoaded(true);
        document.head.appendChild(script);
      }

      if (!existingLink) {
        const link = document.createElement('link');
        link.href = 'https://api.mapbox.com/mapbox-gl-js/v2.15.0/mapbox-gl.css';
        link.rel = 'stylesheet';
        document.head.appendChild(link);
      }
    };

    loadMapbox();
  }, [mapLoaded]);

  return (
    <div className="relative">
      {/* Fallback Static Map */}
      <div className="bg-gradient-to-br from-blue-100 to-green-100 rounded-xl overflow-hidden h-96 relative">
        <div ref={mapRef} className="w-full h-full" />
        
        {/* Static representation when Mapbox not loaded */}
        <div className="absolute inset-0 flex items-center justify-center">
          <div className="text-center p-8 bg-white/90 backdrop-blur-sm rounded-xl shadow-lg max-w-md">
            <MapPin className="w-16 h-16 text-primary-600 mx-auto mb-4" />
            <h3 className="text-xl font-bold text-gray-900 mb-2">Location Map</h3>
            <p className="text-gray-600 mb-4">
              {pois.length} nearby points of interest within walking distance
            </p>
            <div className="grid grid-cols-2 gap-3 text-sm">
              <div className="bg-green-50 p-3 rounded-lg">
                <div className="font-semibold text-green-900">
                  {pois.filter(p => p.distance_meters < 1000).length}
                </div>
                <div className="text-green-700">Within 1km</div>
              </div>
              <div className="bg-blue-50 p-3 rounded-lg">
                <div className="font-semibold text-blue-900">
                  {pois.filter(p => p.walking_time_minutes && p.walking_time_minutes < 15).length}
                </div>
                <div className="text-blue-700">15 min walk</div>
              </div>
            </div>
          </div>
        </div>
      </div>

      {/* Legend */}
      <div className="mt-4 p-4 bg-white rounded-lg border">
        <div className="flex items-center gap-2 mb-3">
          <Navigation className="w-5 h-5 text-gray-600" />
          <h4 className="font-semibold text-gray-900">Map Legend</h4>
        </div>
        <div className="grid grid-cols-2 md:grid-cols-4 gap-2">
          {Array.from(new Set(pois.map(p => p.poi.poi_type))).map((type) => (
            <div key={type} className="flex items-center gap-2">
              <div
                className="w-3 h-3 rounded-full"
                style={{ backgroundColor: getCategoryColor(type) }}
              />
              <span className="text-sm text-gray-700 capitalize">
                {type.replace(/_/g, ' ')}
              </span>
            </div>
          ))}
        </div>
      </div>

      {/* Distance Circles Info */}
      <div className="mt-4 grid grid-cols-3 gap-3">
        <div className="bg-blue-50 rounded-lg p-3 text-center">
          <div className="text-2xl font-bold text-blue-900">
            {pois.filter(p => p.distance_meters <= 500).length}
          </div>
          <div className="text-sm text-blue-700">Within 500m</div>
        </div>
        <div className="bg-green-50 rounded-lg p-3 text-center">
          <div className="text-2xl font-bold text-green-900">
            {pois.filter(p => p.distance_meters <= 1000).length}
          </div>
          <div className="text-sm text-green-700">Within 1km</div>
        </div>
        <div className="bg-purple-50 rounded-lg p-3 text-center">
          <div className="text-2xl font-bold text-purple-900">
            {pois.filter(p => p.distance_meters <= 2000).length}
          </div>
          <div className="text-sm text-purple-700">Within 2km</div>
        </div>
      </div>
    </div>
  );
}
