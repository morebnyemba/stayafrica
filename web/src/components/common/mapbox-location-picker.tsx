'use client';

import { useEffect, useRef, useState } from 'react';
import mapboxgl from 'mapbox-gl';
import { Button } from '@/components/ui';
import { MapPin, Loader, Crosshair } from 'lucide-react';

interface MapboxLocationPickerProps {
  onLocationSelect: (data: { lat: number; lng: number; address?: string }) => void;
  initialLat?: number;
  initialLng?: number;
  className?: string;
}

export function MapboxLocationPicker({
  onLocationSelect,
  initialLat = -17.8252,
  initialLng = 31.0335,
  className = ''
}: MapboxLocationPickerProps) {
  const mapContainer = useRef<HTMLDivElement>(null);
  const map = useRef<mapboxgl.Map | null>(null);
  const marker = useRef<mapboxgl.Marker | null>(null);
  const [loading, setLoading] = useState(true);
  const [gettingLocation, setGettingLocation] = useState(false);
  const [pinned, setPinned] = useState(false);
  const [mapStyle, setMapStyle] = useState<'streets' | 'satellite' | 'outdoors'>('streets');
  const [coordinates, setCoordinates] = useState<{ lat: number; lng: number }>({
    lat: initialLat,
    lng: initialLng,
  });

  // Get Mapbox token from environment
  const mapboxToken = process.env.NEXT_PUBLIC_MAPBOX_TOKEN;

  useEffect(() => {
    if (!mapboxToken) {
      console.error('Mapbox token not found. Please set NEXT_PUBLIC_MAPBOX_TOKEN');
      return;
    }

    if (map.current) return;

    mapboxgl.accessToken = mapboxToken;

    const initialStyle = mapStyle === 'satellite'
      ? 'mapbox://styles/mapbox/satellite-streets-v12'
      : `mapbox://styles/mapbox/${mapStyle}-v12`;

    map.current = new mapboxgl.Map({
      container: mapContainer.current!,
      style: initialStyle,
      center: [coordinates.lng, coordinates.lat],
      zoom: 14,
    });

    // Add zoom controls
    const nav = new mapboxgl.NavigationControl({ visualizePitch: true });
    map.current.addControl(nav, 'top-right');

    // Add initial marker
    marker.current = new mapboxgl.Marker({ color: '#FF6B6B', draggable: true })
      .setLngLat([coordinates.lng, coordinates.lat])
      .addTo(map.current);

    // Handle marker drag
    marker.current.on('dragend', () => {
      const lngLat = marker.current?.getLngLat();
      if (lngLat) {
        setCoordinates({
          lat: lngLat.lat,
          lng: lngLat.lng,
        });
      }
    });

    // Click to place marker
    map.current.on('click', (e) => {
      if (marker.current) {
        marker.current.setLngLat([e.lngLat.lng, e.lngLat.lat]);
      }
      setCoordinates({
        lat: e.lngLat.lat,
        lng: e.lngLat.lng,
      });
    });

    setLoading(false);

    return () => {
      if (map.current) {
        map.current.remove();
        map.current = null;
      }
    };
  }, [mapboxToken]);

  // Update map style when it changes
  useEffect(() => {
    if (map.current && mapStyle) {
      const styleUrl = mapStyle === 'satellite' 
        ? 'mapbox://styles/mapbox/satellite-streets-v12'
        : `mapbox://styles/mapbox/${mapStyle}-v12`;
      map.current.setStyle(styleUrl);
    }
  }, [mapStyle]);

  const handleConfirm = async () => {
    try {
      // Try to reverse geocode using Nominatim
      const response = await fetch(
        `https://nominatim.openstreetmap.org/reverse?lat=${coordinates.lat}&lon=${coordinates.lng}&format=json`
      );
      const data = await response.json();
      
      onLocationSelect({
        lat: coordinates.lat,
        lng: coordinates.lng,
        address: data.address?.road || data.display_name || undefined,
      });
      setPinned(true);
    } catch (err) {
      console.error('Reverse geocoding failed:', err);
      // Still return coordinates even if reverse geocode fails
      onLocationSelect({
        lat: coordinates.lat,
        lng: coordinates.lng,
      });
      setPinned(true);
    }
  };

  const handleGetCurrentLocation = () => {
    if (!navigator.geolocation) {
      alert('Geolocation is not supported by your browser');
      return;
    }

    setGettingLocation(true);
    navigator.geolocation.getCurrentPosition(
      (position) => {
        const lat = position.coords.latitude;
        const lng = position.coords.longitude;
        
        // Update coordinates and map
        setCoordinates({ lat, lng });
        
        // Center map on current location
        if (map.current) {
          map.current.flyTo({
            center: [lng, lat],
            zoom: 16,
            duration: 1000,
          });
        }
        
        // Move marker to current location
        if (marker.current) {
          marker.current.setLngLat([lng, lat]);
        }
        
        setGettingLocation(false);
      },
      (error) => {
        console.error('Geolocation error:', error);
        alert('Unable to get your location. Please enable location services.');
        setGettingLocation(false);
      },
      { enableHighAccuracy: true, timeout: 10000 }
    );
  };

  const handleChangeStyle = (style: 'streets' | 'satellite' | 'outdoors') => {
    setMapStyle(style);
  };

  if (!mapboxToken) {
    return (
      <div className="bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 rounded-lg p-4 text-red-800 dark:text-red-200 text-sm">
        <p className="font-medium">Map Configuration Error</p>
        <p>Mapbox token is not configured. Please set NEXT_PUBLIC_MAPBOX_TOKEN environment variable.</p>
      </div>
    );
  }

  return (
    <div className={`space-y-4 ${className}`}>
      <div className="relative bg-gray-100 dark:bg-primary-800 rounded-lg overflow-hidden border border-primary-300 dark:border-primary-600">
        {loading && (
          <div className="absolute inset-0 flex items-center justify-center bg-white/80 dark:bg-primary-900/80 z-10">
            <div className="flex flex-col items-center gap-2">
              <Loader className="w-6 h-6 animate-spin text-primary-600" />
              <p className="text-sm text-primary-600 dark:text-sand-400">Loading map...</p>
            </div>
          </div>
        )}
        <div ref={mapContainer} className="w-full h-96" />
      </div>

      <div className="bg-blue-50 dark:bg-blue-900/20 border border-blue-200 dark:border-blue-800 rounded-lg p-3 sm:p-4 text-xs sm:text-sm text-blue-800 dark:text-blue-200">
        <p className="font-medium mb-2 flex items-center gap-2">
          <MapPin className="w-4 h-4" />
          How to use the map
        </p>
        <ul className="space-y-1 ml-6 list-disc mb-3">
          <li>Click anywhere on the map to place a marker</li>
          <li>Drag the marker to adjust the exact location</li>
          <li>Click "Confirm Location" when satisfied</li>
        </ul>
        <p className="font-medium mb-2">Map Style:</p>
        <div className="flex flex-wrap gap-2">
          {['streets', 'satellite', 'outdoors'].map((style) => (
            <button
              key={style}
              type="button"
              onClick={() => handleChangeStyle(style as 'streets' | 'satellite' | 'outdoors')}
              className={`px-3 py-1 rounded text-xs font-medium transition-colors ${
                mapStyle === style
                  ? 'bg-blue-600 text-white'
                  : 'bg-blue-100 dark:bg-blue-800 text-blue-900 dark:text-blue-100 hover:bg-blue-200 dark:hover:bg-blue-700'
              }`}
            >
              {style.charAt(0).toUpperCase() + style.slice(1)}
            </button>
          ))}
        </div>
      </div>

      <div className="flex flex-col sm:flex-row gap-2 sm:gap-3">
        <div className="flex-1 bg-gray-50 dark:bg-primary-800 rounded-lg p-3 text-xs sm:text-sm">
          <p className="text-primary-600 dark:text-sand-400 font-medium">Coordinates</p>
          <p className="text-primary-900 dark:text-sand-50 font-mono">
            {coordinates.lat.toFixed(6)}, {coordinates.lng.toFixed(6)}
          </p>
        </div>
        <Button
          type="button"
          onClick={handleGetCurrentLocation}
          disabled={gettingLocation || pinned}
          variant="secondary"
          className="w-full sm:w-auto flex items-center justify-center gap-2"
        >
          <Crosshair className="w-4 h-4" />
          {gettingLocation ? 'Locating...' : 'Current Location'}
        </Button>
        <Button
          type="button"
          onClick={handleConfirm}
          disabled={pinned}
          className="w-full sm:w-auto"
        >
          {pinned ? '✓ Location Pinned' : 'Confirm Location'}
        </Button>
      </div>
    </div>
  );
}
