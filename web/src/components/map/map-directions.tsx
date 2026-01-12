'use client';

import { useEffect, useRef, useState } from 'react';
import mapboxgl from 'mapbox-gl';
import { Button } from '@/components/ui';
import { Navigation, Loader, AlertCircle } from 'lucide-react';
import 'mapbox-gl/dist/mapbox-gl.css';

interface MapDirectionsProps {
  destination: {
    lat: number;
    lng: number;
    address?: string;
    name?: string;
  };
  className?: string;
}

export function MapDirections({ destination, className = '' }: MapDirectionsProps) {
  const mapContainer = useRef<HTMLDivElement>(null);
  const map = useRef<mapboxgl.Map | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [userLocation, setUserLocation] = useState<{ lat: number; lng: number } | null>(null);
  const [distance, setDistance] = useState<string | null>(null);
  const [duration, setDuration] = useState<string | null>(null);

  const mapboxToken = process.env.NEXT_PUBLIC_MAPBOX_TOKEN;

  useEffect(() => {
    if (!mapboxToken) {
      setError('Map configuration error');
      setLoading(false);
      return;
    }

    if (map.current) return;

    mapboxgl.accessToken = mapboxToken;

    // Get user's current location
    if (navigator.geolocation) {
      navigator.geolocation.getCurrentPosition(
        (position) => {
          const userLoc = {
            lat: position.coords.latitude,
            lng: position.coords.longitude,
          };
          setUserLocation(userLoc);
          initializeMap(userLoc);
        },
        (err) => {
          console.error('Geolocation error:', err);
          // Initialize map centered on destination without route
          initializeMap(null);
        },
        { enableHighAccuracy: true, timeout: 10000 }
      );
    } else {
      initializeMap(null);
    }

    return () => {
      if (map.current) {
        map.current.remove();
        map.current = null;
      }
    };
  }, [mapboxToken]);

  const initializeMap = async (userLoc: { lat: number; lng: number } | null) => {
    if (!mapContainer.current) return;

    const center = userLoc || destination;

    map.current = new mapboxgl.Map({
      container: mapContainer.current,
      style: 'mapbox://styles/mapbox/streets-v12',
      center: [center.lng, center.lat],
      zoom: userLoc ? 10 : 14,
    });

    // Add navigation controls
    const nav = new mapboxgl.NavigationControl({ visualizePitch: true });
    map.current.addControl(nav, 'top-right');

    // Add destination marker
    new mapboxgl.Marker({ color: '#FF6B6B' })
      .setLngLat([destination.lng, destination.lat])
      .setPopup(
        new mapboxgl.Popup().setHTML(
          `<div class="p-2">
            <h3 class="font-semibold">${destination.name || 'Destination'}</h3>
            ${destination.address ? `<p class="text-sm text-gray-600">${destination.address}</p>` : ''}
          </div>`
        )
      )
      .addTo(map.current);

    // Add user location marker if available
    if (userLoc) {
      new mapboxgl.Marker({ color: '#4CAF50' })
        .setLngLat([userLoc.lng, userLoc.lat])
        .setPopup(new mapboxgl.Popup().setHTML('<div class="p-2"><p>Your Location</p></div>'))
        .addTo(map.current!);

      // Fetch and display route
      await fetchRoute(userLoc);
    }

    map.current.on('load', () => {
      setLoading(false);
    });
  };

  const fetchRoute = async (origin: { lat: number; lng: number }) => {
    if (!map.current) return;

    try {
      const response = await fetch(
        `https://api.mapbox.com/directions/v5/mapbox/driving/${origin.lng},${origin.lat};${destination.lng},${destination.lat}?geometries=geojson&access_token=${mapboxToken}`
      );
      const data = await response.json();

      if (data.routes && data.routes.length > 0) {
        const route = data.routes[0];
        
        // Calculate distance and duration
        const distanceKm = (route.distance / 1000).toFixed(1);
        const durationMin = Math.round(route.duration / 60);
        setDistance(`${distanceKm} km`);
        setDuration(`${durationMin} min`);

        // Add route to map
        if (map.current.getSource('route')) {
          (map.current.getSource('route') as mapboxgl.GeoJSONSource).setData({
            type: 'Feature',
            properties: {},
            geometry: route.geometry,
          });
        } else {
          map.current.addSource('route', {
            type: 'geojson',
            data: {
              type: 'Feature',
              properties: {},
              geometry: route.geometry,
            },
          });

          map.current.addLayer({
            id: 'route',
            type: 'line',
            source: 'route',
            layout: {
              'line-join': 'round',
              'line-cap': 'round',
            },
            paint: {
              'line-color': '#3b82f6',
              'line-width': 5,
              'line-opacity': 0.75,
            },
          });
        }

        // Fit map to show entire route
        const bounds = new mapboxgl.LngLatBounds();
        bounds.extend([origin.lng, origin.lat]);
        bounds.extend([destination.lng, destination.lat]);
        map.current.fitBounds(bounds, { padding: 100 });
      }
    } catch (err) {
      console.error('Failed to fetch route:', err);
      setError('Unable to calculate route');
    }
  };

  const handleOpenInMaps = () => {
    // Open in device's default maps app
    const url = `https://www.google.com/maps/dir/?api=1&destination=${destination.lat},${destination.lng}`;
    window.open(url, '_blank');
  };

  if (!mapboxToken) {
    return (
      <div className="bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 rounded-lg p-4 text-red-800 dark:text-red-200">
        <p className="font-medium">Map Configuration Error</p>
        <p className="text-sm">Mapbox token is not configured.</p>
      </div>
    );
  }

  return (
    <div className={`space-y-4 ${className}`}>
      {/* Route Info */}
      {userLocation && (distance || duration) && (
        <div className="card p-4">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-sm text-primary-600 dark:text-sand-400 mb-1">
                Route Information
              </p>
              <div className="flex items-center gap-4">
                {distance && (
                  <div className="flex items-center gap-2">
                    <Navigation className="w-4 h-4 text-secondary-600" />
                    <span className="font-semibold text-primary-900 dark:text-sand-50">
                      {distance}
                    </span>
                  </div>
                )}
                {duration && (
                  <div className="text-primary-700 dark:text-sand-300">
                    ~{duration}
                  </div>
                )}
              </div>
            </div>
            <Button onClick={handleOpenInMaps} size="sm" variant="secondary">
              Open in Maps
            </Button>
          </div>
        </div>
      )}

      {/* Map Container */}
      <div className="relative bg-gray-100 dark:bg-primary-800 rounded-lg overflow-hidden border border-primary-300 dark:border-primary-600">
        {loading && (
          <div className="absolute inset-0 flex items-center justify-center bg-white/80 dark:bg-primary-900/80 z-10">
            <div className="flex flex-col items-center gap-2">
              <Loader className="w-6 h-6 animate-spin text-primary-600" />
              <p className="text-sm text-primary-600 dark:text-sand-400">Loading map...</p>
            </div>
          </div>
        )}
        {error && (
          <div className="absolute inset-0 flex items-center justify-center bg-white/80 dark:bg-primary-900/80 z-10">
            <div className="flex flex-col items-center gap-2 text-red-600">
              <AlertCircle className="w-6 h-6" />
              <p className="text-sm">{error}</p>
            </div>
          </div>
        )}
        <div ref={mapContainer} className="w-full h-96" />
      </div>

      {/* Instructions */}
      <div className="bg-blue-50 dark:bg-blue-900/20 border border-blue-200 dark:border-blue-800 rounded-lg p-4 text-sm text-blue-800 dark:text-blue-200">
        <p className="font-medium mb-2 flex items-center gap-2">
          <Navigation className="w-4 h-4" />
          Getting Directions
        </p>
        <ul className="space-y-1 ml-6 list-disc">
          <li>The route is calculated from your current location to the property</li>
          <li>Click "Open in Maps" to start navigation on your device</li>
          <li>Use the map controls to zoom and pan</li>
        </ul>
      </div>
    </div>
  );
}
