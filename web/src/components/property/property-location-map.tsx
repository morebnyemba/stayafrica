'use client';

import { useEffect, useRef, useState } from 'react';
import { MapPin, Loader } from 'lucide-react';
import 'mapbox-gl/dist/mapbox-gl.css';

interface PropertyLocationMapProps {
  location?: { type: string; coordinates: number[] } | null;
  city?: string;
  country?: string;
  suburb?: string;
}

export function PropertyLocationMap({ location, city, country, suburb }: PropertyLocationMapProps) {
  const mapContainer = useRef<HTMLDivElement>(null);
  const mapInstance = useRef<any>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(false);

  const mapboxToken = process.env.NEXT_PUBLIC_MAPBOX_TOKEN;

  // Extract lat/lng from GeoJSON Point
  const lng = location?.coordinates?.[0];
  const lat = location?.coordinates?.[1];

  useEffect(() => {
    if (!mapboxToken || !lat || !lng || !mapContainer.current || mapInstance.current) return;

    let mounted = true;

    const initMap = async () => {
      try {
        const mapboxgl = (await import('mapbox-gl')).default;
        if (!mounted || !mapContainer.current) return;

        mapboxgl.accessToken = mapboxToken;

        const map = new mapboxgl.Map({
          container: mapContainer.current,
          style: 'mapbox://styles/mapbox/streets-v12',
          center: [lng, lat],
          zoom: 14,
          interactive: true,
          scrollZoom: false,
        });

        map.addControl(new mapboxgl.NavigationControl({ showCompass: false }), 'top-right');

        map.on('load', () => {
          if (!mounted) return;

          // Add approximate area circle (Airbnb style — shows general area, not exact pin)
          map.addSource('area-circle', {
            type: 'geojson',
            data: {
              type: 'Feature',
              properties: {},
              geometry: {
                type: 'Point',
                coordinates: [lng, lat],
              },
            },
          });

          map.addLayer({
            id: 'area-circle-fill',
            type: 'circle',
            source: 'area-circle',
            paint: {
              'circle-radius': 80,
              'circle-color': 'rgba(212, 175, 55, 0.15)',
              'circle-stroke-width': 1.5,
              'circle-stroke-color': 'rgba(212, 175, 55, 0.5)',
            },
          });

          // Property pin marker
          const el = document.createElement('div');
          el.className = 'property-map-pin';
          el.innerHTML = `<div style="
            width: 32px; height: 32px;
            background: #1a3c34;
            border-radius: 50% 50% 50% 0;
            transform: rotate(-45deg);
            display: flex; align-items: center; justify-content: center;
            box-shadow: 0 2px 8px rgba(0,0,0,0.3);
          "><svg style="transform: rotate(45deg); width:16px; height:16px; color:white;" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M3 9l9-7 9 7v11a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2z"></path><polyline points="9 22 9 12 15 12 15 22"></polyline></svg></div>`;

          new mapboxgl.Marker(el).setLngLat([lng, lat]).addTo(map);

          setLoading(false);
        });

        mapInstance.current = map;
      } catch {
        if (mounted) setError(true);
        setLoading(false);
      }
    };

    initMap();

    return () => {
      mounted = false;
      mapInstance.current?.remove();
      mapInstance.current = null;
    };
  }, [mapboxToken, lat, lng]);

  if (!lat || !lng) {
    return (
      <div>
        <h2 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-4">
          Where you&apos;ll be
        </h2>
        <div className="flex items-center gap-2 text-primary-700 dark:text-sand-200">
          <MapPin className="w-5 h-5" />
          <span>{[suburb, city, country].filter(Boolean).join(', ') || 'Location details coming soon'}</span>
        </div>
      </div>
    );
  }

  if (!mapboxToken || error) {
    // Fallback: static location info without map
    return (
      <div>
        <h2 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-4">
          Where you&apos;ll be
        </h2>
        <div className="flex items-center gap-2 text-primary-700 dark:text-sand-200">
          <MapPin className="w-5 h-5" />
          <span>{[suburb, city, country].filter(Boolean).join(', ')}</span>
        </div>
      </div>
    );
  }

  return (
    <div>
      <h2 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-4">
        Where you&apos;ll be
      </h2>
      <div className="relative rounded-xl overflow-hidden bg-sand-100 dark:bg-primary-800" style={{ height: 300 }}>
        {loading && (
          <div className="absolute inset-0 flex items-center justify-center bg-sand-100 dark:bg-primary-800 z-10">
            <Loader className="w-6 h-6 animate-spin text-primary-600" />
          </div>
        )}
        <div ref={mapContainer} className="w-full h-full" />
      </div>
      <p className="mt-3 text-[15px] font-medium text-primary-900 dark:text-sand-50">
        {[city, country].filter(Boolean).join(', ')}
      </p>
      {suburb && (
        <p className="text-sm text-primary-600 dark:text-sand-400 mt-1">
          {suburb}
        </p>
      )}
    </div>
  );
}
