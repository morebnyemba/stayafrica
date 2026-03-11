'use client';

import { useEffect, useRef, useState } from 'react';
import { MapPin, Loader } from 'lucide-react';
import 'mapbox-gl/dist/mapbox-gl.css';

interface PropertyLocationMapProps {
  // Accepts multiple formats: GeoJSON, [lng,lat] array, {lat,lng} dict, or WKT string
  location?: { type?: string; coordinates?: number[] } | number[] | { lat?: number; lng?: number; latitude?: number; longitude?: number } | string | null;
  city?: string;
  country?: string;
  suburb?: string;
}

function extractCoords(location: PropertyLocationMapProps['location']): { lat: number; lng: number } | null {
  if (!location) return null;

  // Array format: [lng, lat] (GeoJSON order from DRF PointField serialization)
  if (Array.isArray(location) && location.length >= 2) {
    const [lng, lat] = location;
    if (typeof lng === 'number' && typeof lat === 'number') return { lat, lng };
  }

  if (typeof location === 'object' && !Array.isArray(location)) {
    // GeoJSON: { type: "Point", coordinates: [lng, lat] }
    const geo = location as { type?: string; coordinates?: number[] };
    if (geo.coordinates && Array.isArray(geo.coordinates) && geo.coordinates.length >= 2) {
      return { lng: geo.coordinates[0], lat: geo.coordinates[1] };
    }
    // Dict: { lat, lng } or { latitude, longitude }
    const dict = location as { lat?: number; lng?: number; latitude?: number; longitude?: number };
    const lat = dict.lat ?? dict.latitude;
    const lng = dict.lng ?? dict.longitude;
    if (typeof lat === 'number' && typeof lng === 'number') return { lat, lng };
  }

  // WKT string: "POINT (lng lat)" or "SRID=4326;POINT (lng lat)"
  if (typeof location === 'string') {
    const match = location.match(/POINT\s*\(\s*([-\d.]+)\s+([-\d.]+)\s*\)/i);
    if (match) return { lng: parseFloat(match[1]), lat: parseFloat(match[2]) };
  }

  return null;
}

export function PropertyLocationMap({ location, city, country, suburb }: PropertyLocationMapProps) {
  const mapContainer = useRef<HTMLDivElement>(null);
  const mapInstance = useRef<any>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(false);

  const mapboxToken = process.env.NEXT_PUBLIC_MAPBOX_TOKEN;

  const coords = extractCoords(location);
  const lat = coords?.lat;
  const lng = coords?.lng;

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
        <h2 className="text-xl font-semibold text-primary-900 mb-4">
          Where you&apos;ll be
        </h2>
        <div className="flex items-center gap-2 text-primary-700">
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
        <h2 className="text-xl font-semibold text-primary-900 mb-4">
          Where you&apos;ll be
        </h2>
        <div className="flex items-center gap-2 text-primary-700">
          <MapPin className="w-5 h-5" />
          <span>{[suburb, city, country].filter(Boolean).join(', ')}</span>
        </div>
      </div>
    );
  }

  return (
    <div>
      <h2 className="text-xl font-semibold text-primary-900 mb-4">
        Where you&apos;ll be
      </h2>
      <div className="relative rounded-xl overflow-hidden bg-sand-100" style={{ height: 300 }}>
        {loading && (
          <div className="absolute inset-0 flex items-center justify-center bg-sand-100 z-10">
            <Loader className="w-6 h-6 animate-spin text-primary-600" />
          </div>
        )}
        <div ref={mapContainer} className="w-full h-full" />
      </div>
      <p className="mt-3 text-[15px] font-medium text-primary-900">
        {[city, country].filter(Boolean).join(', ')}
      </p>
      {suburb && (
        <p className="text-sm text-primary-600 mt-1">
          {suburb}
        </p>
      )}
    </div>
  );
}
