'use client';

import { useState } from 'react';
import { MapPin, Loader2, Navigation } from 'lucide-react';
import { Button } from '@/components/ui/Button';
import { toast } from 'react-hot-toast';

interface LocationPickerProps {
  onLocationSelect?: (location: { lat: number; lng: number; address?: string }) => void;
  showMap?: boolean;
  className?: string;
}

export function LocationPicker({ onLocationSelect, showMap = false, className = '' }: LocationPickerProps) {
  const [loading, setLoading] = useState(false);
  const [location, setLocation] = useState<{ lat: number; lng: number; address?: string } | null>(null);
  const [error, setError] = useState<string | null>(null);

  const getCurrentLocation = () => {
    if (!navigator.geolocation) {
      const errorMsg = 'Geolocation is not supported by your browser';
      setError(errorMsg);
      toast.error(errorMsg);
      return;
    }

    setLoading(true);
    setError(null);

    navigator.geolocation.getCurrentPosition(
      async (position) => {
        const { latitude, longitude } = position.coords;
        
        try {
          // Reverse geocoding to get address (using OpenStreetMap Nominatim API - free)
          const response = await fetch(
            `https://nominatim.openstreetmap.org/reverse?format=json&lat=${latitude}&lon=${longitude}`
          );
          const data = await response.json();
          
          const locationData = {
            lat: latitude,
            lng: longitude,
            address: data.display_name || `${latitude.toFixed(4)}, ${longitude.toFixed(4)}`,
          };

          setLocation(locationData);
          onLocationSelect?.(locationData);
          toast.success('Location detected successfully');
        } catch (err) {
          // Fallback if reverse geocoding fails
          const locationData = {
            lat: latitude,
            lng: longitude,
            address: `${latitude.toFixed(4)}, ${longitude.toFixed(4)}`,
          };
          
          setLocation(locationData);
          onLocationSelect?.(locationData);
          toast.success('Location detected');
        }
        
        setLoading(false);
      },
      (err) => {
        let errorMsg = 'Unable to retrieve your location';
        
        switch (err.code) {
          case err.PERMISSION_DENIED:
            errorMsg = 'Location access denied. Please enable location permissions.';
            break;
          case err.POSITION_UNAVAILABLE:
            errorMsg = 'Location information unavailable';
            break;
          case err.TIMEOUT:
            errorMsg = 'Location request timed out';
            break;
        }
        
        setError(errorMsg);
        toast.error(errorMsg);
        setLoading(false);
      },
      {
        enableHighAccuracy: true,
        timeout: 10000,
        maximumAge: 0,
      }
    );
  };

  return (
    <div className={className}>
      <Button
        type="button"
        variant="secondary"
        size="sm"
        onClick={getCurrentLocation}
        disabled={loading}
        isLoading={loading}
        className="w-full sm:w-auto"
      >
        {loading ? (
          <>
            <Loader2 className="w-4 h-4 mr-2 animate-spin" />
            Getting location...
          </>
        ) : (
          <>
            <Navigation className="w-4 h-4 mr-2" />
            Use My Location
          </>
        )}
      </Button>

      {error && (
        <div className="mt-2 text-sm text-error-500 flex items-start gap-2">
          <MapPin className="w-4 h-4 mt-0.5 flex-shrink-0" />
          <span>{error}</span>
        </div>
      )}

      {location && !error && (
        <div className="mt-3 p-3 bg-success-50 dark:bg-success-900/20 rounded-lg border border-success-200 dark:border-success-800">
          <div className="flex items-start gap-2 text-sm">
            <MapPin className="w-4 h-4 mt-0.5 text-success-600 dark:text-success-400 flex-shrink-0" />
            <div className="flex-1">
              <p className="font-medium text-success-900 dark:text-success-100 mb-1">
                Location detected
              </p>
              <p className="text-success-700 dark:text-success-300 text-xs break-words">
                {location.address}
              </p>
              <p className="text-success-600 dark:text-success-400 text-xs mt-1">
                {location.lat.toFixed(6)}, {location.lng.toFixed(6)}
              </p>
            </div>
          </div>
          
          {showMap && (
            <div className="mt-3 rounded-lg overflow-hidden border border-success-200 dark:border-success-800">
              <iframe
                width="100%"
                height="200"
                frameBorder="0"
                scrolling="no"
                marginHeight={0}
                marginWidth={0}
                src={`https://www.openstreetmap.org/export/embed.html?bbox=${location.lng - 0.01},${location.lat - 0.01},${location.lng + 0.01},${location.lat + 0.01}&layer=mapnik&marker=${location.lat},${location.lng}`}
                style={{ border: 0 }}
                title="Location Map"
              />
              <div className="bg-white dark:bg-primary-800 px-3 py-2">
                <a
                  href={`https://www.openstreetmap.org/?mlat=${location.lat}&mlon=${location.lng}#map=15/${location.lat}/${location.lng}`}
                  target="_blank"
                  rel="noopener noreferrer"
                  className="text-xs text-primary-600 dark:text-secondary-400 hover:underline"
                >
                  View on OpenStreetMap →
                </a>
              </div>
            </div>
          )}
        </div>
      )}
    </div>
  );
}
