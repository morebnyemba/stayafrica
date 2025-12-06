import { useEffect, useState } from 'react';
import * as Location from 'expo-location';

export function useUserLocation() {
  const [location, setLocation] = useState<any>(null);
  const [error, setError] = useState<string | null>(null);

  const requestPermission = async () => {
    try {
      const { status } = await Location.requestForegroundPermissionsAsync();
      if (status !== 'granted') {
        setError('Permission to access location was denied');
        return;
      }

      const currentLocation = await Location.getCurrentPositionAsync({
        accuracy: Location.Accuracy.Balanced,
      });

      setLocation({
        latitude: currentLocation.coords.latitude,
        longitude: currentLocation.coords.longitude,
        latitudeDelta: 0.0922,
        longitudeDelta: 0.0421,
      });
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Error getting location');
    }
  };

  return { location, error, requestPermission };
}
