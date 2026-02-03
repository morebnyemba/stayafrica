import { View, Text, ScrollView, TouchableOpacity, ActivityIndicator, Linking, Platform } from 'react-native';
import { useLocalSearchParams, useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { useSafeAreaInsets } from 'react-native-safe-area-context';
import { useState, useEffect } from 'react';
import * as Location from 'expo-location';

export default function BookingDirectionsScreen() {
  const router = useRouter();
  const { id } = useLocalSearchParams<{ id: string }>();
  const { isAuthenticated } = useAuth();
  const insets = useSafeAreaInsets();
  const [loading, setLoading] = useState(true);
  const [locationPermission, setLocationPermission] = useState<boolean | null>(null);
  const [userLocation, setUserLocation] = useState<Location.LocationObject | null>(null);

  // TODO: Fetch booking details from API
  // For now, using placeholder data
  const booking = {
    id: id,
    property: {
      id: '1',
      title: 'Luxury Safari Lodge',
      city: 'Nairobi',
      country: 'Kenya',
      address: '123 Safari Road, Nairobi, Kenya',
      latitude: -1.2921,
      longitude: 36.8219,
    },
    check_in: '2026-03-01',
    check_out: '2026-03-05',
  };

  useEffect(() => {
    (async () => {
      try {
        const { status } = await Location.requestForegroundPermissionsAsync();
        setLocationPermission(status === 'granted');
        
        if (status === 'granted') {
          const location = await Location.getCurrentPositionAsync({});
          setUserLocation(location);
        }
      } catch (error) {
        console.error('Error getting location:', error);
      } finally {
        setLoading(false);
      }
    })();
  }, []);

  const openInMaps = () => {
    const { latitude, longitude, title } = booking.property;
    const label = encodeURIComponent(title);
    
    let url = '';
    if (Platform.OS === 'ios') {
      url = `maps://app?daddr=${latitude},${longitude}&q=${label}`;
    } else {
      url = `geo:${latitude},${longitude}?q=${latitude},${longitude}(${label})`;
    }
    
    Linking.openURL(url).catch(() => {
      // Fallback to Google Maps web
      const googleMapsUrl = `https://www.google.com/maps/dir/?api=1&destination=${latitude},${longitude}`;
      Linking.openURL(googleMapsUrl);
    });
  };

  const openInGoogleMaps = () => {
    const { latitude, longitude } = booking.property;
    const url = `https://www.google.com/maps/dir/?api=1&destination=${latitude},${longitude}`;
    Linking.openURL(url);
  };

  const calculateDistance = () => {
    if (!userLocation) return 'Unknown';
    
    const { latitude, longitude } = booking.property;
    const userLat = userLocation.coords.latitude;
    const userLng = userLocation.coords.longitude;
    
    // Haversine formula for distance calculation
    const R = 6371; // Radius of the Earth in km
    const dLat = (latitude - userLat) * Math.PI / 180;
    const dLon = (longitude - userLng) * Math.PI / 180;
    const a = 
      Math.sin(dLat/2) * Math.sin(dLat/2) +
      Math.cos(userLat * Math.PI / 180) * Math.cos(latitude * Math.PI / 180) * 
      Math.sin(dLon/2) * Math.sin(dLon/2);
    const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
    const distance = R * c;
    
    return distance < 1 ? `${(distance * 1000).toFixed(0)} m` : `${distance.toFixed(1)} km`;
  };

  if (!isAuthenticated) {
    return (
      <View className="flex-1 bg-sand-100">
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: insets.top + 12 }}
        >
          <View className="flex-row items-center justify-between">
            <TouchableOpacity
              onPress={() => router.back()}
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="arrow-back" size={24} color="#fff" />
            </TouchableOpacity>
            <Text className="text-white font-bold text-lg flex-1 text-center mr-10">Directions</Text>
          </View>
        </LinearGradient>

        <View className="flex-1 items-center justify-center px-6">
          <Text className="text-moss text-center">Please sign in to view directions</Text>
        </View>
      </View>
    );
  }

  if (loading) {
    return (
      <View className="flex-1 bg-sand-100">
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: insets.top + 12 }}
        >
          <View className="flex-row items-center justify-between">
            <TouchableOpacity
              onPress={() => router.back()}
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="arrow-back" size={24} color="#fff" />
            </TouchableOpacity>
            <Text className="text-white font-bold text-lg flex-1 text-center mr-10">Directions</Text>
          </View>
        </LinearGradient>

        <View className="flex-1 items-center justify-center">
          <ActivityIndicator size="large" color="#D9B168" />
          <Text className="text-moss mt-4">Loading location...</Text>
        </View>
      </View>
    );
  }

  return (
    <ScrollView 
      className="flex-1 bg-sand-100" 
      showsVerticalScrollIndicator={false}
    >
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f']}
        className="px-4 pb-6"
        style={{ paddingTop: insets.top + 12 }}
      >
        <View className="flex-row items-center justify-between mb-4">
          <TouchableOpacity
            onPress={() => router.back()}
            className="w-10 h-10 rounded-xl items-center justify-center"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </TouchableOpacity>
          <Text className="text-white font-bold text-lg flex-1 text-center mr-10">Directions</Text>
        </View>
        <Text className="text-sand-200 text-base text-center">
          Get directions to your booking
        </Text>
      </LinearGradient>

      <View className="px-4 py-6">
        {/* Property Info Card */}
        <View
          className="bg-white rounded-2xl p-5 mb-4"
          style={{
            shadowColor: '#122F26',
            shadowOffset: { width: 0, height: 4 },
            shadowOpacity: 0.08,
            shadowRadius: 8,
            elevation: 4,
          }}
        >
          <View className="flex-row items-start mb-4">
            <View className="bg-secondary-100 rounded-full p-3 mr-4">
              <Ionicons name="location" size={24} color="#D9B168" />
            </View>
            <View className="flex-1">
              <Text className="text-lg font-bold text-forest mb-1">
                {booking.property.title}
              </Text>
              <Text className="text-moss mb-2">
                {booking.property.city}, {booking.property.country}
              </Text>
              {booking.property.address && (
                <Text className="text-sm text-moss/70">
                  {booking.property.address}
                </Text>
              )}
            </View>
          </View>

          {/* Booking Dates */}
          <View className="border-t border-sand-200 pt-4 mt-2">
            <View className="flex-row justify-between">
              <View className="flex-1">
                <Text className="text-xs text-moss mb-1">Check-in</Text>
                <Text className="text-sm font-semibold text-forest">
                  {new Date(booking.check_in).toLocaleDateString()}
                </Text>
              </View>
              <View className="flex-1">
                <Text className="text-xs text-moss mb-1">Check-out</Text>
                <Text className="text-sm font-semibold text-forest">
                  {new Date(booking.check_out).toLocaleDateString()}
                </Text>
              </View>
            </View>
          </View>
        </View>

        {/* Distance Info */}
        {locationPermission && userLocation && (
          <View
            className="bg-white rounded-2xl p-5 mb-4 flex-row items-center"
            style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 4 },
              shadowOpacity: 0.08,
              shadowRadius: 8,
              elevation: 4,
            }}
          >
            <View className="bg-blue-100 rounded-full p-3 mr-4">
              <Ionicons name="navigate" size={24} color="#3B82F6" />
            </View>
            <View>
              <Text className="text-sm text-moss mb-1">Distance from you</Text>
              <Text className="text-xl font-bold text-forest">
                {calculateDistance()}
              </Text>
            </View>
          </View>
        )}

        {/* Location Permission Warning */}
        {!locationPermission && (
          <View
            className="bg-yellow-50 rounded-2xl p-5 mb-4 flex-row"
            style={{
              shadowColor: '#F59E0B',
              shadowOffset: { width: 0, height: 2 },
              shadowOpacity: 0.1,
              shadowRadius: 4,
              elevation: 2,
            }}
          >
            <Ionicons name="warning" size={24} color="#F59E0B" className="mr-3" />
            <View className="flex-1 ml-3">
              <Text className="text-sm text-yellow-800 font-semibold mb-1">
                Location Permission Required
              </Text>
              <Text className="text-xs text-yellow-700">
                Enable location services to see distance and get personalized directions.
              </Text>
            </View>
          </View>
        )}

        {/* Navigation Buttons */}
        <View className="space-y-3">
          <TouchableOpacity
            onPress={openInMaps}
            className="mb-3"
          >
            <LinearGradient
              colors={['#D9B168', '#bea04f']}
              className="rounded-2xl p-5 flex-row items-center justify-center"
              style={{
                shadowColor: '#D9B168',
                shadowOffset: { width: 0, height: 4 },
                shadowOpacity: 0.3,
                shadowRadius: 8,
                elevation: 5,
              }}
            >
              <Ionicons name="navigate-circle" size={24} color="#122F26" />
              <Text className="text-forest font-bold text-base ml-3">
                Open in Maps
              </Text>
            </LinearGradient>
          </TouchableOpacity>

          <TouchableOpacity
            onPress={openInGoogleMaps}
            className="bg-white rounded-2xl p-5 flex-row items-center justify-center border-2 border-forest"
            style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 2 },
              shadowOpacity: 0.05,
              shadowRadius: 4,
              elevation: 2,
            }}
          >
            <Ionicons name="globe-outline" size={24} color="#3A5C50" />
            <Text className="text-forest font-bold text-base ml-3">
              Open in Google Maps
            </Text>
          </TouchableOpacity>
        </View>

        {/* Coordinates Info */}
        <View className="mt-6 bg-sand-200 rounded-2xl p-4">
          <Text className="text-xs font-semibold text-moss mb-2">Coordinates</Text>
          <Text className="text-xs text-moss font-mono">
            {booking.property.latitude.toFixed(6)}, {booking.property.longitude.toFixed(6)}
          </Text>
        </View>
      </View>
    </ScrollView>
  );
}
