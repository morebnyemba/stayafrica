import { View, Text, FlatList, TouchableOpacity, TextInput } from 'react-native';
import { useNearbyProperties } from '@/hooks/api-hooks';
import { useState, useEffect } from 'react';
import { useUserLocation } from '@/hooks/useUserLocation';
import MapView, { Marker } from 'react-native-maps';

export default function ExploreScreen() {
  const { location, requestPermission } = useUserLocation();
  const [radius, setRadius] = useState('10');
  const { data: properties, isLoading } = useNearbyProperties(
    location?.latitude || 0,
    location?.longitude || 0,
    parseInt(radius) || 10
  );

  useEffect(() => {
    requestPermission();
  }, []);

  return (
    <View className="flex-1 bg-white">
      <View className="px-4 py-4 border-b border-gray-200">
        <Text className="text-2xl font-bold mb-4">Explore Properties</Text>
        <TextInput
          className="w-full px-4 py-2 border border-gray-300 rounded-lg"
          placeholder="Search radius (km)"
          value={radius}
          onChangeText={setRadius}
          keyboardType="numeric"
        />
      </View>

      {location && (
        <MapView
          style={{ height: 300, marginBottom: 12 }}
          initialRegion={{
            latitude: location.latitude,
            longitude: location.longitude,
            latitudeDelta: 0.0922,
            longitudeDelta: 0.0421,
          }}
        >
          {properties?.results?.map((property: any) => (
            <Marker
              key={property.id}
              coordinate={{
                latitude: property.location.latitude,
                longitude: property.location.longitude,
              }}
              title={property.title}
              description={`$${property.price_per_night}/night`}
            />
          ))}
        </MapView>
      )}

      <FlatList
        data={properties?.results}
        keyExtractor={(item) => item.id}
        renderItem={({ item }) => (
          <TouchableOpacity className="px-4 py-3 border-b border-gray-200">
            <Text className="font-semibold text-lg">{item.title}</Text>
            <Text className="text-gray-600 text-sm">{item.location.city}</Text>
            <View className="flex-row justify-between mt-2">
              <Text className="text-primary-600 font-bold">${item.price_per_night}/night</Text>
              <Text className="text-yellow-500">★ {item.rating}</Text>
            </View>
          </TouchableOpacity>
        )}
        ListEmptyComponent={
          !isLoading && (
            <View className="items-center justify-center py-8">
              <Text className="text-gray-600">No properties found</Text>
            </View>
          )
        }
      />
    </View>
  );
}
