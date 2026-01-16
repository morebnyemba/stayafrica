// POI (Points of Interest) Types

export type POIType = 
  | 'restaurant' 
  | 'cafe' 
  | 'bar' 
  | 'attraction' 
  | 'museum' 
  | 'park' 
  | 'beach' 
  | 'shopping' 
  | 'grocery' 
  | 'pharmacy' 
  | 'hospital' 
  | 'bus_station' 
  | 'train_station' 
  | 'airport' 
  | 'gas_station'
  | 'other';

export interface PointOfInterest {
  id: string;
  name: string;
  poi_type: POIType;
  description?: string;
  address: string;
  latitude: number;
  longitude: number;
  rating?: number;
  price_level?: number; // 1-4 ($, $$, $$$, $$$$)
  phone?: string;
  website?: string;
  opening_hours?: string;
  image_url?: string;
  google_place_id?: string;
}

export interface PropertyPOI {
  id: string;
  poi: PointOfInterest;
  distance_meters: number;
  distance_display: string; // e.g., "250m", "1.5km"
  walking_time_minutes?: number;
  driving_time_minutes?: number;
  is_recommended: boolean;
  host_notes?: string;
}

export interface NearbyPOIsResponse {
  property_id: string;
  radius_km: number;
  total_pois: number;
  pois_by_category: Record<POIType, PropertyPOI[]>;
}

export interface DiscoverPOIsRequest {
  radius_km?: number;
}

export interface DiscoverPOIsResponse {
  message: string;
  count: number;
}
