// POI (Points of Interest) API Service
import { apiClient } from './api-client';
import {
  NearbyPOIsResponse,
  DiscoverPOIsRequest,
  DiscoverPOIsResponse,
  POIType,
} from '@/types/poi-types';

export const poiApi = {
  /**
   * Get nearby POIs for a property
   */
  async getNearbyPOIs(
    propertyId: string,
    options?: {
      radiusKm?: number;
      poiTypes?: POIType[];
      recommendedOnly?: boolean;
    }
  ) {
    const params: any = {};
    if (options?.radiusKm) params.radius_km = options.radiusKm;
    if (options?.poiTypes?.length) params.poi_types = options.poiTypes.join(',');
    if (options?.recommendedOnly !== undefined) params.recommended_only = options.recommendedOnly;

    const response = await apiClient.get<NearbyPOIsResponse>(
      `/properties/${propertyId}/nearby_pois/`,
      { params }
    );
    return response.data;
  },

  /**
   * Discover and associate POIs with property (host only)
   */
  async discoverPOIs(propertyId: string, data?: DiscoverPOIsRequest) {
    const response = await apiClient.post<DiscoverPOIsResponse>(
      `/properties/${propertyId}/discover_pois/`,
      data || {}
    );
    return response.data;
  },
};

export default poiApi;
