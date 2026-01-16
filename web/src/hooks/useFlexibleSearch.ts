import { useQuery } from '@tanstack/react-query';
import apiClient from '@/services/api-client';
import { FlexibilityType } from '@/components/search/FlexibleDateSearchPanel';

interface FlexibleSearchParams {
  location?: string;
  checkIn: string;
  checkOut: string;
  flexibility: FlexibilityType;
  days?: number;
  guests?: number;
}

interface DateOption {
  check_in: string;
  check_out: string;
  properties_count: number;
  price_range: {
    min: number;
    max: number;
    avg: number;
  };
  sample_properties: Array<{
    id: string;
    name: string;
    image_url: string;
    location: string;
    price: number;
    rating?: number;
  }>;
}

interface FlexibleSearchResponse {
  flexibility_type: string;
  original_dates: {
    check_in: string;
    check_out: string;
  };
  date_options: DateOption[];
  total_properties: number;
}

export function useFlexibleSearch(params: FlexibleSearchParams) {
  return useQuery({
    queryKey: ['flexible-search', params],
    queryFn: async () => {
      const queryParams: any = {
        check_in: params.checkIn,
        check_out: params.checkOut,
        flexibility: params.flexibility,
      };

      if (params.location) queryParams.location = params.location;
      if (params.days) queryParams.days = params.days;
      if (params.guests) queryParams.guests = params.guests;

      const response = await apiClient.get<FlexibleSearchResponse>(
        '/properties/flexible_search/',
        { params: queryParams }
      );
      return response.data;
    },
    enabled: !!params.checkIn && !!params.checkOut,
  });
}
