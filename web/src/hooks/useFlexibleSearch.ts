import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { FlexibilityType } from '@/components/search/FlexibleDateSearchPanel';

interface FlexibleSearchParams {
  location?: string;
  checkIn: string;
  checkOut: string;
  flexibility: FlexibilityType;
  days?: number;
  guests?: number;
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

      const response = await apiClient.get(
        '/properties/flexible_search/',
        { params: queryParams }
      );
      return response.data;
    },
    enabled: !!params.checkIn && !!params.checkOut,
  });
}
