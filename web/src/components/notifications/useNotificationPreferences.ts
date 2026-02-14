import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import axios from 'axios';
import { NotificationPreferences } from '@/types/notification-types';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8000';

const getAuthHeaders = () => {
  const token = localStorage.getItem('access_token');
  return {
    Authorization: `Bearer ${token}`,
    'Content-Type': 'application/json',
  };
};

export const useNotificationPreferences = () => {
  const queryClient = useQueryClient();

  const { data: preferences, isLoading, error } = useQuery<NotificationPreferences>({
    queryKey: ['notification-preferences'],
    queryFn: async () => {
      const response = await axios.get(
        `${API_BASE_URL}/api/v1/preferences/`,
        { headers: getAuthHeaders() }
      );
      return response.data;
    },
  });

  const updateMutation = useMutation({
    mutationFn: async (data: Partial<NotificationPreferences>) => {
      const response = await axios.put(
        `${API_BASE_URL}/api/v1/preferences/`,
        data,
        { headers: getAuthHeaders() }
      );
      return response.data;
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['notification-preferences'] });
    },
  });

  const updatePreferences = async (data: Partial<NotificationPreferences>) => {
    return updateMutation.mutateAsync(data);
  };

  return {
    preferences,
    isLoading,
    error,
    updatePreferences,
    isUpdating: updateMutation.isPending,
  };
};
