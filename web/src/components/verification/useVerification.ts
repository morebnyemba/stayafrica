import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import axios from 'axios';
import { VerificationStatus } from '@/types/verification-types';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8000';

const getAuthHeaders = () => {
  const token = localStorage.getItem('access_token');
  return {
    Authorization: `Bearer ${token}`,
    'Content-Type': 'application/json',
  };
};

export const useVerification = () => {
  const queryClient = useQueryClient();

  const { data: status, isLoading, error } = useQuery<VerificationStatus>({
    queryKey: ['verification-status'],
    queryFn: async () => {
      const response = await axios.get(
        `${API_BASE_URL}/api/v1/users/verification/current_status/`,
        { headers: getAuthHeaders() }
      );
      return response.data;
    },
    refetchInterval: (data) => {
      // Poll every 30 seconds if under review
      return data?.status === 'UNDER_REVIEW' ? 30000 : false;
    },
  });

  const submitMutation = useMutation({
    mutationFn: async (data: {
      document_type: string;
      document_number: string;
      front_image: string;
      back_image?: string;
      selfie_image: string;
      issued_country: string;
      expiry_date?: string;
    }) => {
      const response = await axios.post(
        `${API_BASE_URL}/api/v1/users/verification/`,
        data,
        { headers: getAuthHeaders() }
      );
      return response.data;
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['verification-status'] });
    },
  });

  const updateMutation = useMutation({
    mutationFn: async (data: Partial<VerificationStatus>) => {
      const response = await axios.patch(
        `${API_BASE_URL}/api/v1/users/verification/${status?.id}/`,
        data,
        { headers: getAuthHeaders() }
      );
      return response.data;
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['verification-status'] });
    },
  });

  const submitVerification = async (data: Parameters<typeof submitMutation.mutateAsync>[0]) => {
    return submitMutation.mutateAsync(data);
  };

  const updateVerification = async (data: Partial<VerificationStatus>) => {
    return updateMutation.mutateAsync(data);
  };

  return {
    status,
    isLoading,
    error,
    submitVerification,
    updateVerification,
    isSubmitting: submitMutation.isPending,
    isUpdating: updateMutation.isPending,
  };
};
