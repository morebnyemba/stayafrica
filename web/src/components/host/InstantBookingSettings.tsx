'use client';

import React, { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { Zap, Shield, Star, Home, CreditCard, Loader2, Save } from 'lucide-react';
import { apiClient } from '@/services/api-client';

interface InstantBookingSettings {
  enabled: boolean;
  require_id_verification: boolean;
  min_reviews: number;
  min_rating: number;
  require_completed_booking: boolean;
  require_payment_method: boolean;
}

interface InstantBookingSettingsProps {
  propertyId: string;
}

export default function InstantBookingSettings({ propertyId }: InstantBookingSettingsProps) {
  const queryClient = useQueryClient();

  const { data: settings, isLoading } = useQuery({
    queryKey: ['instant-booking-settings', propertyId],
    queryFn: async () => {
      const response = await apiClient.get(
        `/properties/${propertyId}/instant_booking_info/`
      );
      return response.data;
    },
  });

  const [formData, setFormData] = useState<InstantBookingSettings>(
    settings || {
      enabled: false,
      require_id_verification: true,
      min_reviews: 0,
      min_rating: 0,
      require_completed_booking: false,
      require_payment_method: true,
    }
  );

  React.useEffect(() => {
    if (settings) {
      setFormData(settings);
    }
  }, [settings]);

  const toggleMutation = useMutation({
    mutationFn: async () => {
      const response = await apiClient.post(
        `/properties/${propertyId}/toggle_instant_booking/`
      );
      return response.data;
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['instant-booking-settings', propertyId] });
    },
  });

  const updateMutation = useMutation({
    mutationFn: async (data: Partial<InstantBookingSettings>) => {
      const response = await apiClient.post(
        `/properties/${propertyId}/instant_booking_info/`,
        data
      );
      return response.data;
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['instant-booking-settings', propertyId] });
    },
  });

  const handleToggle = () => {
    toggleMutation.mutate();
  };

  const handleSave = () => {
    updateMutation.mutate(formData);
  };

  if (isLoading) {
    return (
      <div className="bg-white rounded-xl shadow p-6">
        <div className="animate-pulse space-y-4">
          <div className="h-6 bg-gray-200 rounded w-1/3"></div>
          <div className="h-32 bg-gray-200 rounded"></div>
        </div>
      </div>
    );
  }

  return (
    <div className="bg-white rounded-xl shadow">
      {/* Header */}
      <div className="p-6 border-b">
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-3">
            <div className="p-2 bg-gradient-to-r from-yellow-400 to-orange-500 rounded-lg">
              <Zap className="w-6 h-6 text-white" />
            </div>
            <div>
              <h3 className="text-xl font-bold text-gray-900">Instant Booking</h3>
              <p className="text-sm text-gray-600">Let guests book without waiting for approval</p>
            </div>
          </div>
          <button
            onClick={handleToggle}
            disabled={toggleMutation.isPending}
            className={`
              relative inline-flex h-8 w-14 items-center rounded-full transition-colors
              ${formData.enabled ? 'bg-green-600' : 'bg-gray-300'}
              ${toggleMutation.isPending ? 'opacity-50 cursor-not-allowed' : 'cursor-pointer'}
            `}
          >
            <span
              className={`
                inline-block h-6 w-6 transform rounded-full bg-white transition-transform
                ${formData.enabled ? 'translate-x-7' : 'translate-x-1'}
              `}
            />
          </button>
        </div>
      </div>

      {/* Settings */}
      {formData.enabled && (
        <div className="p-6 space-y-6">
          <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
            <p className="text-sm text-blue-900">
              Set requirements guests must meet to book your property instantly. Higher requirements help ensure quality bookings.
            </p>
          </div>

          {/* ID Verification */}
          <label className="flex items-start gap-3 p-4 border-2 rounded-lg cursor-pointer hover:bg-gray-50 transition">
            <input
              type="checkbox"
              checked={formData.require_id_verification}
              onChange={(e) => setFormData({ ...formData, require_id_verification: e.target.checked })}
              className="mt-1 w-5 h-5 text-primary-600"
            />
            <div className="flex-1">
              <div className="flex items-center gap-2 font-medium text-gray-900">
                <Shield className="w-5 h-5 text-blue-600" />
                Require ID Verification
              </div>
              <p className="text-sm text-gray-600 mt-1">
                Guests must verify their identity with a government-issued ID
              </p>
            </div>
          </label>

          {/* Minimum Reviews */}
          <div className="p-4 border-2 rounded-lg">
            <div className="flex items-center gap-2 mb-3">
              <Star className="w-5 h-5 text-yellow-600" />
              <label className="font-medium text-gray-900">Minimum Reviews</label>
            </div>
            <div className="flex items-center gap-4">
              <input
                type="range"
                min="0"
                max="10"
                value={formData.min_reviews}
                onChange={(e) => setFormData({ ...formData, min_reviews: parseInt(e.target.value) })}
                className="flex-1 h-2 bg-gray-200 rounded-lg appearance-none cursor-pointer"
              />
              <span className="font-semibold text-gray-900 min-w-[60px] text-center">
                {formData.min_reviews} {formData.min_reviews === 1 ? 'review' : 'reviews'}
              </span>
            </div>
            <p className="text-sm text-gray-600 mt-2">
              Guests must have at least this many reviews from previous stays
            </p>
          </div>

          {/* Minimum Rating */}
          <div className="p-4 border-2 rounded-lg">
            <div className="flex items-center gap-2 mb-3">
              <Star className="w-5 h-5 text-yellow-600 fill-yellow-600" />
              <label className="font-medium text-gray-900">Minimum Rating</label>
            </div>
            <div className="flex items-center gap-4">
              <input
                type="range"
                min="0"
                max="5"
                step="0.5"
                value={formData.min_rating}
                onChange={(e) => setFormData({ ...formData, min_rating: parseFloat(e.target.value) })}
                className="flex-1 h-2 bg-gray-200 rounded-lg appearance-none cursor-pointer"
              />
              <span className="font-semibold text-gray-900 min-w-[60px] text-center">
                {formData.min_rating === 0 ? 'None' : `${formData.min_rating.toFixed(1)} ⭐`}
              </span>
            </div>
            <p className="text-sm text-gray-600 mt-2">
              Guests must have at least this average rating
            </p>
          </div>

          {/* Completed Booking */}
          <label className="flex items-start gap-3 p-4 border-2 rounded-lg cursor-pointer hover:bg-gray-50 transition">
            <input
              type="checkbox"
              checked={formData.require_completed_booking}
              onChange={(e) => setFormData({ ...formData, require_completed_booking: e.target.checked })}
              className="mt-1 w-5 h-5 text-primary-600"
            />
            <div className="flex-1">
              <div className="flex items-center gap-2 font-medium text-gray-900">
                <Home className="w-5 h-5 text-green-600" />
                Require Completed Booking
              </div>
              <p className="text-sm text-gray-600 mt-1">
                Guests must have completed at least one previous booking
              </p>
            </div>
          </label>

          {/* Payment Method */}
          <label className="flex items-start gap-3 p-4 border-2 rounded-lg cursor-pointer hover:bg-gray-50 transition">
            <input
              type="checkbox"
              checked={formData.require_payment_method}
              onChange={(e) => setFormData({ ...formData, require_payment_method: e.target.checked })}
              className="mt-1 w-5 h-5 text-primary-600"
            />
            <div className="flex-1">
              <div className="flex items-center gap-2 font-medium text-gray-900">
                <CreditCard className="w-5 h-5 text-purple-600" />
                Require Payment Method
              </div>
              <p className="text-sm text-gray-600 mt-1">
                Guests must have a verified payment method on file
              </p>
            </div>
          </label>

          {/* Save Button */}
          <button
            onClick={handleSave}
            disabled={updateMutation.isPending}
            className="w-full flex items-center justify-center gap-2 py-3 bg-primary-600 text-white font-semibold rounded-lg hover:bg-primary-700 transition disabled:opacity-50 disabled:cursor-not-allowed"
          >
            {updateMutation.isPending ? (
              <>
                <Loader2 className="w-5 h-5 animate-spin" />
                Saving...
              </>
            ) : (
              <>
                <Save className="w-5 h-5" />
                Save Settings
              </>
            )}
          </button>
        </div>
      )}
    </div>
  );
}
