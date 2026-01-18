'use client';

import { useState, useEffect } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import axios from 'axios';
import { AutoMessageSettings as SettingsType } from '@/types/automated-messaging-types';
import { Loader2, Save, Power, PowerOff } from 'lucide-react';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8000';

const triggerLabels: Record<string, { label: string; description: string }> = {
  booking_confirmed: {
    label: 'Booking Confirmed',
    description: 'Sent immediately after booking confirmation',
  },
  check_in_reminder: {
    label: 'Check-in Reminder',
    description: 'Sent 24 hours before check-in',
  },
  check_out_reminder: {
    label: 'Check-out Reminder',
    description: 'Sent on check-out day',
  },
  booking_inquiry: {
    label: 'Booking Inquiry',
    description: 'Auto-reply to guest inquiries',
  },
  booking_cancelled: {
    label: 'Booking Cancelled',
    description: 'Sent when booking is cancelled',
  },
  review_request: {
    label: 'Review Request',
    description: 'Sent after check-out to request review',
  },
  payment_received: {
    label: 'Payment Received',
    description: 'Sent when payment is confirmed',
  },
  custom_trigger: {
    label: 'Custom Trigger',
    description: 'Custom automated message',
  },
};

export const AutoMessageSettings = () => {
  const queryClient = useQueryClient();
  const [localSettings, setLocalSettings] = useState<Partial<SettingsType>>({});
  const [hasChanges, setHasChanges] = useState(false);

  const { data: settings, isLoading } = useQuery<SettingsType>({
    queryKey: ['auto-message-settings'],
    queryFn: async () => {
      const token = localStorage.getItem('access_token');
      const response = await axios.get(
        `${API_BASE_URL}/api/v1/messaging/settings/`,
        {
          headers: {
            Authorization: `Bearer ${token}`,
          },
        }
      );
      return response.data;
    },
  });

  useEffect(() => {
    if (settings) {
      setLocalSettings(settings);
    }
  }, [settings]);

  const updateMutation = useMutation({
    mutationFn: async (data: Partial<SettingsType>) => {
      const token = localStorage.getItem('access_token');
      const response = await axios.patch(
        `${API_BASE_URL}/api/v1/messaging/settings/${settings?.id}/`,
        data,
        {
          headers: {
            Authorization: `Bearer ${token}`,
            'Content-Type': 'application/json',
          },
        }
      );
      return response.data;
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['auto-message-settings'] });
      setHasChanges(false);
    },
  });

  const handleToggleEnabled = async () => {
    const newEnabled = !localSettings.enabled;
    setLocalSettings(prev => ({ ...prev, enabled: newEnabled }));
    await updateMutation.mutateAsync({ enabled: newEnabled });
  };

  const handleToggleTrigger = (trigger: keyof SettingsType['triggers']) => {
    setLocalSettings(prev => ({
      ...prev,
      triggers: {
        ...prev.triggers!,
        [trigger]: !prev.triggers?.[trigger],
      },
    }));
    setHasChanges(true);
  };

  const handleTemplateChange = (trigger: string, value: string) => {
    setLocalSettings(prev => ({
      ...prev,
      templates: {
        ...prev.templates,
        [trigger]: value,
      },
    }));
    setHasChanges(true);
  };

  const handleSave = async () => {
    await updateMutation.mutateAsync({
      triggers: localSettings.triggers,
      templates: localSettings.templates,
    });
  };

  if (isLoading) {
    return (
      <div className="flex items-center justify-center p-8">
        <Loader2 className="h-8 w-8 animate-spin text-gray-400" />
      </div>
    );
  }

  return (
    <div className="bg-white rounded-lg shadow-sm border">
      <div className="p-6 border-b">
        <div className="flex items-center justify-between">
          <div>
            <h2 className="text-xl font-semibold text-gray-900">
              Automated Message Settings
            </h2>
            <p className="text-sm text-gray-600 mt-1">
              Configure automatic responses for common scenarios
            </p>
          </div>

          <div className="flex items-center gap-3">
            {hasChanges && (
              <button
                onClick={handleSave}
                disabled={updateMutation.isPending}
                className="inline-flex items-center gap-2 px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 disabled:opacity-50 transition-colors"
              >
                {updateMutation.isPending ? (
                  <>
                    <Loader2 className="h-4 w-4 animate-spin" />
                    <span>Saving...</span>
                  </>
                ) : (
                  <>
                    <Save className="h-4 w-4" />
                    <span>Save Changes</span>
                  </>
                )}
              </button>
            )}

            <button
              onClick={handleToggleEnabled}
              className={`inline-flex items-center gap-2 px-4 py-2 rounded-lg font-medium transition-colors ${
                localSettings.enabled
                  ? 'bg-green-600 text-white hover:bg-green-700'
                  : 'bg-gray-200 text-gray-700 hover:bg-gray-300'
              }`}
            >
              {localSettings.enabled ? (
                <>
                  <Power className="h-4 w-4" />
                  <span>Enabled</span>
                </>
              ) : (
                <>
                  <PowerOff className="h-4 w-4" />
                  <span>Disabled</span>
                </>
              )}
            </button>
          </div>
        </div>
      </div>

      <div className="p-6 space-y-6">
        {localSettings.triggers && Object.entries(triggerLabels).map(([key, info]) => (
          <div key={key} className="border rounded-lg p-4">
            <label className="flex items-center justify-between mb-3 cursor-pointer">
              <div className="flex-1">
                <h3 className="font-medium text-gray-900">{info.label}</h3>
                <p className="text-sm text-gray-600 mt-0.5">{info.description}</p>
              </div>
              <input
                type="checkbox"
                checked={localSettings.triggers?.[key as keyof typeof localSettings.triggers] ?? false}
                onChange={() => handleToggleTrigger(key as keyof SettingsType['triggers'])}
                disabled={!localSettings.enabled}
                className="h-5 w-5 text-blue-600 rounded focus:ring-2 focus:ring-blue-500 disabled:opacity-50"
              />
            </label>

            {localSettings.triggers?.[key as keyof typeof localSettings.triggers] && (
              <div className="mt-3">
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Message Template
                </label>
                <textarea
                  value={localSettings.templates?.[key] || ''}
                  onChange={(e) => handleTemplateChange(key, e.target.value)}
                  disabled={!localSettings.enabled}
                  placeholder={`Enter message template for ${info.label.toLowerCase()}...`}
                  rows={3}
                  className="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent disabled:bg-gray-50 disabled:cursor-not-allowed text-sm"
                />
                <p className="text-xs text-gray-500 mt-1">
                  Use variables: {'{guest_name}'}, {'{property_name}'}, {'{check_in}'}, {'{check_out}'}
                </p>
              </div>
            )}
          </div>
        ))}
      </div>
    </div>
  );
};
