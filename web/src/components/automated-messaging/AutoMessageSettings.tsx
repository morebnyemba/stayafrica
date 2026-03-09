'use client';

import { useState, useEffect } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { AutoMessageSettings as SettingsType } from '@/types/automated-messaging-types';
import { Loader2, Save, Clock, MessageSquare, Calendar, Moon } from 'lucide-react';

export const AutoMessageSettings = () => {
  const queryClient = useQueryClient();
  const [localSettings, setLocalSettings] = useState<Partial<SettingsType>>({});
  const [hasChanges, setHasChanges] = useState(false);

  const { data: settings, isLoading } = useQuery<SettingsType>({
    queryKey: ['auto-message-settings'],
    queryFn: async () => {
      const response = await apiClient.get('/messaging/settings/');
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
      if (!settings?.id) return;
      const response = await apiClient.patch(`/messaging/settings/${settings.id}/`, data);
      return response.data;
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['auto-message-settings'] });
      setHasChanges(false);
    },
  });

  const handleToggle = async (field: keyof SettingsType) => {
    const newValue = !localSettings[field];
    setLocalSettings(prev => ({ ...prev, [field]: newValue }));
    await updateMutation.mutateAsync({ [field]: newValue } as Partial<SettingsType>);
  };

  const handleChange = (field: keyof SettingsType, value: string | number) => {
    setLocalSettings(prev => ({ ...prev, [field]: value }));
    setHasChanges(true);
  };

  const handleSave = async () => {
    await updateMutation.mutateAsync({
      away_message: localSettings.away_message,
      target_response_time_hours: localSettings.target_response_time_hours,
    });
  };

  if (isLoading) {
    return (
      <div className="flex items-center justify-center p-8">
        <Loader2 className="h-8 w-8 animate-spin text-primary-300 dark:text-primary-500" />
      </div>
    );
  }

  const toggleItems = [
    {
      field: 'enable_auto_responses' as const,
      icon: MessageSquare,
      label: 'Auto Responses',
      description: 'Automatically send responses based on booking triggers',
    },
    {
      field: 'enable_quick_replies' as const,
      icon: MessageSquare,
      label: 'Quick Replies',
      description: 'Show quick reply suggestions in conversations',
    },
    {
      field: 'enable_scheduled_messages' as const,
      icon: Calendar,
      label: 'Scheduled Messages',
      description: 'Send messages at specific scheduled times',
    },
    {
      field: 'away_mode_enabled' as const,
      icon: Moon,
      label: 'Away Mode',
      description: 'Send away message when you are unavailable',
    },
  ];

  return (
    <div className="bg-white dark:bg-primary-800/40 rounded-lg shadow-sm border">
      <div className="p-6 border-b">
        <div className="flex items-center justify-between">
          <div>
            <h2 className="text-xl font-semibold text-primary-900 dark:text-sand-50">
              Messaging Settings
            </h2>
            <p className="text-sm text-primary-500 dark:text-sand-400 mt-1">
              Configure your automated messaging preferences
            </p>
          </div>

          {hasChanges && (
            <button
              onClick={handleSave}
              disabled={updateMutation.isPending}
              className="inline-flex items-center gap-2 px-4 py-2 bg-secondary-600 text-white rounded-lg hover:bg-secondary-700 disabled:opacity-50 transition-colors"
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
        </div>
      </div>

      <div className="p-6 space-y-6">
        {toggleItems.map(({ field, icon: Icon, label, description }) => (
          <div key={field} className="flex items-center justify-between p-4 border rounded-lg">
            <div className="flex items-start gap-3">
              <Icon className="h-5 w-5 text-primary-400 dark:text-sand-500 mt-0.5" />
              <div>
                <h3 className="font-medium text-primary-900 dark:text-sand-50">{label}</h3>
                <p className="text-sm text-primary-500 dark:text-sand-400 mt-0.5">{description}</p>
              </div>
            </div>
            <button
              onClick={() => handleToggle(field)}
              disabled={updateMutation.isPending}
              className={`relative inline-flex h-6 w-11 items-center rounded-full transition-colors ${localSettings[field] ? 'bg-secondary-600' : 'bg-primary-200 dark:bg-primary-700'
                }`}
            >
              <span
                className={`inline-block h-4 w-4 transform rounded-full bg-white transition-transform ${localSettings[field] ? 'translate-x-6' : 'translate-x-1'
                  }`}
              />
            </button>
          </div>
        ))}

        {localSettings.away_mode_enabled && (
          <div className="ml-8 p-4 border rounded-lg bg-sand-50 dark:bg-primary-900">
            <label className="block text-sm font-medium text-primary-700 dark:text-sand-200 mb-2">
              Away Message
            </label>
            <textarea
              value={localSettings.away_message || ''}
              onChange={(e) => handleChange('away_message', e.target.value)}
              placeholder="Enter your away message..."
              rows={3}
              className="w-full px-3 py-2 border border-primary-300 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent text-sm"
            />
          </div>
        )}

        <div className="p-4 border rounded-lg">
          <div className="flex items-start gap-3">
            <Clock className="h-5 w-5 text-primary-400 dark:text-sand-500 mt-0.5" />
            <div className="flex-1">
              <h3 className="font-medium text-primary-900 dark:text-sand-50">Target Response Time</h3>
              <p className="text-sm text-primary-500 dark:text-sand-400 mt-0.5">
                Set your goal for how quickly you respond to messages
              </p>
              <div className="mt-3 flex items-center gap-2">
                <input
                  type="number"
                  min={1}
                  max={72}
                  value={localSettings.target_response_time_hours || 24}
                  onChange={(e) => handleChange('target_response_time_hours', parseInt(e.target.value) || 24)}
                  className="w-20 px-3 py-2 border border-primary-300 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent text-sm"
                />
                <span className="text-sm text-primary-500 dark:text-sand-400">hours</span>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};
