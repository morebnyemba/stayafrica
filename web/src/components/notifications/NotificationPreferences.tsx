'use client';

import { useNotificationPreferences } from './useNotificationPreferences';
import { Loader2, Bell, Mail, MessageSquare, Save } from 'lucide-react';
import { useState, useEffect } from 'react';
import { NotificationPreferences as PreferencesType } from '@/types/notification-types';

export const NotificationPreferences = () => {
  const { preferences, isLoading, updatePreferences, isUpdating } = useNotificationPreferences();
  const [localPreferences, setLocalPreferences] = useState<Partial<PreferencesType>>({});
  const [hasChanges, setHasChanges] = useState(false);

  useEffect(() => {
    if (preferences) {
      setLocalPreferences(preferences);
    }
  }, [preferences]);

  const handleToggle = (key: keyof PreferencesType) => {
    setLocalPreferences(prev => ({
      ...prev,
      [key]: !prev[key],
    }));
    setHasChanges(true);
  };

  const handleSave = async () => {
    try {
      await updatePreferences(localPreferences);
      setHasChanges(false);
    } catch (error) {
      console.error('Failed to update preferences:', error);
    }
  };

  if (isLoading) {
    return (
      <div className="flex items-center justify-center p-8">
        <Loader2 className="h-8 w-8 animate-spin text-primary-300 dark:text-primary-500" />
      </div>
    );
  }

  return (
    <div className="bg-white dark:bg-primary-800/40 rounded-lg shadow-sm border border-sand-200/50 dark:border-primary-700/50 p-6 space-y-6">
      <div className="flex items-center justify-between">
        <h2 className="text-xl font-semibold text-primary-900 dark:text-sand-50">Notification Preferences</h2>
        
        {hasChanges && (
          <button
            onClick={handleSave}
            disabled={isUpdating}
            className="inline-flex items-center gap-2 px-4 py-2 bg-secondary-600 text-white rounded-lg hover:bg-secondary-700 disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
          >
            {isUpdating ? (
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

      <div className="space-y-6">
        <div className="space-y-4">
          <h3 className="text-sm font-medium text-primary-700 dark:text-sand-200 uppercase tracking-wide">
            Notification Channels
          </h3>
          
          <div className="space-y-3">
            <label className="flex items-center justify-between p-4 border border-sand-200/50 dark:border-primary-700/50 rounded-lg hover:bg-sand-50 dark:hover:bg-primary-800 cursor-pointer">
              <div className="flex items-center gap-3">
                <Bell className="h-5 w-5 text-primary-500 dark:text-sand-400" />
                <div>
                  <p className="font-medium text-primary-900 dark:text-sand-50">Push Notifications</p>
                  <p className="text-sm text-primary-400 dark:text-sand-500">Receive push notifications on your device</p>
                </div>
              </div>
              <input
                type="checkbox"
                checked={localPreferences.push_notifications ?? false}
                onChange={() => handleToggle('push_notifications')}
                className="h-5 w-5 text-secondary-600 rounded focus:ring-2 focus:ring-secondary-500"
                aria-label="Toggle push notifications"
              />
            </label>

            <label className="flex items-center justify-between p-4 border border-sand-200/50 dark:border-primary-700/50 rounded-lg hover:bg-sand-50 dark:hover:bg-primary-800 cursor-pointer">
              <div className="flex items-center gap-3">
                <Mail className="h-5 w-5 text-primary-500 dark:text-sand-400" />
                <div>
                  <p className="font-medium text-primary-900 dark:text-sand-50">Email Notifications</p>
                  <p className="text-sm text-primary-400 dark:text-sand-500">Receive notifications via email</p>
                </div>
              </div>
              <input
                type="checkbox"
                checked={localPreferences.email_notifications ?? false}
                onChange={() => handleToggle('email_notifications')}
                className="h-5 w-5 text-secondary-600 rounded focus:ring-2 focus:ring-secondary-500"
                aria-label="Toggle email notifications"
              />
            </label>

            <label className="flex items-center justify-between p-4 border border-sand-200/50 dark:border-primary-700/50 rounded-lg hover:bg-sand-50 dark:hover:bg-primary-800 cursor-pointer">
              <div className="flex items-center gap-3">
                <MessageSquare className="h-5 w-5 text-primary-500 dark:text-sand-400" />
                <div>
                  <p className="font-medium text-primary-900 dark:text-sand-50">SMS Notifications</p>
                  <p className="text-sm text-primary-400 dark:text-sand-500">Receive notifications via text message</p>
                </div>
              </div>
              <input
                type="checkbox"
                checked={localPreferences.sms_notifications ?? false}
                onChange={() => handleToggle('sms_notifications')}
                className="h-5 w-5 text-secondary-600 rounded focus:ring-2 focus:ring-secondary-500"
                aria-label="Toggle SMS notifications"
              />
            </label>
          </div>
        </div>

        <div className="space-y-4">
          <h3 className="text-sm font-medium text-primary-700 dark:text-sand-200 uppercase tracking-wide">
            Notification Types
          </h3>
          
          <div className="space-y-3">
            <label className="flex items-center justify-between p-4 border border-sand-200/50 dark:border-primary-700/50 rounded-lg hover:bg-sand-50 dark:hover:bg-primary-800 cursor-pointer">
              <div>
                <p className="font-medium text-primary-900 dark:text-sand-50">Booking Updates</p>
                <p className="text-sm text-primary-400 dark:text-sand-500">Confirmations, cancellations, and check-in reminders</p>
              </div>
              <input
                type="checkbox"
                checked={localPreferences.booking_updates ?? false}
                onChange={() => handleToggle('booking_updates')}
                className="h-5 w-5 text-secondary-600 rounded focus:ring-2 focus:ring-secondary-500"
                aria-label="Toggle booking updates"
              />
            </label>

            <label className="flex items-center justify-between p-4 border border-sand-200/50 dark:border-primary-700/50 rounded-lg hover:bg-sand-50 dark:hover:bg-primary-800 cursor-pointer">
              <div>
                <p className="font-medium text-primary-900 dark:text-sand-50">Messages</p>
                <p className="text-sm text-primary-400 dark:text-sand-500">New messages from guests or hosts</p>
              </div>
              <input
                type="checkbox"
                checked={localPreferences.messages ?? false}
                onChange={() => handleToggle('messages')}
                className="h-5 w-5 text-secondary-600 rounded focus:ring-2 focus:ring-secondary-500"
                aria-label="Toggle message notifications"
              />
            </label>

            <label className="flex items-center justify-between p-4 border border-sand-200/50 dark:border-primary-700/50 rounded-lg hover:bg-sand-50 dark:hover:bg-primary-800 cursor-pointer">
              <div>
                <p className="font-medium text-primary-900 dark:text-sand-50">Reviews</p>
                <p className="text-sm text-primary-400 dark:text-sand-500">New reviews and review reminders</p>
              </div>
              <input
                type="checkbox"
                checked={localPreferences.reviews ?? false}
                onChange={() => handleToggle('reviews')}
                className="h-5 w-5 text-secondary-600 rounded focus:ring-2 focus:ring-secondary-500"
                aria-label="Toggle review notifications"
              />
            </label>

            <label className="flex items-center justify-between p-4 border border-sand-200/50 dark:border-primary-700/50 rounded-lg hover:bg-sand-50 dark:hover:bg-primary-800 cursor-pointer">
              <div>
                <p className="font-medium text-primary-900 dark:text-sand-50">Promotions</p>
                <p className="text-sm text-primary-400 dark:text-sand-500">Special offers and discounts</p>
              </div>
              <input
                type="checkbox"
                checked={localPreferences.promotions ?? false}
                onChange={() => handleToggle('promotions')}
                className="h-5 w-5 text-secondary-600 rounded focus:ring-2 focus:ring-secondary-500"
                aria-label="Toggle promotion notifications"
              />
            </label>

            <label className="flex items-center justify-between p-4 border border-sand-200/50 dark:border-primary-700/50 rounded-lg hover:bg-sand-50 dark:hover:bg-primary-800 cursor-pointer">
              <div>
                <p className="font-medium text-primary-900 dark:text-sand-50">Account Activity</p>
                <p className="text-sm text-primary-400 dark:text-sand-500">Security alerts and account changes</p>
              </div>
              <input
                type="checkbox"
                checked={localPreferences.account_activity ?? false}
                onChange={() => handleToggle('account_activity')}
                className="h-5 w-5 text-secondary-600 rounded focus:ring-2 focus:ring-secondary-500"
                aria-label="Toggle account activity notifications"
              />
            </label>
          </div>
        </div>
      </div>
    </div>
  );
};
