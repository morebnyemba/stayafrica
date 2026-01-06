'use client';

import { useState } from 'react';
import { useRouter } from 'next/navigation';
import { User, Star, CheckCircle, MessageCircle, Loader2 } from 'lucide-react';
import { formatDistanceToNow } from 'date-fns';
import { toast } from 'react-hot-toast';
import { apiClient } from '@/services/api-client';
import { useAuth } from '@/store/auth-store';

interface PropertyHostCardProps {
  host?: {
    id: string;
    first_name: string;
    last_name: string;
    profile_picture?: string;
    is_verified?: boolean;
    average_rating?: number;
    total_listings?: number;
    response_rate?: number;
    response_time?: string;
    is_online?: boolean;
    last_seen?: string;
  };
  propertyId?: string;
}

export function PropertyHostCard({ host, propertyId }: PropertyHostCardProps) {
  const router = useRouter();
  const { user } = useAuth();
  const [contactingHost, setContactingHost] = useState(false);
  if (!host) {
    return null;
  }

  const getOnlineStatus = () => {
    if (host.is_online) {
      return { text: 'Online now', color: 'text-green-600 dark:text-green-400' };
    }
    if (host.last_seen) {
      const lastSeenText = formatDistanceToNow(new Date(host.last_seen), { addSuffix: true });
      return { text: `Active ${lastSeenText}`, color: 'text-primary-600 dark:text-sand-400' };
    }
    return null;
  };

  const onlineStatus = getOnlineStatus();

  const contactHost = async () => {
    if (!host || !user?.id) {
      toast.error('Host or user information missing');
      return;
    }
    setContactingHost(true);
    try {
      const response = await apiClient.createConversation({
        participants: [host.id, user.id],
        property: propertyId,
        subject: `Inquiry from interested guest`,
      });
      const conversationId = response.data.id;
      toast.success('Conversation started!');
      router.push(`/messaging/conversations/${conversationId}`);
    } catch (error: any) {
      toast.error(error.response?.data?.error || 'Failed to start conversation');
    } finally {
      setContactingHost(false);
    }
  };

  return (
    <div>
      <h2 className="text-2xl font-semibold text-primary-900 dark:text-sand-50 mb-6">
        Hosted by
      </h2>
      <div className="bg-white dark:bg-primary-800 p-6 rounded-lg border border-primary-200 dark:border-primary-700">
        {/* Host profile */}
        <div className="flex items-start justify-between mb-6 pb-6 border-b border-primary-200 dark:border-primary-700">
          <div className="flex items-start space-x-4">
            <div className="relative">
              {host.profile_picture ? (
                <img
                  src={host.profile_picture}
                  alt={`${host.first_name} ${host.last_name}`}
                  className="w-16 h-16 rounded-full object-cover"
                />
              ) : (
                <div className="w-16 h-16 rounded-full bg-primary-200 dark:bg-primary-700 flex items-center justify-center">
                  <User className="w-8 h-8 text-primary-600 dark:text-sand-300" />
                </div>
              )}
              {/* Online indicator */}
              {host.is_online && (
                <div className="absolute bottom-0 right-0 w-4 h-4 bg-green-500 border-2 border-white dark:border-primary-800 rounded-full"></div>
              )}
            </div>
            <div className="flex-1">
              <div className="flex items-center space-x-2 mb-1">
                <h3 className="text-lg font-semibold text-primary-900 dark:text-sand-50">
                  {host.first_name} {host.last_name}
                </h3>
                {host.is_verified && (
                  <CheckCircle className="w-5 h-5 text-green-600 dark:text-green-400" />
                )}
              </div>
              {onlineStatus && (
                <p className={`text-sm ${onlineStatus.color} mb-1`}>
                  {onlineStatus.text}
                </p>
              )}
              <p className="text-sm text-primary-600 dark:text-sand-400">
                Hosting since 2023
              </p>
            </div>
          </div>
        </div>

        {/* Host stats */}
        <div className="grid grid-cols-3 gap-4 mb-6 pb-6 border-b border-primary-200 dark:border-primary-700">
          {host.average_rating !== undefined && (
            <div>
              <div className="flex items-center space-x-1 mb-1">
                <Star className="w-4 h-4 fill-secondary-500 text-secondary-500" />
                <span className="font-semibold text-primary-900 dark:text-sand-50">
                  {host.average_rating.toFixed(1)}
                </span>
              </div>
              <p className="text-xs text-primary-600 dark:text-sand-400">Rating</p>
            </div>
          )}
          {host.total_listings !== undefined && (
            <div>
              <p className="font-semibold text-primary-900 dark:text-sand-50 mb-1">
                {host.total_listings}
              </p>
              <p className="text-xs text-primary-600 dark:text-sand-400">Properties</p>
            </div>
          )}
          {host.response_rate !== undefined && (
            <div>
              <p className="font-semibold text-primary-900 dark:text-sand-50 mb-1">
                {host.response_rate}%
              </p>
              <p className="text-xs text-primary-600 dark:text-sand-400">Response Rate</p>
            </div>
          )}
        </div>

        {/* Response time */}
        {host.response_time && (
          <p className="text-sm text-primary-700 dark:text-sand-200 mb-6">
            Responds within <span className="font-semibold">{host.response_time}</span>
          </p>
        )}

        {/* Contact button */}
        <button
          onClick={contactHost}
          disabled={contactingHost}
          className="w-full inline-flex items-center justify-center gap-2 bg-secondary-600 hover:bg-secondary-700 dark:bg-secondary-700 dark:hover:bg-secondary-600 text-white font-medium py-3 px-4 rounded-lg transition disabled:opacity-50 disabled:cursor-not-allowed"
        >
          {contactingHost ? (
            <>
              <Loader2 className="w-5 h-5 animate-spin" />
              <span>Contacting...</span>
            </>
          ) : (
            <>
              <MessageCircle className="w-5 h-5" />
              <span>Contact Host</span>
            </>
          )}
        </button>
      </div>
    </div>
  );
}
