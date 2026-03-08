'use client';

import { useState } from 'react';
import { useRouter, usePathname } from 'next/navigation';
import { Star, CheckCircle, MessageCircle, Loader2, Shield } from 'lucide-react';
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
  const pathname = usePathname();
  const { user, isAuthenticated } = useAuth();
  const [contactingHost, setContactingHost] = useState(false);
  if (!host) return null;

  const contactHost = async () => {
    if (!isAuthenticated || !user?.id) {
      toast.error('You must be logged in to perform this action');
      router.push(`/login?redirect=${encodeURIComponent(pathname || '/')}`);
      return;
    }
    if (!host) {
      toast.error('Host information is unavailable');
      return;
    }
    setContactingHost(true);
    try {
      await apiClient.createConversation({
        participants: [parseInt(host.id), parseInt(user.id)],
        property: propertyId ? parseInt(propertyId) : undefined,
        subject: `Inquiry from interested guest`,
      });
      toast.success('Conversation started!');
      router.push(`/messages`);
    } catch (error: any) {
      toast.error(error.response?.data?.error || 'Failed to start conversation');
    } finally {
      setContactingHost(false);
    }
  };

  return (
    <div>
      <div className="flex items-start gap-5">
        {/* Avatar */}
        <div className="relative flex-shrink-0">
          {host.profile_picture ? (
            <img
              src={host.profile_picture}
              alt={`${host.first_name} ${host.last_name}`}
              className="w-16 h-16 rounded-full object-cover"
            />
          ) : (
            <div className="w-16 h-16 rounded-full bg-primary-800 dark:bg-primary-600 flex items-center justify-center">
              <span className="text-white text-xl font-semibold">
                {host.first_name?.[0] || 'H'}
              </span>
            </div>
          )}
          {host.is_verified && (
            <div className="absolute -bottom-1 -right-1 bg-white dark:bg-primary-900 rounded-full p-0.5">
              <CheckCircle className="w-5 h-5 text-rose-500" />
            </div>
          )}
        </div>

        {/* Info */}
        <div className="flex-1 min-w-0">
          <h2 className="text-xl font-semibold text-primary-900 dark:text-sand-50">
            Hosted by {host.first_name}
          </h2>
          <p className="text-sm text-primary-500 dark:text-sand-400 mt-0.5">
            {host.is_online ? (
              <span className="text-green-600 dark:text-green-400">Online now</span>
            ) : host.last_seen ? (
              `Active ${formatDistanceToNow(new Date(host.last_seen), { addSuffix: true })}`
            ) : (
              'Host'
            )}
          </p>
        </div>
      </div>

      {/* Stats */}
      <div className="flex items-center gap-6 mt-4 mb-4">
        {host.average_rating !== undefined && host.average_rating > 0 && (
          <div className="flex items-center gap-1.5">
            <Star className="w-4 h-4 fill-primary-900 dark:fill-sand-50 text-primary-900 dark:text-sand-50" />
            <span className="font-semibold text-sm text-primary-900 dark:text-sand-50">
              {host.average_rating.toFixed(1)}
            </span>
            <span className="text-sm text-primary-500 dark:text-sand-400">rating</span>
          </div>
        )}
        {host.total_listings !== undefined && host.total_listings > 0 && (
          <div className="text-sm">
            <span className="font-semibold text-primary-900 dark:text-sand-50">{host.total_listings}</span>
            <span className="text-primary-500 dark:text-sand-400"> listing{host.total_listings !== 1 ? 's' : ''}</span>
          </div>
        )}
        {host.response_rate !== undefined && (
          <div className="text-sm">
            <span className="font-semibold text-primary-900 dark:text-sand-50">{host.response_rate}%</span>
            <span className="text-primary-500 dark:text-sand-400"> response rate</span>
          </div>
        )}
      </div>

      {host.response_time && (
        <p className="text-sm text-primary-600 dark:text-sand-300 mb-4">
          Typically responds within {host.response_time}
        </p>
      )}

      {/* Contact Button */}
      <button
        onClick={contactHost}
        disabled={contactingHost}
        className="inline-flex items-center justify-center gap-2 px-6 py-3 border border-primary-900 dark:border-sand-50 rounded-lg text-primary-900 dark:text-sand-50 font-semibold text-sm hover:bg-sand-50 dark:hover:bg-primary-800 transition disabled:opacity-50 disabled:cursor-not-allowed"
      >
        {contactingHost ? (
          <>
            <Loader2 className="w-4 h-4 animate-spin" />
            Contacting...
          </>
        ) : (
          <>
            <MessageCircle className="w-4 h-4" />
            Contact Host
          </>
        )}
      </button>

      {/* Safety note */}
      <div className="flex items-start gap-3 mt-5 pt-4 border-t border-primary-200 dark:border-primary-700">
        <Shield className="w-5 h-5 text-primary-500 dark:text-sand-400 flex-shrink-0 mt-0.5" />
        <p className="text-xs text-primary-500 dark:text-sand-400 leading-relaxed">
          To protect your payment, never transfer money or communicate outside of the StayAfrica website or app.
        </p>
      </div>
    </div>
  );
}
