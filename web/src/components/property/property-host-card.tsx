'use client';

import Link from 'next/link';
import { User, Star, CheckCircle } from 'lucide-react';

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
  };
}

export function PropertyHostCard({ host }: PropertyHostCardProps) {
  if (!host) {
    return null;
  }

  return (
    <div>
      <h2 className="text-2xl font-semibold text-primary-900 dark:text-sand-50 mb-6">
        Hosted by
      </h2>
      <div className="bg-white dark:bg-primary-800 p-6 rounded-lg border border-primary-200 dark:border-primary-700">
        {/* Host profile */}
        <div className="flex items-start justify-between mb-6 pb-6 border-b border-primary-200 dark:border-primary-700">
          <div className="flex items-start space-x-4">
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
            <div className="flex-1">
              <div className="flex items-center space-x-2">
                <h3 className="text-lg font-semibold text-primary-900 dark:text-sand-50">
                  {host.first_name} {host.last_name}
                </h3>
                {host.is_verified && (
                  <CheckCircle className="w-5 h-5 text-green-600 dark:text-green-400" />
                )}
              </div>
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
        <Link
          href={`/messages?host=${host.id}`}
          className="w-full inline-block text-center bg-secondary-600 hover:bg-secondary-700 dark:bg-secondary-700 dark:hover:bg-secondary-600 text-white font-medium py-3 px-4 rounded-lg transition"
        >
          Contact Host
        </Link>
      </div>
    </div>
  );
}
