'use client';

import { Star, MapPin, Clock, Users } from 'lucide-react';
import { Button } from '@/components/ui';
import Image from 'next/image';

interface ExperienceCardProps {
  experience: {
    id: string;
    title: string;
    description: string;
    city: string;
    country: string;
    price_per_person: number;
    currency: string;
    duration: string;
    duration_hours: number;
    difficulty: string;
    max_participants: number;
    main_image?: string;
    category_name?: string;
    average_rating?: number;
    review_count?: number;
  };
}

export function ExperienceCard({ experience }: ExperienceCardProps) {
  const getDifficultyColor = (difficulty: string) => {
    switch (difficulty) {
      case 'easy':
        return 'bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-200';
      case 'moderate':
        return 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-200';
      case 'challenging':
        return 'bg-orange-100 text-orange-800 dark:bg-orange-900 dark:text-orange-200';
      case 'expert':
        return 'bg-red-100 text-red-800 dark:bg-red-900 dark:text-red-200';
      default:
        return 'bg-primary-100 text-primary-800 dark:bg-primary-700 dark:text-sand-200';
    }
  };

  const getDurationLabel = (duration: string) => {
    switch (duration) {
      case 'half_day':
        return 'Half Day';
      case 'full_day':
        return 'Full Day';
      case 'multi_day':
        return 'Multi-Day';
      case 'hourly':
        return 'Hourly';
      default:
        return duration;
    }
  };

  return (
    <article className="card overflow-hidden hover:shadow-lg transition-shadow duration-200">
      {/* Image */}
      <div className="relative h-48 sm:h-56 bg-primary-200 dark:bg-primary-700">
        {experience.main_image ? (
          <Image
            src={experience.main_image}
            alt={experience.title}
            fill
            className="object-cover"
          />
        ) : (
          <div className="w-full h-full flex items-center justify-center text-primary-400 dark:text-sand-500">
            <MapPin className="w-16 h-16" />
          </div>
        )}
        {experience.category_name && (
          <div className="absolute top-3 left-3">
            <span className="px-3 py-1 bg-white/90 dark:bg-primary-900/90 backdrop-blur-sm rounded-full text-xs font-semibold text-primary-900 dark:text-sand-50">
              {experience.category_name}
            </span>
          </div>
        )}
        <div className="absolute top-3 right-3">
          <span className={`px-3 py-1 rounded-full text-xs font-semibold ${getDifficultyColor(experience.difficulty)}`}>
            {experience.difficulty.charAt(0).toUpperCase() + experience.difficulty.slice(1)}
          </span>
        </div>
      </div>

      {/* Content */}
      <div className="p-4 sm:p-6">
        {/* Title and Location */}
        <div className="mb-3">
          <h3 className="text-lg sm:text-xl font-bold text-primary-900 dark:text-sand-50 mb-2 line-clamp-2">
            {experience.title}
          </h3>
          <div className="flex items-center gap-2 text-sm text-primary-600 dark:text-sand-300">
            <MapPin className="w-4 h-4" />
            <span>
              {experience.city}, {experience.country}
            </span>
          </div>
        </div>

        {/* Description */}
        <p className="text-sm text-primary-700 dark:text-sand-300 mb-4 line-clamp-2">
          {experience.description}
        </p>

        {/* Details */}
        <div className="flex flex-wrap gap-3 mb-4 text-sm text-primary-600 dark:text-sand-300">
          <div className="flex items-center gap-1">
            <Clock className="w-4 h-4" />
            <span>
              {getDurationLabel(experience.duration)} ({experience.duration_hours}h)
            </span>
          </div>
          <div className="flex items-center gap-1">
            <Users className="w-4 h-4" />
            <span>Up to {experience.max_participants} guests</span>
          </div>
        </div>

        {/* Rating */}
        {experience.average_rating && experience.average_rating > 0 && (
          <div className="flex items-center gap-2 mb-4">
            <Star className="w-4 h-4 fill-secondary-500 text-secondary-500" />
            <span className="font-semibold text-primary-900 dark:text-sand-50">
              {experience.average_rating.toFixed(1)}
            </span>
            {experience.review_count && (
              <span className="text-sm text-primary-600 dark:text-sand-400">
                ({experience.review_count} reviews)
              </span>
            )}
          </div>
        )}

        {/* Price and Action */}
        <div className="flex items-center justify-between pt-4 border-t border-primary-200 dark:border-primary-700">
          <div>
            <div className="flex items-baseline gap-1">
              <span className="text-2xl font-bold text-primary-900 dark:text-sand-50">
                {experience.currency} {experience.price_per_person}
              </span>
              <span className="text-sm text-primary-600 dark:text-sand-400">per person</span>
            </div>
          </div>
          <a href={`/experiences/${experience.id}`}>
            <Button size="sm">View Details</Button>
          </a>
        </div>
      </div>
    </article>
  );
}
