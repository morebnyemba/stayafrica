'use client';

import { useEffect, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { Button } from '@/components/ui';
import { Star, MapPin, Clock, Users, Calendar, Shield, ArrowLeft } from 'lucide-react';
import Image from 'next/image';
import { useRouter } from 'next/navigation';

export default function ExperienceDetailPage({ params }: { params: Promise<{ id: string }> }) {
  const router = useRouter();
  const [experienceId, setExperienceId] = useState<string | null>(null);

  useEffect(() => {
    async function initializeParams() {
      const resolvedParams = await params;
      setExperienceId(resolvedParams.id);
    }
    initializeParams();
  }, [params]);

  const { data: experience, isLoading, error } = useQuery({
    queryKey: ['experience', experienceId],
    queryFn: async () => {
      if (!experienceId) throw new Error('Experience ID not available');
      const response = await apiClient.getExperienceById(experienceId);
      return response.data;
    },
    enabled: !!experienceId,
  });

  if (!experienceId) {
    return (
      <div className="max-w-7xl mx-auto px-4 py-12">
        <div className="text-center">
          <p className="text-primary-900 dark:text-sand-100">Loading...</p>
        </div>
      </div>
    );
  }

  if (isLoading) {
    return (
      <div className="bg-sand-100 dark:bg-primary-900 min-h-screen">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
          <div className="animate-pulse space-y-6">
            <div className="h-96 bg-primary-200 dark:bg-primary-700 rounded-lg"></div>
            <div className="h-8 bg-primary-200 dark:bg-primary-700 rounded w-3/4"></div>
            <div className="h-6 bg-primary-200 dark:bg-primary-700 rounded w-1/2"></div>
          </div>
        </div>
      </div>
    );
  }

  if (error || !experience) {
    return (
      <div className="bg-sand-100 dark:bg-primary-900 min-h-screen">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
          <div className="card p-12 text-center">
            <p className="text-primary-600 dark:text-sand-300 mb-4">
              Unable to load experience details.
            </p>
            <Button onClick={() => router.push('/experiences')}>
              Back to Experiences
            </Button>
          </div>
        </div>
      </div>
    );
  }

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

  return (
    <div className="bg-sand-100 dark:bg-primary-900 min-h-screen">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
        {/* Back button */}
        <Button onClick={() => router.back()} variant="outline" size="sm" className="mb-6">
          <ArrowLeft className="w-4 h-4 mr-2" />
          Back
        </Button>

        <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
          {/* Main content */}
          <div className="lg:col-span-2 space-y-6">
            {/* Hero image */}
            <div className="relative h-96 bg-primary-200 dark:bg-primary-700 rounded-lg overflow-hidden">
              {experience.main_image ? (
                <Image
                  src={experience.main_image}
                  alt={experience.title}
                  fill
                  className="object-cover"
                />
              ) : (
                <div className="w-full h-full flex items-center justify-center text-primary-400 dark:text-sand-500">
                  <Calendar className="w-24 h-24" />
                </div>
              )}
            </div>

            {/* Title and location */}
            <div>
              <div className="flex items-start justify-between mb-4">
                <div className="flex-1">
                  <h1 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-3">
                    {experience.title}
                  </h1>
                  <div className="flex items-center gap-2 text-primary-600 dark:text-sand-300 mb-2">
                    <MapPin className="w-5 h-5" />
                    <span className="text-lg">
                      {experience.city}, {experience.country}
                    </span>
                  </div>
                </div>
                <span className={`px-4 py-2 rounded-full text-sm font-semibold ${getDifficultyColor(experience.difficulty)}`}>
                  {experience.difficulty.charAt(0).toUpperCase() + experience.difficulty.slice(1)}
                </span>
              </div>

              {/* Quick info */}
              <div className="flex flex-wrap gap-4 text-sm text-primary-600 dark:text-sand-300">
                <div className="flex items-center gap-2">
                  <Clock className="w-5 h-5" />
                  <span>{experience.duration_hours} hours</span>
                </div>
                <div className="flex items-center gap-2">
                  <Users className="w-5 h-5" />
                  <span>
                    {experience.min_participants}-{experience.max_participants} participants
                  </span>
                </div>
                {experience.category_name && (
                  <div className="flex items-center gap-2">
                    <Shield className="w-5 h-5" />
                    <span>{experience.category_name}</span>
                  </div>
                )}
              </div>
            </div>

            {/* Description */}
            <div className="card p-6">
              <h2 className="text-2xl font-semibold text-primary-900 dark:text-sand-50 mb-4">
                About this Experience
              </h2>
              <p className="text-primary-700 dark:text-sand-300 whitespace-pre-line">
                {experience.description}
              </p>
            </div>

            {/* What's included */}
            {experience.included_items && (
              <div className="card p-6">
                <h2 className="text-2xl font-semibold text-primary-900 dark:text-sand-50 mb-4">
                  What's Included
                </h2>
                <ul className="space-y-2 text-primary-700 dark:text-sand-300">
                  {experience.included_items.split('\n').map((item: string, index: number) => (
                    <li key={index} className="flex items-start gap-2">
                      <span className="text-green-600 mt-1">✓</span>
                      <span>{item}</span>
                    </li>
                  ))}
                </ul>
              </div>
            )}

            {/* Requirements */}
            {experience.requirements && (
              <div className="card p-6">
                <h2 className="text-2xl font-semibold text-primary-900 dark:text-sand-50 mb-4">
                  Requirements
                </h2>
                <p className="text-primary-700 dark:text-sand-300 whitespace-pre-line">
                  {experience.requirements}
                </p>
              </div>
            )}

            {/* Cancellation policy */}
            {experience.cancellation_policy && (
              <div className="card p-6">
                <h2 className="text-2xl font-semibold text-primary-900 dark:text-sand-50 mb-4">
                  Cancellation Policy
                </h2>
                <p className="text-primary-700 dark:text-sand-300 whitespace-pre-line">
                  {experience.cancellation_policy}
                </p>
              </div>
            )}
          </div>

          {/* Booking sidebar */}
          <div className="lg:col-span-1">
            <div className="card p-6 sticky top-24">
              <div className="mb-6">
                <div className="flex items-baseline gap-2 mb-2">
                  <span className="text-3xl font-bold text-primary-900 dark:text-sand-50">
                    {experience.currency} {experience.price_per_person}
                  </span>
                  <span className="text-primary-600 dark:text-sand-400">per person</span>
                </div>
                {experience.average_rating && (
                  <div className="flex items-center gap-2">
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
              </div>

              <Button className="w-full mb-4">Book Experience</Button>

              <p className="text-xs text-center text-primary-600 dark:text-sand-400">
                You won't be charged yet
              </p>

              {/* Host info */}
              {experience.host_name && (
                <div className="mt-6 pt-6 border-t border-primary-200 dark:border-primary-700">
                  <p className="text-sm text-primary-600 dark:text-sand-400 mb-1">Hosted by</p>
                  <p className="font-semibold text-primary-900 dark:text-sand-50">
                    {experience.host_name}
                  </p>
                </div>
              )}
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
