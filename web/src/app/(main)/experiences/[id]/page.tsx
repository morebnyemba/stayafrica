'use client';

import { useEffect, useState, useMemo } from 'react';
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

  const { data: availabilityRes } = useQuery({
    queryKey: ['experience-availability', experienceId],
    queryFn: async () => {
      if (!experienceId) throw new Error('No ID');
      const response = await apiClient.getExperienceAvailability(experienceId);
      return response.data;
    },
    enabled: !!experienceId,
  });

  const WEEKDAY_NAMES = ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'];

  const weekdaySlots = useMemo(() => {
    const data = Array.isArray(availabilityRes) ? availabilityRes : [];
    return data.filter((s: any) => s.weekday !== null && s.weekday !== undefined)
      .sort((a: any, b: any) => a.weekday - b.weekday);
  }, [availabilityRes]);

  const dateSlots = useMemo(() => {
    const data = Array.isArray(availabilityRes) ? availabilityRes : [];
    return data.filter((s: any) => s.specific_date)
      .sort((a: any, b: any) => a.specific_date.localeCompare(b.specific_date));
  }, [availabilityRes]);

  if (!experienceId) {
    return (
      <div className="max-w-7xl mx-auto px-4 py-12">
        <div className="text-center">
          <p className="text-primary-900">Loading...</p>
        </div>
      </div>
    );
  }

  if (isLoading) {
    return (
      <div className="bg-sand-100 min-h-screen">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
          <div className="animate-pulse space-y-6">
            <div className="h-96 bg-primary-200 rounded-lg"></div>
            <div className="h-8 bg-primary-200 rounded w-3/4"></div>
            <div className="h-6 bg-primary-200 rounded w-1/2"></div>
          </div>
        </div>
      </div>
    );
  }

  if (error || !experience) {
    return (
      <div className="bg-sand-100 min-h-screen">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
          <div className="card p-12 text-center">
            <p className="text-primary-600 mb-4">
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
        return 'bg-green-100 text-green-800';
      case 'moderate':
        return 'bg-yellow-100 text-yellow-800';
      case 'challenging':
        return 'bg-orange-100 text-orange-800';
      case 'expert':
        return 'bg-red-100 text-red-800';
      default:
        return 'bg-primary-100 text-primary-800';
    }
  };

  return (
    <div className="bg-sand-100 min-h-screen">
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
            <div className="relative h-96 bg-primary-200 rounded-lg overflow-hidden">
              {experience.main_image ? (
                <Image
                  src={experience.main_image}
                  alt={experience.title}
                  fill
                  className="object-cover"
                />
              ) : (
                <div className="w-full h-full flex items-center justify-center text-primary-400">
                  <Calendar className="w-24 h-24" />
                </div>
              )}
            </div>

            {/* Title and location */}
            <div>
              <div className="flex items-start justify-between mb-4">
                <div className="flex-1">
                  <h1 className="text-3xl md:text-4xl font-bold text-primary-900 mb-3">
                    {experience.title}
                  </h1>
                  <div className="flex items-center gap-2 text-primary-600 mb-2">
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
              <div className="flex flex-wrap gap-4 text-sm text-primary-600">
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
              <h2 className="text-2xl font-semibold text-primary-900 mb-4">
                About this Experience
              </h2>
              <p className="text-primary-700 whitespace-pre-line">
                {experience.description}
              </p>
            </div>

            {/* What's included */}
            {experience.included_items && (
              <div className="card p-6">
                <h2 className="text-2xl font-semibold text-primary-900 mb-4">
                  What's Included
                </h2>
                <ul className="space-y-2 text-primary-700">
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
                <h2 className="text-2xl font-semibold text-primary-900 mb-4">
                  Requirements
                </h2>
                <p className="text-primary-700 whitespace-pre-line">
                  {experience.requirements}
                </p>
              </div>
            )}

            {/* Cancellation policy */}
            {experience.cancellation_policy && (
              <div className="card p-6">
                <h2 className="text-2xl font-semibold text-primary-900 mb-4">
                  Cancellation Policy
                </h2>
                <p className="text-primary-700 whitespace-pre-line">
                  {experience.cancellation_policy}
                </p>
              </div>
            )}

            {/* Availability */}
            {(weekdaySlots.length > 0 || dateSlots.length > 0) && (
              <div className="card p-6">
                <h2 className="text-2xl font-semibold text-primary-900 mb-4">
                  <Calendar className="w-6 h-6 inline mr-2" />
                  Availability
                </h2>

                {weekdaySlots.length > 0 && (
                  <div className="mb-4">
                    <h3 className="text-sm font-semibold text-primary-600 mb-3">Weekly Schedule</h3>
                    <div className="grid grid-cols-2 sm:grid-cols-3 md:grid-cols-4 gap-2">
                      {weekdaySlots.map((slot: any) => (
                        <div key={slot.id} className="bg-secondary-50 rounded-lg px-3 py-2 text-center">
                          <p className="font-semibold text-primary-900 text-sm">{WEEKDAY_NAMES[slot.weekday]}</p>
                          <p className="text-xs text-primary-600">
                            {slot.start_time?.slice(0, 5)} – {slot.end_time?.slice(0, 5)}
                          </p>
                        </div>
                      ))}
                    </div>
                  </div>
                )}

                {dateSlots.length > 0 && (
                  <div>
                    <h3 className="text-sm font-semibold text-primary-600 mb-3">Upcoming Dates</h3>
                    <div className="space-y-2">
                      {dateSlots.slice(0, 6).map((slot: any) => (
                        <div key={slot.id} className="flex items-center justify-between border border-primary-200 rounded-lg px-4 py-2">
                          <span className="font-medium text-primary-900">
                            {new Date(slot.specific_date + 'T00:00:00').toLocaleDateString(undefined, { weekday: 'short', month: 'short', day: 'numeric' })}
                          </span>
                          <span className="text-sm text-primary-600">
                            {slot.start_time?.slice(0, 5)} – {slot.end_time?.slice(0, 5)}
                          </span>
                        </div>
                      ))}
                    </div>
                  </div>
                )}
              </div>
            )}
          </div>

          {/* Booking sidebar */}
          <div className="lg:col-span-1">
            <div className="card p-6 sticky top-24">
              <div className="mb-6">
                <div className="flex items-baseline gap-2 mb-2">
                  <span className="text-3xl font-bold text-primary-900">
                    {experience.currency} {experience.price_per_person}
                  </span>
                  <span className="text-primary-600">per person</span>
                </div>
                {experience.average_rating && (
                  <div className="flex items-center gap-2">
                    <Star className="w-4 h-4 fill-secondary-500 text-secondary-500" />
                    <span className="font-semibold text-primary-900">
                      {experience.average_rating.toFixed(1)}
                    </span>
                    {experience.review_count && (
                      <span className="text-sm text-primary-600">
                        ({experience.review_count} reviews)
                      </span>
                    )}
                  </div>
                )}
              </div>

              <Button className="w-full mb-4">Book Experience</Button>

              <p className="text-xs text-center text-primary-600">
                You won't be charged yet
              </p>

              {/* Host info */}
              {experience.host_name && (
                <div className="mt-6 pt-6 border-t border-primary-200">
                  <p className="text-sm text-primary-600 mb-1">Hosted by</p>
                  <p className="font-semibold text-primary-900">
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
