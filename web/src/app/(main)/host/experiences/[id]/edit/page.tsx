'use client';

import { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { apiClient } from '@/services/api-client';
import { ExperienceForm } from '@/components/host/experience-form';
import { Loader2 } from 'lucide-react';

export default function EditExperiencePage({ params }: { params: Promise<{ id: string }> }) {
  const router = useRouter();
  const [experience, setExperience] = useState<any>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [experienceId, setExperienceId] = useState<string | null>(null);

  useEffect(() => {
    async function initializeParams() {
      const resolvedParams = await params;
      setExperienceId(resolvedParams.id);
    }
    initializeParams();
  }, [params]);

  useEffect(() => {
    if (!experienceId) return;

    async function loadExperience() {
      try {
        setLoading(true);
        const response = await apiClient.getExperienceById(experienceId!);
        setExperience(response.data);
      } catch (err: any) {
        setError(err?.response?.data?.detail || 'Failed to load experience');
      } finally {
        setLoading(false);
      }
    }

    loadExperience();
  }, [experienceId]);

  if (loading) {
    return (
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900 flex items-center justify-center">
        <Loader2 className="w-8 h-8 animate-spin text-secondary-500" />
      </div>
    );
  }

  if (error) {
    return (
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900 flex items-center justify-center">
        <div className="card-gradient p-8 text-center max-w-md">
          <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-2">
            Error Loading Experience
          </h2>
          <p className="text-primary-600 dark:text-sand-300 mb-4">{error}</p>
          <button
            onClick={() => router.push('/host/experiences')}
            className="text-secondary-600 dark:text-secondary-400 hover:underline"
          >
            Back to Experiences
          </button>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
      <div className="max-w-4xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        <div className="mb-8">
          <h1 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-2">
            Edit Experience
          </h1>
          <p className="text-lg text-primary-600 dark:text-sand-300">
            Update your experience details
          </p>
        </div>

        <ExperienceForm mode="edit" experience={experience} />
      </div>
    </div>
  );
}
