import type { Metadata } from 'next';
import { ExperienceForm } from '@/components/host/experience-form';

export const metadata: Metadata = {
  title: 'Create Experience - StayAfrica',
  description: 'Create a new experience on StayAfrica',
};

export default function NewExperiencePage() {
  return (
    <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
      <div className="max-w-3xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        <div className="mb-8">
          <h1 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-2">
            Create an Experience
          </h1>
          <p className="text-lg text-primary-600 dark:text-sand-300">
            Share a unique activity with travelers on StayAfrica
          </p>
        </div>

        <ExperienceForm mode="create" />
      </div>
    </div>
  );
}
