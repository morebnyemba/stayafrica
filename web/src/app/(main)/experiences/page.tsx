import type { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'Experiences - StayAfrica',
  description: 'Discover unique experiences and activities across Africa',
};

export default function ExperiencesPage() {
  return (
    <div className="bg-sand-100 dark:bg-primary-900 min-h-screen">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
        <h1 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-4">
          Experiences
        </h1>
        <p className="text-lg text-primary-600 dark:text-sand-200 mb-8">
          Discover unique activities and adventures across Africa
        </p>
        
        <div className="card p-12 text-center">
          <div className="max-w-md mx-auto">
            <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-4">
              Coming Soon
            </h2>
            <p className="text-primary-600 dark:text-sand-300">
              We&apos;re working on bringing you amazing experiences. Check back soon!
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}
