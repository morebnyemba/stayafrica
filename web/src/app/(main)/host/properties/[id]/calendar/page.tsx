import type { Metadata } from 'next';
import { ProtectedRoute } from '@/components/auth/protected-route';

export const metadata: Metadata = {
  title: 'Manage Calendar - StayAfrica',
  description: 'Manage property availability calendar',
};

export default function PropertyCalendarPage({ params }: { params: { id: string } }) {
  return (
    <ProtectedRoute requiredRole="host">
      <div className="max-w-4xl mx-auto px-4 py-12">
        <h1 className="text-3xl font-bold mb-8">Manage Calendar</h1>
        <div className="bg-white rounded-lg p-8 border border-gray-200 dark:bg-primary-900 dark:border-primary-700">
          <p className="text-gray-600 dark:text-gray-400">
            Calendar management for property {params.id} - coming soon...
          </p>
        </div>
      </div>
    </ProtectedRoute>
  );
}
