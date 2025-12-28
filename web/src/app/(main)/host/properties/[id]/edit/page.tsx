import type { Metadata } from 'next';
import { ProtectedRoute } from '@/components/auth/protected-route';

export const metadata: Metadata = {
  title: 'Edit Property - StayAfrica',
  description: 'Edit your property details',
};

export default function EditPropertyPage({ params }: { params: { id: string } }) {
  return (
    <ProtectedRoute requiredRole="host">
      <div className="max-w-4xl mx-auto px-4 py-12">
        <h1 className="text-3xl font-bold mb-8">Edit Property</h1>
        <div className="bg-white rounded-lg p-8 border border-gray-200 dark:bg-primary-900 dark:border-primary-700">
          <p className="text-gray-600 dark:text-gray-400">
            Edit property details for property {params.id} - coming soon...
          </p>
        </div>
      </div>
    </ProtectedRoute>
  );
}
