import type { Metadata } from 'next';
import { VerificationWizard } from '@/components/verification/VerificationWizard';

export const metadata: Metadata = {
  title: 'Identity Verification - StayAfrica',
  description: 'Verify your identity to become a trusted host on StayAfrica',
};

export default function VerificationPage() {
  return (
    <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        <div className="mb-8">
          <h1 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-2">
            Identity Verification
          </h1>
          <p className="text-lg text-primary-600 dark:text-sand-300">
            Verify your identity to build trust with guests
          </p>
        </div>
        <VerificationWizard />
      </div>
    </div>
  );
}
