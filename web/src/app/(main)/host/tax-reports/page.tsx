import type { Metadata } from 'next';
import { HostTaxReport } from '@/components/tax/HostTaxReport';

export const metadata: Metadata = {
  title: 'Tax Reports - StayAfrica',
  description: 'View your tax reports and documents on StayAfrica',
};

export default function TaxReportsPage() {
  return (
    <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        <div className="mb-8">
          <h1 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-2">
            Tax Reports
          </h1>
          <p className="text-lg text-primary-600 dark:text-sand-300">
            View and export your tax documents for compliance
          </p>
        </div>
        <HostTaxReport />
      </div>
    </div>
  );
}
