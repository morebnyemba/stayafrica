'use client';

import { Building, DollarSign, Calendar, TrendingUp, CheckCircle, Loader2 } from 'lucide-react';
import { useAuth } from '@/context/auth-context';
import { useRouter } from 'next/navigation';
import { useState } from 'react';

export function HostContent() {
  const { user, isAuthenticated, upgradeToHost } = useAuth();
  const router = useRouter();
  const [hostLoading, setHostLoading] = useState(false);

  const handleGetStarted = async () => {
    if (!isAuthenticated) {
      router.push('/register');
      return;
    }

    if (user?.role === 'host') {
      router.push('/dashboard/host');
      return;
    }

    try {
      setHostLoading(true);
      await upgradeToHost();
      router.push('/dashboard/host');
    } catch (error) {
      console.error('Failed to upgrade to host:', error);
      router.push('/host');
    } finally {
      setHostLoading(false);
    }
  };

  return (
    <div className="bg-sand-100 dark:bg-primary-900">
      {/* Hero Section */}
      <div className="bg-gradient-to-br from-primary-900 via-primary-800 to-primary-700 text-sand-50 py-20">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="grid md:grid-cols-2 gap-12 items-center">
            <div>
              <h1 className="text-4xl md:text-5xl font-bold mb-6 text-sand-50">
                Become a StayAfrica Host
              </h1>
              <p className="text-xl text-sand-100 mb-8">
                Share your space with travelers from around the world and earn extra income while showcasing the beauty of Africa.
              </p>
              <button
                onClick={handleGetStarted}
                className="btn-primary px-8 py-4 text-lg font-semibold inline-flex items-center gap-2"
                disabled={hostLoading}
              >
                {hostLoading ? <Loader2 className="w-5 h-5 animate-spin" /> : null}
                <span>Get Started</span>
              </button>
            </div>
            <div className="card p-8">
              <h3 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-6">
                Potential Monthly Earnings
              </h3>
              <div className="space-y-4">
                <div className="flex justify-between items-center">
                  <span className="text-primary-700 dark:text-sand-200">Private Room</span>
                  <span className="text-2xl font-bold text-secondary-600">$300+</span>
                </div>
                <div className="flex justify-between items-center">
                  <span className="text-primary-700 dark:text-sand-200">Entire Apartment</span>
                  <span className="text-2xl font-bold text-secondary-600">$800+</span>
                </div>
                <div className="flex justify-between items-center">
                  <span className="text-primary-700 dark:text-sand-200">Luxury Villa</span>
                  <span className="text-2xl font-bold text-secondary-600">$2,000+</span>
                </div>
              </div>
              <p className="text-sm text-primary-600 dark:text-sand-300 mt-6">
                * Estimates based on average bookings in your area
              </p>
            </div>
          </div>
        </div>
      </div>

      {/* Benefits Section */}
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-16">
        <div className="text-center mb-12">
          <h2 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-4">
            Why Host on StayAfrica?
          </h2>
          <p className="text-lg text-primary-600 dark:text-sand-200 max-w-2xl mx-auto">
            Join hundreds of successful hosts who are earning income and meeting amazing people.
          </p>
        </div>

        <div className="grid md:grid-cols-4 gap-8 mb-20">
          <div className="card p-6 text-center">
            <div className="inline-flex items-center justify-center w-16 h-16 bg-secondary-100 dark:bg-secondary-900 rounded-full mb-4">
              <DollarSign className="w-8 h-8 text-secondary-600" />
            </div>
            <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-3">
              Earn Extra Income
            </h3>
            <p className="text-primary-600 dark:text-sand-300">
              Set your own rates and earn money from your unused space.
            </p>
          </div>
          <div className="card p-6 text-center">
            <div className="inline-flex items-center justify-center w-16 h-16 bg-secondary-100 dark:bg-secondary-900 rounded-full mb-4">
              <Calendar className="w-8 h-8 text-secondary-600" />
            </div>
            <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-3">
              Full Control
            </h3>
            <p className="text-primary-600 dark:text-sand-300">
              Choose when your space is available and who can book.
            </p>
          </div>
          <div className="card p-6 text-center">
            <div className="inline-flex items-center justify-center w-16 h-16 bg-secondary-100 dark:bg-secondary-900 rounded-full mb-4">
              <Building className="w-8 h-8 text-secondary-600" />
            </div>
            <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-3">
              Easy Setup
            </h3>
            <p className="text-primary-600 dark:text-sand-300">
              List your property in minutes with our simple onboarding process.
            </p>
          </div>
          <div className="card p-6 text-center">
            <div className="inline-flex items-center justify-center w-16 h-16 bg-secondary-100 dark:bg-secondary-900 rounded-full mb-4">
              <TrendingUp className="w-8 h-8 text-secondary-600" />
            </div>
            <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-3">
              Marketing Support
            </h3>
            <p className="text-primary-600 dark:text-sand-300">
              We promote your listing to travelers across Africa and beyond.
            </p>
          </div>
        </div>

        {/* How It Works */}
        <div className="mb-20">
          <h2 className="text-3xl font-bold text-primary-900 dark:text-sand-50 mb-12 text-center">
            How It Works
          </h2>
          <div className="grid md:grid-cols-3 gap-8">
            <div className="text-center">
              <div className="inline-flex items-center justify-center w-16 h-16 bg-secondary-500 text-primary-900 rounded-full mb-4 text-2xl font-bold">
                1
              </div>
              <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-3">
                Create Your Listing
              </h3>
              <p className="text-primary-600 dark:text-sand-300">
                Add photos, description, amenities, and set your pricing.
              </p>
            </div>
            <div className="text-center">
              <div className="inline-flex items-center justify-center w-16 h-16 bg-secondary-500 text-primary-900 rounded-full mb-4 text-2xl font-bold">
                2
              </div>
              <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-3">
                Receive Bookings
              </h3>
              <p className="text-primary-600 dark:text-sand-300">
                Guests discover your property and make secure bookings.
              </p>
            </div>
            <div className="text-center">
              <div className="inline-flex items-center justify-center w-16 h-16 bg-secondary-500 text-primary-900 rounded-full mb-4 text-2xl font-bold">
                3
              </div>
              <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-3">
                Welcome Guests & Earn
              </h3>
              <p className="text-primary-600 dark:text-sand-300">
                Host amazing travelers and get paid directly to your account.
              </p>
            </div>
          </div>
        </div>

        {/* Features */}
        <div className="card p-12 mb-12">
          <h2 className="text-3xl font-bold text-primary-900 dark:text-sand-50 mb-8 text-center">
            What You Get as a Host
          </h2>
          <div className="grid md:grid-cols-2 gap-6">
            <div className="flex items-start gap-3">
              <CheckCircle className="w-6 h-6 text-secondary-600 flex-shrink-0 mt-1" />
              <div>
                <h4 className="font-semibold text-primary-900 dark:text-sand-50 mb-1">Secure Payments</h4>
                <p className="text-primary-600 dark:text-sand-300">Get paid on time via mobile money, bank transfer, or other methods.</p>
              </div>
            </div>
            <div className="flex items-start gap-3">
              <CheckCircle className="w-6 h-6 text-secondary-600 flex-shrink-0 mt-1" />
              <div>
                <h4 className="font-semibold text-primary-900 dark:text-sand-50 mb-1">24/7 Support</h4>
                <p className="text-primary-600 dark:text-sand-300">Our support team is here to help you succeed.</p>
              </div>
            </div>
            <div className="flex items-start gap-3">
              <CheckCircle className="w-6 h-6 text-secondary-600 flex-shrink-0 mt-1" />
              <div>
                <h4 className="font-semibold text-primary-900 dark:text-sand-50 mb-1">Host Protection</h4>
                <p className="text-primary-600 dark:text-sand-300">Coverage for property damage up to $1 million.</p>
              </div>
            </div>
            <div className="flex items-start gap-3">
              <CheckCircle className="w-6 h-6 text-secondary-600 flex-shrink-0 mt-1" />
              <div>
                <h4 className="font-semibold text-primary-900 dark:text-sand-50 mb-1">Verified Guests</h4>
                <p className="text-primary-600 dark:text-sand-300">All guests are verified with ID and reviews.</p>
              </div>
            </div>
            <div className="flex items-start gap-3">
              <CheckCircle className="w-6 h-6 text-secondary-600 flex-shrink-0 mt-1" />
              <div>
                <h4 className="font-semibold text-primary-900 dark:text-sand-50 mb-1">Professional Tools</h4>
                <p className="text-primary-600 dark:text-sand-300">Manage bookings, calendar, and pricing with ease.</p>
              </div>
            </div>
            <div className="flex items-start gap-3">
              <CheckCircle className="w-6 h-6 text-secondary-600 flex-shrink-0 mt-1" />
              <div>
                <h4 className="font-semibold text-primary-900 dark:text-sand-50 mb-1">Analytics Dashboard</h4>
                <p className="text-primary-600 dark:text-sand-300">Track your earnings, views, and booking performance.</p>
              </div>
            </div>
          </div>
        </div>

        {/* CTA */}
        <div className="text-center">
          <h2 className="text-3xl font-bold text-primary-900 dark:text-sand-50 mb-4">
            Ready to Get Started?
          </h2>
          <p className="text-lg text-primary-600 dark:text-sand-200 mb-8 max-w-2xl mx-auto">
            Join StayAfrica today and start earning from your property.
          </p>
          <button className="btn-primary px-10 py-4 text-lg font-semibold">
            List Your Property
          </button>
        </div>
      </div>
    </div>
  );
}
