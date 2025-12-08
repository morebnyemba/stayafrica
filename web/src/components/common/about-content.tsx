'use client';

import { Globe, Shield, Heart, Award } from 'lucide-react';

export function AboutContent() {
  return (
    <div className="bg-sand-100 dark:bg-primary-900">
      {/* Hero Section */}
      <div className="bg-gradient-to-br from-primary-900 via-primary-800 to-primary-700 text-sand-50 py-20">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 text-center">
          <h1 className="text-4xl md:text-5xl font-bold mb-6 text-sand-50">
            About StayAfrica
          </h1>
          <p className="text-xl text-sand-100 max-w-3xl mx-auto">
            Connecting travelers with authentic African experiences through unique accommodations and local hospitality.
          </p>
        </div>
      </div>

      {/* Mission Section */}
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-16">
        <div className="grid md:grid-cols-2 gap-12 items-center mb-20">
          <div>
            <h2 className="text-3xl font-bold text-primary-900 dark:text-sand-50 mb-6">
              Our Mission
            </h2>
            <p className="text-lg text-primary-700 dark:text-sand-200 mb-4">
              StayAfrica is dedicated to making travel across Africa accessible, authentic, and unforgettable. 
              We believe in the power of hospitality to bridge cultures and create lasting memories.
            </p>
            <p className="text-lg text-primary-700 dark:text-sand-200">
              Whether you&apos;re seeking a luxury safari lodge, a coastal villa, or a city apartment, 
              we connect you with verified hosts who are passionate about sharing their corner of Africa with the world.
            </p>
          </div>
          <div className="card p-8">
            <div className="grid grid-cols-2 gap-6">
              <div className="text-center">
                <div className="text-4xl font-bold text-secondary-600 mb-2">500+</div>
                <div className="text-sm text-primary-600 dark:text-sand-300">Properties</div>
              </div>
              <div className="text-center">
                <div className="text-4xl font-bold text-secondary-600 mb-2">15</div>
                <div className="text-sm text-primary-600 dark:text-sand-300">Countries</div>
              </div>
              <div className="text-center">
                <div className="text-4xl font-bold text-secondary-600 mb-2">10K+</div>
                <div className="text-sm text-primary-600 dark:text-sand-300">Happy Guests</div>
              </div>
              <div className="text-center">
                <div className="text-4xl font-bold text-secondary-600 mb-2">4.8★</div>
                <div className="text-sm text-primary-600 dark:text-sand-300">Avg Rating</div>
              </div>
            </div>
          </div>
        </div>

        {/* Values Section */}
        <div className="mb-20">
          <h2 className="text-3xl font-bold text-primary-900 dark:text-sand-50 mb-12 text-center">
            Our Values
          </h2>
          <div className="grid md:grid-cols-4 gap-8">
            <div className="card p-6 text-center">
              <div className="inline-flex items-center justify-center w-16 h-16 bg-secondary-100 dark:bg-secondary-900 rounded-full mb-4">
                <Globe className="w-8 h-8 text-secondary-600" />
              </div>
              <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-3">
                Authentic Experiences
              </h3>
              <p className="text-primary-600 dark:text-sand-300">
                We showcase the real Africa through genuine local connections and unique stays.
              </p>
            </div>
            <div className="card p-6 text-center">
              <div className="inline-flex items-center justify-center w-16 h-16 bg-secondary-100 dark:bg-secondary-900 rounded-full mb-4">
                <Shield className="w-8 h-8 text-secondary-600" />
              </div>
              <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-3">
                Trust & Safety
              </h3>
              <p className="text-primary-600 dark:text-sand-300">
                All properties are verified, and secure payment processing ensures peace of mind.
              </p>
            </div>
            <div className="card p-6 text-center">
              <div className="inline-flex items-center justify-center w-16 h-16 bg-secondary-100 dark:bg-secondary-900 rounded-full mb-4">
                <Heart className="w-8 h-8 text-secondary-600" />
              </div>
              <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-3">
                Community First
              </h3>
              <p className="text-primary-600 dark:text-sand-300">
                We support local economies by empowering hosts and promoting sustainable tourism.
              </p>
            </div>
            <div className="card p-6 text-center">
              <div className="inline-flex items-center justify-center w-16 h-16 bg-secondary-100 dark:bg-secondary-900 rounded-full mb-4">
                <Award className="w-8 h-8 text-secondary-600" />
              </div>
              <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-3">
                Excellence
              </h3>
              <p className="text-primary-600 dark:text-sand-300">
                We maintain high standards for all listings and provide exceptional customer service.
              </p>
            </div>
          </div>
        </div>

        {/* CTA Section */}
        <div className="card p-12 text-center bg-gradient-to-br from-primary-800 to-primary-700 border-0">
          <h2 className="text-3xl font-bold text-sand-50 mb-4">
            Ready to Start Your African Adventure?
          </h2>
          <p className="text-xl text-sand-100 mb-8 max-w-2xl mx-auto">
            Join thousands of travelers who have discovered the magic of Africa through StayAfrica.
          </p>
          <div className="flex flex-col sm:flex-row justify-center gap-4">
            <a href="/explore" className="btn-primary px-8 py-3 text-lg">
              Explore Properties
            </a>
            <a href="/host" className="btn-secondary px-8 py-3 text-lg border-2 text-sand-100 border-sand-100 hover:bg-sand-100/10">
              Become a Host
            </a>
          </div>
        </div>
      </div>
    </div>
  );
}
