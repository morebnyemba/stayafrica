'use client';

import { Star, Heart } from 'lucide-react';

export function HeroSection() {
  return (
    <div className="relative overflow-hidden bg-gradient-to-r from-primary-900 via-primary-800 to-primary-700 text-sand-50 py-20 md:py-32">
      <div className="absolute inset-0 pointer-events-none" aria-hidden>
        <div className="absolute -top-20 right-20 h-64 w-64 rounded-full bg-secondary-500/20 blur-3xl" />
        <div className="absolute bottom-0 left-10 h-48 w-48 rounded-full bg-secondary-400/10 blur-2xl" />
      </div>
      <div className="relative max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 text-center">
        <h1 className="text-4xl md:text-6xl font-bold mb-4 animate-fade-in tracking-tight">
          Discover Your Next Adventure in Africa
        </h1>
        
        <p className="text-xl md:text-2xl text-secondary-100 mb-8 max-w-2xl mx-auto">
          Find unique accommodations, connect with local hosts, and create unforgettable memories across the continent.
        </p>

        <div className="flex flex-col sm:flex-row justify-center gap-4 mb-12">
          <button className="btn-primary px-8 py-3">
            Explore Now
          </button>
          <button className="btn-secondary px-8 py-3 border-2">
            Become a Host
          </button>
        </div>

        {/* Stats */}
        <div className="grid grid-cols-3 gap-4 md:gap-8 max-w-2xl mx-auto">
          <div>
            <div className="text-3xl md:text-4xl font-bold">500+</div>
            <div className="text-secondary-100">Properties</div>
          </div>
          <div>
            <div className="text-3xl md:text-4xl font-bold">10K+</div>
            <div className="text-secondary-100">Happy Guests</div>
          </div>
          <div>
            <div className="text-3xl md:text-4xl font-bold">4.8★</div>
            <div className="text-secondary-100">Average Rating</div>
          </div>
        </div>
      </div>
    </div>
  );
}
