'use client';

import { Star, Heart } from 'lucide-react';

export function HeroSection() {
  return (
    <div className="bg-gradient-to-r from-primary-600 via-primary-700 to-primary-800 text-white py-20 md:py-32">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 text-center">
        <h1 className="text-4xl md:text-6xl font-bold mb-4 animate-fade-in">
          Discover Your Next Adventure in Africa
        </h1>
        
        <p className="text-xl md:text-2xl text-primary-100 mb-8 max-w-2xl mx-auto">
          Find unique accommodations, connect with local hosts, and create unforgettable memories across the continent.
        </p>

        <div className="flex flex-col sm:flex-row justify-center gap-4 mb-12">
          <button className="px-8 py-3 bg-white text-primary-600 font-semibold rounded-lg hover:bg-primary-50 transition">
            Explore Now
          </button>
          <button className="px-8 py-3 border-2 border-white text-white font-semibold rounded-lg hover:bg-white hover:text-primary-600 transition">
            Become a Host
          </button>
        </div>

        {/* Stats */}
        <div className="grid grid-cols-3 gap-4 md:gap-8 max-w-2xl mx-auto">
          <div>
            <div className="text-3xl md:text-4xl font-bold">500+</div>
            <div className="text-primary-100">Properties</div>
          </div>
          <div>
            <div className="text-3xl md:text-4xl font-bold">10K+</div>
            <div className="text-primary-100">Happy Guests</div>
          </div>
          <div>
            <div className="text-3xl md:text-4xl font-bold">4.8★</div>
            <div className="text-primary-100">Average Rating</div>
          </div>
        </div>
      </div>
    </div>
  );
}
