'use client';

import { useState, FormEvent } from 'react';
import { Search } from 'lucide-react';
import { useRouter } from 'next/navigation';
import Link from 'next/link';

export function HeroSection() {
  const router = useRouter();
  const [location, setLocation] = useState('');
  const [checkIn, setCheckIn] = useState('');
  const [checkOut, setCheckOut] = useState('');
  const [guests, setGuests] = useState('2');

  const handleSearch = (e: FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    
    const params = new URLSearchParams();
    if (location) params.append('city', location);
    if (checkIn) params.append('check_in', checkIn);
    if (checkOut) params.append('check_out', checkOut);
    if (guests) params.append('guests', guests);

    router.push(`/explore?${params.toString()}`);
  };

  return (
    <div className="relative overflow-hidden bg-gradient-to-br from-primary-900 via-primary-800 to-primary-700 text-sand-50 py-16 md:py-24">
      <div className="absolute inset-0 pointer-events-none" aria-hidden>
        <div className="absolute -top-20 right-20 h-64 w-64 rounded-full bg-secondary-500/20 blur-3xl" />
        <div className="absolute bottom-0 left-10 h-48 w-48 rounded-full bg-secondary-400/10 blur-2xl" />
      </div>
      <div className="relative max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="text-center mb-8">
          <h1 className="text-3xl md:text-5xl font-bold mb-3 animate-fade-in tracking-tight text-sand-50">
            Find your next stay in Africa
          </h1>
          <p className="text-lg md:text-xl text-sand-100 max-w-2xl mx-auto">
            Discover unique homes across the continent
          </p>
        </div>

        {/* Airbnb-style Search Bar */}
        <form
          onSubmit={handleSearch}
          className="bg-ivory/95 backdrop-blur rounded-full shadow-elevated border border-primary-200 p-2 max-w-4xl mx-auto"
        >
          <div className="grid grid-cols-1 md:grid-cols-4 gap-2 items-center">
            {/* Location */}
            <div className="px-4 py-3 hover:bg-sand-100/50 rounded-full cursor-pointer transition">
              <div className="text-xs font-semibold text-primary-900">Where</div>
              <input
                type="text"
                value={location}
                onChange={(e) => setLocation(e.target.value)}
                placeholder="Search destinations"
                className="w-full bg-transparent border-none text-sm text-primary-800 placeholder-primary-500 focus:outline-none p-0"
              />
            </div>

            {/* Check-in */}
            <div className="px-4 py-3 hover:bg-sand-100/50 rounded-full cursor-pointer transition border-l border-primary-200">
              <div className="text-xs font-semibold text-primary-900">Check in</div>
              <input
                type="date"
                value={checkIn}
                onChange={(e) => setCheckIn(e.target.value)}
                className="w-full bg-transparent border-none text-sm text-primary-800 focus:outline-none p-0"
              />
            </div>

            {/* Check-out */}
            <div className="px-4 py-3 hover:bg-sand-100/50 rounded-full cursor-pointer transition border-l border-primary-200">
              <div className="text-xs font-semibold text-primary-900">Check out</div>
              <input
                type="date"
                value={checkOut}
                onChange={(e) => setCheckOut(e.target.value)}
                className="w-full bg-transparent border-none text-sm text-primary-800 focus:outline-none p-0"
              />
            </div>

            {/* Guests + Search Button */}
            <div className="flex items-center gap-2 pl-4">
              <div className="flex-1 hover:bg-sand-100/50 rounded-full px-4 py-3 cursor-pointer transition border-l border-primary-200">
                <div className="text-xs font-semibold text-primary-900">Guests</div>
                <select 
                  value={guests} 
                  onChange={(e) => setGuests(e.target.value)} 
                  className="w-full bg-transparent border-none text-sm text-primary-800 focus:outline-none p-0 cursor-pointer"
                >
                  {[1, 2, 3, 4, 5, 6, 8, 10].map((num) => (
                    <option key={num} value={num}>
                      {num} {num === 1 ? 'guest' : 'guests'}
                    </option>
                  ))}
                </select>
              </div>
              <button 
                type="submit" 
                className="bg-secondary-500 hover:bg-secondary-600 text-primary-900 p-4 rounded-full transition-colors"
                aria-label="Search"
              >
                <Search className="w-5 h-5" />
              </button>
            </div>
          </div>
        </form>

        {/* Host CTA */}
        <div className="mt-8 text-center">
          <Link 
            href="/host"
            className="inline-flex items-center gap-2 text-sand-100 hover:text-secondary-300 transition font-medium text-sm"
          >
            <span>Become a Host</span>
            <svg className="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 5l7 7-7 7" />
            </svg>
          </Link>
        </div>
      </div>
    </div>
  );
}
