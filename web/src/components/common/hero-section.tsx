'use client';

import { useState, FormEvent, ChangeEvent } from 'react';
import { Search, Home, Building2, Building, TreePine, Castle, Wind, Users, Palmtree, Tent, Anchor } from 'lucide-react';
import { useRouter } from 'next/navigation';
import Link from 'next/link';
import { TypeAnimation } from 'react-type-animation';
import { FEATURED_PROPERTY_TYPES, PROPERTY_TYPES } from '@/types/property-types';
import { Input } from '@/components/ui/Input';

const iconMap: Record<string, any> = {
  Home,
  Building2,
  Building,
  TreePine,
  Castle,
  Wind,
  Users,
  Palmtree,
  Tent,
  Anchor,
};

export function HeroSection() {
  const router = useRouter();
  const [location, setLocation] = useState('');
  const [checkIn, setCheckIn] = useState('');
  const [checkOut, setCheckOut] = useState('');
  const [guests, setGuests] = useState('2');
  const [selectedType, setSelectedType] = useState('');
  

  const handleSearch = (e: FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    
    const params = new URLSearchParams();
    if (location) params.append('city', location);
    if (checkIn) params.append('check_in', checkIn);
    if (checkOut) params.append('check_out', checkOut);
    if (guests) params.append('guests', guests);
    if (selectedType) params.append('type', selectedType);

    router.push(`/explore?${params.toString()}`);
  };

  // Create typing sequence for property types
  const typingSequence = [
    'luxury villas',
    2000,
    'cozy B&Bs',
    2000,
    'modern apartments',
    2000,
    'eco-lodges',
    2000,
    'beachfront hotels',
    2000,
    'mountain resorts',
    2000,
  ];

  return (
    <div className="relative overflow-hidden -mt-20 sm:mt-0 z-0 bg-gradient-to-br from-primary-900 via-primary-800 to-primary-700 text-sand-50 py-8 sm:py-16 md:py-24 lg:py-32">
      <div className="absolute inset-0 pointer-events-none" aria-hidden>
        <div className="absolute -top-40 -right-40 h-80 w-80 rounded-full bg-secondary-500/10 blur-3xl hidden sm:block" />
        <div className="absolute -bottom-32 -left-32 h-64 w-64 rounded-full bg-secondary-400/10 blur-2xl hidden sm:block" />
      </div>
      <div className="relative max-w-7xl mx-auto px-3 sm:px-6 lg:px-8">
        {/* Hero Text - Mobile optimized */}
        <div className="text-center mb-8 sm:mb-12">
          <h1 className="text-2xl sm:text-4xl md:text-5xl lg:text-6xl font-bold mb-3 sm:mb-4 tracking-tight text-sand-50 leading-tight">
            Discover Unique Places in Africa
          </h1>
          <div className="text-base sm:text-lg md:text-2xl text-sand-100 max-w-3xl mx-auto min-h-12 sm:min-h-16 flex flex-wrap items-center justify-center gap-1 sm:gap-2">
            <span>From</span>
            <span className="text-secondary-300 font-semibold">
              <TypeAnimation
                sequence={typingSequence}
                wrapper="span"
                cursor
                repeat={Infinity}
                speed={80}
                deletionSpeed={60}
              />
            </span>
          </div>
          <p className="text-sand-200 mt-3 sm:mt-4 text-sm sm:text-base">
            Explore diverse accommodation types across the continent
          </p>
        </div>

        {/* Property Type Quick Select - Mobile optimized */}
        <div className="mb-6 sm:mb-8">
          <p className="text-center text-sand-100 mb-3 sm:mb-4 text-xs sm:text-sm font-medium">Browse by type:</p>
          <div className="grid grid-cols-2 sm:grid-cols-3 md:grid-cols-4 lg:grid-cols-5 gap-2 sm:gap-3 mb-6 sm:mb-8 overflow-x-auto pb-2">
            {FEATURED_PROPERTY_TYPES.map((typeId) => {
              const typeConfig = PROPERTY_TYPES[typeId];
              const Icon = iconMap[typeConfig.icon];
              return (
                <button
                  key={typeId}
                  onClick={() => setSelectedType(typeId)}
                  className={`flex-shrink-0 p-2 sm:p-4 rounded-lg sm:rounded-xl transition-all duration-200 flex flex-col items-center justify-center gap-1 sm:gap-2 min-w-max sm:min-w-fit ${
                    selectedType === typeId
                      ? 'bg-secondary-500 text-primary-900 shadow-lg scale-105'
                      : 'bg-primary-700/50 hover:bg-primary-600/70 text-sand-50 border border-sand-200/20'
                  }`}
                >
                  {Icon && <Icon className="w-5 h-5 sm:w-6 sm:h-6" />}
                  <span className="text-xs sm:text-sm font-semibold text-center line-clamp-1">{typeConfig.label}</span>
                </button>
              );
            })}
          </div>
        </div>

        {/* Airbnb-style Search Bar - Mobile optimized */}
        <form
          onSubmit={handleSearch}
          className="bg-ivory/95 backdrop-blur rounded-2xl sm:rounded-full shadow-elevated border border-primary-200 p-2 sm:p-2 max-w-4xl mx-auto w-full"
        >
          <div className="grid grid-cols-2 sm:grid-cols-2 md:grid-cols-4 gap-1 sm:gap-2 items-stretch">
            {/* Location */}
            <div className="px-2 sm:px-4 py-2 sm:py-3 hover:bg-sand-100/50 rounded-xl sm:rounded-full transition">
              <Input
                type="text"
                value={location}
                onChange={(e) => setLocation(e.target.value)}
                label="Where"
                placeholder="Destination"
              />
            </div>

            {/* Check-in */}
            <div className="px-2 sm:px-4 py-2 sm:py-3 hover:bg-sand-100/50 rounded-xl sm:rounded-full transition border-l border-primary-200">
              <Input
                type="date"
                value={checkIn}
                onChange={(e) => setCheckIn(e.target.value)}
                label="Arrive"
              />
            </div>

            {/* Check-out - Hide on small mobile */}
            <div className="hidden sm:block px-2 sm:px-4 py-2 sm:py-3 hover:bg-sand-100/50 rounded-xl sm:rounded-full transition border-l border-primary-200">
              <Input
                type="date"
                value={checkOut}
                onChange={(e) => setCheckOut(e.target.value)}
                label="Leave"
              />
            </div>

            {/* Guests + Search Button */}
            <div className="flex items-center gap-1 sm:gap-2 pl-1 sm:pl-4 col-span-2 sm:col-span-1">
              <div className="hidden sm:block flex-1">
                <Input
                  select
                  value={guests}
                  onChange={(e: ChangeEvent<HTMLSelectElement>) => setGuests(e.target.value)}
                  label="Guests"
                  options={[1,2,3,4,5,6,8,10].map((n) => ({ value: String(n), label: String(n) }))}
                />
              </div>
              <button 
                type="submit" 
                className="bg-secondary-500 hover:bg-secondary-600 text-primary-900 p-2 sm:p-3 rounded-lg sm:rounded-full transition-colors flex-shrink-0"
                aria-label="Search"
              >
                <Search className="w-4 h-4 sm:w-5 sm:h-5" />
              </button>
            </div>
          </div>
        </form>

        

        {/* Host CTA */}
        <div className="mt-6 sm:mt-8 text-center">
          <Link 
            href="/host"
            className="inline-flex items-center justify-center gap-2 text-sand-100 hover:text-secondary-300 transition font-medium text-xs sm:text-sm"
          >
            <span>Become a Host</span>
            <svg className="w-3 h-3 sm:w-4 sm:h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 5l7 7-7 7" />
            </svg>
          </Link>
        </div>
      </div>
    </div>
  );
}

