'use client';

import { useState, FormEvent } from 'react';
import { Search } from 'lucide-react';
import { useRouter } from 'next/navigation';

export function MobileSearchBar() {
  const router = useRouter();
  const [location, setLocation] = useState('');
  const [isExpanded, setIsExpanded] = useState(false);

  const handleSearch = (e: FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    
    const params = new URLSearchParams();
    if (location) params.append('city', location);

    router.push(`/explore?${params.toString()}`);
    setIsExpanded(false);
    setLocation('');
  };

  return (
    <>
      {/* Fixed, top-of-screen mobile search bar (Airbnb-like) */}
      <div className="fixed top-0 left-0 right-0 md:hidden z-50 bg-primary-800/95 backdrop-blur border-b border-primary-700 shadow-md">
        <div className="px-3 py-2">
          {isExpanded ? (
            // Expanded search form
            <form onSubmit={handleSearch} className="flex gap-2">
              <input
                type="text"
                value={location}
                onChange={(e) => setLocation(e.target.value)}
                placeholder="Where to go?"
                className="flex-1 px-4 py-2 rounded-lg bg-ivory text-primary-900 placeholder-primary-500 text-sm focus:outline-none focus:ring-2 focus:ring-secondary-500"
                autoFocus
              />
              <button
                type="submit"
                className="bg-secondary-500 hover:bg-secondary-600 text-primary-900 px-3 py-2 rounded-lg transition-colors"
                aria-label="Search"
              >
                <Search className="w-4 h-4" />
              </button>
              <button
                type="button"
                onClick={() => {
                  setIsExpanded(false);
                  setLocation('');
                }}
                className="text-sand-200 hover:text-sand-50 px-3 py-2 transition"
              >
                ✕
              </button>
            </form>
          ) : (
            // Collapsed search button
            <button
              onClick={() => setIsExpanded(true)}
              className="w-full flex items-center gap-2 px-4 py-2 bg-primary-700/50 hover:bg-primary-600/70 rounded-full text-sand-300 text-sm transition"
            >
              <Search className="w-4 h-4 flex-shrink-0" />
              <span className="truncate">Where to?</span>
            </button>
          )}
        </div>
      </div>

      {/* Fullscreen overlay when expanded (simple, mobile-first) */}
      {isExpanded && (
        <div className="fixed inset-0 md:hidden z-40 bg-black/40" onClick={() => setIsExpanded(false)} aria-hidden />
      )}
    </>
  );
}
