'use client';

import { useState, FormEvent } from 'react';
import { MapPin, Calendar, Users } from 'lucide-react';
import { useRouter } from 'next/navigation';

export function SearchSection() {
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
    <div className="bg-gradient-to-r from-primary-900 via-primary-800 to-primary-700 py-12 -mt-32 relative z-20 text-sand-50">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <form
          onSubmit={handleSearch}
          className="bg-ivory/95 backdrop-blur rounded-3xl shadow-elevated border border-primary-200 p-6 md:p-8"
        >
          <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
            {/* Location */}
            <div>
              <label className="block text-sm font-medium text-primary-800 mb-2">
                <MapPin className="w-4 h-4 inline mr-2" />
                Location
              </label>
              <input
                type="text"
                value={location}
                onChange={(e) => setLocation(e.target.value)}
                placeholder="Where to?"
                className="input-base"
              />
            </div>

            {/* Check-in */}
            <div>
              <label className="block text-sm font-medium text-primary-800 mb-2">
                <Calendar className="w-4 h-4 inline mr-2" />
                Check-in
              </label>
              <input
                type="date"
                value={checkIn}
                onChange={(e) => setCheckIn(e.target.value)}
                className="input-base"
              />
            </div>

            {/* Check-out */}
            <div>
              <label className="block text-sm font-medium text-primary-800 mb-2">
                <Calendar className="w-4 h-4 inline mr-2" />
                Check-out
              </label>
              <input
                type="date"
                value={checkOut}
                onChange={(e) => setCheckOut(e.target.value)}
                className="input-base"
              />
            </div>

            {/* Guests */}
            <div>
              <label className="block text-sm font-medium text-primary-800 mb-2">
                <Users className="w-4 h-4 inline mr-2" />
                Guests
              </label>
              <select value={guests} onChange={(e) => setGuests(e.target.value)} className="input-base">
                {[1, 2, 3, 4, 5, 6, 8, 10].map((num) => (
                  <option key={num} value={num}>
                    {num} guest{num > 1 ? 's' : ''}
                  </option>
                ))}
              </select>
            </div>
          </div>

          {/* Submit Button */}
          <div className="mt-6 flex flex-col md:flex-row gap-4">
            <button type="submit" className="flex-1 md:flex-none md:w-auto btn-primary">
              Search
            </button>
            <button
              type="button"
              className="flex-1 md:flex-none md:w-auto btn-secondary"
              onClick={() => {
                setLocation('');
                setCheckIn('');
                setCheckOut('');
                setGuests('2');
              }}
            >
              Clear Filters
            </button>
          </div>
        </form>
      </div>
    </div>
  );
}
