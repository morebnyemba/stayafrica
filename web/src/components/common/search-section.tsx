'use client';

import { useState, FormEvent } from 'react';
import { MapPin, Calendar, Users } from 'lucide-react';
import { Button } from '@/components/ui/Button';
import { Input } from '@/components/ui/Input';
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
              <Input
                type="text"
                value={location}
                onChange={(e) => setLocation(e.target.value)}
                placeholder="Where to?"
                label="Location"
                icon={<MapPin className="w-4 h-4" />}
              />
            </div>

            {/* Check-in */}
            <div>
              <Input
                type="date"
                value={checkIn}
                onChange={(e) => setCheckIn(e.target.value)}
                label="Check-in"
                icon={<Calendar className="w-4 h-4" />}
              />
            </div>

            {/* Check-out */}
            <div>
              <Input
                type="date"
                value={checkOut}
                onChange={(e) => setCheckOut(e.target.value)}
                label="Check-out"
                icon={<Calendar className="w-4 h-4" />}
              />
            </div>

            {/* Guests */}
            <div>
              <Input
                select
                value={guests}
                onChange={(e) => setGuests(e.target.value)}
                label="Guests"
                icon={<Users className="w-4 h-4" />}
                options={[1, 2, 3, 4, 5, 6, 8, 10].map((num) => ({
                  value: String(num),
                  label: `${num} guest${num > 1 ? 's' : ''}`,
                }))}
              />
            </div>
          </div>

          {/* Submit Button */}
          <div className="mt-6 flex flex-col md:flex-row gap-4">
              <Button type="submit" variant="primary" size="lg" className="flex-1 md:flex-none md:w-auto">
                Search
              </Button>
              <Button
                type="button"
                variant="secondary"
                size="lg"
                className="flex-1 md:flex-none md:w-auto"
                onClick={() => {
                  setLocation('');
                  setCheckIn('');
                  setCheckOut('');
                  setGuests('2');
                }}
              >
                Clear Filters
              </Button>
          </div>
        </form>
      </div>
    </div>
  );
}
