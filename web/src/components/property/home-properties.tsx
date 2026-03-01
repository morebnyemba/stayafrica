'use client';

import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { MapPin, Star, Home, Users, Building2, Trees, Shield, UtensilsCrossed, Tent, Waves, Loader2 } from 'lucide-react';
import { Button } from '@/components/ui';
import { PropertyListSkeleton } from './property-card-skeleton';
import Link from 'next/link';
import { useAuth } from '@/store/auth-store';
import { useRouter } from 'next/navigation';
import { useState } from 'react';

export function HomeProperties() {
  const { user, isAuthenticated, upgradeToHost } = useAuth();
  const router = useRouter();
  const [hostLoading, setHostLoading] = useState(false);

  const { data: propertiesData, isLoading } = useQuery({
    queryKey: ['home-properties'],
    queryFn: async () => {
      const response = await apiClient.getProperties({ per_page: 12 });
      return response.data;
    },
  });

  const properties = propertiesData?.results || [];

  // Group properties by location for Airbnb-style display
  const locations = [
    { name: 'Cape Town, South Africa', image: 'https://images.unsplash.com/photo-1580060839134-75a5edca2e99' },
    { name: 'Victoria Falls, Zimbabwe', image: 'https://images.unsplash.com/photo-1516426122078-c23e76319801' },
    { name: 'Zanzibar, Tanzania', image: 'https://images.unsplash.com/photo-1568454537842-d933259bb258' },
    { name: 'Marrakech, Morocco', image: 'https://images.unsplash.com/photo-1597212618440-806262de4f6b' },
  ];

  const handleHostUpgrade = async () => {
    if (!isAuthenticated) {
      router.push('/host');
      return;
    }

    if (user?.role === 'host') {
      router.push('/host/dashboard');
      return;
    }

    try {
      setHostLoading(true);
      await upgradeToHost();
      router.push('/host/dashboard');
    } catch (error) {
      console.error('Failed to upgrade to host:', error);
      router.push('/host');
    } finally {
      setHostLoading(false);
    }
  };

  return (
    <div className="bg-sand-100 dark:bg-primary-900">
      {/* Inspiration Section - Popular Locations */}
      <section className="max-w-7xl mx-auto px-3 sm:px-6 lg:px-8 py-8 sm:py-12">
        <h2 className="text-xl sm:text-2xl md:text-3xl font-bold text-primary-900 dark:text-sand-50 mb-6 sm:mb-8">
          Inspiration for your next trip
        </h2>
        <div className="grid grid-cols-2 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-2 sm:gap-4">
          {locations.map((location) => (
            <Link
              key={location.name}
              href={`/explore?city=${encodeURIComponent(location.name)}`}
              className="group cursor-pointer"
            >
              <div className="relative h-40 sm:h-48 md:h-64 lg:h-72 rounded-lg sm:rounded-2xl overflow-hidden mb-2 sm:mb-3">
                {/* eslint-disable-next-line @next/next/no-img-element */}
                <img
                  src={location.image}
                  alt={location.name}
                  loading="lazy"
                  className="w-full h-full object-cover transition duration-300 group-hover:scale-105"
                />
                <div className="absolute inset-0 bg-gradient-to-t from-primary-900/70 via-primary-900/20 to-transparent" />
                <div className="absolute bottom-2 sm:bottom-4 left-2 sm:left-4 text-white">
                  <h3 className="text-sm sm:text-base md:text-lg font-semibold line-clamp-2 text-white">{location.name}</h3>
                </div>
              </div>
            </Link>
          ))}
        </div>
      </section>

      {/* Featured Properties */}
      <section className="max-w-7xl mx-auto px-3 sm:px-6 lg:px-8 py-8 sm:py-12">
        <div className="flex justify-between items-center mb-6 sm:mb-8">
          <h2 className="text-xl sm:text-2xl md:text-3xl font-bold text-primary-900 dark:text-sand-50">
            Stays nearby
          </h2>
          <Link
            href="/explore"
            className="text-secondary-600 hover:text-secondary-700 font-semibold underline text-xs sm:text-sm"
          >
            Show all
          </Link>
        </div>

        {isLoading ? (
          <PropertyListSkeleton count={6} />
        ) : properties.length === 0 ? (
          <div className="card p-8 sm:p-12 text-center">
            <MapPin className="w-12 sm:w-16 h-12 sm:h-16 text-primary-400 dark:text-sand-500 mx-auto mb-4" />
            <h3 className="text-lg sm:text-xl font-semibold text-primary-900 dark:text-sand-50 mb-2">
              No Properties Available
            </h3>
            <p className="text-sm sm:text-base text-primary-600 dark:text-sand-300">
              Check back soon for amazing stays!
            </p>
          </div>
        ) : (
          <div className="grid grid-cols-2 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-3 sm:gap-6">
            {properties.slice(0, 8).map((property: any) => (
              <Link
                key={property.id}
                href={`/properties/${property.id}`}
                className="group cursor-pointer"
              >
                <div className="bg-white dark:bg-primary-800 rounded-xl overflow-hidden shadow-md hover:shadow-2xl transition-all duration-300 border border-primary-100 dark:border-primary-700">
                  {/* Image Container */}
                  <div className="relative h-48 sm:h-52 md:h-60 overflow-hidden">
                    <img
                      src={property.images?.[0]?.image_url || property.main_image || 'https://images.unsplash.com/photo-1512917774080-9991f1c52e1d'}
                      alt={property.title}
                      className="w-full h-full object-cover transition duration-500 group-hover:scale-110"
                    />
                    <div className="absolute inset-0 bg-gradient-to-t from-black/60 via-black/0 to-transparent opacity-0 group-hover:opacity-100 transition-opacity duration-300" />
                    {property.average_rating && (
                      <div className="absolute top-3 right-3 flex items-center gap-1 bg-white/98 dark:bg-primary-900/98 backdrop-blur-sm px-2.5 py-1.5 rounded-lg shadow-lg">
                        <Star className="w-3.5 h-3.5 text-secondary-500" fill="currentColor" />
                        <span className="text-sm font-bold text-primary-900 dark:text-sand-50">{property.average_rating.toFixed(1)}</span>
                      </div>
                    )}
                  </div>

                  {/* Content */}
                  <div className="p-4">
                    <div className="mb-2">
                      <h3 className="font-bold text-base text-primary-900 dark:text-sand-50 group-hover:text-secondary-600 dark:group-hover:text-secondary-400 transition line-clamp-1 mb-1">
                        {property.city}, {property.country}
                      </h3>
                      <p className="text-sm text-primary-600 dark:text-sand-300 line-clamp-1">
                        {property.title}
                      </p>
                    </div>

                    {/* Property details */}
                    {(property.bedrooms || property.max_guests) && (
                      <div className="flex items-center gap-3 text-xs text-primary-500 dark:text-sand-400 mb-3">
                        {property.bedrooms && <span>{property.bedrooms} bed{property.bedrooms > 1 ? 's' : ''}</span>}
                        {property.bedrooms && property.max_guests && <span>•</span>}
                        {property.max_guests && <span>{property.max_guests} guest{property.max_guests > 1 ? 's' : ''}</span>}
                      </div>
                    )}

                    <div className="flex items-baseline justify-between">
                      <div>
                        <span className="font-bold text-lg text-primary-900 dark:text-sand-50">
                          ${property.price_per_night}
                        </span>
                        <span className="text-sm text-primary-600 dark:text-sand-300 ml-1">/ night</span>
                      </div>
                    </div>
                  </div>
                </div>
              </Link>
            ))}
          </div>
        )}
      </section>

      {/* Categories Section - Property Types */}
      <section className="max-w-7xl mx-auto px-3 sm:px-6 lg:px-8 py-8 sm:py-16 border-t border-primary-200 dark:border-primary-700">
        <div className="mb-8 sm:mb-12">
          <h2 className="text-2xl sm:text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-2 sm:mb-4">
            Explore by Property Type
          </h2>
          <p className="text-base sm:text-lg text-primary-600 dark:text-sand-300">
            Find your perfect accommodation, from luxury villas to cozy bed & breakfasts
          </p>
        </div>
        <div className="grid grid-cols-2 sm:grid-cols-3 md:grid-cols-4 lg:grid-cols-5 gap-2 sm:gap-4">
          {[
            { type: 'BNB', label: 'B&Bs', icon: Home },
            { type: 'APARTMENT', label: 'Apartments', icon: Building2 },
            { type: 'HOTEL', label: 'Hotels', icon: UtensilsCrossed },
            { type: 'LODGE', label: 'Lodges', icon: Trees },
            { type: 'VILLA', label: 'Villas', icon: Shield },
            { type: 'GUESTHOUSE', label: 'Guesthouses', icon: Users },
            { type: 'RESORT', label: 'Resorts', icon: Waves },
            { type: 'COTTAGE', label: 'Cottages', icon: Home },
            { type: 'HOUSE', label: 'Houses', icon: Building2 },
            { type: 'CAMPGROUND', label: 'Campgrounds', icon: Tent },
          ].map((item) => {
            const IconComponent = item.icon;
            return (
              <Link
                key={item.type}
                href={`/explore?type=${item.type}`}
                className="card p-3 sm:p-6 hover:shadow-xl hover:scale-105 transition duration-200 cursor-pointer group"
              >
                <div className="bg-secondary-100 dark:bg-secondary-900/30 w-10 h-10 sm:w-12 sm:h-12 rounded-lg flex items-center justify-center mb-2 sm:mb-3 group-hover:scale-110 transition">
                  <IconComponent className="w-5 h-5 sm:w-6 sm:h-6 text-secondary-600" />
                </div>
                <h3 className="text-sm sm:text-lg font-semibold text-primary-900 dark:text-sand-50 group-hover:text-secondary-600 transition">
                  {item.label}
                </h3>
                <p className="text-xs sm:text-sm text-primary-600 dark:text-sand-400 mt-1 sm:mt-2 line-clamp-2">
                  Browse {item.label.toLowerCase()}
                </p>
              </Link>
            );
          })}
        </div>
      </section>

      {/* Become a Host CTA Section */}
      <section className="max-w-7xl mx-auto px-3 sm:px-6 lg:px-8 py-8 sm:py-16 border-t border-primary-200 dark:border-primary-700">
        <div className="card overflow-hidden">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-0">
            <div className="relative h-40 sm:h-64 md:h-auto min-h-48">
              {/* eslint-disable-next-line @next/next/no-img-element */}
              <img
                src="https://images.unsplash.com/photo-1560185007-cde436f6a4d0?w=800"
                alt="Become a host"
                loading="lazy"
                className="w-full h-full object-cover"
              />
            </div>
            <div className="p-6 sm:p-8 md:p-12 flex flex-col justify-center bg-gradient-to-br from-primary-800 to-primary-700 text-sand-50">
              <h2 className="text-2xl sm:text-3xl md:text-4xl font-bold mb-3 sm:mb-4 text-sand-50">
                Become a Host
              </h2>
              <p className="text-base sm:text-lg text-sand-100 mb-4 sm:mb-6">
                Share your space and earn extra income. Join thousands of hosts welcoming guests from around the world.
              </p>
              <div>
                <Button
                  variant="secondary"
                  onClick={handleHostUpgrade}
                  disabled={hostLoading}
                  className="inline-flex items-center gap-2 text-base sm:text-lg"
                >
                  {hostLoading ? <Loader2 className="w-5 h-5 animate-spin" /> : null}
                  <span>Learn More</span>
                </Button>
              </div>
            </div>
          </div>
        </div>
      </section>
    </div>
  );
}
