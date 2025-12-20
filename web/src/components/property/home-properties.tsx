'use client';

import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { MapPin, Star, Home, Users, Building2, Trees, Shield, UtensilsCrossed, Tent, Waves } from 'lucide-react';
import { PropertyListSkeleton } from './property-card-skeleton';
import Link from 'next/link';

export function HomeProperties() {
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

  return (
    <div className="bg-sand-100 dark:bg-primary-900">
      {/* Inspiration Section - Popular Locations */}
      <section className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
        <h2 className="text-2xl md:text-3xl font-bold text-primary-900 dark:text-sand-50 mb-8">
          Inspiration for your next trip
        </h2>
        <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
          {locations.map((location) => (
            <Link
              key={location.name}
              href={`/explore?city=${encodeURIComponent(location.name)}`}
              className="group cursor-pointer"
            >
              <div className="relative h-64 md:h-72 rounded-2xl overflow-hidden mb-3">
                <img
                  src={location.image}
                  alt={location.name}
                  className="w-full h-full object-cover transition duration-300 group-hover:scale-105"
                />
                <div className="absolute inset-0 bg-gradient-to-t from-primary-900/70 via-primary-900/20 to-transparent" />
                <div className="absolute bottom-4 left-4 text-white">
                  <h3 className="text-lg font-semibold">{location.name}</h3>
                </div>
              </div>
            </Link>
          ))}
        </div>
      </section>

      {/* Featured Properties */}
      <section className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
        <div className="flex justify-between items-center mb-8">
          <h2 className="text-2xl md:text-3xl font-bold text-primary-900 dark:text-sand-50">
            Stays nearby
          </h2>
          <Link 
            href="/explore" 
            className="text-secondary-600 hover:text-secondary-700 font-semibold underline"
          >
            Show all
          </Link>
        </div>

        {isLoading ? (
          <PropertyListSkeleton count={6} />
        ) : properties.length === 0 ? (
          <div className="card p-12 text-center">
            <MapPin className="w-16 h-16 text-primary-400 dark:text-sand-500 mx-auto mb-4" />
            <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-2">
              No Properties Available
            </h3>
            <p className="text-primary-600 dark:text-sand-300">
              Check back soon for amazing stays!
            </p>
          </div>
        ) : (
          <div className="grid md:grid-cols-3 lg:grid-cols-4 gap-6">
            {properties.slice(0, 8).map((property: any) => (
              <Link
                key={property.id}
                href={`/properties/${property.id}`}
                className="group cursor-pointer"
              >
                <div className="relative h-64 rounded-2xl overflow-hidden mb-3">
                  <img
                    src={property.images?.[0]?.image_url || 'https://images.unsplash.com/photo-1512917774080-9991f1c52e1d'}
                    alt={property.title}
                    className="w-full h-full object-cover transition duration-300 group-hover:scale-105"
                  />
                  {property.average_rating && (
                    <div className="absolute top-3 right-3 flex items-center gap-1 bg-white/90 backdrop-blur px-2 py-1 rounded-lg text-sm font-semibold">
                      <Star className="w-4 h-4 text-primary-900" fill="currentColor" />
                      <span className="text-primary-900">{property.average_rating.toFixed(1)}</span>
                    </div>
                  )}
                </div>
                <div className="space-y-1">
                  <div className="flex items-start justify-between gap-2">
                    <h3 className="font-semibold text-primary-900 dark:text-sand-50 group-hover:text-secondary-600 transition line-clamp-1">
                      {property.city}, {property.country}
                    </h3>
                  </div>
                  <p className="text-sm text-primary-600 dark:text-sand-300 line-clamp-1">
                    {property.title}
                  </p>
                  <div className="flex items-baseline gap-1">
                    <span className="font-semibold text-primary-900 dark:text-sand-50">
                      ${property.price_per_night}
                    </span>
                    <span className="text-sm text-primary-600 dark:text-sand-300">night</span>
                  </div>
                </div>
              </Link>
            ))}
          </div>
        )}
      </section>

      {/* Categories Section - Property Types */}
      <section className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-16 border-t border-primary-200 dark:border-primary-700">
        <div className="mb-12">
          <h2 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-4">
            Explore by Property Type
          </h2>
          <p className="text-lg text-primary-600 dark:text-sand-300">
            Find your perfect accommodation, from luxury villas to cozy bed & breakfasts
          </p>
        </div>
        <div className="grid grid-cols-2 md:grid-cols-3 lg:grid-cols-5 gap-4">
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
                className="card p-6 hover:shadow-xl hover:scale-105 transition duration-200 cursor-pointer group"
              >
                <div className="bg-secondary-100 dark:bg-secondary-900/30 w-12 h-12 rounded-lg flex items-center justify-center mb-3 group-hover:scale-110 transition">
                  <IconComponent className="w-6 h-6 text-secondary-600" />
                </div>
                <h3 className="text-lg font-semibold text-primary-900 dark:text-sand-50 group-hover:text-secondary-600 transition">
                  {item.label}
                </h3>
                <p className="text-sm text-primary-600 dark:text-sand-400 mt-2">
                  Browse {item.label.toLowerCase()}
                </p>
              </Link>
            );
          })}
        </div>
      </section>

      {/* Become a Host CTA Section */}
      <section className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-16 border-t border-primary-200 dark:border-primary-700">
        <div className="card overflow-hidden">
          <div className="grid md:grid-cols-2 gap-0">
            <div className="relative h-64 md:h-auto">
              <img
                src="https://images.unsplash.com/photo-1560185007-cde436f6a4d0?w=800"
                alt="Become a host"
                className="w-full h-full object-cover"
              />
            </div>
            <div className="p-8 md:p-12 flex flex-col justify-center bg-gradient-to-br from-primary-800 to-primary-700 text-sand-50">
              <h2 className="text-3xl md:text-4xl font-bold mb-4">
                Become a Host
              </h2>
              <p className="text-lg text-sand-100 mb-6">
                Share your space and earn extra income. Join thousands of hosts welcoming guests from around the world.
              </p>
              <div>
                <Link
                  href="/host"
                  className="inline-block btn-primary px-8 py-3 text-lg font-semibold"
                >
                  Learn More
                </Link>
              </div>
            </div>
          </div>
        </div>
      </section>
    </div>
  );
}
