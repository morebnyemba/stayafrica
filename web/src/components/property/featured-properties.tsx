'use client';

import { MapPin, Star } from 'lucide-react';
import { Button } from '@/components/ui';

export function FeaturedProperties() {
  const properties = [
    {
      id: 1,
      title: 'Luxury Villa in Cape Town',
      location: 'Cape Town, South Africa',
      price: 250,
      rating: 4.9,
      image: 'https://images.unsplash.com/photo-1512917774080-9991f1c52e1d',
    },
    {
      id: 2,
      title: 'Cozy Apartment in Harare',
      location: 'Harare, Zimbabwe',
      price: 80,
      rating: 4.7,
      image: 'https://images.unsplash.com/photo-1502672260266-1c1ef2d93688',
    },
    {
      id: 3,
      title: 'Modern Home in Johannesburg',
      location: 'Johannesburg, South Africa',
      price: 150,
      rating: 4.8,
      image: 'https://images.unsplash.com/photo-1522708323590-d24dbb6b0267',
    },
  ];

  return (
    <section className="py-20 bg-sand-100">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="text-center max-w-2xl mx-auto mb-14">
          <span className="inline-flex items-center justify-center rounded-full bg-secondary-100 text-secondary-700 px-4 py-1 text-sm font-semibold tracking-wide uppercase mb-4">
            Featured Stays
          </span>
          <h2 className="text-3xl md:text-4xl font-bold text-primary-900 mb-4">
            Handpicked Escapes Across Africa
          </h2>
          <p className="text-lg text-primary-600">
            Stay in boutique villas, modern city apartments, and serene countryside lodges curated for unforgettable journeys.
          </p>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
          {properties.map((property) => (
            <article
              key={property.id}
              className="card group overflow-hidden border border-primary-100/80 hover:border-secondary-400 transition"
            >
              <div className="relative h-56 overflow-hidden">
                <img
                  src={property.image}
                  alt={property.title}
                  className="w-full h-full object-cover transition duration-700 group-hover:scale-105"
                />
                <div className="absolute inset-0 bg-gradient-to-t from-primary-900/70 via-primary-900/10 to-transparent opacity-0 group-hover:opacity-100 transition" />
                <div className="absolute top-4 left-4 inline-flex items-center bg-secondary-500 text-primary-900 text-xs font-semibold px-3 py-1 rounded-full shadow">
                  Top Rated
                </div>
                <div className="absolute bottom-4 left-4 right-4 flex justify-between items-end text-sand-50">
                  <div>
                    <p className="text-xs uppercase tracking-widest text-secondary-100">Experience</p>
                    <p className="text-xl font-semibold leading-tight drop-shadow-sm">{property.title}</p>
                  </div>
                  <div className="inline-flex items-center gap-1 bg-primary-900/80 backdrop-blur px-3 py-1 rounded-full text-sm font-semibold">
                    <Star className="w-4 h-4 text-secondary-200" fill="currentColor" />
                    <span>{property.rating}</span>
                  </div>
                </div>
              </div>
              <div className="p-6 space-y-4">
                <div className="flex items-center gap-2 text-primary-600 text-sm">
                  <MapPin className="w-4 h-4" />
                  <span>{property.location}</span>
                </div>
                <div className="flex justify-between items-end">
                  <div>
                    <span className="text-3xl font-semibold text-primary-900">${property.price}</span>
                    <span className="text-sm text-primary-500 ml-1 font-medium">/night</span>
                  </div>
                  <Button type="button" variant="secondary" size="sm">
                    View details
                  </Button>
                </div>
              </div>
            </article>
          ))}
        </div>
      </div>
    </section>
  );
}
