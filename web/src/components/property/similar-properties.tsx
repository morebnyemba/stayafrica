'use client';

import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import Link from 'next/link';

interface SimilarPropertiesProps {
  propertyId: string;
  city?: string;
  country?: string;
}

export function SimilarProperties({ propertyId, city, country }: SimilarPropertiesProps) {
  const { data: properties } = useQuery({
    queryKey: ['similar-properties', city, country, propertyId],
    queryFn: async () => {
      const params: Record<string, string> = {};
      if (city) params.city = city;
      if (country) params.country = country;
      const response = await apiClient.getProperties(params);
      const results = response.data?.results || response.data || [];
      return results
        .filter((p: any) => String(p.id) !== String(propertyId))
        .slice(0, 4);
    },
    enabled: !!(city || country),
  });

  if (!properties || properties.length === 0) return null;

  return (
    <div>
      <h2 className="text-xl font-semibold text-primary-900 mb-6">
        More places in {city || country}
      </h2>
      <div className="grid grid-cols-2 sm:grid-cols-2 lg:grid-cols-4 gap-4 sm:gap-6">
        {properties.map((property: any) => (
          <Link
            key={property.id}
            href={`/property/${property.id}`}
            className="group cursor-pointer"
          >
            <div className="bg-white rounded-xl overflow-hidden shadow-md hover:shadow-2xl transition-all duration-300 border border-primary-100">
              <div className="relative h-36 sm:h-44 md:h-52 overflow-hidden">
                <img
                  src={property.images?.[0]?.image_url || property.main_image_url || property.main_image || 'https://images.unsplash.com/photo-1512917774080-9991f1c52e1d?w=400'}
                  alt={property.title}
                  className="w-full h-full object-cover transition duration-500 group-hover:scale-110"
                />
                {property.average_rating > 0 && (
                  <div className="absolute top-2 right-2 flex items-center gap-1 bg-white/95 backdrop-blur-sm px-2 py-1 rounded-lg shadow">
                    <svg className="w-3 h-3 text-secondary-500" fill="currentColor" viewBox="0 0 20 20">
                      <path d="M9.049 2.927c.3-.921 1.603-.921 1.902 0l1.07 3.292a1 1 0 00.95.69h3.462c.969 0 1.371 1.24.588 1.81l-2.8 2.034a1 1 0 00-.364 1.118l1.07 3.292c.3.921-.755 1.688-1.54 1.118l-2.8-2.034a1 1 0 00-1.175 0l-2.8 2.034c-.784.57-1.838-.197-1.539-1.118l1.07-3.292a1 1 0 00-.364-1.118L2.98 8.72c-.783-.57-.38-1.81.588-1.81h3.461a1 1 0 00.951-.69l1.07-3.292z" />
                    </svg>
                    <span className="text-xs font-bold text-primary-900">{property.average_rating.toFixed(1)}</span>
                  </div>
                )}
              </div>
              <div className="p-3">
                <h3 className="font-semibold text-sm text-primary-900 group-hover:text-secondary-600 transition line-clamp-1">
                  {property.city}, {property.country}
                </h3>
                <p className="text-xs text-primary-600 line-clamp-1 mt-0.5">
                  {property.title}
                </p>
                {(property.bedrooms || property.max_guests) && (
                  <div className="flex items-center gap-2 text-xs text-primary-500 mt-1.5">
                    {property.bedrooms > 0 && <span>{property.bedrooms} bed{property.bedrooms > 1 ? 's' : ''}</span>}
                    {property.bedrooms > 0 && property.max_guests > 0 && <span>&middot;</span>}
                    {property.max_guests > 0 && <span>{property.max_guests} guest{property.max_guests > 1 ? 's' : ''}</span>}
                  </div>
                )}
                <div className="mt-2">
                  <span className="font-bold text-sm text-primary-900">
                    ${property.price_per_night}
                  </span>
                  <span className="text-xs text-primary-600 ml-1">/ night</span>
                </div>
              </div>
            </div>
          </Link>
        ))}
      </div>
    </div>
  );
}