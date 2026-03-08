'use client';

import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { PropertyCard, Property } from '@/components/property/PropertyCard';

const API_BASE = (process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8000').replace(/\/api\/v1\/?$/, '');

function resolveImageUrl(url: string): string {
  if (!url) return '';
  if (url.startsWith('http://') || url.startsWith('https://') || url.startsWith('data:')) return url;
  return `${API_BASE}${url.startsWith('/') ? '' : '/'}${url}`;
}

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
      // Exclude current property and limit to 4
      return results
        .filter((p: any) => p.id !== propertyId)
        .slice(0, 4);
    },
    enabled: !!(city || country),
  });

  if (!properties || properties.length === 0) return null;

  // Normalize API data to PropertyCard's Property interface
  const normalized: Property[] = properties.map((p: any) => {
    const imageUrls = (p.images || [])
      .map((img: any) => resolveImageUrl(img.image_url || img.url || img.image || ''))
      .filter(Boolean);
    // Fallback to main_image_url or main_image if images array is empty
    if (imageUrls.length === 0) {
      const fallback = resolveImageUrl(p.main_image_url || p.main_image || '');
      if (fallback) imageUrls.push(fallback);
    }
    return {
      id: p.id,
      title: p.title || '',
      location: [p.city, p.country].filter(Boolean).join(', '),
      price: p.price_per_night || 0,
      rating: p.average_rating || 0,
      reviewCount: p.review_count || 0,
      images: imageUrls,
      amenities: (p.amenities || []).map((a: any) => typeof a === 'string' ? a : a.name),
      beds: p.bedrooms || 0,
      baths: p.bathrooms || 0,
      guests: p.max_guests || 0,
      isFavorite: false,
    };
  });

  return (
    <div>
      <h2 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-6">
        More places in {city || country}
      </h2>
      <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-6">
        {normalized.map((property) => (
          <PropertyCard key={property.id} property={property} />
        ))}
      </div>
    </div>
  );
}
