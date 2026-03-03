import type { Metadata } from 'next';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8000';
const SITE_URL = process.env.NEXT_PUBLIC_SITE_URL || 'https://stayafrica.com';

type Props = {
  params: Promise<{ id: string }>;
  children: React.ReactNode;
};

export async function generateMetadata({ params }: Props): Promise<Metadata> {
  const { id } = await params;

  try {
    const res = await fetch(`${API_BASE_URL}/api/v1/properties/${id}/`, {
      next: { revalidate: 300 },
    });

    if (!res.ok) {
      return { title: 'Property | StayAfrica' };
    }

    const property = await res.json();
    const title = property.title || 'Property';
    const city = property.city || '';
    const country = property.country || '';
    const location = [city, country].filter(Boolean).join(', ');
    const price = property.price_per_night;
    const currency = property.currency || 'USD';
    const rating = property.average_rating;
    const reviewCount = property.review_count || 0;
    const propertyType = property.property_type || 'Property';
    const guests = property.max_guests;
    const bedrooms = property.bedrooms;
    const bathrooms = property.bathrooms;

    const parts: string[] = [];
    if (propertyType) parts.push(propertyType);
    if (location) parts.push(`in ${location}`);
    if (price) parts.push(`from ${currency} ${price}/night`);
    if (rating) parts.push(`★ ${rating.toFixed(1)} (${reviewCount} review${reviewCount !== 1 ? 's' : ''})`);
    const specs = [
      guests && `${guests} guest${guests !== 1 ? 's' : ''}`,
      bedrooms && `${bedrooms} bedroom${bedrooms !== 1 ? 's' : ''}`,
      bathrooms && `${bathrooms} bathroom${bathrooms !== 1 ? 's' : ''}`,
    ].filter(Boolean).join(' · ');
    if (specs) parts.push(specs);

    const description = parts.length > 0
      ? `${parts.join(' · ')}. ${(property.description || '').slice(0, 120)}`
      : property.description?.slice(0, 160) || `Book ${title} on StayAfrica`;

    const images = (property.images || [])
      .slice(0, 4)
      .map((img: any) => img.image_url || img.url || img.image)
      .filter(Boolean)
      .map((url: string) => ({ url, width: 1200, height: 630, alt: title }));
    if (images.length === 0) {
      const fallback = property.main_image_url || property.main_image || property.cover_image || '/og-image.png';
      images.push({ url: fallback, width: 1200, height: 630, alt: title });
    }

    // Canonical points to /property/{id} (this route)
    const canonicalUrl = `${SITE_URL}/property/${id}`;

    return {
      title,
      description,
      alternates: {
        canonical: canonicalUrl,
      },
      openGraph: {
        title: `${title} | StayAfrica`,
        description,
        url: canonicalUrl,
        images,
        type: 'website',
        siteName: 'StayAfrica',
        locale: 'en_US',
      },
      twitter: {
        card: 'summary_large_image',
        title: `${title} | StayAfrica`,
        description,
        images: [images[0]?.url],
      },
    };
  } catch {
    return { title: 'Property | StayAfrica' };
  }
}

export default async function PropertyLayout({ params, children }: Props) {
  const { id } = await params;
  let jsonLd: object | null = null;

  try {
    const res = await fetch(`${API_BASE_URL}/api/v1/properties/${id}/`, {
      next: { revalidate: 300 },
    });
    if (res.ok) {
      const p = await res.json();
      jsonLd = {
        '@context': 'https://schema.org',
        '@type': 'VacationRental',
        name: p.title,
        description: (p.description || '').slice(0, 300),
        url: `${SITE_URL}/property/${id}`,
        image: (p.images || []).slice(0, 5).map((img: any) => img.image_url || img.url || img.image).filter(Boolean),
        address: {
          '@type': 'PostalAddress',
          addressLocality: p.city || '',
          addressCountry: p.country || '',
          streetAddress: p.address || '',
        },
        ...(p.location?.coordinates ? {
          geo: {
            '@type': 'GeoCoordinates',
            latitude: p.location.coordinates[1],
            longitude: p.location.coordinates[0],
          },
        } : {}),
        numberOfBedrooms: p.bedrooms || undefined,
        numberOfBathroomsTotal: p.bathrooms || undefined,
        occupancy: p.max_guests ? {
          '@type': 'QuantitativeValue',
          maxValue: p.max_guests,
        } : undefined,
        ...(p.average_rating ? {
          aggregateRating: {
            '@type': 'AggregateRating',
            ratingValue: p.average_rating,
            reviewCount: p.review_count || 0,
            bestRating: 5,
          },
        } : {}),
        offers: {
          '@type': 'Offer',
          price: p.price_per_night,
          priceCurrency: p.currency || 'USD',
          availability: 'https://schema.org/InStock',
        },
        provider: {
          '@type': 'Organization',
          name: 'StayAfrica',
          url: SITE_URL,
        },
      };
    }
  } catch { /* skip JSON-LD on error */ }

  return (
    <>
      {jsonLd && (
        <script
          type="application/ld+json"
          dangerouslySetInnerHTML={{ __html: JSON.stringify(jsonLd) }}
        />
      )}
      {children}
    </>
  );
}
