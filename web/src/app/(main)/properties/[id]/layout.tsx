import type { Metadata } from 'next';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8000';

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
    const description = property.description?.slice(0, 160) || `Book ${title} on StayAfrica`;
    const image = property.images?.[0]?.image || property.cover_image || '/og-image.png';

    return {
      title,
      description,
      openGraph: {
        title: `${title} | StayAfrica`,
        description,
        images: [{ url: image, width: 1200, height: 630, alt: title }],
        type: 'website',
        siteName: 'StayAfrica',
      },
      twitter: {
        card: 'summary_large_image',
        title: `${title} | StayAfrica`,
        description,
        images: [image],
      },
    };
  } catch {
    return { title: 'Property | StayAfrica' };
  }
}

export default function PropertyLayout({ children }: { children: React.ReactNode }) {
  return <>{children}</>;
}
