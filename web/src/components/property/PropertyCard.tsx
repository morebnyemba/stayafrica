/**
 * Airbnb-style Property Card Component
 * Clean minimal design: rounded image with dot carousel, no borders/shadows
 */
'use client';

import React, { useState, useCallback } from 'react';
import Image from 'next/image';
import Link from 'next/link';
import { useRouter } from 'next/navigation';
import { Heart, ChevronLeft, ChevronRight, Star, Loader2 } from 'lucide-react';
import { cn } from '@/lib/utils';
import { useAuth } from '@/store/auth-store';
import { apiClient } from '@/services/api-client';

const API_BASE = (process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8000').replace(/\/api\/v1\/?$/, '');

/** Ensure image URL is absolute — handles /media/... relative paths from API */
function resolveImageUrl(url: string): string {
  if (!url) return '';
  if (url.startsWith('http://') || url.startsWith('https://') || url.startsWith('data:')) return url;
  return `${API_BASE}${url.startsWith('/') ? '' : '/'}${url}`;
}

export interface Property {
  id: string;
  title: string;
  location: string;
  price: number;
  rating: number;
  reviewCount: number;
  images: string[];
  amenities: string[];
  beds: number;
  baths: number;
  guests: number;
  isFavorite?: boolean;
}

interface PropertyCardProps {
  property: Property;
  onFavorite?: (id: string) => void;
  onBook?: (id: string) => void;
}

export const PropertyCard: React.FC<PropertyCardProps> = ({
  property,
  onFavorite,
}) => {
  const [currentImageIndex, setCurrentImageIndex] = useState(0);
  const [isFavorite, setIsFavorite] = useState(property.isFavorite || false);
  const [favoriteLoading, setFavoriteLoading] = useState(false);
  const { isAuthenticated } = useAuth();
  const router = useRouter();

  const handlePrevImage = (e: React.MouseEvent) => {
    e.preventDefault();
    e.stopPropagation();
    setCurrentImageIndex((prev) =>
      prev === 0 ? property.images.length - 1 : prev - 1
    );
  };

  const handleNextImage = (e: React.MouseEvent) => {
    e.preventDefault();
    e.stopPropagation();
    setCurrentImageIndex((prev) =>
      prev === property.images.length - 1 ? 0 : prev + 1
    );
  };

  const handleFavorite = useCallback(async (e: React.MouseEvent) => {
    e.preventDefault();
    e.stopPropagation();
    if (!isAuthenticated) {
      router.push('/login');
      return;
    }
    if (favoriteLoading) return;
    setFavoriteLoading(true);
    try {
      if (isFavorite) {
        await apiClient.unsaveProperty(property.id);
      } else {
        await apiClient.saveProperty(property.id);
      }
      setIsFavorite(!isFavorite);
      onFavorite?.(property.id);
    } catch {
      // Silently fail — user can retry
    } finally {
      setFavoriteLoading(false);
    }
  }, [isAuthenticated, isFavorite, favoriteLoading, property.id, onFavorite, router]);

  const handleDotClick = (e: React.MouseEvent, index: number) => {
    e.preventDefault();
    e.stopPropagation();
    setCurrentImageIndex(index);
  };

  // Show max 5 dots, centered around current
  const maxDots = 5;
  const totalImages = property.images.length;
  const dots = [];
  if (totalImages <= maxDots) {
    for (let i = 0; i < totalImages; i++) dots.push(i);
  } else {
    let start = Math.max(0, currentImageIndex - Math.floor(maxDots / 2));
    const end = Math.min(totalImages, start + maxDots);
    if (end - start < maxDots) start = end - maxDots;
    for (let i = start; i < end; i++) dots.push(i);
  }

  return (
    <Link href={`/property/${property.id}`} className="group block">
      {/* Image Container */}
      <div className="relative aspect-[4/3] w-full overflow-hidden rounded-xl bg-sand-100 dark:bg-primary-800">
        {totalImages > 0 ? (
          <Image
            src={resolveImageUrl(property.images[currentImageIndex])}
            alt={property.title}
            fill
            className="object-cover transition-opacity duration-300"
            sizes="(max-width: 640px) 100vw, (max-width: 1024px) 50vw, 25vw"
          />
        ) : (
          <div className="absolute inset-0 flex items-center justify-center bg-sand-200 dark:bg-primary-700">
            <span className="text-sm text-primary-400 dark:text-sand-500">No image</span>
          </div>
        )}

        {/* Favorite Button */}
        <button
          onClick={handleFavorite}
          disabled={favoriteLoading}
          className="absolute top-3 right-3 z-10 drop-shadow-md"
          aria-label={isFavorite ? 'Remove from favorites' : 'Add to favorites'}
        >
          {favoriteLoading ? (
            <Loader2 className="h-6 w-6 animate-spin text-white drop-shadow-md" />
          ) : (
            <Heart
              className={cn(
                'h-6 w-6 transition-colors drop-shadow-md',
                isFavorite
                  ? 'fill-red-500 text-red-500'
                  : 'fill-black/30 text-white hover:fill-black/50'
              )}
              strokeWidth={2}
            />
          )}
        </button>

        {/* Hover Navigation Arrows */}
        {totalImages > 1 && (
          <>
            <button
              onClick={handlePrevImage}
              className="absolute left-2 top-1/2 -translate-y-1/2 z-10 rounded-full bg-white/90 dark:bg-primary-800/90 p-1.5 shadow-md opacity-0 group-hover:opacity-100 transition-opacity hover:bg-white dark:hover:bg-primary-700 hover:scale-105"
              aria-label="Previous image"
            >
              <ChevronLeft className="h-4 w-4 text-primary-900 dark:text-sand-50" />
            </button>
            <button
              onClick={handleNextImage}
              className="absolute right-2 top-1/2 -translate-y-1/2 z-10 rounded-full bg-white/90 dark:bg-primary-800/90 p-1.5 shadow-md opacity-0 group-hover:opacity-100 transition-opacity hover:bg-white dark:hover:bg-primary-700 hover:scale-105"
              aria-label="Next image"
            >
              <ChevronRight className="h-4 w-4 text-primary-900 dark:text-sand-50" />
            </button>
          </>
        )}

        {/* Dot Indicators */}
        {totalImages > 1 && (
          <div className="absolute bottom-2.5 left-1/2 -translate-x-1/2 flex items-center gap-1.5 z-10">
            {dots.map((idx) => (
              <button
                key={idx}
                onClick={(e) => handleDotClick(e, idx)}
                aria-label={`Go to image ${idx + 1}`}
                className={cn(
                  'rounded-full transition-all duration-200',
                  idx === currentImageIndex
                    ? 'h-1.5 w-1.5 bg-white shadow-sm'
                    : 'h-1.5 w-1.5 bg-white/60 hover:bg-white/80'
                )}
              />
            ))}
          </div>
        )}
      </div>

      {/* Card Text Content */}
      <div className="mt-3 space-y-0.5">
        {/* Location + Rating */}
        <div className="flex items-center justify-between">
          <p className="font-semibold text-[15px] text-primary-900 dark:text-sand-50 line-clamp-1">
            {property.location}
          </p>
          {property.rating > 0 && (
            <div className="flex items-center gap-1 flex-shrink-0 ml-2">
              <Star className="h-3.5 w-3.5 fill-primary-900 dark:fill-sand-50 text-primary-900 dark:text-sand-50" />
              <span className="text-sm text-primary-900 dark:text-sand-50">
                {property.rating.toFixed(1)}
              </span>
            </div>
          )}
        </div>

        {/* Title */}
        <p className="text-sm text-primary-500 dark:text-sand-400 line-clamp-1">
          {property.title}
        </p>

        {/* Beds · Baths · Guests */}
        <p className="text-sm text-primary-500 dark:text-sand-400">
          {property.beds} bed{property.beds !== 1 ? 's' : ''} · {property.baths} bath{property.baths !== 1 ? 's' : ''} · {property.guests} guest{property.guests !== 1 ? 's' : ''}
        </p>

        {/* Price */}
        <p className="pt-1">
          <span className="font-semibold text-[15px] text-primary-900 dark:text-sand-50">
            ${property.price}
          </span>
          <span className="text-sm text-primary-500 dark:text-sand-400"> night</span>
        </p>
      </div>
    </Link>
  );
};
