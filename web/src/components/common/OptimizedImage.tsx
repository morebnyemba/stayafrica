/**
 * Optimized Image Component
 * Features: Lazy loading, WebP support, responsive sizing, blur placeholder
 */
'use client';

import React from 'react';
import Image from 'next/image';
import { cn } from '@/lib/utils';

interface OptimizedImageProps {
  src: string;
  alt: string;
  width?: number;
  height?: number;
  className?: string;
  objectFit?: 'cover' | 'contain' | 'fill' | 'scale-down';
  priority?: boolean;
  quality?: number;
  blurDataURL?: string;
}

/**
 * Optimized Image Component
 * - Automatically uses WebP with JPG fallback
 * - Lazy loads by default
 * - Provides responsive sizes
 * - Optional blur placeholder
 */
export const OptimizedImage = React.forwardRef<HTMLImageElement, OptimizedImageProps>(
  (
    {
      src,
      alt,
      width,
      height,
      className,
      objectFit = 'cover',
      priority = false,
      quality = 75,
      blurDataURL,
    },
    ref
  ) => {
    // Add WebP format parameter if using a CDN or image service
    const optimizedSrc = src.includes('?')
      ? `${src}&fm=webp&q=${quality}`
      : `${src}?fm=webp&q=${quality}`;

    return (
      <div className={cn('relative', className)}>
        <Image
          ref={ref}
          src={optimizedSrc}
          alt={alt}
          width={width}
          height={height}
          className={cn('w-full h-full', objectFit === 'cover' && 'object-cover')}
          loading={priority ? 'eager' : 'lazy'}
          priority={priority}
          quality={quality}
          placeholder={blurDataURL ? 'blur' : 'empty'}
          blurDataURL={blurDataURL}
          onError={(e) => {
            // Fallback to original if WebP fails
            (e.currentTarget as HTMLImageElement).src = src;
          }}
          sizes="(max-width: 480px) 100vw, (max-width: 768px) 50vw, (max-width: 1200px) 33vw, 25vw"
        />
      </div>
    );
  }
);

OptimizedImage.displayName = 'OptimizedImage';

// Utility to generate blur data URL (placeholder)
export const generateBlurDataURL = (width: number, height: number): string => {
  const canvas = typeof document !== 'undefined' ? document.createElement('canvas') : null;
  if (!canvas) return '';

  canvas.width = width;
  canvas.height = height;
  const ctx = canvas.getContext('2d');
  if (!ctx) return '';

  ctx.fillStyle = '#e5e7eb'; // neutral-200
  ctx.fillRect(0, 0, width, height);
  return canvas.toDataURL();
};
