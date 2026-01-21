/**
 * Skeleton Loader - Animated placeholder for loading states
 */
'use client';

import React from 'react';
import { cn } from '@/lib/utils';

interface SkeletonProps extends React.HTMLAttributes<HTMLDivElement> {
  variant?: 'text' | 'circle' | 'rect' | 'card';
  width?: string | number;
  height?: string | number;
}

export const Skeleton = React.forwardRef<HTMLDivElement, SkeletonProps>(
  ({ className, variant = 'rect', width, height, ...props }, ref) => {
    const sizeClasses = {
      text: 'h-4 w-3/4',
      circle: 'h-10 w-10 rounded-full',
      rect: 'h-12 w-full',
      card: 'h-64 w-full',
    };

    const customSize = width || height ? { width, height } : {};

    return (
      <div
        ref={ref}
        className={cn(
          'animate-pulse rounded-lg bg-gradient-to-r from-neutral-200 via-neutral-100 to-neutral-200 dark:from-neutral-700 dark:via-neutral-600 dark:to-neutral-700 bg-[length:200%_100%] animation-duration-2000',
          sizeClasses[variant],
          className
        )}
        style={customSize}
        role="status"
        aria-busy="true"
        aria-label="Loading content"
        {...props}
      />
    );
  }
);
Skeleton.displayName = 'Skeleton';

// Property Card Skeleton
export const PropertyCardSkeleton: React.FC = () => (
  <div className="space-y-3 rounded-xl border border-neutral-200 dark:border-neutral-700 bg-white dark:bg-neutral-800 p-4" role="status" aria-busy="true" aria-label="Loading property">
    <Skeleton variant="rect" className="h-40 w-full" />
    <Skeleton variant="text" className="h-5 w-4/5" />
    <Skeleton variant="text" className="h-4 w-2/3" />
    <div className="flex justify-between pt-2">
      <Skeleton variant="text" className="h-4 w-1/4" />
      <Skeleton variant="text" className="h-4 w-1/4" />
    </div>
    <span className="sr-only">Loading property card</span>
  </div>
);

// Booking Panel Skeleton
export const BookingPanelSkeleton: React.FC = () => (
  <div className="space-y-4 rounded-xl border border-neutral-200 dark:border-neutral-700 bg-white dark:bg-neutral-800 p-6" role="status" aria-busy="true" aria-label="Loading booking panel">
    <Skeleton variant="text" className="h-6 w-2/3" />
    <Skeleton variant="rect" className="h-10 w-full" />
    <Skeleton variant="rect" className="h-10 w-full" />
    <Skeleton variant="rect" className="h-10 w-full" />
    <div className="space-y-2 border-t border-neutral-200 dark:border-neutral-700 pt-4">
      <Skeleton variant="text" className="h-4 w-2/3" />
      <Skeleton variant="text" className="h-4 w-2/3" />
    </div>
    <Skeleton variant="rect" className="h-12 w-full" />
    <span className="sr-only">Loading booking panel</span>
  </div>
);

// List Skeleton
export const ListSkeleton: React.FC<{ count?: number }> = ({ count = 3 }) => (
  <div className="space-y-4" role="status" aria-busy="true" aria-label="Loading list">
    {Array.from({ length: count }).map((_, i) => (
      <div key={i} className="space-y-2 border-b border-neutral-200 dark:border-neutral-700 pb-4 last:border-b-0">
        <Skeleton variant="text" className="h-5 w-2/3" />
        <Skeleton variant="text" className="h-4 w-4/5" />
      </div>
    ))}
    <span className="sr-only">Loading list items</span>
  </div>
);
