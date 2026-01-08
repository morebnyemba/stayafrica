/**
 * Card Component - Flexible container with elevation and rounded corners
 */
'use client';

import React from 'react';
import { cn } from '@/lib/utils';

interface CardProps extends React.HTMLAttributes<HTMLDivElement> {
  variant?: 'default' | 'elevated' | 'outline';
  hoverable?: boolean;
}

export const Card = React.forwardRef<HTMLDivElement, CardProps>(
  ({ className, variant = 'default', hoverable = false, ...props }, ref) => (
    <div
      ref={ref}
      className={cn(
        'rounded-xl bg-white transition-all duration-200',
        variant === 'default' && 'shadow-card',
        variant === 'elevated' && 'shadow-lg',
        variant === 'outline' && 'border border-neutral-200 shadow-sm',
        hoverable && 'cursor-pointer hover:shadow-hover hover:scale-[1.02]',
        className
      )}
      {...props}
    />
  )
);
Card.displayName = 'Card';

export const CardHeader = React.forwardRef<HTMLDivElement, React.HTMLAttributes<HTMLDivElement>>(
  ({ className, ...props }, ref) => (
    <div ref={ref} className={cn('border-b border-neutral-200 px-6 py-4', className)} {...props} />
  )
);
CardHeader.displayName = 'CardHeader';

export const CardBody = React.forwardRef<HTMLDivElement, React.HTMLAttributes<HTMLDivElement>>(
  ({ className, ...props }, ref) => (
    <div ref={ref} className={cn('px-6 py-4', className)} {...props} />
  )
);
CardBody.displayName = 'CardBody';

export const CardFooter = React.forwardRef<HTMLDivElement, React.HTMLAttributes<HTMLDivElement>>(
  ({ className, ...props }, ref) => (
    <div ref={ref} className={cn('border-t border-neutral-200 px-6 py-4', className)} {...props} />
  )
);
CardFooter.displayName = 'CardFooter';
