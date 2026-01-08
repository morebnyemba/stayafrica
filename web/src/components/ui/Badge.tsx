/**
 * Badge Component - Labels and tags
 */
'use client';

import React from 'react';
import { cva, type VariantProps } from 'class-variance-authority';
import { cn } from '@/lib/utils';

const badgeVariants = cva(
  'inline-flex items-center rounded-full px-3 py-1 text-sm font-medium whitespace-nowrap',
  {
    variants: {
      variant: {
        default: 'bg-primary-100 text-primary-700',
        secondary: 'bg-secondary-100 text-secondary-700',
        success: 'bg-success-50 text-success-700',
        error: 'bg-error-50 text-error-700',
        warning: 'bg-warning-50 text-warning-700',
        info: 'bg-info-50 text-info-700',
        neutral: 'bg-neutral-100 text-neutral-700',
      },
      size: {
        sm: 'px-2 py-0.5 text-xs',
        md: 'px-3 py-1 text-sm',
        lg: 'px-4 py-2 text-base',
      },
    },
    defaultVariants: {
      variant: 'default',
      size: 'md',
    },
  }
);

interface BadgeProps
  extends React.HTMLAttributes<HTMLDivElement>,
    VariantProps<typeof badgeVariants> {
  icon?: React.ReactNode;
  onRemove?: () => void;
}

export const Badge = React.forwardRef<HTMLDivElement, BadgeProps>(
  ({ className, variant, size, icon, onRemove, children, ...props }, ref) => (
    <div
      ref={ref}
      className={cn(badgeVariants({ variant, size }), className)}
      {...props}
    >
      {icon && <span className="mr-1">{icon}</span>}
      {children}
      {onRemove && (
        <button
          onClick={onRemove}
          className="ml-1 hover:opacity-70"
          aria-label="Remove badge"
        >
          ×
        </button>
      )}
    </div>
  )
);
Badge.displayName = 'Badge';
