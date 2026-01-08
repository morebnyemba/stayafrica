/**
 * Input Component - Text input with validation states, textarea support, and select support
 */
'use client';

import React from 'react';
import { cva, type VariantProps } from 'class-variance-authority';
import { cn } from '@/lib/utils';

const inputVariants = cva(
  'flex h-10 w-full rounded-lg border-2 bg-white px-4 py-2 text-base transition-colors placeholder:text-neutral-400 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-offset-2 disabled:cursor-not-allowed disabled:bg-neutral-100 disabled:opacity-50',
  {
    variants: {
      variant: {
        default: 'border-neutral-300 focus-visible:border-primary-500 focus-visible:ring-primary-500',
        error: 'border-error-500 focus-visible:border-error-600 focus-visible:ring-error-500',
        success: 'border-success-500 focus-visible:border-success-600 focus-visible:ring-success-500',
      },
    },
    defaultVariants: {
      variant: 'default',
    },
  }
);

interface SelectOption {
  value: string | number;
  label: string;
}

interface InputProps extends Omit<React.InputHTMLAttributes<HTMLInputElement>, 'type'>,
  VariantProps<typeof inputVariants> {
  label?: string;
  error?: string;
  helpText?: string;
  icon?: React.ReactNode;
  iconPosition?: 'left' | 'right';
  type?: string;
  multiline?: boolean;
  rows?: number;
  select?: boolean;
  options?: SelectOption[];
}

export const Input = React.forwardRef<HTMLInputElement | HTMLTextAreaElement | HTMLSelectElement, InputProps>(
  (
    {
      className,
      variant,
      label,
      error,
      helpText,
      icon,
      iconPosition = 'left',
      type,
      multiline = false,
      rows = 3,
      select = false,
      options = [],
      ...props
    },
    ref
  ) => {
    const variantClass = error ? 'error' : variant;

    return (
      <div className="w-full">
        {label && (
          <label className="mb-2 block text-sm font-medium text-neutral-700">
            {label}
            {props.required && <span className="ml-1 text-error-500">*</span>}
          </label>
        )}
        <div className="relative flex items-center">
          {icon && iconPosition === 'left' && (
            <span className="absolute left-3 flex items-center text-neutral-400">{icon}</span>
          )}
          
          {select ? (
            <select
              ref={ref as React.Ref<HTMLSelectElement>}
              className={cn(
                inputVariants({ variant: variantClass as any }),
                className
              )}
              {...(props as React.SelectHTMLAttributes<HTMLSelectElement>)}
            >
              {options.map((option) => (
                <option key={option.value} value={option.value}>
                  {option.label}
                </option>
              ))}
            </select>
          ) : multiline ? (
            <textarea
              ref={ref as React.Ref<HTMLTextAreaElement>}
              rows={rows}
              className={cn(
                inputVariants({ variant: variantClass as any }),
                'h-auto',
                className
              )}
              {...(props as React.TextareaHTMLAttributes<HTMLTextAreaElement>)}
            />
          ) : (
            <input
              ref={ref as React.Ref<HTMLInputElement>}
              type={type}
              className={cn(
                inputVariants({ variant: variantClass as any }),
                icon && iconPosition === 'left' && 'pl-10',
                icon && iconPosition === 'right' && 'pr-10',
                className
              )}
              {...props}
            />
          )}
          
          {icon && iconPosition === 'right' && !select && (
            <span className="absolute right-3 flex items-center text-neutral-400">{icon}</span>
          )}
        </div>
        {error && <p className="mt-1 text-sm text-error-500">{error}</p>}
        {helpText && !error && <p className="mt-1 text-sm text-neutral-500">{helpText}</p>}
      </div>
    );
  }
);
Input.displayName = 'Input';
