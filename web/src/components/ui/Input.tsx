/**
 * Input Component - Text input with validation states, textarea support, and select support
 */
'use client';

import React, { useId, useState } from 'react';
import { cva, type VariantProps } from 'class-variance-authority';
import { cn } from '@/lib/utils';
import { Eye, EyeOff } from 'lucide-react';

const inputVariants = cva(
  'flex h-10 w-full rounded-lg border-2 bg-white dark:bg-primary-800 px-4 py-2 text-base text-neutral-900 dark:text-sand-100 transition-colors placeholder:text-neutral-400 dark:placeholder:text-sand-500 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-offset-2 dark:focus-visible:ring-offset-primary-900 disabled:cursor-not-allowed disabled:bg-neutral-100 dark:disabled:bg-primary-700 disabled:opacity-50',
  {
    variants: {
      variant: {
        default: 'border-neutral-300 dark:border-primary-600 focus-visible:border-primary-500 focus-visible:ring-primary-500',
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

interface InputProps extends Omit<React.InputHTMLAttributes<HTMLInputElement>, 'type' | 'onChange'>,
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
  onChange?: React.ChangeEventHandler<HTMLInputElement | HTMLTextAreaElement | HTMLSelectElement>;
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
    const [showPassword, setShowPassword] = useState(false);
    const isPasswordType = type === 'password';
    const effectiveType = isPasswordType && showPassword ? 'text' : type;
    const isCheckbox = type === 'checkbox';
    const autoId = useId();
    const inputId = (props as any).id || autoId;
    const errorId = error ? `${inputId}-error` : undefined;
    const helpId = helpText && !error ? `${inputId}-help` : undefined;

    if (isCheckbox) {
      return (
        <div className="w-full">
          <label htmlFor={inputId} className={cn('inline-flex items-center gap-2 cursor-pointer', className)}>
            <input
              ref={ref as React.Ref<HTMLInputElement>}
              id={inputId}
              type="checkbox"
              className="h-5 w-5 rounded border-2 border-neutral-300 bg-white text-primary-600 focus:ring-2 focus:ring-primary-500 focus:ring-offset-2 focus:ring-offset-white transition dark:border-neutral-600 dark:bg-primary-800 dark:text-secondary-300 dark:focus:ring-primary-400 dark:focus:ring-offset-primary-900"
              aria-describedby={errorId || helpId}
              {...(props as React.InputHTMLAttributes<HTMLInputElement>)}
            />
            {label && (
              <span className="text-sm font-medium text-neutral-700 dark:text-neutral-100">
                {label}
                {props.required && <span className="ml-1 text-error-500">*</span>}
              </span>
            )}
          </label>
          {error && <p id={errorId} className="mt-1 text-sm text-error-500" role="alert">{error}</p>}
          {helpText && !error && <p id={helpId} className="mt-1 text-sm text-neutral-500 dark:text-neutral-300">{helpText}</p>}
        </div>
      );
    }

    return (
      <div className="w-full">
        {label && (
          <label htmlFor={inputId} className="mb-2 block text-sm font-medium text-neutral-700 dark:text-sand-100">
            {label}
            {props.required && <span className="ml-1 text-error-500">*</span>}
          </label>
        )}
        <div className="relative flex items-center">
          {icon && iconPosition === 'left' && (
            <span className="absolute left-3 flex items-center text-neutral-400 dark:text-sand-400">{icon}</span>
          )}
          
          {select ? (
            <select
              ref={ref as React.Ref<HTMLSelectElement>}
              id={inputId}
              aria-label={label}
              aria-describedby={errorId || helpId}
              aria-invalid={!!error}
              className={cn(
                inputVariants({ variant: variantClass as any }),
                icon && iconPosition === 'left' && 'pl-10',
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
              id={inputId}
              aria-label={label}
              aria-describedby={errorId || helpId}
              aria-invalid={!!error}
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
              id={inputId}
              type={effectiveType}
              aria-label={label}
              aria-describedby={errorId || helpId}
              aria-invalid={!!error}
              className={cn(
                inputVariants({ variant: variantClass as any }),
                icon && iconPosition === 'left' && 'pl-10',
                (icon && iconPosition === 'right') || isPasswordType ? 'pr-10' : '',
                className
              )}
              {...props}
            />
          )}
          
          {icon && iconPosition === 'right' && !select && !isPasswordType && (
            <span className="absolute right-3 flex items-center text-neutral-400 dark:text-sand-400">{icon}</span>
          )}
          {isPasswordType && (
            <button
              type="button"
              onClick={() => setShowPassword(!showPassword)}
              className="absolute right-3 flex items-center text-neutral-400 dark:text-sand-400 hover:text-neutral-600 dark:hover:text-sand-200"
              aria-label={showPassword ? 'Hide password' : 'Show password'}
            >
              {showPassword ? <EyeOff className="w-5 h-5" /> : <Eye className="w-5 h-5" />}
            </button>
          )}
        </div>
        {error && <p id={errorId} className="mt-1 text-sm text-error-500" role="alert">{error}</p>}
        {helpText && !error && <p id={helpId} className="mt-1 text-sm text-neutral-500 dark:text-sand-400">{helpText}</p>}
      </div>
    );
  }
);
Input.displayName = 'Input';
