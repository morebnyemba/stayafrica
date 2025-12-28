import React from 'react';

type BaseProps = {
  label?: string;
  error?: string;
  hint?: string;
  className?: string;
};

export function FormField({ label, error, hint, className, children }: React.PropsWithChildren<BaseProps>) {
  return (
    <div className={className}>
      {label && <label className="block text-sm font-medium text-primary-900 dark:text-sand-100 mb-2">{label}</label>}
      {children}
      {error ? (
        <p className="mt-1 text-sm text-red-600 dark:text-red-400" role="alert">{error}</p>
      ) : hint ? (
        <p className="mt-1 text-xs text-primary-500 dark:text-sand-400">{hint}</p>
      ) : null}
    </div>
  );
}

export const Input = React.forwardRef<HTMLInputElement, React.InputHTMLAttributes<HTMLInputElement>>(
  function Input({ className = '', ...props }, ref) {
    return (
      <input
        ref={ref}
        {...props}
        className={
          `w-full px-4 py-3 bg-sand-50 dark:bg-primary-700 border rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent transition ` +
          `border-primary-200 dark:border-primary-600 text-primary-900 dark:text-sand-100 placeholder-primary-400 dark:placeholder-sand-400 ` +
          className
        }
      />
    );
  }
);

export const Select = React.forwardRef<HTMLSelectElement, React.SelectHTMLAttributes<HTMLSelectElement>>(
  function Select({ className = '', children, ...props }, ref) {
    return (
      <select
        ref={ref}
        {...props}
        className={
          `w-full px-4 py-3 bg-sand-50 dark:bg-primary-700 border rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent transition ` +
          `border-primary-200 dark:border-primary-600 text-primary-900 dark:text-sand-100 ` +
          className
        }
      >
        {children}
      </select>
    );
  }
);

export const TextArea = React.forwardRef<HTMLTextAreaElement, React.TextareaHTMLAttributes<HTMLTextAreaElement>>(
  function TextArea({ className = '', ...props }, ref) {
    return (
      <textarea
        ref={ref}
        {...props}
        className={
          `w-full px-4 py-3 bg-sand-50 dark:bg-primary-700 border rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent transition ` +
          `border-primary-200 dark:border-primary-600 text-primary-900 dark:text-sand-100 placeholder-primary-400 dark:placeholder-sand-400 ` +
          className
        }
      />
    );
  }
);

export function FieldError({ children }: React.PropsWithChildren<{}>) {
  if (!children) return null;
  return <p className="mt-1 text-sm text-red-600 dark:text-red-400" role="alert">{children}</p>;
}
