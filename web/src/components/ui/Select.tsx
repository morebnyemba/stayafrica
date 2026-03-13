"use client";

import React, { useState, useEffect, useRef } from 'react';
import { cn } from '@/lib/utils';
import { ChevronDown, Check } from 'lucide-react';

interface SelectContextValue {
  value: string;
  onValueChange: (val: string) => void;
  isOpen: boolean;
  setIsOpen: (val: boolean) => void;
  options: { label: string; value: string }[];
  registerOption: (val: string, label: string) => void;
}

const SelectContext = React.createContext<SelectContextValue | null>(null);

export function Select({ 
  value, 
  onValueChange, 
  children,
  required
}: { 
  value?: string; 
  onValueChange?: (val: string) => void; 
  children: React.ReactNode;
  required?: boolean;
}) {
  const [isOpen, setIsOpen] = useState(false);
  const [internalValue, setInternalValue] = useState(value || '');
  const [options, setOptions] = useState<{label: string, value: string}[]>([]);
  
  const ctxValue = value !== undefined ? value : internalValue;
  const ctxOnChange = onValueChange || setInternalValue;

  const registerOption = (val: string, label: string) => {
    setOptions(prev => {
      if (prev.find(o => o.value === val)) return prev;
      return [...prev, { value: val, label }];
    });
  };

  return (
    <SelectContext.Provider value={{
      value: ctxValue,
      onValueChange: ctxOnChange,
      isOpen,
      setIsOpen,
      options,
      registerOption
    }}>
      <div className="relative w-full">
        {children}
        {required && (
          <input type="hidden" required value={ctxValue} />
        )}
      </div>
    </SelectContext.Provider>
  );
}

export const SelectTrigger = React.forwardRef<HTMLButtonElement, React.ButtonHTMLAttributes<HTMLButtonElement>>(
  ({ className, children, ...props }, ref) => {
    const ctx = React.useContext(SelectContext);
    if (!ctx) return null;
    
    return (
      <button
        ref={ref}
        type="button"
        onClick={() => ctx.setIsOpen(!ctx.isOpen)}
        className={cn(
          "flex h-10 w-full items-center justify-between rounded-md border border-input bg-transparent px-3 py-2 text-sm ring-offset-background placeholder:text-muted-foreground focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2 disabled:cursor-not-allowed disabled:opacity-50",
          className
        )}
        {...props}
      >
        {children}
        <ChevronDown className="h-4 w-4 opacity-50" />
      </button>
    );
  }
);
SelectTrigger.displayName = 'SelectTrigger';

export function SelectValue({ placeholder }: { placeholder?: string }) {
  const ctx = React.useContext(SelectContext);
  if (!ctx) return null;
  
  const selectedOption = ctx.options.find(o => o.value === ctx.value);
  return (
    <span className={cn("block truncate", !ctx.value && "text-muted-foreground")}>
      {selectedOption ? selectedOption.label : placeholder}
    </span>
  );
}

export function SelectContent({ children, className }: { children: React.ReactNode; className?: string }) {
  const ctx = React.useContext(SelectContext);
  const ref = useRef<HTMLDivElement>(null);
  
  useEffect(() => {
    const handleClickOutside = (e: MouseEvent) => {
      if (ref.current && !ref.current.contains(e.target as Node)) {
        ctx?.setIsOpen(false);
      }
    };
    
    if (ctx?.isOpen) {
      document.addEventListener('mousedown', handleClickOutside);
    }
    
    return () => document.removeEventListener('mousedown', handleClickOutside);
  }, [ctx?.isOpen]);
  
  if (!ctx?.isOpen) return null;
  
  return (
    <div 
      ref={ref}
      className={cn(
        "absolute z-50 min-w-[8rem] overflow-hidden rounded-md border bg-white p-1 text-popover-foreground shadow-md animate-in fade-in-80 top-[calc(100%+4px)] left-0 w-full",
        className
      )}
    >
      <div className="w-full max-h-96 overflow-y-auto">
        {children}
      </div>
    </div>
  );
}

export const SelectItem = React.forwardRef<HTMLDivElement, React.HTMLAttributes<HTMLDivElement> & { value: string }>(
  ({ className, children, value, ...props }, ref) => {
    const ctx = React.useContext(SelectContext);
    
    useEffect(() => {
      if (ctx && typeof children === 'string') {
        ctx.registerOption(value, children);
      }
    }, [ctx, value, children]);
    
    if (!ctx) return null;
    
    const isSelected = ctx.value === value;
    
    return (
      <div
        ref={ref}
        onClick={() => {
          ctx.onValueChange(value);
          ctx.setIsOpen(false);
        }}
        className={cn(
          "relative flex w-full cursor-default select-none items-center rounded-sm py-1.5 pl-8 pr-2 text-sm outline-none hover:bg-muted/50 focus:bg-accent focus:text-accent-foreground data-[disabled]:pointer-events-none data-[disabled]:opacity-50",
          isSelected && "bg-muted font-medium",
          className
        )}
        {...props}
      >
        <span className="absolute left-2 flex h-3.5 w-3.5 items-center justify-center">
          {isSelected && <Check className="h-4 w-4" />}
        </span>
        {children}
      </div>
    );
  }
);
SelectItem.displayName = 'SelectItem';
