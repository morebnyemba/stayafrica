"use client";

import React, { useState, useEffect } from 'react';
import { cn } from '@/lib/utils';
import { X } from 'lucide-react';

interface DialogContextValue {
  isOpen: boolean;
  setIsOpen: (val: boolean) => void;
}

const DialogContext = React.createContext<DialogContextValue>({ isOpen: false, setIsOpen: () => {} });

export function Dialog({ children, open, onOpenChange }: { children: React.ReactNode, open?: boolean, onOpenChange?: (open: boolean) => void }) {
  const [isOpen, setIsOpen] = useState(open || false);

  useEffect(() => {
    if (open !== undefined) setIsOpen(open);
  }, [open]);

  const handleOpenChange = (val: boolean) => {
    setIsOpen(val);
    if (onOpenChange) onOpenChange(val);
  };

  return (
    <DialogContext.Provider value={{ isOpen, setIsOpen: handleOpenChange }}>
      {children}
    </DialogContext.Provider>
  );
}

export function DialogTrigger({ children, asChild }: { children: React.ReactElement, asChild?: boolean }) {
  const { setIsOpen } = React.useContext(DialogContext);
  
  if (asChild) {
    return React.cloneElement(children, {
      onClick: (e: any) => {
        if (children.props.onClick) children.props.onClick(e);
        setIsOpen(true);
      }
    });
  }
  
  return <div onClick={() => setIsOpen(true)} className="inline-block">{children}</div>;
}

export function DialogContent({ children, className }: { children: React.ReactNode, className?: string }) {
  const { isOpen, setIsOpen } = React.useContext(DialogContext);
  
  if (!isOpen) return null;
  
  return (
    <div className="fixed inset-0 z-50 bg-black/50 flex items-center justify-center p-4">
      <div 
        className={cn("bg-white rounded-xl shadow-xl w-full max-w-lg relative", className)}
        onClick={(e) => e.stopPropagation()}
      >
        <button 
          onClick={() => setIsOpen(false)}
          className="absolute right-4 top-4 rounded-sm opacity-70 hover:opacity-100 focus:outline-none focus:ring-2 focus:ring-ring"
        >
          <X className="h-4 w-4" />
          <span className="sr-only">Close</span>
        </button>
        {children}
      </div>
    </div>
  );
}

export function DialogHeader({ className, ...props }: React.HTMLAttributes<HTMLDivElement>) {
  return (
    <div className={cn("flex flex-col space-y-1.5 text-center sm:text-left p-6", className)} {...props} />
  );
}

export function DialogTitle({ className, ...props }: React.HTMLAttributes<HTMLHeadingElement>) {
  return (
    <h2 className={cn("text-lg font-semibold leading-none tracking-tight", className)} {...props} />
  );
}
