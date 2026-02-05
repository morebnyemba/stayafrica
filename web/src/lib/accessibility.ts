/**
 * Accessibility Utilities & ARIA Helpers
 */
import React from 'react';

/**
 * ARIA Attributes for common patterns
 */
export const ariaLabels = {
  // Navigation
  mainNav: 'Main navigation',
  mobileMenu: 'Mobile menu',
  userMenu: 'User account menu',
  
  // Forms
  required: 'Required field',
  optional: 'Optional field',
  
  // Buttons
  search: 'Search properties',
  close: 'Close dialog',
  menu: 'Menu',
  back: 'Go back',
  
  // Errors
  error: 'Error',
  warning: 'Warning',
  success: 'Success',
  info: 'Information',
};

/**
 * Common ARIA roles for semantic HTML
 */
export const roles = {
  dialog: 'dialog',
  alert: 'alert',
  alertDialog: 'alertdialog',
  button: 'button',
  checkbox: 'checkbox',
  combobox: 'combobox',
  menuitem: 'menuitem',
  option: 'option',
  progressbar: 'progressbar',
  radio: 'radio',
  tab: 'tab',
  tablist: 'tablist',
  textbox: 'textbox',
};

/**
 * Keyboard event helpers
 */
export const keyboardEvents = {
  isEnter: (event: KeyboardEvent): boolean => event.key === 'Enter',
  isEscape: (event: KeyboardEvent): boolean => event.key === 'Escape',
  isArrowUp: (event: KeyboardEvent): boolean => event.key === 'ArrowUp',
  isArrowDown: (event: KeyboardEvent): boolean => event.key === 'ArrowDown',
  isArrowLeft: (event: KeyboardEvent): boolean => event.key === 'ArrowLeft',
  isArrowRight: (event: KeyboardEvent): boolean => event.key === 'ArrowRight',
  isTab: (event: KeyboardEvent): boolean => event.key === 'Tab',
};

/**
 * Keyboard navigation handler for lists/menus
 */
export const handleListKeydown = (
  event: React.KeyboardEvent,
  items: any[],
  selectedIndex: number,
  onSelect: (index: number) => void
) => {
  const length = items.length;

  if (keyboardEvents.isArrowDown(event as any)) {
    event.preventDefault();
    onSelect((selectedIndex + 1) % length);
  } else if (keyboardEvents.isArrowUp(event as any)) {
    event.preventDefault();
    onSelect((selectedIndex - 1 + length) % length);
  } else if (keyboardEvents.isTab(event as any)) {
    onSelect((selectedIndex + 1) % length);
  }
};

/**
 * Focus trap for modals
 */
export const createFocusTrap = (element: HTMLElement) => {
  const focusableElements = element.querySelectorAll(
    'button, [href], input, select, textarea, [tabindex]:not([tabindex="-1"])'
  );
  const firstElement = focusableElements[0] as HTMLElement;
  const lastElement = focusableElements[focusableElements.length - 1] as HTMLElement;

  return {
    firstElement,
    lastElement,
    all: Array.from(focusableElements) as HTMLElement[],
  };
};

/**
 * Announce screen reader messages
 */
export const announceToScreenReader = (message: string, priority: 'polite' | 'assertive' = 'polite') => {
  const announcement = document.createElement('div');
  announcement.setAttribute('role', 'status');
  announcement.setAttribute('aria-live', priority);
  announcement.setAttribute('aria-atomic', 'true');
  announcement.className = 'sr-only'; // Visually hidden but accessible to screen readers
  announcement.textContent = message;

  document.body.appendChild(announcement);

  // Remove after announcement
  setTimeout(() => announcement.remove(), 1000);
};

/**
 * Color contrast checker (WCAG AA minimum: 4.5:1 for normal text, 3:1 for large text)
 */
export const getContrastRatio = (rgb1: string, rgb2: string): number => {
  const getLuminance = (rgb: string): number => {
    const [r, g, b] = rgb.match(/\d+/g)!.map(Number);
    const [rs, gs, bs] = [r, g, b].map((v) => {
      v = v / 255;
      return v <= 0.03928 ? v / 12.92 : Math.pow((v + 0.055) / 1.055, 2.4);
    });
    return 0.2126 * rs + 0.7152 * gs + 0.0722 * bs;
  };

  const l1 = getLuminance(rgb1);
  const l2 = getLuminance(rgb2);
  const lighter = Math.max(l1, l2);
  const darker = Math.min(l1, l2);

  return (lighter + 0.05) / (darker + 0.05);
};

/**
 * Skip to main content link
 */
export const SkipToMainContent = () => (
  React.createElement(
    'a',
    {
      href: '#main-content',
      className:
        'absolute top-0 left-0 -translate-y-full focus:translate-y-0 bg-primary-500 text-white px-4 py-2 rounded-b-lg font-medium',
    },
    'Skip to main content'
  )
);
