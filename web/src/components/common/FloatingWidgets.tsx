"use client";

import { usePathname } from 'next/navigation';
import { SupportChatWidget } from '@/components/support/SupportChatWidget';
import { BugReportButton } from '@/components/bugs/BugReportButton';

export function FloatingWidgets() {
  const pathname = usePathname();
  // Don't show on admin pages
  if (pathname?.startsWith('/admin')) return null;
  return (
    <>
      <SupportChatWidget />
      <BugReportButton />
    </>
  );
}
