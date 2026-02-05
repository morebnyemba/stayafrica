import type { Metadata } from 'next';
import { HostReviews } from '@/components/host/host-reviews';

export const metadata: Metadata = {
  title: 'Guest Reviews - Host Dashboard - StayAfrica',
  description: 'View and manage reviews from your guests',
};

export default function HostReviewsPage() {
  return <HostReviews />;
}
