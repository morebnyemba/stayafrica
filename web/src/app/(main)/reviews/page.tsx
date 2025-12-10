import type { Metadata } from 'next';
import { ReviewsDashboard } from '@/components/buyer/reviews-dashboard';

export const metadata: Metadata = {
  title: 'My Reviews - StayAfrica',
  description: 'View reviews you have written and received',
};

export default function ReviewsPage() {
  return <ReviewsDashboard />;
}
