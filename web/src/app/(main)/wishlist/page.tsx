import type { Metadata } from 'next';
import { WishlistContent } from '@/components/buyer/wishlist-content';

export const metadata: Metadata = {
  title: 'My Wishlist - StayAfrica',
  description: 'View your saved properties',
};

export default function WishlistPage() {
  return <WishlistContent />;
}
