'use client';

import { useState } from 'react';
import Image from 'next/image';
import Link from 'next/link';
import { Calendar, MapPin, MessageSquare, StickyNote } from 'lucide-react';
import { WishlistItem } from '@/types/wishlist-types';
import WishlistVoting from './WishlistVoting';
import WishlistComments from './WishlistComments';

interface WishlistItemCardProps {
  item: WishlistItem;
  wishlistId: string;
  canEdit: boolean;
}

export default function WishlistItemCard({ item, wishlistId, canEdit: _canEdit }: WishlistItemCardProps) {
  const [showComments, setShowComments] = useState(false);

  return (
    <div className="bg-white rounded-xl shadow hover:shadow-lg transition overflow-hidden">
      {/* Property Image */}
      <Link href={`/property/${item.property.id}`} className="block relative h-48">
        <Image
          src={item.property.image_url || '/placeholder-property.jpg'}
          alt={item.property.name}
          fill
          className="object-cover"
        />
      </Link>

      <div className="p-4 space-y-3">
        {/* Property Info */}
        <div>
          <Link
            href={`/property/${item.property.id}`}
            className="font-semibold text-gray-900 hover:text-primary-600 transition line-clamp-2"
          >
            {item.property.name}
          </Link>
          <div className="flex items-center gap-1 text-sm text-gray-600 mt-1">
            <MapPin className="w-3 h-3" />
            <span className="line-clamp-1">{item.property.location}</span>
          </div>
          <div className="text-lg font-bold text-gray-900 mt-2">
            ${item.property.base_price}
            <span className="text-sm font-normal text-gray-600"> / night</span>
          </div>
        </div>

        {/* Preferred Dates */}
        {(item.preferred_check_in || item.preferred_check_out) && (
          <div className="bg-blue-50 border border-blue-200 rounded-lg p-3">
            <div className="flex items-center gap-2 text-sm text-blue-900">
              <Calendar className="w-4 h-4" />
              <span className="font-medium">Preferred dates:</span>
            </div>
            <p className="text-sm text-blue-800 mt-1">
              {item.preferred_check_in && new Date(item.preferred_check_in).toLocaleDateString()} 
              {' - '}
              {item.preferred_check_out && new Date(item.preferred_check_out).toLocaleDateString()}
            </p>
          </div>
        )}

        {/* Notes */}
        {item.notes && (
          <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-3">
            <div className="flex items-center gap-2 text-sm text-yellow-900 mb-1">
              <StickyNote className="w-4 h-4" />
              <span className="font-medium">Notes:</span>
            </div>
            <p className="text-sm text-yellow-800">{item.notes}</p>
          </div>
        )}

        {/* Voting */}
        <WishlistVoting
          wishlistId={wishlistId}
          itemId={item.id}
          voteCount={item.vote_count}
          userVote={item.user_vote}
        />

        {/* Comments Toggle */}
        <button
          onClick={() => setShowComments(!showComments)}
          className="w-full flex items-center justify-center gap-2 px-4 py-2 border border-gray-300 rounded-lg hover:bg-gray-50 transition text-sm font-medium text-gray-700"
        >
          <MessageSquare className="w-4 h-4" />
          <span>
            {item.comments.length} {item.comments.length === 1 ? 'Comment' : 'Comments'}
          </span>
        </button>

        {/* Comments Section */}
        {showComments && (
          <WishlistComments
            wishlistId={wishlistId}
            itemId={item.id}
            comments={item.comments}
          />
        )}

        {/* Added By */}
        <div className="text-xs text-gray-500 pt-3 border-t">
          Added by {item.added_by.name} • {new Date(item.added_at).toLocaleDateString()}
        </div>
      </div>
    </div>
  );
}
