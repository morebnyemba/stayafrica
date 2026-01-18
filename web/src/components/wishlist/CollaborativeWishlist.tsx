'use client';

import { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { Heart, Share2, UserPlus, Lock, Users, Globe, Loader2 } from 'lucide-react';
import wishlistApi from '@/services/wishlist-api';
import WishlistItemCard from './WishlistItemCard';
import WishlistShare from './WishlistShare';
import WishlistCollaborators from './WishlistCollaborators';

interface CollaborativeWishlistProps {
  wishlistId: string;
  isOwner: boolean;
  canEdit: boolean;
}

export default function CollaborativeWishlist({
  wishlistId,
  isOwner,
  canEdit,
}: CollaborativeWishlistProps) {
  const [showShareModal, setShowShareModal] = useState(false);
  const [showCollaborators, setShowCollaborators] = useState(false);
  const queryClient = useQueryClient();

  const { data: wishlist, isLoading } = useQuery({
    queryKey: ['wishlist', wishlistId],
    queryFn: () => wishlistApi.getWishlist(wishlistId),
  });

  const updatePrivacyMutation = useMutation({
    mutationFn: (privacy: 'private' | 'shared' | 'public') =>
      wishlistApi.updateWishlist(wishlistId, { privacy }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['wishlist', wishlistId] });
    },
  });

  if (isLoading) {
    return (
      <div className="flex items-center justify-center py-12">
        <Loader2 className="w-8 h-8 animate-spin text-primary-600" />
      </div>
    );
  }

  if (!wishlist) {
    return (
      <div className="bg-white rounded-xl shadow p-12 text-center">
        <Heart className="w-16 h-16 text-gray-400 mx-auto mb-4" />
        <h3 className="text-xl font-semibold text-gray-900 mb-2">Wishlist not found</h3>
      </div>
    );
  }

  const getPrivacyIcon = (privacy: string) => {
    switch (privacy) {
      case 'private':
        return <Lock className="w-4 h-4" />;
      case 'shared':
        return <Users className="w-4 h-4" />;
      case 'public':
        return <Globe className="w-4 h-4" />;
      default:
        return <Lock className="w-4 h-4" />;
    }
  };

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="bg-white rounded-xl shadow p-6">
        <div className="flex items-start justify-between flex-wrap gap-4">
          <div className="flex-1">
            <div className="flex items-center gap-3 mb-2">
              <Heart className="w-8 h-8 text-red-500 fill-red-500" />
              <h1 className="text-3xl font-bold text-gray-900">{wishlist.name}</h1>
            </div>
            {wishlist.description && (
              <p className="text-gray-600 mt-2">{wishlist.description}</p>
            )}
            <div className="flex items-center gap-4 mt-4 text-sm text-gray-600">
              <span>Created by {wishlist.owner.name}</span>
              <span>•</span>
              <span>{wishlist.items.length} {wishlist.items.length === 1 ? 'property' : 'properties'}</span>
              {wishlist.collaborators.length > 0 && (
                <>
                  <span>•</span>
                  <span>{wishlist.collaborators.length} {wishlist.collaborators.length === 1 ? 'collaborator' : 'collaborators'}</span>
                </>
              )}
            </div>
          </div>

          {/* Actions */}
          <div className="flex items-center gap-2">
            {/* Privacy Toggle */}
            {isOwner && (
              <div className="flex bg-gray-100 rounded-lg p-1">
                {(['private', 'shared', 'public'] as const).map((privacy) => (
                  <button
                    key={privacy}
                    onClick={() => updatePrivacyMutation.mutate(privacy)}
                    disabled={updatePrivacyMutation.isPending}
                    className={`
                      flex items-center gap-1 px-3 py-2 rounded-md text-sm font-medium transition
                      ${wishlist.privacy === privacy
                        ? 'bg-white shadow text-primary-600'
                        : 'text-gray-600 hover:text-gray-900'
                      }
                    `}
                  >
                    {getPrivacyIcon(privacy)}
                    <span className="capitalize">{privacy}</span>
                  </button>
                ))}
              </div>
            )}

            {/* Collaborators Button */}
            {(isOwner || wishlist.collaborators.length > 0) && (
              <button
                onClick={() => setShowCollaborators(true)}
                className="flex items-center gap-2 px-4 py-2 border border-gray-300 rounded-lg hover:bg-gray-50 transition"
              >
                <UserPlus className="w-5 h-5" />
                <span>Collaborators</span>
              </button>
            )}

            {/* Share Button */}
            <button
              onClick={() => setShowShareModal(true)}
              className="flex items-center gap-2 px-4 py-2 bg-primary-600 text-white rounded-lg hover:bg-primary-700 transition"
            >
              <Share2 className="w-5 h-5" />
              <span>Share</span>
            </button>
          </div>
        </div>
      </div>

      {/* Items Grid */}
      {wishlist.items.length === 0 ? (
        <div className="bg-white rounded-xl shadow p-12 text-center">
          <Heart className="w-16 h-16 text-gray-400 mx-auto mb-4" />
          <h3 className="text-xl font-semibold text-gray-900 mb-2">No properties yet</h3>
          <p className="text-gray-600">Start adding properties to your wishlist</p>
        </div>
      ) : (
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
          {wishlist.items.map((item: any) => (
            <WishlistItemCard
              key={item.id}
              item={item}
              wishlistId={wishlistId}
              canEdit={canEdit}
            />
          ))}
        </div>
      )}

      {/* Modals */}
      {showShareModal && (
        <WishlistShare
          wishlistId={wishlistId}
          wishlistName={wishlist.name}
          shareToken={wishlist.share_token}
          privacy={wishlist.privacy}
          onClose={() => setShowShareModal(false)}
        />
      )}

      {showCollaborators && (
        <WishlistCollaborators
          wishlistId={wishlistId}
          collaborators={wishlist.collaborators}
          isOwner={isOwner}
          onClose={() => setShowCollaborators(false)}
        />
      )}
    </div>
  );
}
