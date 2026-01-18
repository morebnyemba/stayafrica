'use client';

import { useMutation, useQueryClient } from '@tanstack/react-query';
import { ThumbsUp, ThumbsDown } from 'lucide-react';
import wishlistApi from '@/services/wishlist-api';

interface WishlistVotingProps {
  wishlistId: string;
  itemId: string;
  voteCount: number;
  userVote: 1 | -1 | null | undefined;
}

export default function WishlistVoting({
  wishlistId,
  itemId,
  voteCount,
  userVote,
}: WishlistVotingProps) {
  const queryClient = useQueryClient();

  const voteMutation = useMutation({
    mutationFn: (vote: 1 | -1) =>
      wishlistApi.voteItem(wishlistId, { item_id: itemId, vote }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['wishlist', wishlistId] });
    },
  });

  const handleVote = (vote: 1 | -1) => {
    if (userVote === vote) {
      return;
    }
    voteMutation.mutate(vote);
  };

  const getVoteColor = () => {
    if (voteCount > 0) return 'text-green-600';
    if (voteCount < 0) return 'text-red-600';
    return 'text-gray-600';
  };

  return (
    <div className="flex items-center gap-2 bg-gray-50 rounded-lg p-3">
      <button
        onClick={() => handleVote(1)}
        disabled={voteMutation.isPending}
        className={`
          p-2 rounded-lg transition
          ${userVote === 1
            ? 'bg-green-100 text-green-600'
            : 'bg-white text-gray-600 hover:bg-green-50 hover:text-green-600'
          }
          disabled:opacity-50 disabled:cursor-not-allowed
        `}
        title="Upvote"
      >
        <ThumbsUp className="w-5 h-5" />
      </button>

      <div className="flex-1 text-center">
        <div className={`text-2xl font-bold ${getVoteColor()}`}>
          {voteCount > 0 ? '+' : ''}{voteCount}
        </div>
        <div className="text-xs text-gray-600">votes</div>
      </div>

      <button
        onClick={() => handleVote(-1)}
        disabled={voteMutation.isPending}
        className={`
          p-2 rounded-lg transition
          ${userVote === -1
            ? 'bg-red-100 text-red-600'
            : 'bg-white text-gray-600 hover:bg-red-50 hover:text-red-600'
          }
          disabled:opacity-50 disabled:cursor-not-allowed
        `}
        title="Downvote"
      >
        <ThumbsDown className="w-5 h-5" />
      </button>
    </div>
  );
}
