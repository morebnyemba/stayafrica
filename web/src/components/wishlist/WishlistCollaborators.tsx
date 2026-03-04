'use client';

import React, { useState } from 'react';
import { useMutation, useQueryClient } from '@tanstack/react-query';
import { X, UserPlus, Trash2, Mail, Loader2 } from 'lucide-react';
import { WishlistCollaborator } from '@/types/wishlist-types';
import wishlistApi from '@/services/wishlist-api';

interface WishlistCollaboratorsProps {
  wishlistId: string;
  collaborators: WishlistCollaborator[];
  isOwner: boolean;
  onClose: () => void;
}

export default function WishlistCollaborators({
  wishlistId,
  collaborators,
  isOwner,
  onClose,
}: WishlistCollaboratorsProps) {
  const [email, setEmail] = useState('');
  const queryClient = useQueryClient();

  const addMutation = useMutation({
    mutationFn: (email: string) =>
      wishlistApi.addCollaborator(wishlistId, { email }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['wishlist', wishlistId] });
      setEmail('');
    },
  });

  const removeMutation = useMutation({
    mutationFn: (userId: string) =>
      wishlistApi.removeCollaborator(wishlistId, userId),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['wishlist', wishlistId] });
    },
  });

  const handleAdd = (e: React.FormEvent) => {
    e.preventDefault();
    if (email.trim()) {
      addMutation.mutate(email.trim());
    }
  };

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/50 p-4">
      <div className="bg-white dark:bg-primary-800/40 rounded-xl shadow-2xl max-w-lg w-full max-h-[90vh] overflow-y-auto">
        {/* Header */}
        <div className="sticky top-0 bg-white dark:bg-primary-800/40 border-b px-6 py-4 flex items-center justify-between">
          <h2 className="text-xl font-bold text-primary-900 dark:text-sand-50">Collaborators</h2>
          <button
            onClick={onClose}
            className="p-2 hover:bg-primary-100 dark:hover:bg-primary-800 rounded-full transition"
          >
            <X className="w-5 h-5" />
          </button>
        </div>

        <div className="p-6 space-y-6">
          {/* Add Collaborator Form */}
          {isOwner && (
            <div>
              <h3 className="font-semibold text-primary-900 dark:text-sand-50 mb-3">Add Collaborator</h3>
              <form onSubmit={handleAdd} className="flex gap-2">
                <input
                  type="email"
                  value={email}
                  onChange={(e) => setEmail(e.target.value)}
                  placeholder="Enter email address"
                  disabled={addMutation.isPending}
                  className="flex-1 px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-primary-500 focus:border-transparent disabled:bg-primary-100 dark:disabled:bg-primary-800 disabled:cursor-not-allowed"
                />
                <button
                  type="submit"
                  disabled={!email.trim() || addMutation.isPending}
                  className="px-4 py-2 bg-primary-600 text-white rounded-lg hover:bg-primary-700 transition disabled:opacity-50 disabled:cursor-not-allowed flex items-center gap-2"
                >
                  {addMutation.isPending ? (
                    <Loader2 className="w-5 h-5 animate-spin" />
                  ) : (
                    <UserPlus className="w-5 h-5" />
                  )}
                </button>
              </form>
              {addMutation.isError && (
                <p className="text-sm text-red-600 mt-2">
                  Failed to add collaborator. Please check the email and try again.
                </p>
              )}
            </div>
          )}

          {/* Collaborators List */}
          <div>
            <h3 className="font-semibold text-primary-900 dark:text-sand-50 mb-3">
              Current Collaborators ({collaborators.length})
            </h3>
            {collaborators.length === 0 ? (
              <div className="text-center py-8 text-primary-400 dark:text-sand-500">
                <UserPlus className="w-12 h-12 mx-auto mb-3 text-primary-300 dark:text-primary-500" />
                <p>No collaborators yet</p>
                <p className="text-sm mt-1">Add someone to share this wishlist with</p>
              </div>
            ) : (
              <div className="space-y-2">
                {collaborators.map((collaborator) => (
                  <div
                    key={collaborator.id}
                    className="flex items-center justify-between p-4 bg-sand-50 dark:bg-primary-900 rounded-lg"
                  >
                    <div className="flex items-center gap-3">
                      {collaborator.user.avatar ? (
                        <img
                          src={collaborator.user.avatar}
                          alt={collaborator.user.name}
                          className="w-10 h-10 rounded-full"
                        />
                      ) : (
                        <div className="w-10 h-10 rounded-full bg-primary-100 flex items-center justify-center">
                          <span className="text-primary-700 font-semibold">
                            {collaborator.user.name.charAt(0).toUpperCase()}
                          </span>
                        </div>
                      )}
                      <div>
                        <div className="font-medium text-primary-900 dark:text-sand-50">
                          {collaborator.user.name}
                        </div>
                        <div className="flex items-center gap-1 text-sm text-primary-500 dark:text-sand-400">
                          <Mail className="w-3 h-3" />
                          {collaborator.user.email}
                        </div>
                        <div className="text-xs text-primary-400 dark:text-sand-500 mt-0.5">
                          Added {new Date(collaborator.added_at).toLocaleDateString()}
                        </div>
                      </div>
                    </div>
                    {isOwner && (
                      <button
                        onClick={() => removeMutation.mutate(collaborator.user.id)}
                        disabled={removeMutation.isPending}
                        className="p-2 text-red-600 hover:bg-red-50 rounded-lg transition disabled:opacity-50 disabled:cursor-not-allowed"
                        title="Remove collaborator"
                      >
                        <Trash2 className="w-5 h-5" />
                      </button>
                    )}
                  </div>
                ))}
              </div>
            )}
          </div>

          {/* Info */}
          <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
            <p className="text-sm text-blue-900">
              Collaborators can add properties, vote, and comment on items in this wishlist.
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}
