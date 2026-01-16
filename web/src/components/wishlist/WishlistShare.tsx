'use client';

import React, { useState, useEffect } from 'react';
import { X, Copy, Check, Mail, Facebook, Twitter, Link as LinkIcon } from 'lucide-react';

interface WishlistShareProps {
  wishlistId: string;
  wishlistName: string;
  shareToken?: string;
  privacy: 'private' | 'shared' | 'public';
  onClose: () => void;
}

export default function WishlistShare({
  wishlistId,
  wishlistName,
  shareToken,
  privacy,
  onClose,
}: WishlistShareProps) {
  const [copied, setCopied] = useState(false);
  const [shareUrl, setShareUrl] = useState('');

  useEffect(() => {
    if (typeof window !== 'undefined') {
      const url = shareToken
        ? `${window.location.origin}/wishlists/shared/${shareToken}`
        : `${window.location.origin}/wishlists/${wishlistId}`;
      setShareUrl(url);
    }
  }, [wishlistId, shareToken]);

  const handleCopy = async () => {
    try {
      await navigator.clipboard.writeText(shareUrl);
      setCopied(true);
      setTimeout(() => setCopied(false), 2000);
    } catch (err) {
      console.error('Failed to copy:', err);
    }
  };

  const shareViaEmail = () => {
    const subject = encodeURIComponent(`Check out my wishlist: ${wishlistName}`);
    const body = encodeURIComponent(`I'd like to share my wishlist with you!\n\n${shareUrl}`);
    window.location.href = `mailto:?subject=${subject}&body=${body}`;
  };

  const shareViaFacebook = () => {
    window.open(
      `https://www.facebook.com/sharer/sharer.php?u=${encodeURIComponent(shareUrl)}`,
      '_blank',
      'width=600,height=400'
    );
  };

  const shareViaTwitter = () => {
    const text = encodeURIComponent(`Check out my wishlist: ${wishlistName}`);
    window.open(
      `https://twitter.com/intent/tweet?url=${encodeURIComponent(shareUrl)}&text=${text}`,
      '_blank',
      'width=600,height=400'
    );
  };

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/50 p-4">
      <div className="bg-white rounded-xl shadow-2xl max-w-md w-full">
        {/* Header */}
        <div className="flex items-center justify-between p-6 border-b">
          <h2 className="text-xl font-bold text-gray-900">Share Wishlist</h2>
          <button
            onClick={onClose}
            className="p-2 hover:bg-gray-100 rounded-full transition"
          >
            <X className="w-5 h-5" />
          </button>
        </div>

        <div className="p-6 space-y-6">
          {/* Privacy Notice */}
          {privacy === 'private' && (
            <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-4">
              <p className="text-sm text-yellow-900">
                This wishlist is private. Only you can see it. Change to "shared" or "public" to share with others.
              </p>
            </div>
          )}

          {/* Share URL */}
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Share link
            </label>
            <div className="flex gap-2">
              <input
                type="text"
                value={shareUrl}
                readOnly
                className="flex-1 px-4 py-2 border border-gray-300 rounded-lg bg-gray-50 text-sm"
              />
              <button
                onClick={handleCopy}
                className={`
                  px-4 py-2 rounded-lg font-medium transition flex items-center gap-2
                  ${copied
                    ? 'bg-green-600 text-white'
                    : 'bg-primary-600 text-white hover:bg-primary-700'
                  }
                `}
              >
                {copied ? (
                  <>
                    <Check className="w-4 h-4" />
                    Copied!
                  </>
                ) : (
                  <>
                    <Copy className="w-4 h-4" />
                    Copy
                  </>
                )}
              </button>
            </div>
          </div>

          {/* Share Buttons */}
          {privacy !== 'private' && (
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-3">
                Share via
              </label>
              <div className="grid grid-cols-2 gap-3">
                <button
                  onClick={shareViaEmail}
                  className="flex items-center justify-center gap-2 px-4 py-3 border border-gray-300 rounded-lg hover:bg-gray-50 transition"
                >
                  <Mail className="w-5 h-5 text-gray-600" />
                  <span className="font-medium text-gray-700">Email</span>
                </button>
                <button
                  onClick={shareViaFacebook}
                  className="flex items-center justify-center gap-2 px-4 py-3 border border-gray-300 rounded-lg hover:bg-gray-50 transition"
                >
                  <Facebook className="w-5 h-5 text-blue-600" />
                  <span className="font-medium text-gray-700">Facebook</span>
                </button>
                <button
                  onClick={shareViaTwitter}
                  className="flex items-center justify-center gap-2 px-4 py-3 border border-gray-300 rounded-lg hover:bg-gray-50 transition"
                >
                  <Twitter className="w-5 h-5 text-blue-400" />
                  <span className="font-medium text-gray-700">Twitter</span>
                </button>
                <button
                  onClick={handleCopy}
                  className="flex items-center justify-center gap-2 px-4 py-3 border border-gray-300 rounded-lg hover:bg-gray-50 transition"
                >
                  <LinkIcon className="w-5 h-5 text-gray-600" />
                  <span className="font-medium text-gray-700">Copy Link</span>
                </button>
              </div>
            </div>
          )}

          {/* Info */}
          <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
            <p className="text-sm text-blue-900">
              {privacy === 'public' 
                ? 'Anyone with the link can view this wishlist.'
                : privacy === 'shared'
                ? 'Only people with the link can view this wishlist.'
                : 'This wishlist is private. Only you can see it.'
              }
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}
