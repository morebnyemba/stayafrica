// Wishlist API Service
import { apiClient } from './api-client';
import {
  Wishlist,
  CreateWishlistRequest,
  AddPropertyToWishlistRequest,
  AddCollaboratorRequest,
  VoteItemRequest,
  CommentItemRequest,
} from '@/types/wishlist-types';

export const wishlistApi = {
  /**
   * Get all wishlists for current user
   */
  async getWishlists() {
    const response = await apiClient.get<Wishlist[]>('/wishlists/');
    return response.data;
  },

  /**
   * Get single wishlist by ID
   */
  async getWishlist(wishlistId: string) {
    const response = await apiClient.get<Wishlist>(`/wishlists/${wishlistId}/`);
    return response.data;
  },

  /**
   * Get wishlist by share token (public access)
   */
  async getSharedWishlist(shareToken: string) {
    const response = await apiClient.get<Wishlist>(`/wishlists/${shareToken}/shared/`);
    return response.data;
  },

  /**
   * Create new wishlist
   */
  async createWishlist(data: CreateWishlistRequest) {
    const response = await apiClient.post<Wishlist>('/wishlists/', data);
    return response.data;
  },

  /**
   * Update wishlist
   */
  async updateWishlist(wishlistId: string, data: Partial<CreateWishlistRequest>) {
    const response = await apiClient.put<Wishlist>(`/wishlists/${wishlistId}/`, data);
    return response.data;
  },

  /**
   * Delete wishlist
   */
  async deleteWishlist(wishlistId: string) {
    await apiClient.delete(`/wishlists/${wishlistId}/`);
  },

  /**
   * Add property to wishlist
   */
  async addProperty(wishlistId: string, data: AddPropertyToWishlistRequest) {
    const response = await apiClient.post(
      `/wishlists/${wishlistId}/add_property/`,
      data
    );
    return response.data;
  },

  /**
   * Remove property from wishlist
   */
  async removeProperty(wishlistId: string, propertyId: string) {
    await apiClient.post(`/wishlists/${wishlistId}/remove_property/`, {
      property_id: propertyId,
    });
  },

  /**
   * Add collaborator to wishlist
   */
  async addCollaborator(wishlistId: string, data: AddCollaboratorRequest) {
    const response = await apiClient.post(
      `/wishlists/${wishlistId}/add_collaborator/`,
      data
    );
    return response.data;
  },

  /**
   * Remove collaborator from wishlist
   */
  async removeCollaborator(wishlistId: string, userId: string) {
    await apiClient.post(`/wishlists/${wishlistId}/remove_collaborator/`, {
      user_id: userId,
    });
  },

  /**
   * Vote on wishlist item (upvote: 1, downvote: -1)
   */
  async voteItem(wishlistId: string, data: VoteItemRequest) {
    const response = await apiClient.post(
      `/wishlists/${wishlistId}/vote_item/`,
      data
    );
    return response.data;
  },

  /**
   * Add comment to wishlist item
   */
  async commentItem(wishlistId: string, data: CommentItemRequest) {
    const response = await apiClient.post(
      `/wishlists/${wishlistId}/comment_item/`,
      data
    );
    return response.data;
  },
};

export default wishlistApi;
