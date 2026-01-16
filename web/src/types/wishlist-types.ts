// Wishlist Types for Collaborative Wishlists

export interface Wishlist {
  id: string;
  name: string;
  description?: string;
  privacy: 'private' | 'shared' | 'public';
  owner: {
    id: string;
    name: string;
    avatar?: string;
  };
  collaborators: WishlistCollaborator[];
  items: WishlistItem[];
  share_token?: string;
  created_at: string;
  updated_at: string;
}

export interface WishlistCollaborator {
  id: string;
  user: {
    id: string;
    name: string;
    email: string;
    avatar?: string;
  };
  added_at: string;
}

export interface WishlistItem {
  id: string;
  property: {
    id: string;
    name: string;
    image_url: string;
    base_price: number;
    location: string;
  };
  notes?: string;
  preferred_check_in?: string;
  preferred_check_out?: string;
  vote_count: number;
  user_vote?: 1 | -1 | null;
  comments: WishlistComment[];
  added_by: {
    id: string;
    name: string;
  };
  added_at: string;
}

export interface WishlistVote {
  id: string;
  user: {
    id: string;
    name: string;
  };
  vote: 1 | -1;
  created_at: string;
}

export interface WishlistComment {
  id: string;
  user: {
    id: string;
    name: string;
    avatar?: string;
  };
  text: string;
  created_at: string;
}

export interface CreateWishlistRequest {
  name: string;
  description?: string;
  privacy: 'private' | 'shared' | 'public';
}

export interface AddPropertyToWishlistRequest {
  property_id: string;
  notes?: string;
  preferred_check_in?: string;
  preferred_check_out?: string;
}

export interface AddCollaboratorRequest {
  email: string;
}

export interface VoteItemRequest {
  item_id: string;
  vote: 1 | -1;
}

export interface CommentItemRequest {
  item_id: string;
  text: string;
}
