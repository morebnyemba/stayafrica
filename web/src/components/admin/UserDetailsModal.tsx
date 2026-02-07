'use client';

import { useState, useEffect } from 'react';
import Modal from './Modal';
import { User } from '@/types';
import { adminApi } from '@/lib/admin-api';
import { 
  Mail, Phone, MapPin, Calendar, CheckCircle, XCircle, 
  Shield, User as UserIcon, Briefcase 
} from 'lucide-react';

interface UserDetailsModalProps {
  isOpen: boolean;
  onClose: () => void;
  userId: string;
}

export default function UserDetailsModal({ isOpen, onClose, userId }: UserDetailsModalProps) {
  const [user, setUser] = useState<User | null>(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    if (isOpen && userId) {
      loadUserDetails();
    }
  }, [isOpen, userId]);

  const loadUserDetails = async () => {
    try {
      setLoading(true);
      const data = await adminApi.getUserById(userId);
      setUser(data);
    } catch (error) {
      console.error('Error loading user details:', error);
    } finally {
      setLoading(false);
    }
  };

  if (!isOpen) return null;

  return (
    <Modal
      isOpen={isOpen}
      onClose={onClose}
      title="User Details"
      size="lg"
    >
      {loading ? (
        <div className="flex items-center justify-center h-64">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-[#D9B168]"></div>
        </div>
      ) : user ? (
        <div className="space-y-6">
          {/* Profile Header */}
          <div className="flex items-center space-x-4 pb-6 border-b">
            <div className="h-20 w-20 rounded-full bg-[#F4F1EA] flex items-center justify-center">
              {user.profile_picture ? (
                <img
                  src={user.profile_picture}
                  alt={`${user.first_name} ${user.last_name}`}
                  className="h-20 w-20 rounded-full object-cover"
                />
              ) : (
                <span className="text-3xl text-[#D9B168] font-medium">
                  {user.first_name?.[0]}{user.last_name?.[0]}
                </span>
              )}
            </div>
            <div className="flex-1">
              <h3 className="text-2xl font-bold text-[#122F26]">
                {user.first_name} {user.last_name}
              </h3>
              <p className="text-[#3A5C50]">{user.username}</p>
              <div className="flex items-center space-x-2 mt-2">
                {user.is_verified ? (
                  <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
                    <CheckCircle className="w-3 h-3 mr-1" />
                    Verified
                  </span>
                ) : (
                  <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-yellow-100 text-yellow-800">
                    <XCircle className="w-3 h-3 mr-1" />
                    Unverified
                  </span>
                )}
                {user.is_active ? (
                  <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-blue-100 text-blue-800">
                    Active
                  </span>
                ) : (
                  <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
                    Inactive
                  </span>
                )}
              </div>
            </div>
          </div>

          {/* Contact Information */}
          <div>
            <h4 className="text-lg font-semibold text-[#122F26] mb-3 flex items-center">
              <UserIcon className="w-5 h-5 mr-2" />
              Contact Information
            </h4>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              <div className="flex items-start space-x-3">
                <Mail className="w-5 h-5 text-[#3A5C50] mt-0.5" />
                <div>
                  <p className="text-sm text-gray-500">Email</p>
                  <p className="text-sm text-[#122F26] font-medium">{user.email}</p>
                </div>
              </div>
              <div className="flex items-start space-x-3">
                <Phone className="w-5 h-5 text-[#3A5C50] mt-0.5" />
                <div>
                  <p className="text-sm text-gray-500">Phone</p>
                  <p className="text-sm text-[#122F26] font-medium">
                    {user.phone_number || 'Not provided'}
                  </p>
                </div>
              </div>
            </div>
          </div>

          {/* Role & Location */}
          <div>
            <h4 className="text-lg font-semibold text-[#122F26] mb-3 flex items-center">
              <Briefcase className="w-5 h-5 mr-2" />
              Role & Location
            </h4>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              <div className="flex items-start space-x-3">
                <Shield className="w-5 h-5 text-[#3A5C50] mt-0.5" />
                <div>
                  <p className="text-sm text-gray-500">Role</p>
                  <p className="text-sm text-[#122F26] font-medium capitalize">{user.role}</p>
                </div>
              </div>
              <div className="flex items-start space-x-3">
                <MapPin className="w-5 h-5 text-[#3A5C50] mt-0.5" />
                <div>
                  <p className="text-sm text-gray-500">Country</p>
                  <p className="text-sm text-[#122F26] font-medium">
                    {user.country_of_residence || 'Not specified'}
                  </p>
                </div>
              </div>
            </div>
          </div>

          {/* Bio */}
          {user.bio && (
            <div>
              <h4 className="text-lg font-semibold text-[#122F26] mb-3">Bio</h4>
              <p className="text-sm text-[#3A5C50]">{user.bio}</p>
            </div>
          )}

          {/* Account Information */}
          <div>
            <h4 className="text-lg font-semibold text-[#122F26] mb-3 flex items-center">
              <Calendar className="w-5 h-5 mr-2" />
              Account Information
            </h4>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              <div>
                <p className="text-sm text-gray-500">Date Joined</p>
                <p className="text-sm text-[#122F26] font-medium">
                  {new Date(user.created_at).toLocaleDateString('en-US', {
                    year: 'numeric',
                    month: 'long',
                    day: 'numeric'
                  })}
                </p>
              </div>
              {user.last_login && (
                <div>
                  <p className="text-sm text-gray-500">Last Login</p>
                  <p className="text-sm text-[#122F26] font-medium">
                    {new Date(user.last_login).toLocaleDateString('en-US', {
                      year: 'numeric',
                      month: 'long',
                      day: 'numeric'
                    })}
                  </p>
                </div>
              )}
            </div>
          </div>

          {/* Permissions */}
          {(user.is_staff || user.is_superuser) && (
            <div>
              <h4 className="text-lg font-semibold text-[#122F26] mb-3">Permissions</h4>
              <div className="flex flex-wrap gap-2">
                {user.is_staff && (
                  <span className="inline-flex items-center px-3 py-1 rounded-full text-xs font-medium bg-purple-100 text-purple-800">
                    Staff
                  </span>
                )}
                {user.is_superuser && (
                  <span className="inline-flex items-center px-3 py-1 rounded-full text-xs font-medium bg-red-100 text-red-800">
                    Superuser
                  </span>
                )}
              </div>
            </div>
          )}

          {/* Action Buttons */}
          <div className="flex justify-end space-x-3 pt-4 border-t">
            <button
              onClick={onClose}
              className="px-6 py-2 border border-gray-300 text-gray-700 rounded-lg hover:bg-gray-50 transition-colors"
            >
              Close
            </button>
          </div>
        </div>
      ) : (
        <div className="text-center py-8 text-gray-500">
          User not found
        </div>
      )}
    </Modal>
  );
}
