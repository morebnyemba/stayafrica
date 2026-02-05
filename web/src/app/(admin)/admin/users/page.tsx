'use client';

import { useEffect, useState } from 'react';
import { adminApi } from '@/lib/admin-api';
import { User } from '@/types';
import { Search, CheckCircle, XCircle, Edit, UserPlus, Ban, Trash2 } from 'lucide-react';
import toast from 'react-hot-toast';
import UserModal from '@/components/admin/UserModal';
import ConfirmDialog from '@/components/admin/ConfirmDialog';

export default function UsersManagement() {
  const [users, setUsers] = useState<User[]>([]);
  const [loading, setLoading] = useState(true);
  const [search, setSearch] = useState('');
  const [roleFilter, setRoleFilter] = useState<string>('');
  const [page, setPage] = useState(1);
  const [totalCount, setTotalCount] = useState(0);
  const [showUserModal, setShowUserModal] = useState(false);
  const [selectedUser, setSelectedUser] = useState<User | null>(null);
  const [showConfirmDialog, setShowConfirmDialog] = useState(false);
  const [confirmAction, setConfirmAction] = useState<{ type: string; userId: string; data?: any } | null>(null);

  useEffect(() => {
    loadUsers();
  }, [page, roleFilter]);

  const loadUsers = async () => {
    try {
      setLoading(true);
      const data = await adminApi.getUsers({ 
        page, 
        role: roleFilter || undefined,
        search: search || undefined,
      });
      setUsers(data.results);
      setTotalCount(data.count);
    } catch (err) {
      toast.error('Failed to load users');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleSearch = () => {
    setPage(1);
    loadUsers();
  };

  const handleVerify = async (userId: string) => {
    try {
      await adminApi.verifyUser(userId);
      toast.success('User verified successfully');
      loadUsers();
    } catch (err) {
      toast.error('Failed to verify user');
      console.error(err);
    }
  };

  const handleRoleChange = async (userId: string, newRole: 'admin' | 'guest' | 'host') => {
    try {
      await adminApi.updateUser(userId, { role: newRole });
      toast.success('User role updated successfully');
      loadUsers();
    } catch (err) {
      toast.error('Failed to update user role');
      console.error(err);
    }
  };

  const handleOpenUserModal = (user?: User) => {
    setSelectedUser(user || null);
    setShowUserModal(true);
  };

  const handleSaveUser = async (data: Partial<User>) => {
    try {
      if (selectedUser) {
        await adminApi.updateUser(selectedUser.id, data);
        toast.success('User updated successfully');
      } else {
        // Create new user would need a create endpoint
        toast.error('User creation not implemented yet');
      }
      loadUsers();
    } catch (err) {
      toast.error('Failed to save user');
      console.error(err);
    }
  };

  const handleSuspend = (userId: string) => {
    setConfirmAction({ type: 'suspend', userId });
    setShowConfirmDialog(true);
  };

  const handleDelete = (userId: string) => {
    setConfirmAction({ type: 'delete', userId });
    setShowConfirmDialog(true);
  };

  const handleConfirm = async () => {
    if (!confirmAction) return;

    try {
      if (confirmAction.type === 'suspend') {
        await adminApi.suspendUser(confirmAction.userId, 'Suspended by admin');
        toast.success('User suspended successfully');
      } else if (confirmAction.type === 'delete') {
        await adminApi.deleteUser(confirmAction.userId);
        toast.success('User deleted successfully');
      }
      loadUsers();
    } catch (err) {
      toast.error(`Failed to ${confirmAction.type} user`);
      console.error(err);
    }
  };

  return (
    <div className="p-8">
      <div className="mb-8 flex justify-between items-center">
        <div>
          <h1 className="text-3xl font-bold text-[#122F26]">User Management</h1>
        <p className="text-[#3A5C50] mt-2">Manage and monitor all platform users</p>
        </div>
        <button
          onClick={() => handleOpenUserModal()}
          className="flex items-center space-x-2 px-4 py-2 bg-[#D9B168] text-[#122F26] font-medium rounded-lg hover:bg-[#c9a158] transition-colors"
        >
          <UserPlus className="w-5 h-5" />
          <span>Add User</span>
        </button>
      </div>

      {/* Filters and Search */}
      <div className="bg-white rounded-lg shadow p-6 mb-6">
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
          <div className="md:col-span-2">
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Search Users
            </label>
            <div className="flex space-x-2">
              <div className="flex-1 relative">
                <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-gray-400 w-5 h-5" />
                <input
                  type="text"
                  value={search}
                  onChange={(e) => setSearch(e.target.value)}
                  onKeyPress={(e) => e.key === 'Enter' && handleSearch()}
                  placeholder="Search by name, email..."
                  className="w-full pl-10 pr-4 py-2 border border-[#3A5C50] rounded-lg focus:ring-2 focus:ring-[#D9B168] focus:border-transparent"
                />
              </div>
              <button
                onClick={handleSearch}
                className="px-6 py-2 bg-[#D9B168] text-[#122F26] font-medium rounded-lg hover:bg-[#c9a158] transition-colors"
              >
                Search
              </button>
            </div>
          </div>
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Filter by Role
            </label>
            <select
              value={roleFilter}
              onChange={(e) => {
                setRoleFilter(e.target.value);
                setPage(1);
              }}
              className="w-full px-4 py-2 border border-[#3A5C50] rounded-lg focus:ring-2 focus:ring-[#D9B168] focus:border-transparent"
            >
              <option value="">All Roles</option>
              <option value="guest">Guests</option>
              <option value="host">Hosts</option>
              <option value="admin">Admins</option>
            </select>
          </div>
        </div>
      </div>

      {/* Stats Cards */}
      <div className="grid grid-cols-1 md:grid-cols-4 gap-4 mb-6">
        <div className="bg-white rounded-lg shadow p-4">
          <p className="text-sm text-[#3A5C50]">Total Users</p>
          <p className="text-2xl font-bold text-[#122F26]">{totalCount}</p>
        </div>
        <div className="bg-white rounded-lg shadow p-4">
          <p className="text-sm text-[#3A5C50]">Guests</p>
          <p className="text-2xl font-bold text-[#122F26]">
            {users.filter(u => u.role === 'guest').length}
          </p>
        </div>
        <div className="bg-white rounded-lg shadow p-4">
          <p className="text-sm text-[#3A5C50]">Hosts</p>
          <p className="text-2xl font-bold text-[#122F26]">
            {users.filter(u => u.role === 'host').length}
          </p>
        </div>
        <div className="bg-white rounded-lg shadow p-4">
          <p className="text-sm text-[#3A5C50]">Verified</p>
          <p className="text-2xl font-bold text-green-600">
            {users.filter(u => u.is_verified).length}
          </p>
        </div>
      </div>

      {/* Users Table */}
      <div className="bg-white rounded-lg shadow overflow-hidden">
        {loading ? (
          <div className="flex items-center justify-center h-64">
            <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-[#D9B168]"></div>
          </div>
        ) : (
          <>
            <div className="overflow-x-auto">
              <table className="min-w-full divide-y divide-gray-200">
                <thead className="bg-gray-50">
                  <tr>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      User
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Contact
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Role
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Status
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Joined
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Actions
                    </th>
                  </tr>
                </thead>
                <tbody className="bg-white divide-y divide-gray-200">
                  {users.map((user) => (
                    <tr key={user.id} className="hover:bg-gray-50">
                      <td className="px-6 py-4 whitespace-nowrap">
                        <div className="flex items-center">
                          <div className="h-10 w-10 rounded-full bg-orange-100 flex items-center justify-center">
                            <span className="text-orange-600 font-medium">
                              {user.first_name?.[0]}{user.last_name?.[0]}
                            </span>
                          </div>
                          <div className="ml-4">
                            <div className="text-sm font-medium text-gray-900">
                              {user.first_name} {user.last_name}
                            </div>
                            <div className="text-sm text-gray-500">{user.id}</div>
                          </div>
                        </div>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap">
                        <div className="text-sm text-gray-900">{user.email}</div>
                        <div className="text-sm text-gray-500">{user.phone_number || 'N/A'}</div>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap">
                        <select
                          value={user.role}
                          onChange={(e) => handleRoleChange(user.id, e.target.value as 'admin' | 'guest' | 'host')}
                          className="text-sm border border-[#3A5C50] rounded px-2 py-1 focus:ring-2 focus:ring-[#D9B168]"
                        >
                          <option value="guest">Guest</option>
                          <option value="host">Host</option>
                          <option value="admin">Admin</option>
                        </select>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap">
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
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                        {new Date(user.created_at).toLocaleDateString()}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm font-medium">
                        <div className="flex items-center space-x-2">
                          {!user.is_verified && (
                            <button
                              onClick={() => handleVerify(user.id)}
                              className="px-3 py-1 text-sm bg-green-100 text-green-700 rounded hover:bg-green-200 transition-colors"
                            >
                              Verify
                            </button>
                          )}
                          <button
                            onClick={() => handleOpenUserModal(user)}
                            className="p-1 text-[#D9B168] hover:text-[#c9a158] transition-colors"
                            title="Edit user"
                          >
                            <Edit className="w-4 h-4" />
                          </button>
                          <button
                            onClick={() => handleSuspend(user.id)}
                            className="p-1 text-yellow-600 hover:text-yellow-800 transition-colors"
                            title="Suspend user"
                          >
                            <Ban className="w-4 h-4" />
                          </button>
                          <button
                            onClick={() => handleDelete(user.id)}
                            className="p-1 text-red-600 hover:text-red-800 transition-colors"
                            title="Delete user"
                          >
                            <Trash2 className="w-4 h-4" />
                          </button>
                        </div>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>

            {/* Pagination */}
            <div className="bg-gray-50 px-6 py-4 flex items-center justify-between border-t">
              <div className="text-sm text-gray-700">
                Showing {(page - 1) * 30 + 1} to {Math.min(page * 30, totalCount)} of {totalCount} users
              </div>
              <div className="flex space-x-2">
                <button
                  onClick={() => setPage(p => Math.max(1, p - 1))}
                  disabled={page === 1}
                  className="px-4 py-2 border border-gray-300 rounded-lg text-sm font-medium text-gray-700 hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  Previous
                </button>
                <button
                  onClick={() => setPage(p => p + 1)}
                  disabled={page * 30 >= totalCount}
                  className="px-4 py-2 border border-gray-300 rounded-lg text-sm font-medium text-gray-700 hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  Next
                </button>
              </div>
            </div>
          </>
        )}
      </div>

      {/* Modals */}
      <UserModal
        isOpen={showUserModal}
        onClose={() => setShowUserModal(false)}
        onSave={handleSaveUser}
        user={selectedUser}
      />
      
      <ConfirmDialog
        isOpen={showConfirmDialog}
        onClose={() => setShowConfirmDialog(false)}
        onConfirm={handleConfirm}
        title={confirmAction?.type === 'delete' ? 'Delete User' : 'Suspend User'}
        message={
          confirmAction?.type === 'delete'
            ? 'Are you sure you want to delete this user? This action cannot be undone.'
            : 'Are you sure you want to suspend this user? They will not be able to access the platform.'
        }
        variant={confirmAction?.type === 'delete' ? 'danger' : 'warning'}
        confirmText={confirmAction?.type === 'delete' ? 'Delete' : 'Suspend'}
      />
    </div>
  );
}
