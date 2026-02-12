'use client';

import { useEffect, useState } from 'react';
import { adminApi } from '@/lib/admin-api';
import { Search, CheckCircle, XCircle, Ban, ChevronDown } from 'lucide-react';
import toast from 'react-hot-toast';
import ConfirmDialog from '@/components/admin/ConfirmDialog';

export default function WalletsManagement() {
  const [wallets, setWallets] = useState<any[]>([]);
  const [loading, setLoading] = useState(true);
  const [search, setSearch] = useState('');
  const [statusFilter, setStatusFilter] = useState<string>('');
  const [page, setPage] = useState(1);
  const [totalCount, setTotalCount] = useState(0);
  const [selectedWallets, setSelectedWallets] = useState<string[]>([]);
  const [showBulkActions, setShowBulkActions] = useState(false);
  const [showConfirmDialog, setShowConfirmDialog] = useState(false);
  const [confirmAction, setConfirmAction] = useState<{ type: string; walletId?: string } | null>(null);
  const ITEMS_PER_PAGE = 20;

  useEffect(() => {
    loadWallets();
  }, [page, statusFilter, search]);

  const loadWallets = async () => {
    try {
      setLoading(true);
      const data = await adminApi.getWallets({ 
        page, 
        status: statusFilter || undefined,
        search: search.trim() || undefined,
        per_page: ITEMS_PER_PAGE,
      });
      setWallets(data.results || []);
      setTotalCount(data.count || 0);
    } catch (err: any) {
      const errorMsg = err?.response?.data?.detail || 'Failed to load wallets';
      toast.error(errorMsg);
      console.error('Wallets load error:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleSearch = () => {
    setPage(1);
  };

  const toggleSelectWallet = (walletId: string) => {
    setSelectedWallets(prev => 
      prev.includes(walletId) 
        ? prev.filter(id => id !== walletId)
        : [...prev, walletId]
    );
  };

  const handleActivate = (walletId: string) => {
    setConfirmAction({ type: 'activate', walletId });
    setShowConfirmDialog(true);
  };

  const handleSuspend = (walletId: string) => {
    setConfirmAction({ type: 'suspend', walletId });
    setShowConfirmDialog(true);
  };

  const handleClose = (walletId: string) => {
    setConfirmAction({ type: 'close', walletId });
    setShowConfirmDialog(true);
  };

  const handleBulkAction = async (action: string) => {
    if (selectedWallets.length === 0) {
      toast.error('No wallets selected');
      return;
    }

    try {
      switch (action) {
        case 'activate':
          await Promise.all(selectedWallets.map(id => adminApi.activateWallet(id)));
          toast.success(`${selectedWallets.length} wallets activated`);
          break;
        case 'suspend':
          await Promise.all(selectedWallets.map(id => adminApi.suspendWallet(id)));
          toast.success(`${selectedWallets.length} wallets suspended`);
          break;
        case 'close':
          await Promise.all(selectedWallets.map(id => adminApi.closeWallet(id)));
          toast.success(`${selectedWallets.length} wallets closed`);
          break;
        default:
          toast.error('Unknown action');
          return;
      }
      setSelectedWallets([]);
      setShowBulkActions(false);
      loadWallets();
    } catch (err) {
      toast.error('Failed to perform bulk action');
      console.error(err);
    }
  };

  const handleConfirm = async () => {
    if (!confirmAction || !confirmAction.walletId) return;

    try {
      switch (confirmAction.type) {
        case 'activate':
          await adminApi.activateWallet(confirmAction.walletId);
          toast.success('Wallet activated successfully');
          break;
        case 'suspend':
          await adminApi.suspendWallet(confirmAction.walletId);
          toast.success('Wallet suspended successfully');
          break;
        case 'close':
          await adminApi.closeWallet(confirmAction.walletId);
          toast.success('Wallet closed successfully');
          break;
      }
      loadWallets();
    } catch (err) {
      toast.error(`Failed to ${confirmAction.type} wallet`);
      console.error(err);
    }
  };

  const getStatusBadge = (status: string) => {
    const badges: Record<string, { color: string; text: string }> = {
      active: { color: 'bg-green-100 text-green-800', text: 'Active' },
      suspended: { color: 'bg-yellow-100 text-yellow-800', text: 'Suspended' },
      closed: { color: 'bg-red-100 text-red-800', text: 'Closed' },
    };
    return badges[status] || { color: 'bg-gray-100 text-gray-800', text: status };
  };

  return (
    <div className="p-8">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-[#122F26]">Wallets Management</h1>
        <p className="text-[#3A5C50] mt-2">Monitor and manage user wallets</p>
      </div>

      {/* Filters and Search */}
      <div className="bg-white rounded-lg shadow p-6 mb-6">
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
          <div className="md:col-span-2">
            <label className="block text-sm font-medium text-[#122F26] mb-2">
              Search Wallets
            </label>
            <div className="flex space-x-2">
              <div className="flex-1 relative">
                <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-[#3A5C50] w-5 h-5" />
                <input
                  type="text"
                  value={search}
                  onChange={(e) => setSearch(e.target.value)}
                  onKeyPress={(e) => e.key === 'Enter' && handleSearch()}
                  placeholder="Search by user email..."
                  className="w-full pl-10 pr-4 py-2 border border-[#3A5C50] rounded-lg focus:ring-2 focus:ring-[#D9B168] focus:border-transparent"
                />
              </div>
              <button
                onClick={handleSearch}
                className="px-6 py-2 bg-[#D9B168] text-[#122F26] rounded-lg hover:bg-[#c9a158] transition-colors"
              >
                Search
              </button>
            </div>
          </div>
          <div>
            <label className="block text-sm font-medium text-[#122F26] mb-2">
              Filter by Status
            </label>
            <select
              value={statusFilter}
              onChange={(e) => {
                setStatusFilter(e.target.value);
                setPage(1);
              }}
              className="w-full px-4 py-2 border border-[#3A5C50] rounded-lg focus:ring-2 focus:ring-[#D9B168] focus:border-transparent"
            >
              <option value="">All Status</option>
              <option value="active">Active</option>
              <option value="suspended">Suspended</option>
              <option value="closed">Closed</option>
            </select>
          </div>
        </div>

        {/* Bulk Actions */}
        {selectedWallets.length > 0 && (
          <div className="mt-4 flex items-center justify-between bg-[#F4F1EA] px-4 py-3 rounded-lg">
            <p className="text-sm text-[#122F26] font-medium">
              {selectedWallets.length} wallet{selectedWallets.length > 1 ? 's' : ''} selected
            </p>
            <div className="relative">
              <button
                onClick={() => setShowBulkActions(!showBulkActions)}
                className="flex items-center space-x-2 px-4 py-2 bg-[#D9B168] text-[#122F26] font-medium rounded-lg hover:bg-[#c9a158] transition-colors"
              >
                <span>Bulk Actions</span>
                <ChevronDown className="w-4 h-4" />
              </button>
              {showBulkActions && (
                <div className="absolute right-0 mt-2 w-48 bg-white rounded-lg shadow-lg border border-gray-200 z-10">
                  <button
                    onClick={() => handleBulkAction('activate')}
                    className="w-full text-left px-4 py-2 text-sm text-gray-700 hover:bg-gray-100 first:rounded-t-lg"
                  >
                    Activate Wallets
                  </button>
                  <button
                    onClick={() => handleBulkAction('suspend')}
                    className="w-full text-left px-4 py-2 text-sm text-gray-700 hover:bg-gray-100"
                  >
                    Suspend Wallets
                  </button>
                  <button
                    onClick={() => handleBulkAction('close')}
                    className="w-full text-left px-4 py-2 text-sm text-gray-700 hover:bg-gray-100 last:rounded-b-lg"
                  >
                    Close Wallets
                  </button>
                </div>
              )}
            </div>
          </div>
        )}
      </div>

      {/* Stats Cards */}
      <div className="grid grid-cols-1 md:grid-cols-4 gap-4 mb-6">
        <div className="bg-white rounded-lg shadow p-4">
          <p className="text-sm text-[#3A5C50]">Total Wallets</p>
          <p className="text-2xl font-bold text-[#122F26]">{totalCount}</p>
        </div>
        <div className="bg-white rounded-lg shadow p-4">
          <p className="text-sm text-[#3A5C50]">Active</p>
          <p className="text-2xl font-bold text-green-600">
            {wallets.filter(w => w.status === 'active').length}
          </p>
        </div>
        <div className="bg-white rounded-lg shadow p-4">
          <p className="text-sm text-[#3A5C50]">Suspended</p>
          <p className="text-2xl font-bold text-yellow-600">
            {wallets.filter(w => w.status === 'suspended').length}
          </p>
        </div>
        <div className="bg-white rounded-lg shadow p-4">
          <p className="text-sm text-[#3A5C50]">Closed</p>
          <p className="text-2xl font-bold text-red-600">
            {wallets.filter(w => w.status === 'closed').length}
          </p>
        </div>
      </div>

      {/* Wallets Table */}
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
                    <th className="px-6 py-3 text-left">
                      <input
                        type="checkbox"
                        onChange={(e) => {
                          if (e.target.checked) {
                            setSelectedWallets(wallets.map(w => w.id));
                          } else {
                            setSelectedWallets([]);
                          }
                        }}
                        checked={selectedWallets.length === wallets.length && wallets.length > 0}
                        className="rounded border-gray-300"
                      />
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      User
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Balance
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Currency
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Status
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Updated
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Actions
                    </th>
                  </tr>
                </thead>
                <tbody className="bg-white divide-y divide-gray-200">
                  {wallets.map((wallet) => {
                    const statusBadge = getStatusBadge(wallet.status);
                    return (
                      <tr key={wallet.id} className="hover:bg-gray-50">
                        <td className="px-6 py-4">
                          <input
                            type="checkbox"
                            checked={selectedWallets.includes(wallet.id)}
                            onChange={() => toggleSelectWallet(wallet.id)}
                            className="rounded border-gray-300"
                          />
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap">
                          <div className="text-sm font-medium text-[#122F26]">
                            {wallet.user?.email || `User #${wallet.user_id}`}
                          </div>
                          <div className="text-sm text-[#3A5C50]">
                            ID: {wallet.id}
                          </div>
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap">
                          <div className="text-sm font-medium text-[#122F26]">
                            {wallet.balance?.toLocaleString() || '0'}
                          </div>
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-[#122F26]">
                          {wallet.currency || 'USD'}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap">
                          <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${statusBadge.color}`}>
                            {statusBadge.text}
                          </span>
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50]">
                          {new Date(wallet.updated_at).toLocaleDateString()}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm font-medium">
                          <div className="flex items-center space-x-2">
                            {wallet.status !== 'active' && (
                              <button
                                onClick={() => handleActivate(wallet.id)}
                                className="text-green-600 hover:text-green-900"
                                title="Activate wallet"
                              >
                                <CheckCircle className="w-5 h-5" />
                              </button>
                            )}
                            {wallet.status === 'active' && (
                              <button
                                onClick={() => handleSuspend(wallet.id)}
                                className="text-yellow-600 hover:text-yellow-900"
                                title="Suspend wallet"
                              >
                                <Ban className="w-5 h-5" />
                              </button>
                            )}
                            {wallet.status !== 'closed' && (
                              <button
                                onClick={() => handleClose(wallet.id)}
                                className="text-red-600 hover:text-red-900"
                                title="Close wallet"
                              >
                                <XCircle className="w-5 h-5" />
                              </button>
                            )}
                          </div>
                        </td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            </div>

            {/* Pagination */}
            <div className="bg-gray-50 px-6 py-4 flex items-center justify-between border-t">
              <div className="text-sm text-[#122F26]">
                Showing {totalCount > 0 ? (page - 1) * ITEMS_PER_PAGE + 1 : 0} to {Math.min(page * ITEMS_PER_PAGE, totalCount)} of {totalCount} wallets
              </div>
              <div className="flex space-x-2">
                <button
                  onClick={() => setPage(p => Math.max(1, p - 1))}
                  disabled={page === 1}
                  className="px-4 py-2 border border-[#3A5C50] rounded-lg text-sm font-medium text-[#122F26] hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  Previous
                </button>
                <button
                  onClick={() => setPage(p => p + 1)}
                  disabled={page * ITEMS_PER_PAGE >= totalCount}
                  className="px-4 py-2 border border-[#3A5C50] rounded-lg text-sm font-medium text-[#122F26] hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  Next
                </button>
              </div>
            </div>
          </>
        )}
      </div>

      {/* Confirm Dialog */}
      <ConfirmDialog
        isOpen={showConfirmDialog}
        onClose={() => setShowConfirmDialog(false)}
        onConfirm={handleConfirm}
        title={
          confirmAction?.type === 'close' ? 'Close Wallet' :
          confirmAction?.type === 'suspend' ? 'Suspend Wallet' : 'Activate Wallet'
        }
        message={
          confirmAction?.type === 'close'
            ? 'Are you sure you want to close this wallet? This action cannot be undone.'
            : confirmAction?.type === 'suspend'
            ? 'Are you sure you want to suspend this wallet? The user will not be able to use it.'
            : 'Are you sure you want to activate this wallet?'
        }
        variant={confirmAction?.type === 'close' ? 'danger' : 'warning'}
        confirmText={confirmAction?.type === 'close' ? 'Close' : confirmAction?.type === 'suspend' ? 'Suspend' : 'Activate'}
      />
    </div>
  );
}
