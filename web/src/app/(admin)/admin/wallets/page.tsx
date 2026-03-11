'use client';

import { useEffect, useState } from 'react';
import { adminApi } from '@/lib/admin-api';
import { Wallet } from '@/types/admin-types';
import { Search, CheckCircle, XCircle, Ban, ChevronDown, RefreshCw } from 'lucide-react';
import toast from 'react-hot-toast';
import ConfirmDialog from '@/components/admin/ConfirmDialog';

export default function WalletsManagement() {
  const [wallets, setWallets] = useState<Wallet[]>([]);
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
      // Ensure data.results is an array, handle API discrepancies
      setWallets(Array.isArray(data.results) ? data.results : []);
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
      const promises = selectedWallets.map(id => {
        switch (action) {
          case 'activate': return adminApi.activateWallet(id);
          case 'suspend': return adminApi.suspendWallet(id);
          case 'close': return adminApi.closeWallet(id);
          default: return Promise.resolve();
        }
      });
      
      await Promise.all(promises);
      toast.success(`${selectedWallets.length} wallets updated`);
      
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
    } finally {
      setShowConfirmDialog(false);
    }
  };

  const getStatusBadge = (status: string) => {
    const badges: Record<string, { color: string; text: string }> = {
      active: { color: 'bg-green-100 text-green-800 border-green-200', text: 'Active' },
      suspended: { color: 'bg-yellow-100 text-yellow-800 border-yellow-200', text: 'Suspended' },
      closed: { color: 'bg-red-100 text-red-800 border-red-200', text: 'Closed' },
    };
    return badges[status] || { color: 'bg-gray-100 text-gray-800 border-gray-200', text: status };
  };

  const formatCurrency = (amount: number, currency: string) => {
    try {
      return new Intl.NumberFormat('en-US', {
        style: 'currency',
        currency: currency || 'USD',
      }).format(amount);
    } catch (e) {
      return `${currency} ${amount}`;
    }
  };

  const formatDate = (dateString: string) => {
    if (!dateString) return 'N/A';
    return new Date(dateString).toLocaleDateString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
    });
  };

  return (
    <div className="p-8 max-w-7xl mx-auto">
      <div className="mb-8 flex flex-col sm:flex-row sm:items-center sm:justify-between">
        <div>
          <h1 className="text-3xl font-bold text-[#122F26]">Wallets Management</h1>
          <p className="text-[#3A5C50] mt-2">Monitor and manage user wallets and balances</p>
        </div>
        <button 
          onClick={loadWallets} 
          className="mt-4 sm:mt-0 flex items-center px-4 py-2 bg-white border border-gray-300 rounded-lg text-sm font-medium text-gray-700 hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-[#D9B168]"
        >
          <RefreshCw className={`w-4 h-4 mr-2 ${loading ? 'animate-spin' : ''}`} />
          Refresh
        </button>
      </div>

      {/* Filters and Search */}
      <div className="bg-white rounded-xl shadow-sm border border-gray-200 p-6 mb-6">
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
                  placeholder="Search by user email or name..."
                  className="w-full pl-10 pr-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-[#D9B168] focus:border-transparent"
                />
              </div>
              <button
                onClick={handleSearch}
                className="px-6 py-2 bg-[#D9B168] text-[#122F26] font-medium rounded-lg hover:bg-[#c9a158] transition-colors shadow-sm"
              >
                Search
              </button>
            </div>
          </div>
          <div>
            <label className="block text-sm font-medium text-[#122F26] mb-2">
              Filter by Status
            </label>
            <div className="relative">
              <select
                value={statusFilter}
                onChange={(e) => {
                  setStatusFilter(e.target.value);
                  setPage(1);
                }}
                className="w-full pl-4 pr-10 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-[#D9B168] focus:border-transparent appearance-none bg-white"
              >
                <option value="">All Status</option>
                <option value="active">Active</option>
                <option value="suspended">Suspended</option>
                <option value="closed">Closed</option>
              </select>
              <ChevronDown className="absolute right-3 top-1/2 transform -translate-y-1/2 text-gray-400 w-4 h-4 pointer-events-none" />
            </div>
          </div>
        </div>

        {/* Bulk Actions */}
        {selectedWallets.length > 0 && (
          <div className="mt-4 flex items-center justify-between bg-sand-50 border border-sand-200 px-4 py-3 rounded-lg">
            <p className="text-sm text-[#122F26] font-medium">
              {selectedWallets.length} wallet{selectedWallets.length > 1 ? 's' : ''} selected
            </p>
            <div className="relative">
              <button
                onClick={() => setShowBulkActions(!showBulkActions)}
                className="flex items-center space-x-2 px-4 py-2 bg-white border border-gray-300 text-[#122F26] font-medium rounded-lg hover:bg-gray-50 transition-colors shadow-sm"
              >
                <span>Bulk Actions</span>
                <ChevronDown className="w-4 h-4" />
              </button>
              {showBulkActions && (
                <div className="absolute right-0 mt-2 w-48 bg-white rounded-lg shadow-lg border border-gray-200 z-10 overflow-hidden">
                  <button
                    onClick={() => handleBulkAction('activate')}
                    className="w-full text-left px-4 py-3 text-sm text-gray-700 hover:bg-gray-50 border-b border-gray-100"
                  >
                    Activate Wallets
                  </button>
                  <button
                    onClick={() => handleBulkAction('suspend')}
                    className="w-full text-left px-4 py-3 text-sm text-gray-700 hover:bg-gray-50 border-b border-gray-100"
                  >
                    Suspend Wallets
                  </button>
                  <button
                    onClick={() => handleBulkAction('close')}
                    className="w-full text-left px-4 py-3 text-sm text-red-600 hover:bg-red-50"
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
      <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
        <div className="bg-white rounded-xl shadow-sm border border-gray-200 p-5">
          <p className="text-sm font-medium text-gray-500 uppercase tracking-wider">Total Wallets</p>
          <p className="text-2xl font-bold text-[#122F26] mt-1">{totalCount}</p>
        </div>
        <div className="bg-white rounded-xl shadow-sm border border-gray-200 p-5 border-l-4 border-l-green-500">
          <p className="text-sm font-medium text-gray-500 uppercase tracking-wider">Active</p>
          <p className="text-2xl font-bold text-green-600 mt-1">
            {wallets.filter(w => w.status === 'active').length}
          </p>
        </div>
        <div className="bg-white rounded-xl shadow-sm border border-gray-200 p-5 border-l-4 border-l-yellow-500">
          <p className="text-sm font-medium text-gray-500 uppercase tracking-wider">Suspended</p>
          <p className="text-2xl font-bold text-yellow-600 mt-1">
            {wallets.filter(w => w.status === 'suspended').length}
          </p>
        </div>
        <div className="bg-white rounded-xl shadow-sm border border-gray-200 p-5 border-l-4 border-l-red-500">
          <p className="text-sm font-medium text-gray-500 uppercase tracking-wider">Closed</p>
          <p className="text-2xl font-bold text-red-600 mt-1">
            {wallets.filter(w => w.status === 'closed').length}
          </p>
        </div>
      </div>

      {/* Wallets Table */}
      <div className="bg-white rounded-xl shadow-sm border border-gray-200 overflow-hidden">
        {loading ? (
          <div className="flex flex-col items-center justify-center h-64">
            <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-[#D9B168]"></div>
            <p className="mt-4 text-gray-500">Loading wallets...</p>
          </div>
        ) : wallets.length === 0 ? (
          <div className="flex flex-col items-center justify-center h-64 text-center px-4">
            <div className="bg-gray-100 p-3 rounded-full mb-3">
              <Search className="w-6 h-6 text-gray-400" />
            </div>
            <h3 className="text-lg font-medium text-gray-900">No wallets found</h3>
            <p className="text-gray-500 mt-1 max-w-sm">
              {search || statusFilter 
                ? "Try adjusting your search or filters to find what you're looking for." 
                : "No wallets exist in the system yet."}
            </p>
            {(search || statusFilter) && (
              <button 
                onClick={() => {setSearch(''); setStatusFilter('');}}
                className="mt-4 text-[#D9B168] hover:text-[#c9a158] font-medium"
              >
                Clear all filters
              </button>
            )}
          </div>
        ) : (
          <>
            <div className="overflow-x-auto">
              <table className="min-w-full divide-y divide-gray-200">
                <thead className="bg-gray-50">
                  <tr>
                    <th className="px-6 py-3 text-left w-10">
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
                        className="rounded border-gray-300 text-[#D9B168] focus:ring-[#D9B168]"
                      />
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      User
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Balance
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Status
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Updated
                    </th>
                    <th className="px-6 py-3 text-right text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Actions
                    </th>
                  </tr>
                </thead>
                <tbody className="bg-white divide-y divide-gray-200">
                  {wallets.map((wallet) => {
                    const statusBadge = getStatusBadge(wallet.status);
                    const userInitial = wallet.user?.first_name?.[0] || wallet.user?.email?.[0] || '?';
                    
                    return (
                      <tr key={wallet.id} className="hover:bg-gray-50 transition-colors">
                        <td className="px-6 py-4">
                          <input
                            type="checkbox"
                            checked={selectedWallets.includes(wallet.id)}
                            onChange={() => toggleSelectWallet(wallet.id)}
                            className="rounded border-gray-300 text-[#D9B168] focus:ring-[#D9B168]"
                          />
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap">
                          <div className="flex items-center">
                            <div className="flex-shrink-0 h-10 w-10 rounded-full bg-[#122F26] flex items-center justify-center text-white font-medium">
                              {userInitial.toUpperCase()}
                            </div>
                            <div className="ml-4">
                              <div className="text-sm font-medium text-gray-900">
                                {wallet.user ? (
                                  <>
                                    {wallet.user.first_name} {wallet.user.last_name}
                                  </>
                                ) : (
                                  `User #${wallet.user_id}`
                                )}
                              </div>
                              <div className="text-sm text-gray-500">
                                {wallet.user?.email || 'No email'}
                              </div>
                            </div>
                          </div>
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap">
                          <div className="text-sm font-bold text-[#122F26]">
                            {formatCurrency(wallet.balance, wallet.currency)}
                          </div>
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap">
                          <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium border ${statusBadge.color}`}>
                            {statusBadge.text}
                          </span>
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                          {formatDate(wallet.updated_at)}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-right text-sm font-medium">
                          <div className="flex items-center justify-end space-x-3">
                            {wallet.status !== 'active' && (
                              <button
                                onClick={() => handleActivate(wallet.id)}
                                className="text-green-600 hover:text-green-900 bg-green-50 p-1.5 rounded-full hover:bg-green-100 transition-colors"
                                title="Activate wallet"
                              >
                                <CheckCircle className="w-5 h-5" />
                              </button>
                            )}
                            {wallet.status === 'active' && (
                              <button
                                onClick={() => handleSuspend(wallet.id)}
                                className="text-yellow-600 hover:text-yellow-900 bg-yellow-50 p-1.5 rounded-full hover:bg-yellow-100 transition-colors"
                                title="Suspend wallet"
                              >
                                <Ban className="w-5 h-5" />
                              </button>
                            )}
                            {wallet.status !== 'closed' && (
                              <button
                                onClick={() => handleClose(wallet.id)}
                                className="text-red-600 hover:text-red-900 bg-red-50 p-1.5 rounded-full hover:bg-red-100 transition-colors"
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
            <div className="bg-gray-50 px-6 py-4 flex items-center justify-between border-t border-gray-200">
              <div className="text-sm text-gray-700">
                Showing <span className="font-medium">{totalCount > 0 ? (page - 1) * ITEMS_PER_PAGE + 1 : 0}</span> to <span className="font-medium">{Math.min(page * ITEMS_PER_PAGE, totalCount)}</span> of <span className="font-medium">{totalCount}</span> wallets
              </div>
              <div className="flex space-x-2">
                <button
                  onClick={() => setPage(p => Math.max(1, p - 1))}
                  disabled={page === 1}
                  className="px-4 py-2 border border-gray-300 rounded-lg text-sm font-medium text-gray-700 bg-white hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed shadow-sm"
                >
                  Previous
                </button>
                <button
                  onClick={() => setPage(p => p + 1)}
                  disabled={page * ITEMS_PER_PAGE >= totalCount}
                  className="px-4 py-2 border border-gray-300 rounded-lg text-sm font-medium text-gray-700 bg-white hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed shadow-sm"
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
