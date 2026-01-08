'use client';

import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { Wallet, ArrowUpRight, ArrowDownLeft, Building, Plus, CheckCircle, Clock, XCircle } from 'lucide-react';
import dynamic from 'next/dynamic';
import { useState } from 'react';
const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });
import { Button } from '@/components/ui/Button';

export function WalletDashboard() {
  const [showWithdrawModal, setShowWithdrawModal] = useState(false);
  const [showBankAccountModal, setShowBankAccountModal] = useState(false);
  const [activeTab, setActiveTab] = useState<'transactions' | 'withdrawals' | 'banks'>('transactions');

  // Fetch wallet data
  const { data: wallet, isLoading: walletLoading } = useQuery({
    queryKey: ['wallet'],
    queryFn: async () => {
      const response = await apiClient.getMyWallet();
      return response.data;
    },
  });

  // Fetch transactions
  const { data: transactionsData, isLoading: transactionsLoading } = useQuery({
    queryKey: ['wallet-transactions', wallet?.id],
    queryFn: async () => {
      if (!wallet?.id) return { results: [] };
      const response = await apiClient.getWalletTransactions(wallet.id);
      return response.data;
    },
    enabled: !!wallet?.id,
  });

  // Fetch withdrawals
  const { data: withdrawalsData, isLoading: withdrawalsLoading } = useQuery({
    queryKey: ['withdrawals'],
    queryFn: async () => {
      const response = await apiClient.getWithdrawals();
      return response.data;
    },
  });

  // Fetch bank accounts
  const { data: bankAccounts, isLoading: banksLoading } = useQuery({
    queryKey: ['bank-accounts'],
    queryFn: async () => {
      const response = await apiClient.getBankAccounts();
      return response.data;
    },
  });

  const transactions = transactionsData?.results || [];
  const withdrawals = withdrawalsData?.results || [];
  const banks = bankAccounts?.results || bankAccounts || [];

  return (
    <ProtectedRoute>
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
        <div className="max-w-6xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          {/* Header */}
          <header className="mb-8">
            <h1 className="text-3xl font-bold text-primary-900 dark:text-sand-50 mb-2">My Wallet</h1>
            <p className="text-primary-600 dark:text-sand-300">Manage your funds, transactions, and withdrawals</p>
          </header>

          {/* Wallet Balance Card */}
          <div className="bg-gradient-to-br from-primary-600 to-primary-800 dark:from-primary-700 dark:to-primary-900 rounded-xl p-6 sm:p-8 mb-8 text-white shadow-lg">
            <div className="flex items-center gap-3 mb-4">
              <Wallet className="w-8 h-8" />
              <h2 className="text-lg font-semibold">Available Balance</h2>
            </div>
            {walletLoading ? (
              <div className="animate-pulse">
                <div className="h-12 w-48 bg-primary-500/30 rounded mb-2"></div>
                <div className="h-4 w-32 bg-primary-500/30 rounded"></div>
              </div>
            ) : (
              <>
                <div className="text-4xl sm:text-5xl font-bold mb-2">
                  {wallet?.currency || 'USD'} {parseFloat(wallet?.balance || '0').toLocaleString(undefined, { minimumFractionDigits: 2, maximumFractionDigits: 2 })}
                </div>
                <div className="text-primary-200 text-sm">
                  Status: <span className="font-semibold capitalize">{wallet?.status || 'Active'}</span>
                </div>
              </>
            )}
            <div className="flex gap-3 mt-6">
              <button
                onClick={() => setShowWithdrawModal(true)}
                disabled={!wallet || wallet?.status !== 'active' || parseFloat(wallet?.balance || '0') < 10}
                className="flex items-center gap-2 bg-white text-primary-700 px-4 py-2 rounded-lg font-semibold hover:bg-primary-50 transition disabled:opacity-50 disabled:cursor-not-allowed"
              >
                <ArrowUpRight className="w-5 h-5" />
                Withdraw
              </button>
              <button
                onClick={() => setShowBankAccountModal(true)}
                className="flex items-center gap-2 bg-primary-700 text-white px-4 py-2 rounded-lg font-semibold hover:bg-primary-600 transition border border-primary-500"
              >
                <Building className="w-5 h-5" />
                Manage Banks
              </button>
            </div>
          </div>

          {/* Tabs */}
          <div className="mb-6 border-b border-primary-200 dark:border-primary-700">
            <nav className="flex gap-6">
              <button
                onClick={() => setActiveTab('transactions')}
                className={`pb-3 px-1 font-semibold transition ${
                  activeTab === 'transactions'
                    ? 'border-b-2 border-primary-600 text-primary-900 dark:text-sand-50'
                    : 'text-primary-600 dark:text-sand-400 hover:text-primary-900 dark:hover:text-sand-200'
                }`}
              >
                Transactions
              </button>
              <button
                onClick={() => setActiveTab('withdrawals')}
                className={`pb-3 px-1 font-semibold transition ${
                  activeTab === 'withdrawals'
                    ? 'border-b-2 border-primary-600 text-primary-900 dark:text-sand-50'
                    : 'text-primary-600 dark:text-sand-400 hover:text-primary-900 dark:hover:text-sand-200'
                }`}
              >
                Withdrawals
              </button>
              <button
                onClick={() => setActiveTab('banks')}
                className={`pb-3 px-1 font-semibold transition ${
                  activeTab === 'banks'
                    ? 'border-b-2 border-primary-600 text-primary-900 dark:text-sand-50'
                    : 'text-primary-600 dark:text-sand-400 hover:text-primary-900 dark:hover:text-sand-200'
                }`}
              >
                Bank Accounts
              </button>
            </nav>
          </div>

          {/* Tab Content */}
          {activeTab === 'transactions' && (
            <TransactionsList transactions={transactions} loading={transactionsLoading} />
          )}
          {activeTab === 'withdrawals' && (
            <WithdrawalsList withdrawals={withdrawals} loading={withdrawalsLoading} />
          )}
          {activeTab === 'banks' && (
            <BankAccountsList
              bankAccounts={banks}
              loading={banksLoading}
              onAddNew={() => setShowBankAccountModal(true)}
            />
          )}

          {/* Modals */}
          {showWithdrawModal && (
            <WithdrawModal
              wallet={wallet}
              bankAccounts={banks}
              onClose={() => setShowWithdrawModal(false)}
            />
          )}
          {showBankAccountModal && (
            <BankAccountModal onClose={() => setShowBankAccountModal(false)} />
          )}
        </div>
      </div>
    </ProtectedRoute>
  );
}

function TransactionsList({ transactions, loading }: { transactions: any[]; loading: boolean }) {
  if (loading) {
    return (
      <div className="space-y-3">
        {[1, 2, 3].map(i => (
          <div key={i} className="animate-pulse h-20 bg-white dark:bg-primary-800 rounded-lg"></div>
        ))}
      </div>
    );
  }

  if (transactions.length === 0) {
    return (
      <div className="bg-white dark:bg-primary-800 rounded-lg p-8 text-center border border-primary-200 dark:border-primary-700">
        <p className="text-primary-600 dark:text-sand-300">No transactions yet</p>
      </div>
    );
  }

  return (
    <div className="space-y-3">
      {transactions.map((txn: any) => (
        <div
          key={txn.id}
          className="bg-white dark:bg-primary-800 rounded-lg p-4 border border-primary-100 dark:border-primary-700 flex items-center justify-between"
        >
          <div className="flex items-center gap-4">
            <div className={`p-3 rounded-full ${
              txn.txn_type === 'credit' || txn.txn_type === 'refund'
                ? 'bg-green-100 text-green-600'
                : 'bg-red-100 text-red-600'
            }`}>
              {txn.txn_type === 'credit' || txn.txn_type === 'refund' ? (
                <ArrowDownLeft className="w-5 h-5" />
              ) : (
                <ArrowUpRight className="w-5 h-5" />
              )}
            </div>
            <div>
              <div className="font-semibold text-primary-900 dark:text-sand-50 capitalize">{txn.txn_type}</div>
              <div className="text-sm text-primary-600 dark:text-sand-400">{txn.reference}</div>
              <div className="text-xs text-primary-500 dark:text-sand-500">
                {new Date(txn.created_at).toLocaleDateString()} {new Date(txn.created_at).toLocaleTimeString()}
              </div>
            </div>
          </div>
          <div className="text-right">
            <div className={`text-lg font-bold ${
              txn.txn_type === 'credit' || txn.txn_type === 'refund'
                ? 'text-green-600'
                : 'text-red-600'
            }`}>
              {txn.txn_type === 'credit' || txn.txn_type === 'refund' ? '+' : '-'}
              {txn.currency} {parseFloat(txn.amount).toLocaleString(undefined, { minimumFractionDigits: 2 })}
            </div>
            <div className="flex items-center gap-1 justify-end mt-1">
              {txn.status === 'completed' ? (
                <CheckCircle className="w-4 h-4 text-green-500" />
              ) : txn.status === 'pending' ? (
                <Clock className="w-4 h-4 text-yellow-500" />
              ) : (
                <XCircle className="w-4 h-4 text-red-500" />
              )}
              <span className="text-xs text-primary-600 dark:text-sand-400 capitalize">{txn.status}</span>
            </div>
          </div>
        </div>
      ))}
    </div>
  );
}

function WithdrawalsList({ withdrawals, loading }: { withdrawals: any[]; loading: boolean }) {
  if (loading) {
    return (
      <div className="space-y-3">
        {[1, 2, 3].map(i => (
          <div key={i} className="animate-pulse h-20 bg-white dark:bg-primary-800 rounded-lg"></div>
        ))}
      </div>
    );
  }

  if (withdrawals.length === 0) {
    return (
      <div className="bg-white dark:bg-primary-800 rounded-lg p-8 text-center border border-primary-200 dark:border-primary-700">
        <p className="text-primary-600 dark:text-sand-300">No withdrawals yet</p>
      </div>
    );
  }

  return (
    <div className="space-y-3">
      {withdrawals.map((withdrawal: any) => (
        <div
          key={withdrawal.id}
          className="bg-white dark:bg-primary-800 rounded-lg p-4 border border-primary-100 dark:border-primary-700"
        >
          <div className="flex items-start justify-between">
            <div>
              <div className="font-semibold text-primary-900 dark:text-sand-50">
                {withdrawal.currency} {parseFloat(withdrawal.amount).toLocaleString(undefined, { minimumFractionDigits: 2 })}
              </div>
              <div className="text-sm text-primary-600 dark:text-sand-400 mt-1">
                To: {withdrawal.bank_account_details?.bank_name} •••• {withdrawal.bank_account_details?.account_number?.slice(-4)}
              </div>
              <div className="text-xs text-primary-500 dark:text-sand-500 mt-1">
                {withdrawal.reference}
              </div>
              <div className="text-xs text-primary-500 dark:text-sand-500">
                {new Date(withdrawal.created_at).toLocaleDateString()} {new Date(withdrawal.created_at).toLocaleTimeString()}
              </div>
            </div>
            <div className="flex items-center gap-2">
              {withdrawal.status === 'completed' ? (
                <span className="px-3 py-1 bg-green-100 text-green-700 rounded-full text-sm font-medium">
                  Completed
                </span>
              ) : withdrawal.status === 'pending' ? (
                <span className="px-3 py-1 bg-yellow-100 text-yellow-700 rounded-full text-sm font-medium">
                  Pending
                </span>
              ) : withdrawal.status === 'processing' ? (
                <span className="px-3 py-1 bg-blue-100 text-blue-700 rounded-full text-sm font-medium">
                  Processing
                </span>
              ) : (
                <span className="px-3 py-1 bg-red-100 text-red-700 rounded-full text-sm font-medium">
                  Failed
                </span>
              )}
            </div>
          </div>
          {withdrawal.notes && (
            <div className="mt-2 text-sm text-primary-600 dark:text-sand-400 bg-primary-50 dark:bg-primary-900/50 p-2 rounded">
              {withdrawal.notes}
            </div>
          )}
        </div>
      ))}
    </div>
  );
}

function BankAccountsList({ bankAccounts, loading, onAddNew }: { bankAccounts: any[]; loading: boolean; onAddNew: () => void }) {
  const queryClient = useQueryClient();

  const setPrimaryMutation = useMutation({
    mutationFn: (accountId: string) => apiClient.setPrimaryBankAccount(accountId),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['bank-accounts'] });
    },
  });

  const deleteMutation = useMutation({
    mutationFn: (accountId: string) => apiClient.deleteBankAccount(accountId),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['bank-accounts'] });
    },
  });

  if (loading) {
    return (
      <div className="space-y-3">
        {[1, 2].map(i => (
          <div key={i} className="animate-pulse h-24 bg-white dark:bg-primary-800 rounded-lg"></div>
        ))}
      </div>
    );
  }

  return (
    <div className="space-y-3">
      <button
        onClick={onAddNew}
        className="w-full bg-white dark:bg-primary-800 rounded-lg p-4 border-2 border-dashed border-primary-300 dark:border-primary-600 hover:border-primary-500 dark:hover:border-primary-500 transition flex items-center justify-center gap-2 text-primary-700 dark:text-sand-300 font-semibold"
      >
        <Plus className="w-5 h-5" />
        Add Bank Account
      </button>
      
      {bankAccounts.map((account: any) => (
        <div
          key={account.id}
          className="bg-white dark:bg-primary-800 rounded-lg p-4 border border-primary-100 dark:border-primary-700"
        >
          <div className="flex items-start justify-between">
            <div className="flex items-start gap-3">
              <div className="p-2 bg-primary-100 dark:bg-primary-700 rounded">
                <Building className="w-5 h-5 text-primary-600 dark:text-sand-400" />
              </div>
              <div>
                <div className="font-semibold text-primary-900 dark:text-sand-50">{account.bank_name}</div>
                <div className="text-sm text-primary-600 dark:text-sand-400">{account.account_name}</div>
                <div className="text-sm text-primary-600 dark:text-sand-400">•••• {account.account_number?.slice(-4)}</div>
                {account.is_primary && (
                  <span className="inline-block mt-2 px-2 py-1 bg-primary-600 text-white text-xs rounded">
                    Primary
                  </span>
                )}
              </div>
            </div>
            <div className="flex gap-2">
              {!account.is_primary && (
                <button
                  onClick={() => setPrimaryMutation.mutate(account.id)}
                  disabled={setPrimaryMutation.isPending}
                  className="px-3 py-1 text-sm bg-primary-100 dark:bg-primary-700 text-primary-700 dark:text-sand-300 rounded hover:bg-primary-200 dark:hover:bg-primary-600 transition disabled:opacity-50"
                >
                  Set Primary
                </button>
              )}
              <button
                onClick={() => {
                  if (confirm('Are you sure you want to delete this bank account?')) {
                    deleteMutation.mutate(account.id);
                  }
                }}
                disabled={deleteMutation.isPending}
                className="px-3 py-1 text-sm bg-red-100 text-red-700 rounded hover:bg-red-200 transition disabled:opacity-50"
              >
                Delete
              </button>
            </div>
          </div>
        </div>
      ))}
    </div>
  );
}

function WithdrawModal({ wallet, bankAccounts, onClose }: { wallet: any; bankAccounts: any[]; onClose: () => void }) {
  const [amount, setAmount] = useState('');
  const [selectedBank, setSelectedBank] = useState('');
  const [notes, setNotes] = useState('');
  const queryClient = useQueryClient();

  const withdrawMutation = useMutation({
    mutationFn: (data: any) => apiClient.initiateWithdrawal(data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['wallet'] });
      queryClient.invalidateQueries({ queryKey: ['withdrawals'] });
      queryClient.invalidateQueries({ queryKey: ['wallet-transactions'] });
      onClose();
    },
  });

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    withdrawMutation.mutate({
      wallet: wallet.id,
      bank_account: selectedBank,
      amount: amount,
      currency: wallet.currency,
      notes: notes,
    });
  };

  return (
    <div className="fixed inset-0 bg-black/50 flex items-center justify-center p-4 z-50" onClick={onClose}>
      <div className="bg-white dark:bg-primary-800 rounded-xl p-6 max-w-md w-full" onClick={(e) => e.stopPropagation()}>
        <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-4">Withdraw Funds</h2>
        
        {bankAccounts.length === 0 ? (
          <div className="text-center py-4">
            <p className="text-primary-600 dark:text-sand-400 mb-4">You need to add a bank account first</p>
            <Button onClick={onClose} variant="primary" size="sm">
              Close
            </Button>
          </div>
        ) : (
          <form onSubmit={handleSubmit} className="space-y-4">
            <div>
              <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-1">
                Amount ({wallet.currency})
              </label>
              <input
                type="number"
                step="0.01"
                min="10"
                max={wallet.balance}
                value={amount}
                onChange={(e) => setAmount(e.target.value)}
                required
                className="w-full px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg bg-white dark:bg-primary-900 text-primary-900 dark:text-sand-50 focus:ring-2 focus:ring-primary-500 focus:border-primary-500"
                placeholder="Min 10.00"
              />
              <p className="text-xs text-primary-600 dark:text-sand-400 mt-1">
                Available: {wallet.currency} {parseFloat(wallet.balance).toLocaleString(undefined, { minimumFractionDigits: 2 })}
              </p>
            </div>

            <div>
              <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-1">
                Bank Account
              </label>
              <select
                value={selectedBank}
                onChange={(e) => setSelectedBank(e.target.value)}
                required
                className="w-full px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg bg-white dark:bg-primary-900 text-primary-900 dark:text-sand-50 focus:ring-2 focus:ring-primary-500 focus:border-primary-500"
              >
                <option value="">Select bank account</option>
                {bankAccounts.map((account: any) => (
                  <option key={account.id} value={account.id}>
                    {account.bank_name} - {account.account_name} (•••• {account.account_number?.slice(-4)})
                    {account.is_primary ? ' - Primary' : ''}
                  </option>
                ))}
              </select>
            </div>

            <div>
              <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-1">
                Notes (Optional)
              </label>
              <textarea
                value={notes}
                onChange={(e) => setNotes(e.target.value)}
                className="w-full px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg bg-white dark:bg-primary-900 text-primary-900 dark:text-sand-50 focus:ring-2 focus:ring-primary-500 focus:border-primary-500"
                rows={3}
                placeholder="Any additional notes..."
              />
            </div>

            {withdrawMutation.isError && (
              <div className="bg-red-100 dark:bg-red-900/30 text-red-700 dark:text-red-400 p-3 rounded-lg text-sm">
                {(withdrawMutation.error as any)?.response?.data?.error || 'Failed to initiate withdrawal'}
              </div>
            )}

            <div className="flex gap-3">
              <button
                type="button"
                onClick={onClose}
                className="flex-1 px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg text-primary-700 dark:text-sand-300 font-semibold hover:bg-primary-50 dark:hover:bg-primary-700 transition"
              >
                Cancel
              </button>
              <button
                type="submit"
                disabled={withdrawMutation.isPending}
                className="flex-1 px-4 py-2 bg-primary-600 text-white rounded-lg font-semibold hover:bg-primary-700 transition disabled:opacity-50"
              >
                {withdrawMutation.isPending ? 'Processing...' : 'Withdraw'}
              </button>
            </div>
          </form>
        )}
      </div>
    </div>
  );
}

function BankAccountModal({ onClose }: { onClose: () => void }) {
  const [formData, setFormData] = useState({
    bank_name: '',
    account_name: '',
    account_number: '',
    branch_code: '',
    country: '',
    is_primary: false,
  });
  const queryClient = useQueryClient();

  const createMutation = useMutation({
    mutationFn: (data: any) => apiClient.createBankAccount(data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['bank-accounts'] });
      onClose();
    },
  });

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    createMutation.mutate(formData);
  };

  return (
    <div className="fixed inset-0 bg-black/50 flex items-center justify-center p-4 z-50" onClick={onClose}>
      <div className="bg-white dark:bg-primary-800 rounded-xl p-6 max-w-md w-full max-h-[90vh] overflow-y-auto" onClick={(e) => e.stopPropagation()}>
        <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-4">Add Bank Account</h2>
        
        <form onSubmit={handleSubmit} className="space-y-4">
          <div>
            <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-1">
              Bank Name *
            </label>
            <input
              type="text"
              value={formData.bank_name}
              onChange={(e) => setFormData({ ...formData, bank_name: e.target.value })}
              required
              className="w-full px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg bg-white dark:bg-primary-900 text-primary-900 dark:text-sand-50 focus:ring-2 focus:ring-primary-500 focus:border-primary-500"
              placeholder="e.g., Standard Bank"
            />
          </div>

          <div>
            <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-1">
              Account Name *
            </label>
            <input
              type="text"
              value={formData.account_name}
              onChange={(e) => setFormData({ ...formData, account_name: e.target.value })}
              required
              className="w-full px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg bg-white dark:bg-primary-900 text-primary-900 dark:text-sand-50 focus:ring-2 focus:ring-primary-500 focus:border-primary-500"
              placeholder="Account holder name"
            />
          </div>

          <div>
            <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-1">
              Account Number *
            </label>
            <input
              type="text"
              value={formData.account_number}
              onChange={(e) => setFormData({ ...formData, account_number: e.target.value })}
              required
              className="w-full px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg bg-white dark:bg-primary-900 text-primary-900 dark:text-sand-50 focus:ring-2 focus:ring-primary-500 focus:border-primary-500"
              placeholder="1234567890"
            />
          </div>

          <div>
            <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-1">
              Branch Code
            </label>
            <input
              type="text"
              value={formData.branch_code}
              onChange={(e) => setFormData({ ...formData, branch_code: e.target.value })}
              className="w-full px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg bg-white dark:bg-primary-900 text-primary-900 dark:text-sand-50 focus:ring-2 focus:ring-primary-500 focus:border-primary-500"
              placeholder="Optional"
            />
          </div>

          <div>
            <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-1">
              Country
            </label>
            <input
              type="text"
              value={formData.country}
              onChange={(e) => setFormData({ ...formData, country: e.target.value })}
              className="w-full px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg bg-white dark:bg-primary-900 text-primary-900 dark:text-sand-50 focus:ring-2 focus:ring-primary-500 focus:border-primary-500"
              placeholder="e.g., Zimbabwe"
            />
          </div>

          <div className="flex items-center gap-2">
            <input
              type="checkbox"
              id="is_primary"
              checked={formData.is_primary}
              onChange={(e) => setFormData({ ...formData, is_primary: e.target.checked })}
              className="w-4 h-4 text-primary-600 border-primary-300 dark:border-primary-600 rounded focus:ring-primary-500"
            />
            <label htmlFor="is_primary" className="text-sm text-primary-900 dark:text-sand-50">
              Set as primary account
            </label>
          </div>

          {createMutation.isError && (
            <div className="bg-red-100 dark:bg-red-900/30 text-red-700 dark:text-red-400 p-3 rounded-lg text-sm">
              {(createMutation.error as any)?.response?.data?.error || 'Failed to add bank account'}
            </div>
          )}

          <div className="flex gap-3">
            <button
              type="button"
              onClick={onClose}
              className="flex-1 px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg text-primary-700 dark:text-sand-300 font-semibold hover:bg-primary-50 dark:hover:bg-primary-700 transition"
            >
              Cancel
            </button>
            <button
              type="submit"
              disabled={createMutation.isPending}
              className="flex-1 px-4 py-2 bg-primary-600 text-white rounded-lg font-semibold hover:bg-primary-700 transition disabled:opacity-50"
            >
              {createMutation.isPending ? 'Adding...' : 'Add Account'}
            </button>
          </div>
        </form>
      </div>
    </div>
  );
}
