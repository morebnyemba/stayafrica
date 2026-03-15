'use client';

import { useAuth } from '@/store/auth-store';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import dynamic from 'next/dynamic';
const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });
import { useState } from 'react';
import {
  DollarSign,
  TrendingUp,
  TrendingDown,
  Calendar,
  Download,
  Filter,
  CreditCard,
  Clock,
  CheckCircle,
  Plus,
  Trash2,
  Edit2,
  Building2,
  Wallet,
  ArrowUpRight,
  XCircle,
} from 'lucide-react';
import { toast } from 'react-hot-toast';
import { Button } from '@/components/ui';
import { Input } from '@/components/ui/Input';
import { HostTaxReport } from '@/components/tax/HostTaxReport';
import { ConfirmActionModal } from '@/components/common/confirm-action-modal';

type Period = 'week' | 'month' | 'year';

function formatCurrency(amount: number | string | undefined | null, currency?: string): string {
  const num = typeof amount === 'string' ? parseFloat(amount) : (amount ?? 0);
  const sym = currency || 'USD';
  return `${sym} ${num.toLocaleString(undefined, { minimumFractionDigits: 2, maximumFractionDigits: 2 })}`;
}

export function HostEarningsContent() {
  const { user, isAuthenticated } = useAuth();
  const [period, setPeriod] = useState<Period>('month');
  const [showFilters, setShowFilters] = useState(false);
  const [showBankAccountForm, setShowBankAccountForm] = useState(false);
  const [showWithdrawModal, setShowWithdrawModal] = useState(false);
  const [editingAccount, setEditingAccount] = useState<any>(null);
  const [pendingDeleteBankAccountId, setPendingDeleteBankAccountId] = useState<string | null>(null);
  const queryClient = useQueryClient();

  const [bankFormData, setBankFormData] = useState({
    bank_name: '',
    account_name: '',
    account_number: '',
    branch_code: '',
    country: '',
    is_primary: false,
  });

  // Fetch wallet data
  const { data: walletData, isLoading: loadingWallet } = useQuery({
    queryKey: ['wallet'],
    queryFn: async () => {
      const response = await apiClient.getMyWallet();
      return response.data;
    },
    enabled: isAuthenticated,
  });

  // Fetch recent withdrawals
  const { data: withdrawalsData } = useQuery({
    queryKey: ['withdrawals'],
    queryFn: async () => {
      const response = await apiClient.getWithdrawals();
      return response.data;
    },
    enabled: isAuthenticated,
  });

  // Fetch earnings data
  const { data: earningsData, isLoading: loadingEarnings } = useQuery({
    queryKey: ['host', 'earnings', period],
    queryFn: async () => {
      const response = await apiClient.getHostEarnings(period);
      return response.data;
    },
    enabled: isAuthenticated && user?.role === 'host',
  });

  // Fetch analytics for summary
  const { data: analytics } = useQuery({
    queryKey: ['host', 'analytics'],
    queryFn: async () => {
      const response = await apiClient.getHostAnalytics();
      return response.data;
    },
    enabled: isAuthenticated && user?.role === 'host',
  });

  // Fetch bank accounts
  const { data: bankAccountsData, isLoading: loadingBankAccounts } = useQuery({
    queryKey: ['bank-accounts'],
    queryFn: async () => {
      const response = await apiClient.getBankAccounts();
      return response.data;
    },
    enabled: isAuthenticated && user?.role === 'host',
  });

  // Create bank account mutation
  const createBankAccountMutation = useMutation({
    mutationFn: (data: any) => apiClient.createBankAccount(data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['bank-accounts'] });
      toast.success('Bank account added successfully');
      setShowBankAccountForm(false);
      resetBankForm();
    },
    onError: (error: any) => {
      toast.error(error.response?.data?.message || 'Failed to add bank account');
    },
  });

  // Update bank account mutation
  const updateBankAccountMutation = useMutation({
    mutationFn: ({ id, data }: { id: string; data: any }) => 
      apiClient.updateBankAccount(id, data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['bank-accounts'] });
      toast.success('Bank account updated successfully');
      setShowBankAccountForm(false);
      setEditingAccount(null);
      resetBankForm();
    },
    onError: (error: any) => {
      toast.error(error.response?.data?.message || 'Failed to update bank account');
    },
  });

  // Delete bank account mutation
  const deleteBankAccountMutation = useMutation({
    mutationFn: (id: string) => apiClient.deleteBankAccount(id),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['bank-accounts'] });
      toast.success('Bank account deleted successfully');
    },
    onError: (error: any) => {
      toast.error(error.response?.data?.message || 'Failed to delete bank account');
    },
  });

  // Set primary bank account mutation
  const setPrimaryBankAccountMutation = useMutation({
    mutationFn: (id: string) => apiClient.setPrimaryBankAccount(id),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['bank-accounts'] });
      toast.success('Primary bank account updated');
    },
    onError: (error: any) => {
      toast.error(error.response?.data?.message || 'Failed to set primary account');
    },
  });

  const bankAccounts = bankAccountsData?.results || bankAccountsData || [];
  const earnings = earningsData?.earnings || [];
  const withdrawals = withdrawalsData?.results || withdrawalsData || [];
  const currency = walletData?.currency || 'USD';
  const walletBalance = parseFloat(walletData?.balance || '0');
  
  // Get trend (compare last 2 periods)
  const lastPeriod = earnings[earnings.length - 1];
  const prevPeriod = earnings[earnings.length - 2];
  const trend = lastPeriod && prevPeriod 
    ? ((parseFloat(lastPeriod.total) - parseFloat(prevPeriod.total)) / parseFloat(prevPeriod.total)) * 100
    : 0;

  const resetBankForm = () => {
    setBankFormData({
      bank_name: '',
      account_name: '',
      account_number: '',
      branch_code: '',
      country: '',
      is_primary: false,
    });
  };

  const handleAddBankAccount = () => {
    setEditingAccount(null);
    resetBankForm();
    setShowBankAccountForm(true);
  };

  const handleEditBankAccount = (account: any) => {
    setEditingAccount(account);
    setBankFormData({
      bank_name: account.bank_name,
      account_name: account.account_name,
      account_number: account.account_number,
      branch_code: account.branch_code || '',
      country: account.country || '',
      is_primary: account.is_primary,
    });
    setShowBankAccountForm(true);
  };

  const handleSubmitBankAccount = (e: React.FormEvent) => {
    e.preventDefault();
    if (editingAccount) {
      updateBankAccountMutation.mutate({ id: editingAccount.id, data: bankFormData });
    } else {
      createBankAccountMutation.mutate(bankFormData);
    }
  };

  const handleDeleteBankAccount = (id: string) => {
    setPendingDeleteBankAccountId(id);
  };

  return (
    <ProtectedRoute>
      <div className="min-h-screen bg-sand-100">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          {/* Header */}
          <header className="mb-8">
            <div className="flex flex-col sm:flex-row sm:items-center sm:justify-between gap-4">
              <div>
                <h1 className="text-2xl sm:text-3xl md:text-4xl font-bold text-primary-900 mb-2">
                  Earnings &amp; Wallet
                </h1>
                <p className="text-base sm:text-lg text-primary-600">
                  Track your revenue, manage payouts, and withdraw funds
                </p>
              </div>
              
              <div className="flex items-center gap-3">
                <Button
                  onClick={() => setShowFilters(!showFilters)}
                  variant="secondary"
                  size="sm"
                  aria-label="Toggle filters"
                >
                  <Filter className="w-4 h-4" />
                  <span className="hidden sm:inline">Filters</span>
                </Button>
                <Button
                  size="sm"
                  aria-label="Download earnings report"
                >
                  <Download className="w-4 h-4" />
                  <span className="hidden sm:inline">Export</span>
                </Button>
              </div>
            </div>
          </header>

          {/* Wallet Balance & Withdraw — Hero Card */}
          <section aria-labelledby="wallet-heading" className="mb-8">
            <div className="card p-6 sm:p-8 bg-gradient-to-r from-secondary-600 to-secondary-700 text-white rounded-xl">
              <div className="flex flex-col sm:flex-row sm:items-center sm:justify-between gap-6">
                <div>
                  <div className="flex items-center gap-2 mb-2">
                    <Wallet className="w-5 h-5 text-white/80" />
                    <h2 id="wallet-heading" className="text-sm font-medium text-white/80 uppercase tracking-wide">
                      Available Balance
                    </h2>
                  </div>
                  {loadingWallet ? (
                    <div className="animate-pulse h-10 w-48 bg-white/20 rounded" />
                  ) : (
                    <p className="text-3xl sm:text-4xl font-bold">
                      {formatCurrency(walletBalance, currency)}
                    </p>
                  )}
                  <p className="text-sm text-white/70 mt-1">
                    Funds available for withdrawal
                  </p>
                </div>
                <div className="flex flex-col sm:flex-row gap-3">
                  <Button
                    onClick={() => setShowWithdrawModal(true)}
                    disabled={walletBalance < 10 || !walletData}
                    className="bg-white text-secondary-700 hover:bg-white/90 font-semibold px-6 py-3 rounded-lg disabled:opacity-50 disabled:cursor-not-allowed"
                  >
                    <ArrowUpRight className="w-4 h-4 mr-2" />
                    Withdraw Funds
                  </Button>
                </div>
              </div>
              {walletBalance < 10 && walletData && (
                <p className="text-xs text-white/60 mt-3">
                  Minimum withdrawal amount is {formatCurrency(10, currency)}
                </p>
              )}
            </div>
          </section>

          {/* Period Selector */}
          <div className="mb-6">
            <div className="inline-flex rounded-lg border border-primary-200 bg-white p-1">
              {(['week', 'month', 'year'] as Period[]).map((p) => (
                <button
                  key={p}
                  onClick={() => setPeriod(p)}
                  className={`px-4 sm:px-6 py-2 rounded-md font-medium transition-colors ${
                    period === p
                      ? 'bg-secondary-500 text-white'
                      : 'text-primary-700 hover:bg-primary-50'
                  }`}
                  aria-label={`View ${p} earnings`}
                >
                  {p.charAt(0).toUpperCase() + p.slice(1)}
                </button>
              ))}
            </div>
          </div>

          {/* Summary Cards */}
          <section aria-labelledby="earnings-summary-heading" className="mb-8">
            <h2 id="earnings-summary-heading" className="sr-only">Earnings Summary</h2>
            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 sm:gap-6">
              {[
                {
                  title: 'Total Earnings',
                  value: formatCurrency(analytics?.total_earnings, currency),
                  icon: DollarSign,
                  color: 'text-green-600',
                  bgColor: 'bg-green-100',
                  description: 'All-time net earnings',
                },
                {
                  title: 'This Period',
                  value: formatCurrency(lastPeriod?.total, currency),
                  icon: Calendar,
                  color: 'text-blue-600',
                  bgColor: 'bg-blue-100',
                  description: `${period} earnings`,
                  trend: trend !== 0 ? trend : null,
                },
                {
                  title: 'Pending Payout',
                  value: formatCurrency(analytics?.pending_earnings, currency),
                  icon: Clock,
                  color: 'text-yellow-600',
                  bgColor: 'bg-yellow-100',
                  description: 'Awaiting completion',
                },
                {
                  title: 'Completed Bookings',
                  value: analytics?.completed_bookings || 0,
                  icon: CheckCircle,
                  color: 'text-purple-600',
                  bgColor: 'bg-purple-100',
                  description: 'Paid bookings',
                },
              ].map((card, index) => (
                <div
                  key={index}
                  className="card p-4 sm:p-6 hover:shadow-lg transition-shadow"
                >
                  <div className="flex items-start justify-between mb-3">
                    <div className={`p-3 rounded-lg ${card.bgColor}`}>
                      <card.icon className={`w-5 h-5 sm:w-6 sm:h-6 ${card.color}`} />
                    </div>
                    {card.trend !== null && card.trend !== undefined && (
                      <div className={`flex items-center gap-1 text-xs sm:text-sm font-semibold ${
                        card.trend > 0 ? 'text-green-600' : 'text-red-600'
                      }`}>
                        {card.trend > 0 ? (
                          <TrendingUp className="w-4 h-4" aria-hidden="true" />
                        ) : (
                          <TrendingDown className="w-4 h-4" aria-hidden="true" />
                        )}
                        {Math.abs(card.trend).toFixed(1)}%
                      </div>
                    )}
                  </div>
                  <h3 className="text-xs sm:text-sm font-medium text-primary-600 mb-1">
                    {card.title}
                  </h3>
                  <p className="text-xl sm:text-2xl font-bold text-primary-900 mb-1">
                    {card.value}
                  </p>
                  <p className="text-xs text-primary-500">
                    {card.description}
                  </p>
                </div>
              ))}
            </div>
          </section>

          {/* Charges Breakdown Summary */}
          {analytics && (
            <section aria-labelledby="charges-summary-heading" className="card p-4 sm:p-6 mb-8">
              <h2 id="charges-summary-heading" className="text-xl sm:text-2xl font-bold text-primary-900 mb-4">
                Your Disbursement Breakdown
              </h2>
              <div className="bg-sand-50 rounded-lg p-4 sm:p-6">
                <div className="space-y-3">
                  <div className="flex justify-between items-center text-primary-900">
                    <span className="font-medium">Total Gross Revenue</span>
                    <span className="text-xl font-bold">{formatCurrency(analytics.gross_earnings, currency)}</span>
                  </div>
                  <div className="h-px bg-primary-200"></div>
                  <div className="flex justify-between items-center text-red-600">
                    <span className="pl-4">Platform Commission (15%)</span>
                    <span className="font-semibold">-{formatCurrency(analytics.total_commission, currency)}</span>
                  </div>
                  <div className="h-px bg-primary-200"></div>
                  <div className="flex justify-between items-center text-green-600">
                    <span className="font-bold text-lg">Net Earnings to You</span>
                    <span className="text-2xl font-bold">{formatCurrency(analytics.total_earnings, currency)}</span>
                  </div>
                </div>
                <div className="mt-4 p-3 bg-blue-50 rounded-lg border border-blue-200">
                  <p className="text-xs text-blue-800">
                    <strong>Note:</strong> The platform commission of 15% helps us provide secure payments, customer support, marketing, and platform maintenance. Guest service fees and taxes are charged separately to guests and don&apos;t affect your payout.
                  </p>
                </div>
              </div>
            </section>
          )}

          {/* Earnings Timeline */}
          <section aria-labelledby="earnings-timeline-heading" className="card p-4 sm:p-6 mb-8">
            <h2 id="earnings-timeline-heading" className="text-xl sm:text-2xl font-bold text-primary-900 mb-6">
              Earnings Timeline
            </h2>
            
            {loadingEarnings ? (
              <div className="space-y-4" aria-busy="true" aria-live="polite">
                {[1, 2, 3].map((i) => (
                  <div key={i} className="animate-pulse h-16 bg-primary-200 rounded-lg" />
                ))}
              </div>
            ) : earnings.length > 0 ? (
              <div className="space-y-4">
                {earnings.map((earning: any, index: number) => {
                  const amount = parseFloat(earning.total || 0);
                  const gross = parseFloat(earning.gross_earnings || 0);
                  const commission = parseFloat(earning.commission || 0);
                  const bookingsCount = earning.bookings || 0;
                  
                  return (
                    <article
                      key={index}
                      className="p-4 border border-primary-100 rounded-lg hover:bg-primary-50 transition-colors"
                      aria-label={`Earnings for ${earning.period}`}
                    >
                      <div className="flex flex-col sm:flex-row sm:items-start justify-between gap-4 mb-4">
                        <div className="flex-1">
                          <div className="flex items-center gap-3 mb-2">
                            <Calendar className="w-5 h-5 text-secondary-600" aria-hidden="true" />
                            <h3 className="font-semibold text-primary-900">
                              {earning.period}
                            </h3>
                          </div>
                          <div className="flex items-center gap-4 text-sm text-primary-600">
                            <span>{bookingsCount} booking{bookingsCount !== 1 ? 's' : ''}</span>
                            <span>•</span>
                            <span>Avg: {formatCurrency(bookingsCount > 0 ? amount / bookingsCount : 0, currency)}</span>
                          </div>
                        </div>
                        
                        <div className="text-right">
                          <p className="text-xl sm:text-2xl font-bold text-green-600">
                            {formatCurrency(amount, currency)}
                          </p>
                          <p className="text-xs text-primary-500">
                            Net earnings
                          </p>
                        </div>
                      </div>

                      {/* Charge Breakdown */}
                      <div className="border-t border-primary-100 pt-3 mt-3">
                        <p className="text-xs font-semibold text-primary-700 mb-2">Disbursement Breakdown:</p>
                        <div className="space-y-1.5 text-xs">
                          <div className="flex justify-between text-primary-600">
                            <span className="flex items-center gap-1.5">
                              <DollarSign className="w-3 h-3" />
                              Gross Revenue
                            </span>
                            <span className="font-medium">{formatCurrency(gross, currency)}</span>
                          </div>
                          <div className="flex justify-between text-red-600">
                            <span className="flex items-center gap-1.5 pl-4">
                              - Platform Commission (15%)
                            </span>
                            <span className="font-medium">-{formatCurrency(commission, currency)}</span>
                          </div>
                          <div className="flex justify-between font-semibold text-green-600 pt-1.5 border-t border-primary-100">
                            <span>Net to Host</span>
                            <span>{formatCurrency(amount, currency)}</span>
                          </div>
                        </div>
                      </div>
                    </article>
                  );
                })}
              </div>
            ) : (
              <div className="text-center py-12">
                <CreditCard className="w-12 h-12 text-primary-300 mx-auto mb-4" />
                <h3 className="text-lg font-semibold text-primary-900 mb-2">
                  No Earnings Yet
                </h3>
                <p className="text-primary-600">
                  Start receiving bookings to see your earnings here.
                </p>
              </div>
            )}
          </section>

          {/* Recent Withdrawals */}
          <section aria-labelledby="withdrawals-heading" className="card p-4 sm:p-6 mb-8">
            <div className="flex items-center justify-between mb-4">
              <h2 id="withdrawals-heading" className="text-xl sm:text-2xl font-bold text-primary-900">
                Recent Withdrawals
              </h2>
              {walletBalance >= 10 && (
                <Button onClick={() => setShowWithdrawModal(true)} size="sm">
                  <ArrowUpRight className="w-4 h-4" />
                  <span className="hidden sm:inline">Withdraw</span>
                </Button>
              )}
            </div>

            {withdrawals.length > 0 ? (
              <div className="space-y-3">
                {withdrawals.slice(0, 5).map((w: any) => {
                  const statusColors: Record<string, string> = {
                    completed: 'bg-green-100 text-green-800',
                    pending: 'bg-yellow-100 text-yellow-800',
                    processing: 'bg-blue-100 text-blue-800',
                    failed: 'bg-red-100 text-red-800',
                    cancelled: 'bg-gray-100 text-gray-800',
                  };
                  const StatusIcon = w.status === 'completed' ? CheckCircle
                    : w.status === 'failed' ? XCircle
                    : Clock;
                  return (
                    <div key={w.id} className="flex items-center justify-between p-3 border border-primary-100 rounded-lg">
                      <div className="flex items-center gap-3">
                        <StatusIcon className={`w-5 h-5 ${
                          w.status === 'completed' ? 'text-green-600' :
                          w.status === 'failed' ? 'text-red-600' : 'text-yellow-600'
                        }`} />
                        <div>
                          <p className="font-medium text-primary-900 text-sm">
                            {formatCurrency(w.amount, w.currency || currency)}
                          </p>
                          <p className="text-xs text-primary-500">
                            {w.bank_account_details?.bank_name || 'Bank account'} •{' '}
                            {new Date(w.created_at).toLocaleDateString()}
                          </p>
                        </div>
                      </div>
                      <span className={`px-2 py-0.5 text-xs font-medium rounded-full ${statusColors[w.status] || 'bg-gray-100 text-gray-800'}`}>
                        {w.status?.charAt(0).toUpperCase() + w.status?.slice(1)}
                      </span>
                    </div>
                  );
                })}
              </div>
            ) : (
              <div className="text-center py-8">
                <ArrowUpRight className="w-10 h-10 text-primary-300 mx-auto mb-3" />
                <p className="text-sm text-primary-600">No withdrawals yet</p>
                <p className="text-xs text-primary-400 mt-1">
                  When you withdraw funds, they will appear here
                </p>
              </div>
            )}
          </section>

          {/* Payout Information */}
          <section aria-labelledby="payout-info-heading" className="card p-4 sm:p-6 mb-8">
            <h2 id="payout-info-heading" className="text-xl sm:text-2xl font-bold text-primary-900 mb-4">
              How Payouts Work
            </h2>
            
            <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
              <div className="p-4 bg-sand-50 rounded-lg">
                <div className="flex items-center gap-2 mb-2">
                  <div className="w-8 h-8 bg-secondary-100 rounded-full flex items-center justify-center text-secondary-700 font-bold text-sm">1</div>
                  <h3 className="font-semibold text-primary-900 text-sm">Booking Completed</h3>
                </div>
                <p className="text-xs text-primary-600">
                  After guest check-in, earnings are credited to your wallet after the platform commission (15%) is deducted.
                </p>
              </div>
              <div className="p-4 bg-sand-50 rounded-lg">
                <div className="flex items-center gap-2 mb-2">
                  <div className="w-8 h-8 bg-secondary-100 rounded-full flex items-center justify-center text-secondary-700 font-bold text-sm">2</div>
                  <h3 className="font-semibold text-primary-900 text-sm">Balance Available</h3>
                </div>
                <p className="text-xs text-primary-600">
                  Funds appear in your wallet balance above. You can accumulate earnings or withdraw anytime.
                </p>
              </div>
              <div className="p-4 bg-sand-50 rounded-lg">
                <div className="flex items-center gap-2 mb-2">
                  <div className="w-8 h-8 bg-secondary-100 rounded-full flex items-center justify-center text-secondary-700 font-bold text-sm">3</div>
                  <h3 className="font-semibold text-primary-900 text-sm">Withdraw to Bank</h3>
                </div>
                <p className="text-xs text-primary-600">
                  Click &quot;Withdraw Funds&quot; to send money to your bank account. Minimum: {formatCurrency(10, currency)}. Processing: 1-3 business days.
                </p>
              </div>
            </div>
          </section>

          {/* Bank Accounts Section */}
          <section aria-labelledby="bank-accounts-heading" className="card p-4 sm:p-6 mb-8">
            <div className="flex items-center justify-between mb-6">
              <div>
                <h2 id="bank-accounts-heading" className="text-xl sm:text-2xl font-bold text-primary-900 mb-1">
                  Disbursement Details
                </h2>
                <p className="text-sm text-primary-600">
                  Manage your bank accounts for receiving payouts
                </p>
              </div>
              <Button
                onClick={handleAddBankAccount}
                size="sm"
              >
                <Plus className="w-4 h-4" />
                <span className="hidden sm:inline">Add Account</span>
              </Button>
            </div>

            {/* Bank Account Form */}
            {showBankAccountForm && (
              <form onSubmit={handleSubmitBankAccount} className="mb-6 p-4 bg-sand-50 rounded-lg border border-primary-200">
                <h3 className="font-semibold text-primary-900 mb-4">
                  {editingAccount ? 'Edit' : 'Add'} Bank Account
                </h3>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                  <div>
                    <Input
                      type="text"
                      required
                      value={bankFormData.bank_name}
                      onChange={(e) => setBankFormData({ ...bankFormData, bank_name: e.target.value })}
                      label="Bank Name *"
                      placeholder="e.g., First National Bank"
                    />
                  </div>
                  <div>
                    <Input
                      type="text"
                      required
                      value={bankFormData.account_name}
                      onChange={(e) => setBankFormData({ ...bankFormData, account_name: e.target.value })}
                      label="Account Name *"
                      placeholder="Account holder name"
                    />
                  </div>
                  <div>
                    <Input
                      type="text"
                      required
                      value={bankFormData.account_number}
                      onChange={(e) => setBankFormData({ ...bankFormData, account_number: e.target.value })}
                      label="Account Number *"
                      placeholder="Account number"
                    />
                  </div>
                  <div>
                    <Input
                      type="text"
                      value={bankFormData.branch_code}
                      onChange={(e) => setBankFormData({ ...bankFormData, branch_code: e.target.value })}
                      label="Branch Code"
                      placeholder="Branch code (optional)"
                    />
                  </div>
                  <div>
                    <Input
                      type="text"
                      value={bankFormData.country}
                      onChange={(e) => setBankFormData({ ...bankFormData, country: e.target.value })}
                      label="Country"
                      placeholder="Country (optional)"
                    />
                  </div>
                  <div className="md:col-span-2 pt-6">
                    <Input
                      type="checkbox"
                      checked={bankFormData.is_primary}
                      onChange={(e) => setBankFormData({ ...bankFormData, is_primary: (e as React.ChangeEvent<HTMLInputElement>).target.checked })}
                      label="Set as primary account"
                      helpText="Use this account for payouts by default."
                      className="!w-auto"
                    />
                  </div>
                </div>
                <div className="flex gap-3 mt-4">
                  <Button
                    type="submit"
                    variant="primary"
                    size="sm"
                    disabled={createBankAccountMutation.isPending || updateBankAccountMutation.isPending}
                  >
                    {editingAccount ? 'Update' : 'Add'} Account
                  </Button>
                  <Button
                    type="button"
                    variant="secondary"
                    size="sm"
                    onClick={() => {
                      setShowBankAccountForm(false);
                      setEditingAccount(null);
                      resetBankForm();
                    }}
                  >
                    Cancel
                  </Button>
                </div>
              </form>
            )}

            {/* Bank Accounts List */}
            {loadingBankAccounts ? (
              <div className="space-y-3">
                {[1, 2].map(i => (
                  <div key={i} className="animate-pulse h-20 bg-primary-100 rounded-lg"></div>
                ))}
              </div>
            ) : bankAccounts.length > 0 ? (
              <div className="space-y-3">
                {bankAccounts.map((account: any) => (
                  <div
                    key={account.id}
                    className="p-4 border border-primary-200 rounded-lg hover:bg-primary-50 transition"
                  >
                    <div className="flex items-start justify-between">
                      <div className="flex items-start gap-3">
                        <div className="p-2 bg-secondary-100 rounded-lg">
                          <Building2 className="w-5 h-5 text-secondary-600" />
                        </div>
                        <div>
                          <div className="flex items-center gap-2 mb-1">
                            <h4 className="font-semibold text-primary-900">
                              {account.bank_name}
                            </h4>
                            {account.is_primary && (
                              <span className="px-2 py-0.5 bg-green-100 text-green-800 text-xs font-medium rounded">
                                Primary
                              </span>
                            )}
                          </div>
                          <p className="text-sm text-primary-600">
                            {account.account_name}
                          </p>
                          <p className="text-sm text-primary-500">
                            ****{account.account_number.slice(-4)}
                          </p>
                          {account.branch_code && (
                            <p className="text-xs text-primary-400">
                              Branch: {account.branch_code}
                            </p>
                          )}
                        </div>
                      </div>
                      <div className="flex items-center gap-2">
                        {!account.is_primary && (
                          <button
                            onClick={() => setPrimaryBankAccountMutation.mutate(account.id)}
                            disabled={setPrimaryBankAccountMutation.isPending}
                            className="p-2 hover:bg-primary-100 rounded-lg transition text-xs"
                            title="Set as primary"
                          >
                            <CheckCircle className="w-4 h-4 text-primary-600" />
                          </button>
                        )}
                        <button
                          onClick={() => handleEditBankAccount(account)}
                          className="p-2 hover:bg-primary-100 rounded-lg transition"
                          title="Edit account"
                        >
                          <Edit2 className="w-4 h-4 text-primary-600" />
                        </button>
                        <button
                          onClick={() => handleDeleteBankAccount(account.id)}
                          disabled={deleteBankAccountMutation.isPending}
                          className="p-2 hover:bg-red-100 rounded-lg transition"
                          title="Delete account"
                        >
                          <Trash2 className="w-4 h-4 text-red-600" />
                        </button>
                      </div>
                    </div>
                  </div>
                ))}
              </div>
            ) : (
              <div className="text-center py-12">
                <Building2 className="w-12 h-12 text-primary-300 mx-auto mb-4" />
                <h3 className="text-lg font-semibold text-primary-900 mb-2">
                  No Bank Accounts
                </h3>
                <p className="text-primary-600 mb-4">
                  Add a bank account to withdraw your earnings
                </p>
                <Button
                  onClick={handleAddBankAccount}
                  variant="primary"
                  size="lg"
                >
                  Add Bank Account
                </Button>
              </div>
            )}
          </section>

          {/* Tax Report Section */}
          <section aria-labelledby="tax-report-heading">
            <HostTaxReport />
          </section>
        </div>
      </div>

      {/* Withdraw Modal */}
      {showWithdrawModal && walletData && (
        <WithdrawModal
          wallet={walletData}
          bankAccounts={bankAccounts}
          onClose={() => setShowWithdrawModal(false)}
        />
      )}
      <ConfirmActionModal
        isOpen={Boolean(pendingDeleteBankAccountId)}
        title="Delete Bank Account"
        message="Are you sure you want to delete this bank account?"
        confirmText="Delete Account"
        variant="danger"
        isLoading={deleteBankAccountMutation.isPending}
        onCancel={() => setPendingDeleteBankAccountId(null)}
        onConfirm={() => {
          if (!pendingDeleteBankAccountId) return;
          deleteBankAccountMutation.mutate(pendingDeleteBankAccountId);
          setPendingDeleteBankAccountId(null);
        }}
      />
    </ProtectedRoute>
  );
}

/** Withdraw funds modal */
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
      toast.success('Withdrawal initiated! It will be processed within 1–3 business days.');
      onClose();
    },
    onError: (error: any) => {
      toast.error(error?.response?.data?.error || 'Failed to initiate withdrawal.');
    },
  });

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    withdrawMutation.mutate({
      wallet: wallet.id,
      bank_account: selectedBank,
      amount,
      currency: wallet.currency,
      notes,
    });
  };

  return (
    <div
      className="fixed inset-0 bg-black/50 flex items-center justify-center p-4 z-50"
      onClick={onClose}
      onKeyDown={(e) => e.key === 'Escape' && onClose()}
      role="dialog"
      aria-modal="true"
      aria-labelledby="withdraw-modal-title"
    >
      <div className="bg-white rounded-xl p-6 max-w-md w-full" onClick={(e) => e.stopPropagation()}>
        <h2 id="withdraw-modal-title" className="text-2xl font-bold text-primary-900 mb-4">Withdraw Funds</h2>

        {bankAccounts.length === 0 ? (
          <div className="text-center py-4">
            <p className="text-primary-600 mb-4">You need to add a bank account first</p>
            <Button onClick={onClose} variant="primary" size="sm">Close</Button>
          </div>
        ) : (
          <form onSubmit={handleSubmit} className="space-y-4">
            <div>
              <label htmlFor="earnings-withdraw-amount" className="block text-sm font-medium text-primary-900 mb-1">
                Amount ({wallet.currency})
              </label>
              <input
                id="earnings-withdraw-amount"
                type="number"
                step="0.01"
                min="10"
                max={wallet.balance}
                value={amount}
                onChange={(e) => setAmount(e.target.value)}
                required
                className="w-full px-4 py-2 border border-primary-300 rounded-lg bg-white text-primary-900 focus:ring-2 focus:ring-secondary-500 focus:border-primary-500"
                placeholder="Min 10.00"
              />
              <p className="text-xs text-primary-600 mt-1">
                Available: {formatCurrency(wallet.balance, wallet.currency)}
              </p>
            </div>

            <div>
              <label htmlFor="earnings-withdraw-bank" className="block text-sm font-medium text-primary-900 mb-1">
                Bank Account
              </label>
              <select
                id="earnings-withdraw-bank"
                value={selectedBank}
                onChange={(e) => setSelectedBank(e.target.value)}
                required
                className="w-full px-4 py-2 border border-primary-300 rounded-lg bg-white text-primary-900 focus:ring-2 focus:ring-secondary-500 focus:border-primary-500"
              >
                <option value="">Select bank account</option>
                {bankAccounts.map((account: any) => (
                  <option key={account.id} value={account.id}>
                    {account.bank_name} - {account.account_name} (•••• {account.account_number?.slice(-4)})
                    {account.is_primary ? ' ★ Primary' : ''}
                  </option>
                ))}
              </select>
            </div>

            <div>
              <label htmlFor="earnings-withdraw-notes" className="block text-sm font-medium text-primary-900 mb-1">
                Notes (Optional)
              </label>
              <textarea
                id="earnings-withdraw-notes"
                value={notes}
                onChange={(e) => setNotes(e.target.value)}
                className="w-full px-4 py-2 border border-primary-300 rounded-lg bg-white text-primary-900 focus:ring-2 focus:ring-secondary-500 focus:border-primary-500"
                rows={2}
                placeholder="Any additional notes..."
              />
            </div>

            {withdrawMutation.isError && (
              <div className="bg-red-100 text-red-700 p-3 rounded-lg text-sm" role="alert">
                {(withdrawMutation.error as any)?.response?.data?.error || 'Failed to initiate withdrawal'}
              </div>
            )}

            <div className="flex gap-3">
              <button
                type="button"
                onClick={onClose}
                className="flex-1 px-4 py-2 border border-primary-300 rounded-lg text-primary-700 font-semibold hover:bg-primary-50 transition"
              >
                Cancel
              </button>
              <button
                type="submit"
                disabled={withdrawMutation.isPending}
                aria-busy={withdrawMutation.isPending}
                className="flex-1 px-4 py-2 bg-secondary-600 text-white rounded-lg font-semibold hover:bg-secondary-700 transition disabled:opacity-50"
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
