'use client';

import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { CreditCard, CheckCircle, XCircle, Clock } from 'lucide-react';
import { ProtectedRoute } from '@/components/auth/protected-route';

export function PaymentHistory() {
  const { data, isLoading, error } = useQuery({
    queryKey: ['payments', 'history'],
    queryFn: async () => {
      const response = await apiClient.getPaymentHistory();
      return response.data?.results || [];
    },
  });

  return (
    <ProtectedRoute>
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
        <div className="max-w-4xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          <header className="mb-6">
            <h1 className="text-2xl sm:text-3xl font-bold text-primary-900 dark:text-sand-50">Payment History</h1>
          </header>
          {isLoading ? (
            <div className="space-y-4">
              {[1,2,3].map(i => (
                <div key={i} className="animate-pulse h-16 bg-primary-200 dark:bg-primary-700 rounded-lg" />
              ))}
            </div>
          ) : error ? (
            <div className="bg-white dark:bg-primary-800 p-8 rounded-lg text-center border border-primary-200 dark:border-primary-700">
              <p className="text-primary-600 dark:text-sand-300 mb-4">Unable to load payment history.</p>
              <button className="btn-primary px-6 py-2" onClick={() => window.location.reload()}>Retry</button>
            </div>
          ) : data.length === 0 ? (
            <div className="bg-white dark:bg-primary-800 p-8 rounded-lg text-center border border-primary-200 dark:border-primary-700">
              <CreditCard className="w-12 h-12 text-primary-400 dark:text-sand-500 mx-auto mb-4" />
              <h3 className="text-xl font-semibold mb-2">No Payments Found</h3>
              <p className="text-primary-600 dark:text-sand-300">You have not made any payments yet.</p>
            </div>
          ) : (
            <div className="space-y-4">
              {data.map((payment: any) => (
                <article
                  key={payment.id}
                  className="bg-white dark:bg-primary-800 p-4 sm:p-6 rounded-lg border border-primary-100 dark:border-primary-700 flex flex-col sm:flex-row items-start sm:items-center justify-between gap-4"
                  aria-label={`Payment for ${payment.provider} - $${payment.amount}`}
                >
                  <div className="flex-1 min-w-0">
                    <div className="font-semibold text-primary-900 dark:text-sand-50 mb-1">{payment.provider}</div>
                    <div className="text-sm text-primary-600 dark:text-sand-300 mb-1 break-all">Booking Ref: {payment.booking?.booking_ref}</div>
                    <div className="text-xs text-primary-500 dark:text-sand-400">
                      <time dateTime={payment.created_at}>
                        {new Date(payment.created_at).toLocaleDateString()} {new Date(payment.created_at).toLocaleTimeString()}
                      </time>
                    </div>
                  </div>
                  <div className="flex items-center gap-3 self-end sm:self-auto">
                    <span className="text-lg sm:text-xl font-bold text-primary-900 dark:text-sand-50">${payment.amount}</span>
                    {payment.status === 'success' ? (
                      <CheckCircle className="w-6 h-6 text-green-500" aria-label="Payment successful" />
                    ) : payment.status === 'pending' ? (
                      <Clock className="w-6 h-6 text-yellow-500" aria-label="Payment pending" />
                    ) : (
                      <XCircle className="w-6 h-6 text-red-500" aria-label="Payment failed" />
                    )}
                  </div>
                </article>
              ))}
            </div>
          )}
        </div>
      </div>
    </ProtectedRoute>
  );
}
