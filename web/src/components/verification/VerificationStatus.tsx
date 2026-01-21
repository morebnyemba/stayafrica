'use client';

import { useVerification } from './useVerification';
import { Loader2, CheckCircle, XCircle, Clock, AlertCircle } from 'lucide-react';
import { formatDistanceToNow } from 'date-fns';

export const VerificationStatus = () => {
  const { status, isLoading } = useVerification();

  if (isLoading) {
    return (
      <div className="flex items-center justify-center p-8" role="status" aria-busy="true" aria-label="Loading verification status">
        <Loader2 className="h-8 w-8 animate-spin text-gray-400" aria-hidden="true" />
        <span className="sr-only">Loading verification status...</span>
      </div>
    );
  }

  if (!status) {
    return (
      <div className="bg-yellow-50 dark:bg-yellow-900/30 border border-yellow-200 dark:border-yellow-700 rounded-lg p-6">
        <div className="flex items-start gap-3">
          <AlertCircle className="h-6 w-6 text-yellow-600 dark:text-yellow-400 flex-shrink-0 mt-0.5" aria-hidden="true" />
          <div>
            <h3 className="text-lg font-medium text-yellow-900 dark:text-yellow-100">Verification Not Started</h3>
            <p className="text-sm text-yellow-800 dark:text-yellow-200 mt-1">
              Complete the verification process to unlock all platform features.
            </p>
          </div>
        </div>
      </div>
    );
  }

  const getStatusIcon = () => {
    switch (status.status) {
      case 'APPROVED':
        return <CheckCircle className="h-8 w-8 text-green-600 dark:text-green-400" aria-hidden="true" />;
      case 'REJECTED':
        return <XCircle className="h-8 w-8 text-red-600 dark:text-red-400" aria-hidden="true" />;
      case 'UNDER_REVIEW':
        return <Clock className="h-8 w-8 text-blue-600 dark:text-blue-400" aria-hidden="true" />;
      case 'PENDING':
        return <AlertCircle className="h-8 w-8 text-yellow-600 dark:text-yellow-400" aria-hidden="true" />;
    }
  };

  const getStatusColor = () => {
    switch (status.status) {
      case 'APPROVED':
        return 'bg-green-50 dark:bg-green-900/30 border-green-200 dark:border-green-700';
      case 'REJECTED':
        return 'bg-red-50 dark:bg-red-900/30 border-red-200 dark:border-red-700';
      case 'UNDER_REVIEW':
        return 'bg-blue-50 dark:bg-blue-900/30 border-blue-200 dark:border-blue-700';
      case 'PENDING':
        return 'bg-yellow-50 dark:bg-yellow-900/30 border-yellow-200 dark:border-yellow-700';
    }
  };

  const getStatusText = () => {
    switch (status.status) {
      case 'APPROVED':
        return 'Verified';
      case 'REJECTED':
        return 'Rejected';
      case 'UNDER_REVIEW':
        return 'Under Review';
      case 'PENDING':
        return 'Pending Submission';
    }
  };

  const getStatusDescription = () => {
    switch (status.status) {
      case 'APPROVED':
        return 'Your identity has been successfully verified.';
      case 'REJECTED':
        return 'Your verification was rejected. Please review the reason below and resubmit.';
      case 'UNDER_REVIEW':
        return 'Our team is reviewing your verification documents. This usually takes 1-2 business days.';
      case 'PENDING':
        return 'Please complete the verification wizard to submit your documents.';
    }
  };

  return (
    <div className={`border rounded-lg p-6 ${getStatusColor()}`} role="status" aria-label={`Verification status: ${getStatusText()}`}>
      <div className="flex items-start gap-4">
        {getStatusIcon()}
        
        <div className="flex-1">
          <div className="flex items-center justify-between">
            <h3 className="text-xl font-semibold text-gray-900 dark:text-gray-100">
              {getStatusText()}
            </h3>
            
            {status.updated_at && (
              <span className="text-sm text-gray-600 dark:text-gray-400">
                {formatDistanceToNow(new Date(status.updated_at), { addSuffix: true })}
              </span>
            )}
          </div>
          
          <p className="text-sm text-gray-700 dark:text-gray-300 mt-2">
            {getStatusDescription()}
          </p>

          {status.status === 'REJECTED' && status.rejection_reason && (
            <div className="mt-4 p-4 bg-white dark:bg-gray-800 border border-red-200 dark:border-red-700 rounded-lg" role="alert">
              <h4 className="text-sm font-medium text-red-900 dark:text-red-200 mb-1">Rejection Reason</h4>
              <p className="text-sm text-red-800 dark:text-red-300">{status.rejection_reason}</p>
            </div>
          )}

          {status.admin_notes && (
            <div className="mt-4 p-4 bg-white dark:bg-gray-800 border border-gray-200 dark:border-gray-700 rounded-lg">
              <h4 className="text-sm font-medium text-gray-900 dark:text-gray-100 mb-1">Admin Notes</h4>
              <p className="text-sm text-gray-700 dark:text-gray-300">{status.admin_notes}</p>
            </div>
          )}

          {status.verified_at && (
            <p className="text-xs text-gray-500 dark:text-gray-400 mt-4">
              Verified on {new Date(status.verified_at).toLocaleDateString()}
            </p>
          )}
        </div>
      </div>
    </div>
  );
};
