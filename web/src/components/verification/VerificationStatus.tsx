'use client';

import { useVerification } from './useVerification';
import { Loader2, CheckCircle, XCircle, Clock, AlertCircle } from 'lucide-react';
import { formatDistanceToNow } from 'date-fns';

export const VerificationStatus = () => {
  const { status, isLoading } = useVerification();

  if (isLoading) {
    return (
      <div className="flex items-center justify-center p-8">
        <Loader2 className="h-8 w-8 animate-spin text-gray-400" />
      </div>
    );
  }

  if (!status) {
    return (
      <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-6">
        <div className="flex items-start gap-3">
          <AlertCircle className="h-6 w-6 text-yellow-600 flex-shrink-0 mt-0.5" />
          <div>
            <h3 className="text-lg font-medium text-yellow-900">Verification Not Started</h3>
            <p className="text-sm text-yellow-800 mt-1">
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
        return <CheckCircle className="h-8 w-8 text-green-600" />;
      case 'REJECTED':
        return <XCircle className="h-8 w-8 text-red-600" />;
      case 'UNDER_REVIEW':
        return <Clock className="h-8 w-8 text-blue-600" />;
      case 'PENDING':
        return <AlertCircle className="h-8 w-8 text-yellow-600" />;
    }
  };

  const getStatusColor = () => {
    switch (status.status) {
      case 'APPROVED':
        return 'bg-green-50 border-green-200';
      case 'REJECTED':
        return 'bg-red-50 border-red-200';
      case 'UNDER_REVIEW':
        return 'bg-blue-50 border-blue-200';
      case 'PENDING':
        return 'bg-yellow-50 border-yellow-200';
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
    <div className={`border rounded-lg p-6 ${getStatusColor()}`}>
      <div className="flex items-start gap-4">
        {getStatusIcon()}
        
        <div className="flex-1">
          <div className="flex items-center justify-between">
            <h3 className="text-xl font-semibold text-gray-900">
              {getStatusText()}
            </h3>
            
            {status.updated_at && (
              <span className="text-sm text-gray-600">
                {formatDistanceToNow(new Date(status.updated_at), { addSuffix: true })}
              </span>
            )}
          </div>
          
          <p className="text-sm text-gray-700 mt-2">
            {getStatusDescription()}
          </p>

          {status.status === 'REJECTED' && status.rejection_reason && (
            <div className="mt-4 p-4 bg-white border border-red-200 rounded-lg">
              <h4 className="text-sm font-medium text-red-900 mb-1">Rejection Reason</h4>
              <p className="text-sm text-red-800">{status.rejection_reason}</p>
            </div>
          )}

          {status.admin_notes && (
            <div className="mt-4 p-4 bg-white border border-gray-200 rounded-lg">
              <h4 className="text-sm font-medium text-gray-900 mb-1">Admin Notes</h4>
              <p className="text-sm text-gray-700">{status.admin_notes}</p>
            </div>
          )}

          {status.verified_at && (
            <p className="text-xs text-gray-500 mt-4">
              Verified on {new Date(status.verified_at).toLocaleDateString()}
            </p>
          )}
        </div>
      </div>
    </div>
  );
};
