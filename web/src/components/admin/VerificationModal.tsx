'use client';

import { IdentityVerification } from '@/types/admin-types';
import { CheckCircle, XCircle, X } from 'lucide-react';
import { useState } from 'react';
import toast from 'react-hot-toast';

interface VerificationModalProps {
  verification: IdentityVerification;
  onClose: () => void;
  onApprove: (notes: string) => Promise<void>;
  onReject: (reason: string, notes: string) => Promise<void>;
  loading: boolean;
}

export default function VerificationModal({
  verification,
  onClose,
  onApprove,
  onReject,
  loading,
}: VerificationModalProps) {
  const [approvalNotes, setApprovalNotes] = useState('');
  const [rejectionReason, setRejectionReason] = useState('');
  const [rejectionNotes, setRejectionNotes] = useState('');
  const [processingApproval, setProcessingApproval] = useState(false);
  const [processingRejection, setProcessingRejection] = useState(false);

  const handleApprove = async () => {
    if (!approvalNotes.trim()) {
      toast.error('Please add approval notes');
      return;
    }
    try {
      setProcessingApproval(true);
      await onApprove(approvalNotes);
    } finally {
      setProcessingApproval(false);
    }
  };

  const handleReject = async () => {
    if (!rejectionReason.trim()) {
      toast.error('Please provide a rejection reason');
      return;
    }
    try {
      setProcessingRejection(true);
      await onReject(rejectionReason, rejectionNotes);
    } finally {
      setProcessingRejection(false);
    }
  };

  const formatDate = (dateString: string | undefined) => {
    if (!dateString) return '-';
    return new Date(dateString).toLocaleDateString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
    });
  };

  return (
    <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50 p-4">
      <div className="bg-white rounded-lg shadow-lg max-w-4xl w-full max-h-[90vh] overflow-y-auto">
        {/* Header */}
        <div className="sticky top-0 flex items-center justify-between p-6 border-b border-gray-200 bg-white z-10">
          <div>
            <h2 className="text-2xl font-bold text-gray-900">
              {verification.user.first_name} {verification.user.last_name}
            </h2>
            <p className="text-sm text-gray-500">{verification.user.email}</p>
          </div>
          <button
            onClick={onClose}
            className="text-gray-400 hover:text-gray-600 transition"
          >
            <X className="w-6 h-6" />
          </button>
        </div>

        {/* Content */}
        <div className="p-6 space-y-6">
          {/* Verification Info */}
          <div className="grid grid-cols-2 gap-4">
            <div>
              <p className="text-sm text-gray-500 mb-1">Document Type</p>
              <p className="text-base font-medium text-gray-900 capitalize">
                {verification.document_type.replace('_', ' ')}
              </p>
            </div>
            <div>
              <p className="text-sm text-gray-500 mb-1">Document Number</p>
              <p className="text-base font-medium text-gray-900">{verification.document_number}</p>
            </div>
            <div>
              <p className="text-sm text-gray-500 mb-1">Document Country</p>
              <p className="text-base font-medium text-gray-900">{verification.document_country}</p>
            </div>
            <div>
              <p className="text-sm text-gray-500 mb-1">Expiry Date</p>
              <p className="text-base font-medium text-gray-900">
                {verification.document_expiry_date ? formatDate(verification.document_expiry_date) : 'Not specified'}
              </p>
            </div>
            <div>
              <p className="text-sm text-gray-500 mb-1">Submitted</p>
              <p className="text-base font-medium text-gray-900">{formatDate(verification.submitted_at)}</p>
            </div>
            <div>
              <p className="text-sm text-gray-500 mb-1">Verification Method</p>
              <p className="text-base font-medium text-gray-900 capitalize">{verification.verification_method}</p>
            </div>
          </div>

          {/* Status */}
          {verification.status !== 'pending' && verification.status !== 'under_review' && (
            <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
              <p className="text-sm text-gray-600 mb-2">
                <strong>Current Status:</strong>
              </p>
              <p className="text-base capitalize text-blue-900 font-medium">
                {verification.status.replace('_', ' ')}
              </p>
              {verification.reviewed_at && (
                <p className="text-xs text-gray-500 mt-2">
                  Reviewed on {formatDate(verification.reviewed_at)}
                </p>
              )}
              {verification.rejection_reason && (
                <div className="mt-2">
                  <p className="text-sm text-gray-600">
                    <strong>Rejection Reason:</strong>
                  </p>
                  <p className="text-sm text-red-700">{verification.rejection_reason}</p>
                </div>
              )}
              {verification.admin_notes && (
                <div className="mt-2">
                  <p className="text-sm text-gray-600">
                    <strong>Admin Notes:</strong>
                  </p>
                  <p className="text-sm text-gray-700">{verification.admin_notes}</p>
                </div>
              )}
            </div>
          )}

          {/* Document Images */}
          <div>
            <h3 className="text-lg font-semibold text-gray-900 mb-4">Document Images</h3>
            <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
              {verification.document_front_image && (
                <div>
                  <p className="text-sm text-gray-600 mb-2 font-medium">Front Side</p>
                  <img
                    src={verification.document_front_image}
                    alt="Document Front"
                    className="w-full h-64 object-cover rounded-lg border border-gray-200"
                  />
                </div>
              )}
              {verification.document_back_image && (
                <div>
                  <p className="text-sm text-gray-600 mb-2 font-medium">Back Side</p>
                  <img
                    src={verification.document_back_image}
                    alt="Document Back"
                    className="w-full h-64 object-cover rounded-lg border border-gray-200"
                  />
                </div>
              )}
              {verification.selfie_image && (
                <div>
                  <p className="text-sm text-gray-600 mb-2 font-medium">Selfie</p>
                  <img
                    src={verification.selfie_image}
                    alt="Selfie"
                    className="w-full h-64 object-cover rounded-lg border border-gray-200"
                  />
                </div>
              )}
            </div>
          </div>

          {/* Action Forms */}
          {(verification.status === 'pending' || verification.status === 'under_review') && (
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6 pt-4 border-t border-gray-200">
              {/* Approval Form */}
              <div className="bg-green-50 p-4 rounded-lg border border-green-200">
                <div className="flex items-center gap-2 mb-4">
                  <CheckCircle className="w-5 h-5 text-green-600" />
                  <h4 className="font-semibold text-green-900">Approve</h4>
                </div>
                <div>
                  <label className="block text-sm font-medium text-gray-700 mb-2">
                    Approval Notes
                  </label>
                  <textarea
                    value={approvalNotes}
                    onChange={(e) => setApprovalNotes(e.target.value)}
                    placeholder="Add notes about why you approved this verification..."
                    className="w-full px-3 py-2 border border-gray-300 rounded-lg text-sm focus:outline-none focus:ring-2 focus:ring-green-500"
                    rows={4}
                  />
                </div>
                <button
                  onClick={handleApprove}
                  disabled={loading || processingApproval}
                  className="mt-4 w-full bg-green-600 hover:bg-green-700 disabled:bg-gray-400 disabled:cursor-not-allowed text-white font-medium py-2 px-4 rounded-lg transition"
                >
                  {processingApproval ? 'Processing...' : 'Approve Verification'}
                </button>
              </div>

              {/* Rejection Form */}
              <div className="bg-red-50 p-4 rounded-lg border border-red-200">
                <div className="flex items-center gap-2 mb-4">
                  <XCircle className="w-5 h-5 text-red-600" />
                  <h4 className="font-semibold text-red-900">Reject</h4>
                </div>
                <div className="space-y-4">
                  <div>
                    <label className="block text-sm font-medium text-gray-700 mb-2">
                      Rejection Reason *
                    </label>
                    <select
                      value={rejectionReason}
                      onChange={(e) => setRejectionReason(e.target.value)}
                      className="w-full px-3 py-2 border border-gray-300 rounded-lg text-sm focus:outline-none focus:ring-2 focus:ring-red-500"
                    >
                      <option value="">Select a reason...</option>
                      <option value="Document expired">Document expired</option>
                      <option value="Document unclear or damaged">Document unclear or damaged</option>
                      <option value="Mismatched information">Mismatched information</option>
                      <option value="Invalid document type">Invalid document type</option>
                      <option value="Suspicious activity detected">Suspicious activity detected</option>
                      <option value="Other">Other</option>
                    </select>
                  </div>
                  <div>
                    <label className="block text-sm font-medium text-gray-700 mb-2">
                      Additional Notes
                    </label>
                    <textarea
                      value={rejectionNotes}
                      onChange={(e) => setRejectionNotes(e.target.value)}
                      placeholder="Explain why this verification was rejected..."
                      className="w-full px-3 py-2 border border-gray-300 rounded-lg text-sm focus:outline-none focus:ring-2 focus:ring-red-500"
                      rows={4}
                    />
                  </div>
                </div>
                <button
                  onClick={handleReject}
                  disabled={loading || processingRejection}
                  className="mt-4 w-full bg-red-600 hover:bg-red-700 disabled:bg-gray-400 disabled:cursor-not-allowed text-white font-medium py-2 px-4 rounded-lg transition"
                >
                  {processingRejection ? 'Processing...' : 'Reject Verification'}
                </button>
              </div>
            </div>
          )}
        </div>

        {/* Footer */}
        <div className="sticky bottom-0 border-t border-gray-200 bg-gray-50 px-6 py-4 flex justify-end z-10">
          <button
            onClick={onClose}
            className="px-6 py-2 border border-gray-300 rounded-lg text-gray-700 font-medium hover:bg-gray-100 transition"
          >
            Close
          </button>
        </div>
      </div>
    </div>
  );
}
