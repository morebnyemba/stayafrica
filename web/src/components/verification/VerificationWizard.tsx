'use client';

import { useState, useId, useCallback } from 'react';
import { DocumentUpload } from './DocumentUpload';
import { SelfieCapture } from './SelfieCapture';
import { useVerification } from './useVerification';
import {
  ChevronLeft,
  ChevronRight,
  Check,
  Loader2,
  Clock,
  CheckCircle,
  XCircle,
  AlertCircle,
  FileText,
  Camera,
  Shield,
  RefreshCw,
  Eye,
  EyeOff,
  Calendar,
  MapPin,
  Hash,
} from 'lucide-react';
import { formatDistanceToNow, format } from 'date-fns';
import toast from 'react-hot-toast';

type Step = 'document' | 'selfie' | 'review';

const countries = [
  'United States', 'United Kingdom', 'Canada', 'Australia', 'South Africa',
  'Nigeria', 'Kenya', 'Ghana', 'Uganda', 'Tanzania', 'Zimbabwe',
];

const docTypeLabels: Record<string, string> = {
  PASSPORT: 'Passport',
  NATIONAL_ID: 'National ID Card',
  DRIVERS_LICENSE: "Driver's License",
  passport: 'Passport',
  national_id: 'National ID Card',
  drivers_license: "Driver's License",
};

/* ────────────────────────────────────────── */
/* Status badge pill                          */
/* ────────────────────────────────────────── */
function StatusBadge({ status }: { status: string }) {
  const config: Record<string, { bg: string; text: string; icon: React.ReactNode; label: string }> = {
    PENDING: {
      bg: 'bg-amber-100 dark:bg-amber-900/40',
      text: 'text-amber-800 dark:text-amber-200',
      icon: <Clock className="h-4 w-4" />,
      label: 'Pending Review',
    },
    UNDER_REVIEW: {
      bg: 'bg-blue-100 dark:bg-blue-900/40',
      text: 'text-blue-800 dark:text-blue-200',
      icon: <Eye className="h-4 w-4" />,
      label: 'Under Review',
    },
    APPROVED: {
      bg: 'bg-green-100 dark:bg-green-900/40',
      text: 'text-green-800 dark:text-green-200',
      icon: <CheckCircle className="h-4 w-4" />,
      label: 'Verified',
    },
    REJECTED: {
      bg: 'bg-red-100 dark:bg-red-900/40',
      text: 'text-red-800 dark:text-red-200',
      icon: <XCircle className="h-4 w-4" />,
      label: 'Rejected',
    },
  };
  const c = config[status] || config.PENDING;
  return (
    <span className={`inline-flex items-center gap-1.5 px-3 py-1 rounded-full text-sm font-medium ${c.bg} ${c.text}`}>
      {c.icon}
      {c.label}
    </span>
  );
}

/* ────────────────────────────────────────── */
/* Blurred image preview (document privacy)   */
/* ────────────────────────────────────────── */
function ImagePreview({ src, alt }: { src?: string; alt: string }) {
  const [revealed, setRevealed] = useState(false);

  if (!src) return null;

  const apiBase = process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8000';
  const imgSrc = src.startsWith('http') ? src : `${apiBase}/media/${src}`;

  return (
    <div className="relative group rounded-lg overflow-hidden border border-gray-200 dark:border-gray-700 bg-gray-100 dark:bg-gray-800">
      <div className="aspect-[4/3] relative">
        {/* eslint-disable-next-line @next/next/no-img-element */}
        <img
          src={imgSrc}
          alt={alt}
          className={`w-full h-full object-cover transition-all duration-300 ${
            revealed ? '' : 'blur-lg scale-105'
          }`}
        />
        {!revealed && (
          <div className="absolute inset-0 flex items-center justify-center bg-black/10 dark:bg-black/30">
            <button
              onClick={() => setRevealed(true)}
              className="flex items-center gap-2 px-4 py-2 bg-white/90 dark:bg-gray-800/90 rounded-lg shadow-sm text-sm font-medium text-gray-700 dark:text-gray-200 hover:bg-white dark:hover:bg-gray-700 transition-colors"
            >
              <Eye className="h-4 w-4" />
              Show preview
            </button>
          </div>
        )}
        {revealed && (
          <button
            onClick={() => setRevealed(false)}
            className="absolute top-2 right-2 p-1.5 bg-black/50 rounded-full text-white hover:bg-black/70 transition-colors"
            aria-label="Hide preview"
          >
            <EyeOff className="h-3.5 w-3.5" />
          </button>
        )}
      </div>
      <p className="text-xs text-center py-1.5 text-gray-500 dark:text-gray-400">{alt}</p>
    </div>
  );
}

/* ────────────────────────────────────────── */
/* Pulsing timeline for pending status        */
/* ────────────────────────────────────────── */
function StatusTimeline({ status }: { status: string }) {
  const stages = [
    { id: 'submitted', label: 'Submitted', icon: <FileText className="h-4 w-4" /> },
    { id: 'review', label: 'Under Review', icon: <Eye className="h-4 w-4" /> },
    { id: 'decision', label: 'Decision', icon: <Shield className="h-4 w-4" /> },
  ];
  const activeIndex = status === 'PENDING' ? 0 : status === 'UNDER_REVIEW' ? 1 : 2;

  return (
    <div className="flex items-center w-full my-6">
      {stages.map((stage, i) => (
        <div key={stage.id} className="flex items-center flex-1">
          <div className="flex flex-col items-center">
            <div
              className={`
                flex items-center justify-center w-10 h-10 rounded-full border-2 transition-all
                ${i < activeIndex
                  ? 'border-green-500 bg-green-500 text-white'
                  : i === activeIndex
                    ? 'border-blue-500 bg-blue-500 text-white animate-pulse'
                    : 'border-gray-300 dark:border-gray-600 bg-white dark:bg-gray-800 text-gray-400 dark:text-gray-500'
                }
              `}
            >
              {i < activeIndex ? <Check className="h-5 w-5" /> : stage.icon}
            </div>
            <span className={`mt-2 text-xs font-medium ${
              i <= activeIndex ? 'text-gray-900 dark:text-gray-100' : 'text-gray-400 dark:text-gray-500'
            }`}>
              {stage.label}
            </span>
          </div>
          {i < stages.length - 1 && (
            <div className={`flex-1 h-0.5 mx-3 ${i < activeIndex ? 'bg-green-500' : 'bg-gray-300 dark:bg-gray-600'}`} />
          )}
        </div>
      ))}
    </div>
  );
}

/* ════════════════════════════════════════════ */
/* Main component                              */
/* ════════════════════════════════════════════ */
export const VerificationWizard = () => {
  const { status, isLoading, submitVerification, isSubmitting } = useVerification();

  /* ── wizard form state ── */
  const [currentStep, setCurrentStep] = useState<Step>('document');
  const [documentType, setDocumentType] = useState<'PASSPORT' | 'NATIONAL_ID' | 'DRIVERS_LICENSE'>('PASSPORT');
  const [documentNumber, setDocumentNumber] = useState('');
  const [issuedCountry, setIssuedCountry] = useState('');
  const [expiryDate, setExpiryDate] = useState('');
  const [frontImageUrl, setFrontImageUrl] = useState('');
  const [backImageUrl, setBackImageUrl] = useState('');
  const [selfieUrl, setSelfieUrl] = useState('');
  const [showForm, setShowForm] = useState(false); // for rejected re-submit

  const docTypeId = useId();
  const docNumberId = useId();
  const issuedCountryId = useId();
  const expiryDateId = useId();

  const steps: { id: Step; label: string }[] = [
    { id: 'document', label: 'Document' },
    { id: 'selfie', label: 'Selfie' },
    { id: 'review', label: 'Review' },
  ];
  const currentStepIndex = steps.findIndex(s => s.id === currentStep);
  const requiresBackImage = documentType === 'NATIONAL_ID' || documentType === 'DRIVERS_LICENSE';

  const canProceedFromDocument =
    documentType && documentNumber.trim() && issuedCountry && frontImageUrl && (!requiresBackImage || backImageUrl);
  const canProceedFromSelfie = selfieUrl !== '';

  const handleNext = useCallback(() => {
    if (currentStep === 'document' && canProceedFromDocument) setCurrentStep('selfie');
    else if (currentStep === 'selfie' && canProceedFromSelfie) setCurrentStep('review');
  }, [currentStep, canProceedFromDocument, canProceedFromSelfie]);

  const handleBack = useCallback(() => {
    if (currentStep === 'selfie') setCurrentStep('document');
    else if (currentStep === 'review') setCurrentStep('selfie');
  }, [currentStep]);

  const handleSubmit = async () => {
    try {
      await submitVerification({
        document_type: documentType,
        document_number: documentNumber,
        issued_country: issuedCountry,
        expiry_date: expiryDate || undefined,
        front_image: frontImageUrl,
        back_image: backImageUrl || undefined,
        selfie_image: selfieUrl,
      });
      toast.success("Verification submitted! We'll review your documents within 1\u20132 business days.", { duration: 5000 });
      setShowForm(false);
    } catch (error) {
      console.error('Verification submission failed:', error);
      toast.error('Failed to submit verification. Please try again.');
    }
  };

  /* ── loading ── */
  if (isLoading) {
    return (
      <div className="flex flex-col items-center justify-center py-16 gap-3" role="status" aria-busy="true">
        <Loader2 className="h-10 w-10 animate-spin text-primary-500" />
        <p className="text-sm text-gray-500 dark:text-gray-400">Checking verification status\u2026</p>
      </div>
    );
  }

  /* ════════════════════════════════════════════ */
  /* ACTIVE VERIFICATION  (pending / under_review) */
  /* ════════════════════════════════════════════ */
  if (status && (status.status === 'PENDING' || status.status === 'UNDER_REVIEW')) {
    return (
      <div className="space-y-6" role="region" aria-label="Verification status">
        {/* Main status card */}
        <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm border border-gray-200 dark:border-gray-700 overflow-hidden">
          {/* Header banner */}
          <div className={`px-6 py-5 ${
            status.status === 'UNDER_REVIEW'
              ? 'bg-gradient-to-r from-blue-50 to-indigo-50 dark:from-blue-900/30 dark:to-indigo-900/30'
              : 'bg-gradient-to-r from-amber-50 to-yellow-50 dark:from-amber-900/30 dark:to-yellow-900/30'
          }`}>
            <div className="flex items-center justify-between flex-wrap gap-3">
              <div className="flex items-center gap-3">
                {status.status === 'UNDER_REVIEW' ? (
                  <div className="p-2.5 bg-blue-100 dark:bg-blue-900/50 rounded-full">
                    <Eye className="h-6 w-6 text-blue-600 dark:text-blue-400" />
                  </div>
                ) : (
                  <div className="p-2.5 bg-amber-100 dark:bg-amber-900/50 rounded-full">
                    <Clock className="h-6 w-6 text-amber-600 dark:text-amber-400" />
                  </div>
                )}
                <div>
                  <h2 className="text-xl font-bold text-gray-900 dark:text-gray-100">
                    Verification {status.status === 'UNDER_REVIEW' ? 'In Progress' : 'Submitted'}
                  </h2>
                  <p className="text-sm text-gray-600 dark:text-gray-400">
                    {status.status === 'UNDER_REVIEW'
                      ? 'Our team is currently reviewing your documents.'
                      : 'Your documents are in the queue. Review typically takes 1\u20132 business days.'}
                  </p>
                </div>
              </div>
              <StatusBadge status={status.status} />
            </div>
          </div>

          {/* Timeline */}
          <div className="px-6">
            <StatusTimeline status={status.status} />
          </div>

          {/* Submission details */}
          <div className="px-6 pb-6">
            <h3 className="text-sm font-semibold text-gray-500 dark:text-gray-400 uppercase tracking-wider mb-4">
              Submission Details
            </h3>
            <div className="grid grid-cols-1 sm:grid-cols-2 gap-4">
              <div className="flex items-start gap-3 p-3 rounded-lg bg-gray-50 dark:bg-gray-900/50">
                <FileText className="h-5 w-5 text-gray-400 mt-0.5 flex-shrink-0" />
                <div>
                  <p className="text-xs text-gray-500 dark:text-gray-400">Document Type</p>
                  <p className="font-medium text-gray-900 dark:text-gray-100">
                    {docTypeLabels[status.document_type || ''] || status.document_type_display || status.document_type || '\u2014'}
                  </p>
                </div>
              </div>
              <div className="flex items-start gap-3 p-3 rounded-lg bg-gray-50 dark:bg-gray-900/50">
                <Hash className="h-5 w-5 text-gray-400 mt-0.5 flex-shrink-0" />
                <div>
                  <p className="text-xs text-gray-500 dark:text-gray-400">Document Number</p>
                  <p className="font-medium text-gray-900 dark:text-gray-100 font-mono">
                    {status.document_number ? `\u2022\u2022\u2022\u2022${status.document_number.slice(-4)}` : '\u2014'}
                  </p>
                </div>
              </div>
              <div className="flex items-start gap-3 p-3 rounded-lg bg-gray-50 dark:bg-gray-900/50">
                <MapPin className="h-5 w-5 text-gray-400 mt-0.5 flex-shrink-0" />
                <div>
                  <p className="text-xs text-gray-500 dark:text-gray-400">Issuing Country</p>
                  <p className="font-medium text-gray-900 dark:text-gray-100">
                    {status.document_country || '\u2014'}
                  </p>
                </div>
              </div>
              <div className="flex items-start gap-3 p-3 rounded-lg bg-gray-50 dark:bg-gray-900/50">
                <Calendar className="h-5 w-5 text-gray-400 mt-0.5 flex-shrink-0" />
                <div>
                  <p className="text-xs text-gray-500 dark:text-gray-400">Submitted</p>
                  <p className="font-medium text-gray-900 dark:text-gray-100">
                    {status.created_at
                      ? `${format(new Date(status.created_at), 'PPp')} (${formatDistanceToNow(new Date(status.created_at), { addSuffix: true })})`
                      : '\u2014'}
                  </p>
                </div>
              </div>
            </div>

            {/* Image previews */}
            <h3 className="text-sm font-semibold text-gray-500 dark:text-gray-400 uppercase tracking-wider mt-6 mb-4">
              Uploaded Documents
            </h3>
            <div className="grid grid-cols-1 sm:grid-cols-3 gap-4">
              <ImagePreview src={status.document_front_image} alt="Document \u2013 Front" />
              {status.document_back_image && (
                <ImagePreview src={status.document_back_image} alt="Document \u2013 Back" />
              )}
              <ImagePreview src={status.selfie_image} alt="Selfie" />
            </div>
          </div>
        </div>

        {/* Info box */}
        <div className="bg-blue-50 dark:bg-blue-900/20 border border-blue-200 dark:border-blue-800 rounded-lg p-4">
          <div className="flex gap-3">
            <AlertCircle className="h-5 w-5 text-blue-600 dark:text-blue-400 flex-shrink-0 mt-0.5" />
            <div className="text-sm text-blue-800 dark:text-blue-200">
              <p className="font-medium mb-1">What happens next?</p>
              <ul className="list-disc list-inside space-y-1 text-blue-700 dark:text-blue-300">
                <li>Our team will verify your documents against the selfie you provided.</li>
                <li>You&apos;ll receive a notification once the review is complete.</li>
                <li>If we need additional information, we&apos;ll reach out via email.</li>
              </ul>
            </div>
          </div>
        </div>
      </div>
    );
  }

  /* ════════════════════════════════════════════ */
  /* APPROVED                                     */
  /* ════════════════════════════════════════════ */
  if (status && status.status === 'APPROVED') {
    return (
      <div className="space-y-6" role="region" aria-label="Verification approved">
        <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm border border-green-200 dark:border-green-800 overflow-hidden">
          <div className="px-6 py-8 text-center bg-gradient-to-b from-green-50 to-white dark:from-green-900/20 dark:to-gray-800">
            <div className="inline-flex items-center justify-center w-16 h-16 bg-green-100 dark:bg-green-900/50 rounded-full mb-4">
              <CheckCircle className="h-8 w-8 text-green-600 dark:text-green-400" />
            </div>
            <h2 className="text-2xl font-bold text-gray-900 dark:text-gray-100">Identity Verified</h2>
            <p className="text-gray-600 dark:text-gray-400 mt-2 max-w-md mx-auto">
              Your identity has been successfully verified. You now have full access to all platform features.
            </p>
            <div className="mt-4">
              <StatusBadge status="APPROVED" />
            </div>
          </div>

          <div className="px-6 py-5 border-t border-gray-100 dark:border-gray-700">
            <div className="grid grid-cols-1 sm:grid-cols-3 gap-4 text-sm">
              <div>
                <p className="text-gray-500 dark:text-gray-400">Document</p>
                <p className="font-medium text-gray-900 dark:text-gray-100">
                  {docTypeLabels[status.document_type || ''] || status.document_type_display || '\u2014'}
                </p>
              </div>
              <div>
                <p className="text-gray-500 dark:text-gray-400">Verified On</p>
                <p className="font-medium text-gray-900 dark:text-gray-100">
                  {status.verified_at ? format(new Date(status.verified_at), 'PPP') : '\u2014'}
                </p>
              </div>
              {status.expires_at && (
                <div>
                  <p className="text-gray-500 dark:text-gray-400">Expires</p>
                  <p className="font-medium text-gray-900 dark:text-gray-100">
                    {format(new Date(status.expires_at), 'PPP')}
                  </p>
                </div>
              )}
            </div>
          </div>
        </div>
      </div>
    );
  }

  /* ════════════════════════════════════════════ */
  /* REJECTED — details + resubmit option         */
  /* ════════════════════════════════════════════ */
  if (status && status.status === 'REJECTED' && !showForm) {
    return (
      <div className="space-y-6" role="region" aria-label="Verification rejected">
        <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm border border-red-200 dark:border-red-800 overflow-hidden">
          <div className="px-6 py-6 bg-gradient-to-r from-red-50 to-orange-50 dark:from-red-900/20 dark:to-orange-900/20">
            <div className="flex items-center gap-3">
              <div className="p-2.5 bg-red-100 dark:bg-red-900/50 rounded-full">
                <XCircle className="h-6 w-6 text-red-600 dark:text-red-400" />
              </div>
              <div>
                <h2 className="text-xl font-bold text-gray-900 dark:text-gray-100">Verification Not Approved</h2>
                <p className="text-sm text-gray-600 dark:text-gray-400">
                  Submitted {status.created_at ? formatDistanceToNow(new Date(status.created_at), { addSuffix: true }) : ''}
                </p>
              </div>
            </div>
          </div>

          {/* Rejection reason */}
          {status.rejection_reason && (
            <div className="mx-6 mt-5 p-4 bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 rounded-lg" role="alert">
              <h4 className="text-sm font-semibold text-red-900 dark:text-red-200 mb-1">Reason for rejection</h4>
              <p className="text-sm text-red-800 dark:text-red-300">{status.rejection_reason}</p>
            </div>
          )}
          {status.admin_notes && (
            <div className="mx-6 mt-3 p-4 bg-gray-50 dark:bg-gray-900/50 border border-gray-200 dark:border-gray-700 rounded-lg">
              <h4 className="text-sm font-semibold text-gray-700 dark:text-gray-300 mb-1">Admin Notes</h4>
              <p className="text-sm text-gray-600 dark:text-gray-400">{status.admin_notes}</p>
            </div>
          )}

          {/* Previous submission preview */}
          <div className="px-6 py-5">
            <h3 className="text-sm font-semibold text-gray-500 dark:text-gray-400 uppercase tracking-wider mb-3">
              Previous Submission
            </h3>
            <div className="grid grid-cols-2 sm:grid-cols-4 gap-3 text-sm">
              <div>
                <p className="text-gray-500 dark:text-gray-400">Type</p>
                <p className="font-medium text-gray-900 dark:text-gray-100">
                  {docTypeLabels[status.document_type || ''] || status.document_type_display || '\u2014'}
                </p>
              </div>
              <div>
                <p className="text-gray-500 dark:text-gray-400">Number</p>
                <p className="font-medium text-gray-900 dark:text-gray-100 font-mono">
                  {status.document_number ? `\u2022\u2022\u2022\u2022${status.document_number.slice(-4)}` : '\u2014'}
                </p>
              </div>
              <div>
                <p className="text-gray-500 dark:text-gray-400">Country</p>
                <p className="font-medium text-gray-900 dark:text-gray-100">{status.document_country || '\u2014'}</p>
              </div>
              <div>
                <p className="text-gray-500 dark:text-gray-400">Submitted</p>
                <p className="font-medium text-gray-900 dark:text-gray-100">
                  {status.created_at ? format(new Date(status.created_at), 'PP') : '\u2014'}
                </p>
              </div>
            </div>
          </div>

          {/* Resubmit CTA */}
          <div className="px-6 pb-6">
            <button
              onClick={() => setShowForm(true)}
              className="w-full sm:w-auto inline-flex items-center justify-center gap-2 px-6 py-3 bg-primary-600 dark:bg-primary-500 text-white font-medium rounded-lg hover:bg-primary-700 dark:hover:bg-primary-600 transition-colors"
            >
              <RefreshCw className="h-4 w-4" />
              Submit New Verification
            </button>
          </div>
        </div>
      </div>
    );
  }

  /* ════════════════════════════════════════════ */
  /* WIZARD FORM  (no status, or rejected+showForm) */
  /* ════════════════════════════════════════════ */
  return (
    <div className="max-w-3xl mx-auto" role="region" aria-label="Verification wizard">
      {/* Back to rejection card (if resubmitting) */}
      {status?.status === 'REJECTED' && showForm && (
        <button
          onClick={() => setShowForm(false)}
          className="inline-flex items-center gap-1.5 text-sm text-gray-600 dark:text-gray-400 hover:text-gray-900 dark:hover:text-gray-200 mb-4 transition-colors"
        >
          <ChevronLeft className="h-4 w-4" /> Back to rejection details
        </button>
      )}

      {/* Progress Steps */}
      <div className="mb-8" role="navigation" aria-label="Verification progress">
        <div className="flex items-center justify-between">
          {steps.map((step, index) => (
            <div key={step.id} className="flex items-center flex-1">
              <div className="flex items-center">
                <div
                  className={`flex items-center justify-center w-10 h-10 rounded-full border-2 ${
                    index <= currentStepIndex
                      ? 'border-primary-600 bg-primary-600 text-white dark:border-primary-500 dark:bg-primary-500'
                      : 'border-gray-300 dark:border-gray-600 bg-white dark:bg-gray-800 text-gray-500 dark:text-gray-400'
                  }`}
                  aria-current={index === currentStepIndex ? 'step' : undefined}
                >
                  {index < currentStepIndex ? (
                    <Check className="h-5 w-5" aria-hidden="true" />
                  ) : (
                    <span>{index + 1}</span>
                  )}
                </div>
                <span
                  className={`ml-2 text-sm font-medium ${
                    index <= currentStepIndex ? 'text-gray-900 dark:text-gray-100' : 'text-gray-500 dark:text-gray-400'
                  }`}
                >
                  {step.label}
                </span>
              </div>
              {index < steps.length - 1 && (
                <div
                  className={`flex-1 h-0.5 mx-4 ${
                    index < currentStepIndex ? 'bg-primary-600 dark:bg-primary-500' : 'bg-gray-300 dark:bg-gray-600'
                  }`}
                  aria-hidden="true"
                />
              )}
            </div>
          ))}
        </div>
      </div>

      {/* Step Content */}
      <div className="bg-white dark:bg-gray-800 rounded-lg shadow-sm border border-gray-200 dark:border-gray-700 p-6 mb-6">
        {currentStep === 'document' && (
          <div className="space-y-6">
            <h2 className="text-2xl font-bold text-gray-900 dark:text-gray-100">Upload Your ID Document</h2>

            <div>
              <label htmlFor={docTypeId} className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                Document Type *
              </label>
              <select
                id={docTypeId}
                value={documentType}
                onChange={(e) => setDocumentType(e.target.value as typeof documentType)}
                className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-900 text-gray-900 dark:text-gray-100 focus:ring-2 focus:ring-primary-500 focus:border-transparent"
              >
                <option value="PASSPORT">Passport</option>
                <option value="NATIONAL_ID">National ID Card</option>
                <option value="DRIVERS_LICENSE">Driver&apos;s License</option>
              </select>
            </div>

            <div>
              <label htmlFor={docNumberId} className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                Document Number *
              </label>
              <input
                id={docNumberId}
                type="text"
                value={documentNumber}
                onChange={(e) => setDocumentNumber(e.target.value)}
                className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-900 text-gray-900 dark:text-gray-100 focus:ring-2 focus:ring-primary-500 focus:border-transparent"
                placeholder="Enter document number"
                aria-required="true"
              />
            </div>

            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              <div>
                <label htmlFor={issuedCountryId} className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                  Issued Country *
                </label>
                <select
                  id={issuedCountryId}
                  value={issuedCountry}
                  onChange={(e) => setIssuedCountry(e.target.value)}
                  className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-900 text-gray-900 dark:text-gray-100 focus:ring-2 focus:ring-primary-500 focus:border-transparent"
                  aria-required="true"
                >
                  <option value="">Select country</option>
                  {countries.map(country => (
                    <option key={country} value={country}>{country}</option>
                  ))}
                </select>
              </div>

              <div>
                <label htmlFor={expiryDateId} className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                  Expiry Date (Optional)
                </label>
                <input
                  id={expiryDateId}
                  type="date"
                  value={expiryDate}
                  onChange={(e) => setExpiryDate(e.target.value)}
                  className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-900 text-gray-900 dark:text-gray-100 focus:ring-2 focus:ring-primary-500 focus:border-transparent"
                />
              </div>
            </div>

            <DocumentUpload
              documentType={documentType}
              requiresBackImage={requiresBackImage}
              onUploadComplete={(front, back) => {
                setFrontImageUrl(front);
                if (back) setBackImageUrl(back);
              }}
            />
          </div>
        )}

        {currentStep === 'selfie' && (
          <div className="space-y-6">
            <h2 className="text-2xl font-bold text-gray-900 dark:text-gray-100">Take a Selfie</h2>
            <p className="text-gray-600 dark:text-gray-400">
              We&apos;ll use this to verify that you&apos;re the person in your ID document.
            </p>
            <SelfieCapture onCaptureComplete={setSelfieUrl} />
          </div>
        )}

        {currentStep === 'review' && (
          <div className="space-y-6">
            <h2 className="text-2xl font-bold text-gray-900 dark:text-gray-100">Review &amp; Submit</h2>

            <div className="space-y-4">
              <div className="p-4 bg-gray-50 dark:bg-gray-900 rounded-lg">
                <h3 className="font-medium text-gray-900 dark:text-gray-100 mb-2">Document Information</h3>
                <dl className="space-y-2 text-sm">
                  <div className="flex justify-between">
                    <dt className="text-gray-600 dark:text-gray-400">Type:</dt>
                    <dd className="text-gray-900 dark:text-gray-100">{docTypeLabels[documentType] || documentType}</dd>
                  </div>
                  <div className="flex justify-between">
                    <dt className="text-gray-600 dark:text-gray-400">Number:</dt>
                    <dd className="text-gray-900 dark:text-gray-100">{documentNumber}</dd>
                  </div>
                  <div className="flex justify-between">
                    <dt className="text-gray-600 dark:text-gray-400">Country:</dt>
                    <dd className="text-gray-900 dark:text-gray-100">{issuedCountry}</dd>
                  </div>
                  {expiryDate && (
                    <div className="flex justify-between">
                      <dt className="text-gray-600 dark:text-gray-400">Expiry:</dt>
                      <dd className="text-gray-900 dark:text-gray-100">{expiryDate}</dd>
                    </div>
                  )}
                </dl>
              </div>

              <div className="bg-primary-50 dark:bg-primary-900/30 border border-primary-200 dark:border-primary-700 rounded-lg p-4">
                <p className="text-sm text-primary-800 dark:text-primary-200">
                  By submitting, you confirm that all information provided is accurate and that you are the person shown in the documents.
                </p>
              </div>
            </div>
          </div>
        )}
      </div>

      {/* Navigation Buttons */}
      <div className="flex justify-between" role="group" aria-label="Wizard navigation">
        <button
          onClick={handleBack}
          disabled={currentStep === 'document'}
          className="inline-flex items-center gap-2 px-6 py-3 border border-gray-300 dark:border-gray-600 rounded-lg text-gray-700 dark:text-gray-300 hover:bg-gray-50 dark:hover:bg-gray-800 disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
          aria-label="Go to previous step"
        >
          <ChevronLeft className="h-4 w-4" aria-hidden="true" />
          <span>Back</span>
        </button>

        {currentStep !== 'review' ? (
          <button
            onClick={handleNext}
            disabled={
              (currentStep === 'document' && !canProceedFromDocument) ||
              (currentStep === 'selfie' && !canProceedFromSelfie)
            }
            className="inline-flex items-center gap-2 px-6 py-3 bg-primary-600 dark:bg-primary-500 text-white rounded-lg hover:bg-primary-700 dark:hover:bg-primary-600 disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
            aria-label="Go to next step"
          >
            <span>Next</span>
            <ChevronRight className="h-4 w-4" aria-hidden="true" />
          </button>
        ) : (
          <button
            onClick={handleSubmit}
            disabled={isSubmitting}
            aria-busy={isSubmitting}
            className="inline-flex items-center gap-2 px-6 py-3 bg-green-600 text-white rounded-lg hover:bg-green-700 disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
          >
            {isSubmitting ? (
              <>
                <Loader2 className="h-4 w-4 animate-spin" aria-hidden="true" />
                <span>Submitting\u2026</span>
              </>
            ) : (
              <>
                <Check className="h-4 w-4" aria-hidden="true" />
                <span>Submit Verification</span>
              </>
            )}
          </button>
        )}
      </div>
    </div>
  );
};
