'use client';

import { useState, useId } from 'react';
import { DocumentUpload } from './DocumentUpload';
import { SelfieCapture } from './SelfieCapture';
import { useVerification } from './useVerification';
import { ChevronLeft, ChevronRight, Check, Loader2 } from 'lucide-react';
import toast from 'react-hot-toast';

type Step = 'document' | 'selfie' | 'review';

const countries = [
  'United States', 'United Kingdom', 'Canada', 'Australia', 'South Africa',
  'Nigeria', 'Kenya', 'Ghana', 'Uganda', 'Tanzania', 'Zimbabwe',
];

export const VerificationWizard = () => {
  const [currentStep, setCurrentStep] = useState<Step>('document');
  const [documentType, setDocumentType] = useState<'PASSPORT' | 'NATIONAL_ID' | 'DRIVERS_LICENSE'>('PASSPORT');
  const [documentNumber, setDocumentNumber] = useState('');
  const [issuedCountry, setIssuedCountry] = useState('');
  const [expiryDate, setExpiryDate] = useState('');
  const [frontImageUrl, setFrontImageUrl] = useState('');
  const [backImageUrl, setBackImageUrl] = useState('');
  const [selfieUrl, setSelfieUrl] = useState('');

  // Generate unique IDs for accessibility
  const docTypeId = useId();
  const docNumberId = useId();
  const issuedCountryId = useId();
  const expiryDateId = useId();

  const { submitVerification, isSubmitting } = useVerification();

  const steps: { id: Step; label: string }[] = [
    { id: 'document', label: 'Document' },
    { id: 'selfie', label: 'Selfie' },
    { id: 'review', label: 'Review' },
  ];

  const currentStepIndex = steps.findIndex(s => s.id === currentStep);
  const requiresBackImage = documentType === 'NATIONAL_ID' || documentType === 'DRIVERS_LICENSE';

  const canProceedFromDocument = 
    documentType && 
    documentNumber.trim() && 
    issuedCountry && 
    frontImageUrl && 
    (!requiresBackImage || backImageUrl);

  const canProceedFromSelfie = selfieUrl !== '';

  const handleNext = () => {
    if (currentStep === 'document' && canProceedFromDocument) {
      setCurrentStep('selfie');
    } else if (currentStep === 'selfie' && canProceedFromSelfie) {
      setCurrentStep('review');
    }
  };

  const handleBack = () => {
    if (currentStep === 'selfie') {
      setCurrentStep('document');
    } else if (currentStep === 'review') {
      setCurrentStep('selfie');
    }
  };

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
      
      // Success - show toast notification
      toast.success('Verification submitted successfully! We\'ll review your documents within 1-2 business days.', {
        duration: 5000,
      });
    } catch (error) {
      console.error('Verification submission failed:', error);
      toast.error('Failed to submit verification. Please try again.');
    }
  };

  return (
    <div className="max-w-3xl mx-auto" role="region" aria-label="Verification wizard">
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
                <option value="DRIVERS_LICENSE">Driver's License</option>
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
              We'll use this to verify that you're the person in your ID document.
            </p>
            
            <SelfieCapture onCaptureComplete={setSelfieUrl} />
          </div>
        )}

        {currentStep === 'review' && (
          <div className="space-y-6">
            <h2 className="text-2xl font-bold text-gray-900 dark:text-gray-100">Review & Submit</h2>
            
            <div className="space-y-4">
              <div className="p-4 bg-gray-50 dark:bg-gray-900 rounded-lg">
                <h3 className="font-medium text-gray-900 dark:text-gray-100 mb-2">Document Information</h3>
                <dl className="space-y-2 text-sm">
                  <div className="flex justify-between">
                    <dt className="text-gray-600 dark:text-gray-400">Type:</dt>
                    <dd className="text-gray-900 dark:text-gray-100">{documentType.replace('_', ' ')}</dd>
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
                <span>Submitting...</span>
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
