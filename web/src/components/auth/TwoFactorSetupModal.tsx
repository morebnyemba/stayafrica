'use client';

import { useState } from 'react';
import { toast } from 'react-hot-toast';
import axios from 'axios';

interface TwoFactorSetupModalProps {
  isOpen: boolean;
  onClose: () => void;
  onSuccess: () => void;
}

export default function TwoFactorSetupModal({ isOpen, onClose, onSuccess }: TwoFactorSetupModalProps) {
  const [step, setStep] = useState<'setup' | 'verify'>('setup');
  const [qrCode, setQrCode] = useState<string>('');
  const [secret, setSecret] = useState<string>('');
  const [backupCodes, setBackupCodes] = useState<string[]>([]);
  const [verificationCode, setVerificationCode] = useState('');
  const [isLoading, setIsLoading] = useState(false);

  const handleSetup = async () => {
    setIsLoading(true);
    try {
      const token = localStorage.getItem('access_token');
      const response = await axios.post(
        `${process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000'}/api/v1/2fa/setup/`,
        {},
        {
          headers: {
            Authorization: `Bearer ${token}`,
          },
        }
      );

      setQrCode(response.data.qr_code);
      setSecret(response.data.secret);
      setBackupCodes(response.data.backup_codes);
      setStep('verify');
      toast.success(response.data.message);
    } catch (error: any) {
      toast.error(error.response?.data?.error || 'Failed to setup 2FA');
    } finally {
      setIsLoading(false);
    }
  };

  const handleVerifyAndEnable = async (e: React.FormEvent) => {
    e.preventDefault();
    setIsLoading(true);
    try {
      const token = localStorage.getItem('access_token');
      const response = await axios.post(
        `${process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000'}/api/v1/2fa/enable/`,
        { token: verificationCode },
        {
          headers: {
            Authorization: `Bearer ${token}`,
          },
        }
      );

      toast.success(response.data.message);
      onSuccess();
      onClose();
    } catch (error: any) {
      toast.error(error.response?.data?.error || 'Invalid verification code');
    } finally {
      setIsLoading(false);
    }
  };

  const downloadBackupCodes = () => {
    const text = backupCodes.join('\n');
    const blob = new Blob([text], { type: 'text/plain' });
    const url = window.URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = 'stayafrica-backup-codes.txt';
    a.click();
    window.URL.revokeObjectURL(url);
  };

  if (!isOpen) return null;

  return (
    <div className="fixed inset-0 z-50 overflow-y-auto">
      <div className="flex min-h-full items-end justify-center p-4 text-center sm:items-center sm:p-0">
        <div className="fixed inset-0 bg-gray-500 bg-opacity-75 transition-opacity" onClick={onClose} />
        
        <div className="relative transform overflow-hidden rounded-lg bg-white px-4 pb-4 pt-5 text-left shadow-xl transition-all sm:my-8 sm:w-full sm:max-w-lg sm:p-6">
          <div className="absolute right-0 top-0 pr-4 pt-4">
            <button
              type="button"
              className="rounded-md bg-white text-gray-400 hover:text-gray-500"
              onClick={onClose}
            >
              <span className="sr-only">Close</span>
              <svg className="h-6 w-6" fill="none" viewBox="0 0 24 24" strokeWidth="1.5" stroke="currentColor">
                <path strokeLinecap="round" strokeLinejoin="round" d="M6 18L18 6M6 6l12 12" />
              </svg>
            </button>
          </div>

          <div className="sm:flex sm:items-start">
            <div className="mt-3 text-center sm:mt-0 sm:text-left w-full">
              <h3 className="text-lg font-semibold leading-6 text-gray-900 mb-4">
                {step === 'setup' ? 'Setup Two-Factor Authentication' : 'Verify 2FA Setup'}
              </h3>

              {step === 'setup' ? (
                <div>
                  <p className="text-sm text-gray-500 mb-4">
                    Two-factor authentication adds an extra layer of security to your account.
                    You'll need to enter a code from your authenticator app each time you log in.
                  </p>
                  <button
                    onClick={handleSetup}
                    disabled={isLoading}
                    className="w-full rounded-md bg-orange-600 px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-orange-500 disabled:opacity-50"
                  >
                    {isLoading ? 'Setting up...' : 'Begin Setup'}
                  </button>
                </div>
              ) : (
                <div>
                  <div className="mb-6">
                    <p className="text-sm text-gray-500 mb-4">
                      1. Scan this QR code with your authenticator app (Google Authenticator, Authy, etc.)
                    </p>
                    {qrCode && (
                      <div className="flex justify-center mb-4">
                        <img src={qrCode} alt="QR Code" className="w-64 h-64" />
                      </div>
                    )}
                    <p className="text-xs text-gray-500 text-center mb-4">
                      Or enter this code manually: <code className="bg-gray-100 px-2 py-1 rounded">{secret}</code>
                    </p>
                  </div>

                  <div className="mb-6">
                    <p className="text-sm font-medium text-gray-700 mb-2">Backup Codes</p>
                    <p className="text-xs text-gray-500 mb-2">
                      Save these backup codes in a safe place. You can use them to access your account if you lose your phone.
                    </p>
                    <div className="bg-gray-50 rounded-lg p-3 mb-2">
                      <div className="grid grid-cols-2 gap-2 text-xs font-mono">
                        {backupCodes.map((code, index) => (
                          <div key={index}>{code}</div>
                        ))}
                      </div>
                    </div>
                    <button
                      onClick={downloadBackupCodes}
                      className="text-sm text-orange-600 hover:text-orange-500"
                    >
                      Download backup codes
                    </button>
                  </div>

                  <form onSubmit={handleVerifyAndEnable}>
                    <p className="text-sm text-gray-500 mb-2">
                      2. Enter the 6-digit code from your authenticator app to verify setup
                    </p>
                    <input
                      type="text"
                      maxLength={6}
                      value={verificationCode}
                      onChange={(e) => setVerificationCode(e.target.value.replace(/\D/g, ''))}
                      placeholder="000000"
                      className="w-full rounded-md border border-gray-300 px-3 py-2 text-center text-2xl tracking-widest mb-4"
                      required
                    />
                    <button
                      type="submit"
                      disabled={isLoading || verificationCode.length !== 6}
                      className="w-full rounded-md bg-orange-600 px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-orange-500 disabled:opacity-50"
                    >
                      {isLoading ? 'Verifying...' : 'Verify and Enable 2FA'}
                    </button>
                  </form>
                </div>
              )}
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
