'use client';

import { useState } from 'react';
import { toast } from 'react-hot-toast';

interface TwoFactorVerifyProps {
  email: string;
  password: string;
  onSuccess: (tokens: { access: string; refresh: string; user: any }) => void;
  onBack: () => void;
}

export default function TwoFactorVerify({ email, password, onSuccess, onBack }: TwoFactorVerifyProps) {
  const [code, setCode] = useState('');
  const [useBackupCode, setUseBackupCode] = useState(false);
  const [backupCode, setBackupCode] = useState('');
  const [isLoading, setIsLoading] = useState(false);

  const handleVerify = async (e: React.FormEvent) => {
    e.preventDefault();
    setIsLoading(true);

    try {
      const response = await fetch(
        `${process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000'}/api/v1/auth/login/2fa/`,
        {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
          },
          body: JSON.stringify({
            email,
            password,
            ...(useBackupCode ? { backup_code: backupCode } : { token: code }),
          }),
        }
      );

      const data = await response.json();

      if (response.ok) {
        toast.success('Login successful!');
        onSuccess(data);
      } else {
        toast.error(data.error || 'Verification failed');
      }
    } catch (error) {
      toast.error('An error occurred. Please try again.');
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div className="space-y-6">
      <div>
        <h2 className="text-2xl font-bold text-gray-900">Two-Factor Authentication</h2>
        <p className="mt-2 text-sm text-gray-600">
          {useBackupCode
            ? 'Enter one of your backup codes'
            : 'Enter the 6-digit code from your authenticator app'}
        </p>
      </div>

      <form onSubmit={handleVerify} className="space-y-4">
        {useBackupCode ? (
          <div>
            <label htmlFor="backup-code" className="block text-sm font-medium text-gray-700 mb-2">
              Backup Code
            </label>
            <input
              id="backup-code"
              type="text"
              value={backupCode}
              onChange={(e) => setBackupCode(e.target.value)}
              placeholder="xxxx-xxxx-xxxx-xxxx"
              className="w-full rounded-md border border-gray-300 px-3 py-2"
              required
            />
          </div>
        ) : (
          <div>
            <label htmlFor="code" className="block text-sm font-medium text-gray-700 mb-2">
              Verification Code
            </label>
            <input
              id="code"
              type="text"
              maxLength={6}
              value={code}
              onChange={(e) => setCode(e.target.value.replace(/\D/g, ''))}
              placeholder="000000"
              className="w-full rounded-md border border-gray-300 px-3 py-2 text-center text-2xl tracking-widest"
              required
            />
          </div>
        )}

        <button
          type="submit"
          disabled={isLoading || (useBackupCode ? !backupCode : code.length !== 6)}
          className="w-full rounded-md bg-orange-600 px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-orange-500 disabled:opacity-50 disabled:cursor-not-allowed"
        >
          {isLoading ? 'Verifying...' : 'Verify'}
        </button>

        <div className="flex items-center justify-between text-sm">
          <button
            type="button"
            onClick={() => setUseBackupCode(!useBackupCode)}
            className="text-orange-600 hover:text-orange-500"
          >
            {useBackupCode ? 'Use authenticator app' : 'Use backup code'}
          </button>
          <button
            type="button"
            onClick={onBack}
            className="text-gray-600 hover:text-gray-500"
          >
            ← Back to login
          </button>
        </div>
      </form>

      <div className="rounded-md bg-blue-50 p-4">
        <div className="flex">
          <div className="flex-shrink-0">
            <svg className="h-5 w-5 text-blue-400" viewBox="0 0 20 20" fill="currentColor">
              <path
                fillRule="evenodd"
                d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z"
                clipRule="evenodd"
              />
            </svg>
          </div>
          <div className="ml-3 flex-1">
            <p className="text-sm text-blue-700">
              Lost access to your authenticator app? Use a backup code to sign in.
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}
