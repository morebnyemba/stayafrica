'use client';

import { useState, useEffect } from 'react';
import { toast } from 'react-hot-toast';
import axios from 'axios';
import TwoFactorSetupModal from '@/components/auth/TwoFactorSetupModal';

export default function TwoFactorSettings() {
  const [isEnabled, setIsEnabled] = useState(false);
  const [isLoading, setIsLoading] = useState(true);
  const [showSetupModal, setShowSetupModal] = useState(false);
  const [showDisableConfirm, setShowDisableConfirm] = useState(false);
  const [password, setPassword] = useState('');
  const [showRegenerateConfirm, setShowRegenerateConfirm] = useState(false);

  useEffect(() => {
    fetchStatus();
  }, []);

  const fetchStatus = async () => {
    try {
      const token = localStorage.getItem('access_token');
      const response = await axios.get(
        `${process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000'}/api/v1/2fa/status/`,
        {
          headers: {
            Authorization: `Bearer ${token}`,
          },
        }
      );
      setIsEnabled(response.data.two_factor_enabled);
    } catch (error) {
      console.error('Failed to fetch 2FA status', error);
    } finally {
      setIsLoading(false);
    }
  };

  const handleDisable2FA = async (e: React.FormEvent) => {
    e.preventDefault();
    setIsLoading(true);
    try {
      const token = localStorage.getItem('access_token');
      await axios.post(
        `${process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000'}/api/v1/2fa/disable/`,
        { password },
        {
          headers: {
            Authorization: `Bearer ${token}`,
          },
        }
      );
      setIsEnabled(false);
      setShowDisableConfirm(false);
      setPassword('');
      toast.success('2FA disabled successfully');
    } catch (error: any) {
      toast.error(error.response?.data?.error || 'Failed to disable 2FA');
    } finally {
      setIsLoading(false);
    }
  };

  const handleRegenerateBackupCodes = async (e: React.FormEvent) => {
    e.preventDefault();
    setIsLoading(true);
    try {
      const token = localStorage.getItem('access_token');
      const response = await axios.post(
        `${process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000'}/api/v1/2fa/backup-codes/regenerate/`,
        { password },
        {
          headers: {
            Authorization: `Bearer ${token}`,
          },
        }
      );
      
      // Download backup codes
      const codes = response.data.backup_codes.join('\n');
      const blob = new Blob([codes], { type: 'text/plain' });
      const url = window.URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = 'stayafrica-backup-codes.txt';
      a.click();
      window.URL.revokeObjectURL(url);
      
      setShowRegenerateConfirm(false);
      setPassword('');
      toast.success('Backup codes regenerated successfully');
    } catch (error: any) {
      toast.error(error.response?.data?.error || 'Failed to regenerate backup codes');
    } finally {
      setIsLoading(false);
    }
  };

  if (isLoading) {
    return <div className="animate-pulse">Loading...</div>;
  }

  return (
    <div className="bg-white dark:bg-primary-800/40 shadow sm:rounded-lg">
      <div className="px-4 py-5 sm:p-6">
        <div className="sm:flex sm:items-start sm:justify-between">
          <div>
            <h3 className="text-lg font-medium leading-6 text-primary-900 dark:text-sand-50">
              Two-Factor Authentication
            </h3>
            <div className="mt-2 max-w-xl text-sm text-primary-400 dark:text-sand-500">
              <p>
                Add an extra layer of security to your account. When enabled, you'll need to enter a code
                from your authenticator app in addition to your password.
              </p>
            </div>
            <div className="mt-3">
              <span
                className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${
                  isEnabled ? 'bg-green-100 text-green-800' : 'bg-primary-100 dark:bg-primary-800 text-primary-800 dark:text-sand-100'
                }`}
              >
                {isEnabled ? 'Enabled' : 'Disabled'}
              </span>
            </div>
          </div>
          <div className="mt-5 sm:ml-6 sm:mt-0 sm:flex sm:flex-shrink-0 sm:items-center">
            {isEnabled ? (
              <div className="space-y-2">
                <button
                  type="button"
                  onClick={() => setShowDisableConfirm(true)}
                  className="inline-flex w-full items-center justify-center rounded-md bg-red-600 px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-red-500"
                >
                  Disable 2FA
                </button>
                <button
                  type="button"
                  onClick={() => setShowRegenerateConfirm(true)}
                  className="inline-flex w-full items-center justify-center rounded-md bg-primary-600 dark:bg-primary-500 px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-primary-500 dark:hover:bg-primary-400"
                >
                  Regenerate Backup Codes
                </button>
              </div>
            ) : (
              <button
                type="button"
                onClick={() => setShowSetupModal(true)}
                className="inline-flex items-center rounded-md bg-orange-600 px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-orange-500"
              >
                Enable 2FA
              </button>
            )}
          </div>
        </div>
      </div>

      {/* Setup Modal */}
      <TwoFactorSetupModal
        isOpen={showSetupModal}
        onClose={() => setShowSetupModal(false)}
        onSuccess={() => {
          setIsEnabled(true);
          fetchStatus();
        }}
      />

      {/* Disable Confirmation Modal */}
      {showDisableConfirm && (
        <div className="fixed inset-0 z-50 overflow-y-auto">
          <div className="flex min-h-full items-end justify-center p-4 text-center sm:items-center sm:p-0">
            <div className="fixed inset-0 bg-primary-500 dark:bg-primary-900 bg-opacity-75 transition-opacity" onClick={() => setShowDisableConfirm(false)} />
            <div className="relative transform overflow-hidden rounded-lg bg-white dark:bg-primary-800/40 px-4 pb-4 pt-5 text-left shadow-xl transition-all sm:my-8 sm:w-full sm:max-w-lg sm:p-6">
              <h3 className="text-lg font-semibold leading-6 text-primary-900 dark:text-sand-50 mb-4">
                Disable Two-Factor Authentication
              </h3>
              <p className="text-sm text-primary-400 dark:text-sand-500 mb-4">
                Enter your password to confirm disabling 2FA
              </p>
              <form onSubmit={handleDisable2FA}>
                <input
                  type="password"
                  value={password}
                  onChange={(e) => setPassword(e.target.value)}
                  placeholder="Password"
                  className="w-full rounded-md border border-primary-300 dark:border-primary-600 px-3 py-2 mb-4"
                  required
                />
                <div className="flex gap-3">
                  <button
                    type="button"
                    onClick={() => {
                      setShowDisableConfirm(false);
                      setPassword('');
                    }}
                    className="flex-1 rounded-md bg-white dark:bg-primary-800/40 px-3 py-2 text-sm font-semibold text-primary-900 dark:text-sand-50 shadow-sm ring-1 ring-inset ring-primary-300 dark:ring-primary-600 hover:bg-sand-50 dark:hover:bg-primary-800"
                  >
                    Cancel
                  </button>
                  <button
                    type="submit"
                    disabled={isLoading}
                    className="flex-1 rounded-md bg-red-600 px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-red-500 disabled:opacity-50"
                  >
                    {isLoading ? 'Disabling...' : 'Disable 2FA'}
                  </button>
                </div>
              </form>
            </div>
          </div>
        </div>
      )}

      {/* Regenerate Backup Codes Modal */}
      {showRegenerateConfirm && (
        <div className="fixed inset-0 z-50 overflow-y-auto">
          <div className="flex min-h-full items-end justify-center p-4 text-center sm:items-center sm:p-0">
            <div className="fixed inset-0 bg-primary-500 dark:bg-primary-900 bg-opacity-75 transition-opacity" onClick={() => setShowRegenerateConfirm(false)} />
            <div className="relative transform overflow-hidden rounded-lg bg-white dark:bg-primary-800/40 px-4 pb-4 pt-5 text-left shadow-xl transition-all sm:my-8 sm:w-full sm:max-w-lg sm:p-6">
              <h3 className="text-lg font-semibold leading-6 text-primary-900 dark:text-sand-50 mb-4">
                Regenerate Backup Codes
              </h3>
              <p className="text-sm text-primary-400 dark:text-sand-500 mb-4">
                This will invalidate your old backup codes. Enter your password to confirm.
              </p>
              <form onSubmit={handleRegenerateBackupCodes}>
                <input
                  type="password"
                  value={password}
                  onChange={(e) => setPassword(e.target.value)}
                  placeholder="Password"
                  className="w-full rounded-md border border-primary-300 dark:border-primary-600 px-3 py-2 mb-4"
                  required
                />
                <div className="flex gap-3">
                  <button
                    type="button"
                    onClick={() => {
                      setShowRegenerateConfirm(false);
                      setPassword('');
                    }}
                    className="flex-1 rounded-md bg-white dark:bg-primary-800/40 px-3 py-2 text-sm font-semibold text-primary-900 dark:text-sand-50 shadow-sm ring-1 ring-inset ring-primary-300 dark:ring-primary-600 hover:bg-sand-50 dark:hover:bg-primary-800"
                  >
                    Cancel
                  </button>
                  <button
                    type="submit"
                    disabled={isLoading}
                    className="flex-1 rounded-md bg-orange-600 px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-orange-500 disabled:opacity-50"
                  >
                    {isLoading ? 'Regenerating...' : 'Regenerate'}
                  </button>
                </div>
              </form>
            </div>
          </div>
        </div>
      )}
    </div>
  );
}
