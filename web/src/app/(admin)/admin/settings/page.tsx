'use client';

import { useEffect, useState } from 'react';
import { adminApi } from '@/lib/admin-api';
import { SystemConfig } from '@/types/admin-types';
import toast from 'react-hot-toast';

export default function SettingsPage() {
  const [config, setConfig] = useState<SystemConfig | null>(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    loadConfig();
  }, []);

  const loadConfig = async () => {
    try {
      setLoading(true);
      const data = await adminApi.getSystemConfig();
      setConfig(data || null);
    } catch (err: any) {
      const errorMsg = err?.response?.data?.detail || 'Failed to load system configuration';
      toast.error(errorMsg);
      console.error('Config load error:', err);
    } finally {
      setLoading(false);
    }
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center h-full">
        <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-[#D9B168]"></div>
      </div>
    );
  }

  if (!config) {
    return (
      <div className="p-8">
        <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded">
          Failed to load system configuration
        </div>
      </div>
    );
  }

  return (
    <div className="p-8">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-[#122F26]">System Settings</h1>
        <p className="text-[#3A5C50] mt-2">Configure system-wide settings and parameters</p>
      </div>

      <div className="space-y-6">
        {/* Pricing Configuration */}
        <div className="bg-white rounded-lg shadow p-6">
          <h2 className="text-xl font-semibold text-[#122F26] mb-4">Pricing Configuration</h2>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Commission Rate (%)
              </label>
              <input
                type="number"
                step="0.01"
                value={parseFloat(config.commission_rate) * 100}
                readOnly
                className="w-full px-4 py-2 border border-[#3A5C50] rounded-lg bg-gray-50"
              />
              <p className="text-xs text-gray-500 mt-1">
                Current: {(parseFloat(config.commission_rate) * 100).toFixed(2)}%
              </p>
            </div>
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Service Fee
              </label>
              <input
                type="number"
                step="0.01"
                value={config.service_fee}
                readOnly
                className="w-full px-4 py-2 border border-[#3A5C50] rounded-lg bg-gray-50"
              />
              <p className="text-xs text-gray-500 mt-1">Per booking</p>
            </div>
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Default Currency
              </label>
              <input
                type="text"
                value={config.default_currency}
                readOnly
                className="w-full px-4 py-2 border border-[#3A5C50] rounded-lg bg-gray-50"
              />
            </div>
          </div>
        </div>

        {/* Business Rules */}
        <div className="bg-white rounded-lg shadow p-6">
          <h2 className="text-xl font-semibold text-[#122F26] mb-4">Business Rules</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Max Advance Booking (days)
              </label>
              <input
                type="number"
                value={config.max_advance_booking_days}
                readOnly
                className="w-full px-4 py-2 border border-[#3A5C50] rounded-lg bg-gray-50"
              />
            </div>
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Max Stay Duration (days)
              </label>
              <input
                type="number"
                value={config.max_stay_duration_days}
                readOnly
                className="w-full px-4 py-2 border border-[#3A5C50] rounded-lg bg-gray-50"
              />
            </div>
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Review Window (days)
              </label>
              <input
                type="number"
                value={config.review_window_days}
                readOnly
                className="w-full px-4 py-2 border border-[#3A5C50] rounded-lg bg-gray-50"
              />
              <p className="text-xs text-gray-500 mt-1">Days after checkout to submit review</p>
            </div>
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Review Edit Window (days)
              </label>
              <input
                type="number"
                value={config.review_edit_window_days}
                readOnly
                className="w-full px-4 py-2 border border-[#3A5C50] rounded-lg bg-gray-50"
              />
              <p className="text-xs text-gray-500 mt-1">Days to edit review after submission</p>
            </div>
          </div>
        </div>

        {/* Email Configuration */}
        <div className="bg-white rounded-lg shadow p-6">
          <h2 className="text-xl font-semibold text-[#122F26] mb-4">Email Configuration</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Admin Email
              </label>
              <input
                type="email"
                value={config.admin_email}
                readOnly
                className="w-full px-4 py-2 border border-[#3A5C50] rounded-lg bg-gray-50"
              />
            </div>
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Support Email
              </label>
              <input
                type="email"
                value={config.support_email}
                readOnly
                className="w-full px-4 py-2 border border-[#3A5C50] rounded-lg bg-gray-50"
              />
            </div>
          </div>
        </div>

        {/* Payment Gateway Status */}
        <div className="bg-white rounded-lg shadow p-6">
          <h2 className="text-xl font-semibold text-[#122F26] mb-4">Payment Gateways</h2>
          <div className="space-y-4">
            <div className="flex items-center justify-between p-4 border border-gray-200 rounded-lg">
              <div>
                <h3 className="font-medium text-[#122F26]">Paynow</h3>
                <p className="text-sm text-[#3A5C50]">
                  {config.paynow_integration_id ? 'Configured' : 'Not configured'}
                </p>
              </div>
              <span className={`px-3 py-1 rounded-full text-sm font-medium ${
                config.paynow_integration_id 
                  ? 'bg-green-100 text-green-800' 
                  : 'bg-gray-100 text-gray-800'
              }`}>
                {config.paynow_integration_id ? 'Active' : 'Inactive'}
              </span>
            </div>
            <div className="flex items-center justify-between p-4 border border-gray-200 rounded-lg">
              <div>
                <h3 className="font-medium text-[#122F26]">PayFast</h3>
                <p className="text-sm text-[#3A5C50]">
                  {config.payfast_merchant_id ? 'Configured' : 'Not configured'}
                </p>
              </div>
              <span className={`px-3 py-1 rounded-full text-sm font-medium ${
                config.payfast_merchant_id 
                  ? 'bg-green-100 text-green-800' 
                  : 'bg-gray-100 text-gray-800'
              }`}>
                {config.payfast_merchant_id ? 'Active' : 'Inactive'}
              </span>
            </div>
            <div className="flex items-center justify-between p-4 border border-gray-200 rounded-lg">
              <div>
                <h3 className="font-medium text-[#122F26]">Stripe</h3>
                <p className="text-sm text-[#3A5C50]">
                  {config.stripe_secret_key ? 'Configured' : 'Not configured'}
                </p>
              </div>
              <span className={`px-3 py-1 rounded-full text-sm font-medium ${
                config.stripe_secret_key 
                  ? 'bg-green-100 text-green-800' 
                  : 'bg-gray-100 text-gray-800'
              }`}>
                {config.stripe_secret_key ? 'Active' : 'Inactive'}
              </span>
            </div>
          </div>
        </div>

        {/* Maintenance Mode */}
        <div className="bg-white rounded-lg shadow p-6">
          <h2 className="text-xl font-semibold text-[#122F26] mb-4">Maintenance Mode</h2>
          <div className="space-y-4">
            <div className="flex items-center justify-between p-4 border border-gray-200 rounded-lg">
              <div>
                <h3 className="font-medium text-[#122F26]">System Status</h3>
                <p className="text-sm text-[#3A5C50]">
                  {config.maintenance_mode ? 'Maintenance mode is active' : 'System is operational'}
                </p>
              </div>
              <span className={`px-3 py-1 rounded-full text-sm font-medium ${
                config.maintenance_mode 
                  ? 'bg-yellow-100 text-yellow-800' 
                  : 'bg-green-100 text-green-800'
              }`}>
                {config.maintenance_mode ? 'Maintenance' : 'Active'}
              </span>
            </div>
            {config.maintenance_mode && config.maintenance_message && (
              <div className="p-4 bg-yellow-50 border border-yellow-200 rounded-lg">
                <p className="text-sm text-yellow-800">{config.maintenance_message}</p>
              </div>
            )}
          </div>
        </div>

        <div className="bg-blue-50 border border-blue-200 p-4 rounded-lg">
          <p className="text-sm text-blue-800">
            <strong>Note:</strong> System configuration can only be modified through Django admin panel for security reasons.
            Access Django admin at /django-admin/
          </p>
        </div>
      </div>
    </div>
  );
}
