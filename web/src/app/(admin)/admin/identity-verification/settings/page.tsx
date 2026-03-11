'use client';

import { useState, useEffect } from 'react';
import { adminApi } from '@/lib/admin-api';
import { VerificationSettings } from '@/types/admin-types';
import toast from 'react-hot-toast';
import { Save, ShieldAlert, Settings as SettingsIcon } from 'lucide-react';
import Link from 'next/link';

export default function IdentityVerificationSettings() {
    const [settings, setSettings] = useState<VerificationSettings | null>(null);
    const [loading, setLoading] = useState(true);
    const [saving, setSaving] = useState(false);

    useEffect(() => {
        loadSettings();
    }, []);

    const loadSettings = async () => {
        try {
            setLoading(true);
            const data = await adminApi.getVerificationSettings();
            setSettings(data);
        } catch (err: any) {
            toast.error('Failed to load verification settings');
            console.error(err);
        } finally {
            setLoading(false);
        }
    };

    const handleSave = async () => {
        if (!settings) return;
        try {
            setSaving(true);
            const updated = await adminApi.updateVerificationSettings(settings);
            setSettings(updated);
            toast.success('Settings updated successfully');
        } catch (err: any) {
            toast.error(err?.response?.data?.detail || 'Failed to update settings');
        } finally {
            setSaving(false);
        }
    };

    if (loading) {
        return (
            <div className="flex justify-center items-center h-full min-h-[50vh]">
                <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-[#D9B168]"></div>
            </div>
        );
    }

    if (!settings) {
        return <div className="p-8 text-center text-red-600">Failed to load settings.</div>;
    }

    return (
        <div className="p-8 max-w-5xl mx-auto">
            <div className="mb-8 flex justify-between items-center">
                <div>
                    <h1 className="text-3xl font-bold text-[#122F26] flex items-center gap-2">
                        <SettingsIcon className="w-8 h-8" />
                        Verification Settings
                    </h1>
                    <p className="text-[#3A5C50] mt-2">Manage the rules and limits for KYC Identity Verification.</p>
                </div>
                <Link href="/admin/identity-verification">
                    <button className="px-4 py-2 border border-[#3A5C50] text-[#122F26] rounded-lg hover:bg-sand-50 transition-colors">
                        Back to Dashboard
                    </button>
                </Link>
            </div>

            <div className="bg-white rounded-lg shadow-sm border border-sand-200 overflow-hidden">
                {/* Form Grid */}
                <div className="p-6 md:p-8 space-y-8">

                    {/* Section: Platform Requirements */}
                    <div>
                        <h3 className="text-lg font-bold text-[#122F26] mb-4 border-b pb-2 flex items-center gap-2">
                            <ShieldAlert className="w-5 h-5" /> Platform Requirements
                        </h3>
                        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                            <label className="flex items-center gap-3 cursor-pointer">
                                <input
                                    type="checkbox"
                                    checked={settings.require_verification_for_hosting}
                                    onChange={(e) => setSettings({ ...settings, require_verification_for_hosting: e.target.checked })}
                                    className="w-5 h-5 text-[#D9B168] rounded focus:ring-[#D9B168]"
                                />
                                <span className="text-sm font-medium text-[#122F26]">Require Verification for Hosting</span>
                            </label>

                            <label className="flex items-center gap-3 cursor-pointer">
                                <input
                                    type="checkbox"
                                    checked={settings.require_verification_for_booking}
                                    onChange={(e) => setSettings({ ...settings, require_verification_for_booking: e.target.checked })}
                                    className="w-5 h-5 text-[#D9B168] rounded focus:ring-[#D9B168]"
                                />
                                <span className="text-sm font-medium text-[#122F26]">Require Verification for Booking</span>
                            </label>

                            <label className="flex items-center gap-3 cursor-pointer">
                                <input
                                    type="checkbox"
                                    checked={settings.use_third_party_service}
                                    onChange={(e) => setSettings({ ...settings, use_third_party_service: e.target.checked })}
                                    className="w-5 h-5 text-[#D9B168] rounded focus:ring-[#D9B168]"
                                />
                                <span className="text-sm font-medium text-[#122F26]">Use Third Party Service (Automated KYC)</span>
                            </label>
                        </div>
                    </div>

                    {/* Section: Submission Requirements */}
                    <div>
                        <h3 className="text-lg font-bold text-[#122F26] mb-4 border-b pb-2">Submission Document Requirements</h3>
                        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                            <label className="flex items-center gap-3 cursor-pointer">
                                <input
                                    type="checkbox"
                                    checked={settings.require_document_back}
                                    onChange={(e) => setSettings({ ...settings, require_document_back: e.target.checked })}
                                    className="w-5 h-5 text-[#D9B168] rounded focus:ring-[#D9B168]"
                                />
                                <span className="text-sm font-medium text-[#122F26]">Require Document Back Side</span>
                            </label>

                            <label className="flex items-center gap-3 cursor-pointer">
                                <input
                                    type="checkbox"
                                    checked={settings.require_selfie}
                                    onChange={(e) => setSettings({ ...settings, require_selfie: e.target.checked })}
                                    className="w-5 h-5 text-[#D9B168] rounded focus:ring-[#D9B168]"
                                />
                                <span className="text-sm font-medium text-[#122F26]">Require Live Selfie</span>
                            </label>

                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-2">Verification Validity (Years)</label>
                                <input
                                    type="number"
                                    min="1"
                                    max="10"
                                    value={settings.verification_valid_years}
                                    onChange={(e) => setSettings({ ...settings, verification_valid_years: parseInt(e.target.value) || 1 })}
                                    className="w-full p-2 border border-sand-300 rounded focus:ring-[#D9B168]"
                                />
                                <p className="text-xs text-gray-500 mt-1">Number of years before users must re-verify</p>
                            </div>
                        </div>
                    </div>

                    {/* Section: Attempt Limits */}
                    <div>
                        <h3 className="text-lg font-bold text-[#122F26] mb-4 border-b pb-2">Rate Limiting</h3>
                        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-2">Max Attempts Per Day</label>
                                <input
                                    type="number"
                                    min="1"
                                    value={settings.max_attempts_per_day}
                                    onChange={(e) => setSettings({ ...settings, max_attempts_per_day: parseInt(e.target.value) || 1 })}
                                    className="w-full p-2 border border-sand-300 rounded focus:ring-[#D9B168]"
                                />
                            </div>

                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-2">Max Attempts Per Month</label>
                                <input
                                    type="number"
                                    min="1"
                                    value={settings.max_attempts_per_month}
                                    onChange={(e) => setSettings({ ...settings, max_attempts_per_month: parseInt(e.target.value) || 1 })}
                                    className="w-full p-2 border border-sand-300 rounded focus:ring-[#D9B168]"
                                />
                            </div>
                        </div>
                    </div>

                    {/* Section: Image Quality Requirements */}
                    <div>
                        <h3 className="text-lg font-bold text-[#122F26] mb-4 border-b pb-2">Image Constraints</h3>
                        <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-2">Min Width (px)</label>
                                <input
                                    type="number"
                                    value={settings.min_image_width}
                                    onChange={(e) => setSettings({ ...settings, min_image_width: parseInt(e.target.value) || 0 })}
                                    className="w-full p-2 border border-sand-300 rounded focus:ring-[#D9B168]"
                                />
                            </div>
                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-2">Min Height (px)</label>
                                <input
                                    type="number"
                                    value={settings.min_image_height}
                                    onChange={(e) => setSettings({ ...settings, min_image_height: parseInt(e.target.value) || 0 })}
                                    className="w-full p-2 border border-sand-300 rounded focus:ring-[#D9B168]"
                                />
                            </div>
                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-2">Max Size (MB)</label>
                                <input
                                    type="number"
                                    step="0.1"
                                    value={settings.max_image_size_mb}
                                    onChange={(e) => setSettings({ ...settings, max_image_size_mb: parseFloat(e.target.value) || 0 })}
                                    className="w-full p-2 border border-sand-300 rounded focus:ring-[#D9B168]"
                                />
                            </div>
                        </div>
                    </div>

                </div>

                {/* Footer actions */}
                <div className="bg-[#F4F1EA] px-8 py-5 border-t border-sand-200 flex justify-end">
                    <button
                        onClick={handleSave}
                        disabled={saving}
                        className="flex items-center gap-2 bg-[#D9B168] hover:bg-[#c9a158] text-white px-6 py-2 rounded-lg font-medium transition disabled:opacity-50"
                    >
                        <Save className="w-5 h-5" />
                        {saving ? 'Saving...' : 'Save Settings'}
                    </button>
                </div>
            </div>
        </div>
    );
}
