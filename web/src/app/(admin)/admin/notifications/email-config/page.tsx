'use client';

import { useEffect, useState } from 'react';
import { adminApi } from '@/lib/admin-api';
import { Mail, Shield, Save, Key, AlertCircle, CheckCircle, RefreshCw } from 'lucide-react';
import toast from 'react-hot-toast';

export default function EmailConfigurationManagement() {
    const [config, setConfig] = useState<any>(null);
    const [loading, setLoading] = useState(true);
    const [saving, setSaving] = useState(false);
    const [testing, setTesting] = useState(false);

    useEffect(() => {
        loadConfig();
    }, []);

    const loadConfig = async () => {
        try {
            setLoading(true);
            const data = await adminApi.getEmailConfigs({});
            if (data.results && data.results.length > 0) {
                setConfig(data.results[0]);
            }
        } catch (err: any) {
            toast.error('Failed to load email configuration');
        } finally {
            setLoading(false);
        }
    };

    const handleSave = async (e: React.FormEvent<HTMLFormElement>) => {
        e.preventDefault();
        if (!config) return;

        setSaving(true);
        const formData = new FormData(e.currentTarget);

        const payload: any = {
            name: formData.get('name'),
            backend: formData.get('backend'),
            host: formData.get('host'),
            port: parseInt(formData.get('port') as string),
            encryption: formData.get('encryption'),
            username: formData.get('username'),
            default_from_email: formData.get('default_from_email'),
            default_from_name: formData.get('default_from_name'),
            timeout: parseInt(formData.get('timeout') as string),
            fail_silently: formData.get('fail_silently') === 'true',
            is_active: formData.get('is_active') === 'true',
        };

        const password = formData.get('password');
        if (password) {
            payload.password = password;
        }

        try {
            const updated = await adminApi.updateEmailConfig(config.id, payload);
            setConfig(updated);
            toast.success('Email configuration saved successfully');
        } catch (err: any) {
            toast.error(err?.response?.data?.detail || 'Failed to update email configuration');
        } finally {
            setSaving(false);
        }
    };

    const handleTestConnection = async () => {
        if (!config) return;
        try {
            setTesting(true);
            const res = await adminApi.testEmailConnection(config.id);
            if (res.success) {
                toast.success(res.message || 'SMTP Connection successful!');
                loadConfig(); // Reload to get updated timestamp
            } else {
                toast.error(res.message || 'SMTP Connection failed.');
                loadConfig();
            }
        } catch (err: any) {
            toast.error('An error occurred while testing the connection.');
            loadConfig();
        } finally {
            setTesting(false);
        }
    };

    if (loading) {
        return (
            <div className="p-8 flex items-center justify-center h-[60vh]">
                <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-[#D9B168]"></div>
            </div>
        );
    }

    if (!config) {
        return (
            <div className="p-8">
                <div className="bg-red-50 text-red-800 p-4 rounded-lg flex items-start">
                    <AlertCircle className="w-5 h-5 mr-3 mt-0.5" />
                    <div>
                        <h3 className="font-bold">Configuration Not Found</h3>
                        <p className="text-sm mt-1">Failed to initialize email configuration on the server.</p>
                    </div>
                </div>
            </div>
        );
    }

    return (
        <div className="p-8 max-w-7xl mx-auto">
            <div className="mb-8 flex justify-between items-center">
                <div>
                    <h1 className="text-3xl font-bold text-[#122F26]">Email Configuration</h1>
                    <p className="text-[#3A5C50] mt-2">Manage platform SMTP settings and outgoing email delivery</p>
                </div>
                <div className="flex space-x-3">
                    <button
                        onClick={handleTestConnection}
                        disabled={testing}
                        className="flex items-center space-x-2 px-4 py-2 border border-[#D9B168] text-[#122F26] rounded-lg hover:bg-sand-50 transition-colors font-medium disabled:opacity-50"
                    >
                        {testing ? <RefreshCw className="w-5 h-5 animate-spin" /> : <Mail className="w-5 h-5" />}
                        <span>Test SMTP Connection</span>
                    </button>
                </div>
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
                <div className="lg:col-span-2">
                    <form onSubmit={handleSave} className="bg-white rounded-xl shadow-sm border border-primary-100 overflow-hidden">
                        <div className="p-6 border-b border-primary-100 bg-sand-50/50">
                            <h2 className="text-xl font-bold text-[#122F26] flex items-center">
                                <Shield className="w-5 h-5 mr-2 text-[#D9B168]" /> Primary Settings
                            </h2>
                        </div>

                        <div className="p-6 space-y-6">
                            <div className="grid grid-cols-2 gap-6">
                                <div>
                                    <label className="block text-sm font-medium text-[#122F26] mb-1">Configuration Name</label>
                                    <input
                                        required
                                        type="text"
                                        name="name"
                                        defaultValue={config.name}
                                        className="w-full px-4 py-2 bg-gray-50 border border-primary-200 rounded-lg focus:ring-2 focus:ring-[#D9B168]"
                                    />
                                </div>
                                <div>
                                    <label className="block text-sm font-medium text-[#122F26] mb-1">Status</label>
                                    <select
                                        name="is_active"
                                        defaultValue={String(config.is_active)}
                                        className="w-full px-4 py-2 bg-gray-50 border border-primary-200 rounded-lg focus:ring-2 focus:ring-[#D9B168]"
                                    >
                                        <option value="true">Active</option>
                                        <option value="false">Inactive</option>
                                    </select>
                                </div>
                            </div>

                            <hr className="border-primary-100" />

                            <h3 className="font-semibold text-[#122F26] text-lg">SMTP Server Details</h3>

                            <div className="grid grid-cols-2 gap-6">
                                <div className="col-span-2">
                                    <label className="block text-sm font-medium text-[#122F26] mb-1">Email Backend</label>
                                    <select
                                        name="backend"
                                        defaultValue={config.backend}
                                        className="w-full px-4 py-2 bg-gray-50 border border-primary-200 rounded-lg focus:ring-2 focus:ring-[#D9B168]"
                                    >
                                        <option value="django.core.mail.backends.smtp.EmailBackend">Standard SMTP (Default)</option>
                                        <option value="django.core.mail.backends.console.EmailBackend">Console (Development only)</option>
                                        <option value="django_ses.SESBackend">Amazon SES</option>
                                        <option value="anymail.backends.mailgun.EmailBackend">Mailgun</option>
                                        <option value="anymail.backends.sendgrid.EmailBackend">SendGrid</option>
                                    </select>
                                </div>
                                <div>
                                    <label className="block text-sm font-medium text-[#122F26] mb-1">SMTP Host</label>
                                    <input
                                        required
                                        type="text"
                                        name="host"
                                        defaultValue={config.host}
                                        className="w-full px-4 py-2 bg-gray-50 border border-primary-200 rounded-lg focus:ring-2 focus:ring-[#D9B168]"
                                    />
                                </div>
                                <div>
                                    <label className="block text-sm font-medium text-[#122F26] mb-1">Port</label>
                                    <input
                                        required
                                        type="number"
                                        name="port"
                                        defaultValue={config.port}
                                        className="w-full px-4 py-2 bg-gray-50 border border-primary-200 rounded-lg focus:ring-2 focus:ring-[#D9B168]"
                                    />
                                </div>
                                <div>
                                    <label className="block text-sm font-medium text-[#122F26] mb-1">Encryption Setup</label>
                                    <select
                                        name="encryption"
                                        defaultValue={config.encryption}
                                        className="w-full px-4 py-2 bg-gray-50 border border-primary-200 rounded-lg focus:ring-2 focus:ring-[#D9B168]"
                                    >
                                        <option value="none">None</option>
                                        <option value="tls">TLS (usually Port 587)</option>
                                        <option value="ssl">SSL (usually Port 465)</option>
                                    </select>
                                </div>
                            </div>

                            <hr className="border-primary-100" />

                            <h3 className="font-semibold text-[#122F26] text-lg">Authentication</h3>

                            <div className="grid grid-cols-2 gap-6">
                                <div>
                                    <label className="block text-sm font-medium text-[#122F26] mb-1">SMTP Username</label>
                                    <div className="relative">
                                        <input
                                            type="text"
                                            name="username"
                                            defaultValue={config.username}
                                            className="w-full pl-10 pr-4 py-2 bg-gray-50 border border-primary-200 rounded-lg focus:ring-2 focus:ring-[#D9B168]"
                                        />
                                        <Key className="absolute left-3 top-1/2 transform -translate-y-1/2 text-gray-400 w-4 h-4" />
                                    </div>
                                </div>
                                <div>
                                    <label className="block text-sm font-medium text-[#122F26] mb-1">SMTP Password</label>
                                    <input
                                        type="password"
                                        name="password"
                                        placeholder="••••••••••••••••"
                                        className="w-full px-4 py-2 bg-gray-50 border border-primary-200 rounded-lg focus:ring-2 focus:ring-[#D9B168]"
                                    />
                                    <p className="text-xs text-gray-500 mt-1">Leave blank to keep existing password.</p>
                                </div>
                            </div>

                            <hr className="border-primary-100" />

                            <h3 className="font-semibold text-[#122F26] text-lg">Sender Details</h3>

                            <div className="grid grid-cols-2 gap-6">
                                <div>
                                    <label className="block text-sm font-medium text-[#122F26] mb-1">Default From Name</label>
                                    <input
                                        required
                                        type="text"
                                        name="default_from_name"
                                        defaultValue={config.default_from_name}
                                        className="w-full px-4 py-2 bg-gray-50 border border-primary-200 rounded-lg focus:ring-2 focus:ring-[#D9B168]"
                                    />
                                </div>
                                <div>
                                    <label className="block text-sm font-medium text-[#122F26] mb-1">Default From Email</label>
                                    <input
                                        required
                                        type="email"
                                        name="default_from_email"
                                        defaultValue={config.default_from_email}
                                        className="w-full px-4 py-2 bg-gray-50 border border-primary-200 rounded-lg focus:ring-2 focus:ring-[#D9B168]"
                                    />
                                </div>
                            </div>

                            <hr className="border-primary-100" />

                            <h3 className="font-semibold text-[#122F26] text-lg">Advanced</h3>

                            <div className="grid grid-cols-2 gap-6">
                                <div>
                                    <label className="block text-sm font-medium text-[#122F26] mb-1">Timeout (seconds)</label>
                                    <input
                                        required
                                        type="number"
                                        name="timeout"
                                        defaultValue={config.timeout}
                                        className="w-full px-4 py-2 bg-gray-50 border border-primary-200 rounded-lg focus:ring-2 focus:ring-[#D9B168]"
                                    />
                                </div>
                                <div>
                                    <label className="block text-sm font-medium text-[#122F26] mb-1">Fail Silently</label>
                                    <select
                                        name="fail_silently"
                                        defaultValue={String(config.fail_silently)}
                                        className="w-full px-4 py-2 bg-gray-50 border border-primary-200 rounded-lg focus:ring-2 focus:ring-[#D9B168]"
                                    >
                                        <option value="false">No (Errors are raised)</option>
                                        <option value="true">Yes (Errors are ignored)</option>
                                    </select>
                                </div>
                            </div>

                        </div>

                        <div className="p-6 border-t border-primary-100 bg-gray-50 flex justify-end">
                            <button
                                type="submit"
                                disabled={saving}
                                className="flex items-center space-x-2 px-6 py-2 bg-[#D9B168] text-[#122F26] rounded-lg font-bold hover:bg-[#c9a158] transition-colors disabled:opacity-50"
                            >
                                {saving ? <RefreshCw className="w-5 h-5 animate-spin" /> : <Save className="w-5 h-5" />}
                                <span>Save Configuration</span>
                            </button>
                        </div>
                    </form>
                </div>

                <div className="space-y-6">
                    <div className="bg-white rounded-xl shadow-sm border border-primary-100 p-6">
                        <h3 className="font-bold text-[#122F26] mb-4">Diagnostics Log</h3>

                        <div className="space-y-4">
                            <div>
                                <p className="text-sm text-gray-500 mb-1">Last Tested</p>
                                <p className="font-medium text-[#122F26]">
                                    {config.last_tested_at ? new Date(config.last_tested_at).toLocaleString() : 'Never'}
                                </p>
                            </div>

                            <div className="pt-2">
                                <p className="text-sm text-gray-500 mb-1">SMTP Connection</p>
                                {config.last_tested_at ? (
                                    config.last_test_success ? (
                                        <div className="flex items-center text-green-600 font-medium">
                                            <CheckCircle className="w-4 h-4 mr-1.5" /> Successful
                                        </div>
                                    ) : (
                                        <div className="flex items-center text-red-600 font-medium">
                                            <AlertCircle className="w-4 h-4 mr-1.5" /> Failed
                                        </div>
                                    )
                                ) : (
                                    <div className="text-gray-400 italic">No diagnostic data</div>
                                )}
                            </div>

                            {config.last_test_error && (
                                <div className="pt-2">
                                    <p className="text-sm text-gray-500 mb-1">Error Traceback</p>
                                    <div className="bg-red-50 text-red-800 text-xs p-3 rounded font-mono break-words max-h-48 overflow-y-auto">
                                        {config.last_test_error}
                                    </div>
                                </div>
                            )}
                        </div>
                    </div>

                    <div className="bg-[#122F26] rounded-xl shadow p-6 text-white text-sm">
                        <h3 className="font-bold mb-3 flex items-center">
                            <Shield className="w-4 h-4 mr-2 text-[#D9B168]" /> Security Note
                        </h3>
                        <p className="text-gray-300 mb-2">
                            StayAfrica uses singleton architecture for email configuration. Changes made here apply globally to all outgoing platform emails (invoices, requests, etc).
                        </p>
                        <p className="text-gray-300">
                            For security, exact SMTP passwords are never sent back to the client interface. If you enter a new password, it will override the existing one.
                        </p>
                    </div>
                </div>
            </div>
        </div>
    );
}
