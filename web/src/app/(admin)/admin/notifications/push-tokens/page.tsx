'use client';

import { useEffect, useState } from 'react';
import { adminApi } from '@/lib/admin-api';
import { Search, Smartphone, Apple, Monitor } from 'lucide-react';
import toast from 'react-hot-toast';

export default function PushTokensManagement() {
    const [tokens, setTokens] = useState<any[]>([]);
    const [loading, setLoading] = useState(true);
    const [search, setSearch] = useState('');
    const [page, setPage] = useState(1);
    const [totalCount, setTotalCount] = useState(0);
    const ITEMS_PER_PAGE = 20;

    useEffect(() => {
        loadTokens();
    }, [page, search]);

    const loadTokens = async () => {
        try {
            setLoading(true);
            const data = await adminApi.getPushTokens({ page, search: search.trim() || undefined, per_page: ITEMS_PER_PAGE });
            setTokens(data.results || []);
            setTotalCount(data.count || 0);
        } catch (err: any) {
            toast.error('Failed to load push tokens');
        } finally {
            setLoading(false);
        }
    };

    const getPlatformIcon = (platform: string) => {
        switch (platform) {
            case 'ios': return <Apple className="w-4 h-4 text-gray-700" />;
            case 'android': return <Smartphone className="w-4 h-4 text-green-600" />;
            case 'web': return <Monitor className="w-4 h-4 text-blue-600" />;
            default: return null;
        }
    };

    return (
        <div className="p-8">
            <div className="mb-8 flex justify-between items-center">
                <div>
                    <h1 className="text-3xl font-bold text-[#122F26]">Push Tokens</h1>
                    <p className="text-[#3A5C50] mt-2">Track and oversee registered devices for mobile alerts</p>
                </div>
            </div>

            <div className="bg-white rounded-lg shadow p-6 mb-6">
                <div className="relative">
                    <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-[#3A5C50] w-5 h-5" />
                    <input
                        type="text"
                        value={search}
                        onChange={(e) => setSearch(e.target.value)}
                        onKeyPress={(e) => e.key === 'Enter' && loadTokens()}
                        placeholder="Search by token or device ID..."
                        className="w-full pl-10 pr-4 py-2 border border-[#3A5C50] rounded-lg focus:ring-2 focus:ring-[#D9B168]"
                    />
                </div>
            </div>

            <div className="bg-white rounded-lg shadow overflow-hidden">
                {loading ? (
                    <div className="flex items-center justify-center h-64">
                        <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-[#D9B168]"></div>
                    </div>
                ) : (
                    <table className="min-w-full divide-y divide-primary-200">
                        <thead className="bg-sand-50">
                            <tr>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">User Profile</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Platform</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Token & Device</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Status</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Last Used</th>
                            </tr>
                        </thead>
                        <tbody className="divide-y divide-primary-200 text-sm">
                            {tokens.map(token => (
                                <tr key={token.id} className="hover:bg-sand-50 transition-colors">
                                    <td className="px-6 py-4 whitespace-nowrap">
                                        <div className="font-medium text-[#122F26]">{token.user_email || `User #${token.user}`}</div>
                                    </td>
                                    <td className="px-6 py-4 whitespace-nowrap">
                                        <div className="flex items-center gap-2">
                                            {getPlatformIcon(token.platform)}
                                            <span className="capitalize text-[#3A5C50] font-medium">{token.platform}</span>
                                        </div>
                                    </td>
                                    <td className="px-6 py-4">
                                        <div className="text-[#122F26] font-mono text-xs truncate max-w-[200px]" title={token.token}>
                                            {token.token.substring(0, 20)}...
                                        </div>
                                        {token.device_id && (
                                            <div className="text-[#3A5C50] text-xs mt-1">Device: {token.device_id}</div>
                                        )}
                                    </td>
                                    <td className="px-6 py-4 whitespace-nowrap">
                                        <span className={`inline-flex items-center px-2 py-0.5 rounded text-xs font-medium ${token.is_active ? 'bg-green-100 text-green-800' : 'bg-gray-100 text-gray-800'
                                            }`}>
                                            {token.is_active ? 'Active' : 'Inactive'}
                                        </span>
                                    </td>
                                    <td className="px-6 py-4 whitespace-nowrap text-[#3A5C50]">
                                        {new Date(token.last_used_at).toLocaleString()}
                                    </td>
                                </tr>
                            ))}
                            {tokens.length === 0 && (
                                <tr>
                                    <td colSpan={5} className="px-6 py-8 text-center text-[#3A5C50] flex flex-col items-center">
                                        <Smartphone className="w-8 h-8 opacity-20 mb-2" />
                                        No registered push tokens found.
                                    </td>
                                </tr>
                            )}
                        </tbody>
                    </table>
                )}
            </div>
        </div>
    );
}
