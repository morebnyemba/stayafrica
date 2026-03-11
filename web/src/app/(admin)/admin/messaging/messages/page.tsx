'use client';

import { useEffect, useState } from 'react';
import { adminApi } from '@/lib/admin-api';
import { Search, ShieldAlert, CheckCircle, MessageCircle } from 'lucide-react';
import toast from 'react-hot-toast';
import ConfirmDialog from '@/components/admin/ConfirmDialog';

export default function MessagesManagement() {
    const [messages, setMessages] = useState<any[]>([]);
    const [loading, setLoading] = useState(true);
    const [search, setSearch] = useState('');
    const [page, setPage] = useState(1);
    const [totalCount, setTotalCount] = useState(0);
    const ITEMS_PER_PAGE = 20;

    const [showConfirm, setShowConfirm] = useState(false);
    const [confirmId, setConfirmId] = useState<string | null>(null);

    useEffect(() => {
        loadMessages();
    }, [page, search]);

    const loadMessages = async () => {
        try {
            setLoading(true);
            const data = await adminApi.getMessages({ page, search: search.trim() || undefined, per_page: ITEMS_PER_PAGE });
            setMessages(data.results || []);
            setTotalCount(data.count || 0);
        } catch (err: any) {
            toast.error('Failed to load messages');
        } finally {
            setLoading(false);
        }
    };

    const handleModerate = async () => {
        if (!confirmId) return;
        try {
            await adminApi.moderateMessage(confirmId);
            toast.success('Message moderated successfully');
            loadMessages();
            setShowConfirm(false);
        } catch (err: any) {
            toast.error(err?.response?.data?.detail || 'Failed to moderate message');
        }
    };

    return (
        <div className="p-8">
            <div className="mb-8 flex justify-between items-center">
                <div>
                    <h1 className="text-3xl font-bold text-[#122F26]">All Messages</h1>
                    <p className="text-[#3A5C50] mt-2">Track and moderate platform communications</p>
                </div>
            </div>

            <div className="bg-white rounded-lg shadow p-6 mb-6">
                <div className="relative">
                    <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-[#3A5C50] w-5 h-5" />
                    <input
                        type="text"
                        value={search}
                        onChange={(e) => setSearch(e.target.value)}
                        onKeyPress={(e) => e.key === 'Enter' && loadMessages()}
                        placeholder="Search message text..."
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
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Sender → Receiver</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Type</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Text Content</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Sent At</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Actions</th>
                            </tr>
                        </thead>
                        <tbody className="divide-y divide-primary-200 text-sm">
                            {messages.map(msg => {
                                const isModerated = msg.text?.includes('hidden by moderator');
                                return (
                                    <tr key={msg.id} className="hover:bg-sand-50 transition-colors">
                                        <td className="px-6 py-4 whitespace-nowrap">
                                            <div className="font-medium text-[#122F26]">{msg.sender_name}</div>
                                            <div className="text-gray-500 text-xs mt-0.5">to {msg.receiver_name}</div>
                                        </td>
                                        <td className="px-6 py-4 whitespace-nowrap">
                                            <span className="inline-flex items-center px-2 py-0.5 rounded text-xs font-medium bg-gray-100 text-gray-800 uppercase tracking-wide">
                                                {msg.message_type.replace('_', ' ')}
                                            </span>
                                        </td>
                                        <td className="px-6 py-4">
                                            {isModerated ? (
                                                <div className="flex items-center text-red-500 italic max-w-sm truncate" title={msg.text}>
                                                    <ShieldAlert className="w-4 h-4 mr-1" />
                                                    {msg.text}
                                                </div>
                                            ) : (
                                                <div className="text-[#3A5C50] max-w-sm truncate" title={msg.text}>
                                                    {msg.text}
                                                </div>
                                            )}
                                        </td>
                                        <td className="px-6 py-4 whitespace-nowrap text-[#3A5C50]">
                                            {new Date(msg.created_at).toLocaleString()}
                                        </td>
                                        <td className="px-6 py-4 whitespace-nowrap font-medium">
                                            {!isModerated && msg.message_type === 'text' ? (
                                                <button
                                                    onClick={() => {
                                                        setConfirmId(msg.id);
                                                        setShowConfirm(true);
                                                    }}
                                                    className="text-red-600 hover:text-red-900 flex items-center gap-1 transition-colors"
                                                >
                                                    <ShieldAlert className="w-4 h-4" /> Moderate
                                                </button>
                                            ) : (
                                                <span className="text-gray-400 text-xs flex items-center gap-1">
                                                    <CheckCircle className="w-4 h-4" /> Moderated / System
                                                </span>
                                            )}
                                        </td>
                                    </tr>
                                );
                            })}
                            {messages.length === 0 && (
                                <tr>
                                    <td colSpan={5} className="px-6 py-8 text-center text-[#3A5C50] flex flex-col items-center">
                                        <MessageCircle className="w-8 h-8 opacity-20 mb-2" />
                                        No messages found matching your search.
                                    </td>
                                </tr>
                            )}
                        </tbody>
                    </table>
                )}
            </div>

            {/* Pagination */}
            {totalCount > ITEMS_PER_PAGE && (
                <div className="bg-white rounded-xl shadow-sm border border-gray-200 mt-4 px-6 py-4 flex items-center justify-between">
                    <div className="text-sm text-gray-700">
                        Showing <span className="font-medium">{totalCount > 0 ? (page - 1) * ITEMS_PER_PAGE + 1 : 0}</span> to <span className="font-medium">{Math.min(page * ITEMS_PER_PAGE, totalCount)}</span> of <span className="font-medium">{totalCount}</span> messages
                    </div>
                    <div className="flex space-x-2">
                        <button
                            onClick={() => setPage(p => Math.max(1, p - 1))}
                            disabled={page === 1}
                            className="px-4 py-2 border border-gray-300 rounded-lg text-sm font-medium text-gray-700 bg-white hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed"
                        >
                            Previous
                        </button>
                        <button
                            onClick={() => setPage(p => p + 1)}
                            disabled={page * ITEMS_PER_PAGE >= totalCount}
                            className="px-4 py-2 border border-gray-300 rounded-lg text-sm font-medium text-gray-700 bg-white hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed"
                        >
                            Next
                        </button>
                    </div>
                </div>
            )}

            <ConfirmDialog
                isOpen={showConfirm}
                title="Moderate Message"
                message="Are you sure you want to moderate this message? The original text will be permanently hidden and replaced with a moderation notice. This action is irreversible."
                onConfirm={handleModerate}
                onClose={() => setShowConfirm(false)}
                variant="danger"
                confirmText="Hide Text"
            />
        </div>
    );
}
