'use client';

import { useEffect, useState } from 'react';
import { adminApi } from '@/lib/admin-api';
import { Search, Eye } from 'lucide-react';
import toast from 'react-hot-toast';

export default function ConversationsManagement() {
    const [conversations, setConversations] = useState<any[]>([]);
    const [loading, setLoading] = useState(true);
    const [search, setSearch] = useState('');
    const [page, setPage] = useState(1);
    const [totalCount, setTotalCount] = useState(0);
    const ITEMS_PER_PAGE = 20;

    const [selectedConversation, setSelectedConversation] = useState<any | null>(null);
    const [messages, setMessages] = useState<any[]>([]);
    const [loadingMessages, setLoadingMessages] = useState(false);

    useEffect(() => {
        loadConversations();
    }, [page, search]);

    const loadConversations = async () => {
        try {
            setLoading(true);
            const data = await adminApi.getConversations({ page, search: search.trim() || undefined, per_page: ITEMS_PER_PAGE });
            setConversations(data.results || []);
            setTotalCount(data.count || 0);
        } catch (err: any) {
            toast.error('Failed to load conversations');
        } finally {
            setLoading(false);
        }
    };

    const loadMessages = async (convId: string) => {
        try {
            setLoadingMessages(true);
            const res = await adminApi.getMessages({ conversation: convId, per_page: 100 });
            setMessages(res.results.reverse() || []); // reverse to show oldest first or newest at bottom
        } catch (err: any) {
            toast.error('Failed to load messages for conversation');
        } finally {
            setLoadingMessages(false);
        }
    };

    const handleView = (conv: any) => {
        setSelectedConversation(conv);
        loadMessages(conv.id);
    };

    return (
        <div className="p-8">
            <div className="mb-8">
                <h1 className="text-3xl font-bold text-[#122F26]">Conversations</h1>
                <p className="text-[#3A5C50] mt-2">Oversee host-guest communication</p>
            </div>

            <div className="bg-white rounded-lg shadow p-6 mb-6">
                <div className="relative">
                    <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-[#3A5C50] w-5 h-5" />
                    <input
                        type="text"
                        value={search}
                        onChange={(e) => setSearch(e.target.value)}
                        onKeyPress={(e) => e.key === 'Enter' && loadConversations()}
                        placeholder="Search by subject or booking ID..."
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
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Subject / Property</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Participants</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Last Message</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Last Updated</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Actions</th>
                            </tr>
                        </thead>
                        <tbody className="divide-y divide-primary-200">
                            {conversations.map(conv => (
                                <tr key={conv.id} className="hover:bg-sand-50">
                                    <td className="px-6 py-4">
                                        <div className="text-sm font-medium text-[#122F26]">{conv.subject || 'No Subject'}</div>
                                        <div className="text-xs text-[#3A5C50] mt-1">{conv.property_title || 'No Property'}</div>
                                    </td>
                                    <td className="px-6 py-4 text-sm text-[#3A5C50]">
                                        {conv.participants?.map((p: any) => p.name).join(', ')}
                                    </td>
                                    <td className="px-6 py-4">
                                        <div className="text-sm text-[#3A5C50] truncate max-w-xs">
                                            {conv.last_message ? conv.last_message.text : 'No messages'}
                                        </div>
                                    </td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50]">
                                        {new Date(conv.updated_at).toLocaleDateString()}
                                    </td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm font-medium">
                                        <button
                                            onClick={() => handleView(conv)}
                                            className="text-blue-600 hover:text-blue-900 flex items-center gap-1"
                                        >
                                            <Eye className="w-4 h-4" /> View
                                        </button>
                                    </td>
                                </tr>
                            ))}
                            {conversations.length === 0 && (
                                <tr>
                                    <td colSpan={5} className="px-6 py-8 text-center text-[#3A5C50]">
                                        No conversations found.
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
                        Showing <span className="font-medium">{totalCount > 0 ? (page - 1) * ITEMS_PER_PAGE + 1 : 0}</span> to <span className="font-medium">{Math.min(page * ITEMS_PER_PAGE, totalCount)}</span> of <span className="font-medium">{totalCount}</span> conversations
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

            {selectedConversation && (
                <div className="fixed inset-0 z-50 flex items-center justify-center bg-black bg-opacity-50">
                    <div className="bg-gray-50 rounded-lg shadow-xl w-full max-w-3xl flex flex-col h-[80vh]">
                        <div className="p-4 border-b bg-white rounded-t-lg flex justify-between items-center">
                            <div>
                                <h3 className="text-lg font-bold text-[#122F26]">
                                    {selectedConversation.subject || 'Conversation'}
                                </h3>
                                <p className="text-sm text-[#3A5C50]">
                                    Participants: {selectedConversation.participants?.map((p: any) => p.name).join(', ')}
                                </p>
                            </div>
                            <button
                                onClick={() => setSelectedConversation(null)}
                                className="p-2 text-gray-500 hover:text-gray-700"
                            >
                                Close
                            </button>
                        </div>

                        <div className="flex-1 overflow-y-auto p-4 space-y-4">
                            {loadingMessages ? (
                                <div className="flex justify-center p-8">
                                    <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-[#D9B168]"></div>
                                </div>
                            ) : messages.length === 0 ? (
                                <div className="text-center text-gray-500 py-8">No messages in this conversation.</div>
                            ) : (
                                messages.map(msg => (
                                    <div
                                        key={msg.id}
                                        className="bg-white p-4 rounded-lg shadow-sm border border-gray-100"
                                    >
                                        <div className="flex justify-between items-start mb-2">
                                            <span className="font-medium text-[#122F26] text-sm">
                                                {msg.sender_name} → {msg.receiver_name}
                                            </span>
                                            <span className="text-xs text-gray-500">
                                                {new Date(msg.created_at).toLocaleString()}
                                            </span>
                                        </div>
                                        <p className={`text-sm text-gray-800 whitespace-pre-wrap ${msg.text.includes('hidden by moderator') ? 'italic text-red-500' : ''}`}>
                                            {msg.text}
                                        </p>
                                    </div>
                                ))
                            )}
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
