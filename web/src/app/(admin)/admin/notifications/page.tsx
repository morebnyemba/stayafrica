'use client';

import { useEffect, useState } from 'react';
import { adminApi } from '@/lib/admin-api';
import { Search, Bell } from 'lucide-react';
import toast from 'react-hot-toast';

export default function NotificationsManagement() {
    const [notifications, setNotifications] = useState<any[]>([]);
    const [loading, setLoading] = useState(true);
    const [search, setSearch] = useState('');
    const [page, setPage] = useState(1);
    const [totalCount, setTotalCount] = useState(0);
    const ITEMS_PER_PAGE = 20;

    useEffect(() => {
        loadNotifications();
    }, [page, search]);

    const loadNotifications = async () => {
        try {
            setLoading(true);
            const data = await adminApi.getNotifications({ page, search: search.trim() || undefined, per_page: ITEMS_PER_PAGE });
            setNotifications(data.results || []);
            setTotalCount(data.count || 0);
        } catch (err: any) {
            toast.error('Failed to load notifications');
        } finally {
            setLoading(false);
        }
    };

    const getStatusBadge = (status: string) => {
        switch (status) {
            case 'sent':
            case 'delivered':
            case 'read':
                return <span className="inline-flex items-center px-2 py-0.5 rounded text-xs font-medium bg-green-100 text-green-800 tracking-wide capitalize">{status}</span>;
            case 'failed':
                return <span className="inline-flex items-center px-2 py-0.5 rounded text-xs font-medium bg-red-100 text-red-800 tracking-wide capitalize">{status}</span>;
            default:
                return <span className="inline-flex items-center px-2 py-0.5 rounded text-xs font-medium bg-yellow-100 text-yellow-800 tracking-wide capitalize">{status}</span>;
        }
    };

    const getTypeBadge = (type: string) => {
        return (
            <span className="inline-flex items-center px-2 py-1 rounded-full text-xs font-medium bg-gray-100 text-gray-700 tracking-wide uppercase border border-gray-200">
                {type.replace('_', ' ')}
            </span>
        );
    };

    return (
        <div className="p-8">
            <div className="mb-8">
                <h1 className="text-3xl font-bold text-[#122F26]">Notification Log</h1>
                <p className="text-[#3A5C50] mt-2">Track global push and email notifications sent to users</p>
            </div>

            <div className="bg-white rounded-lg shadow p-6 mb-6">
                <div className="relative">
                    <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-[#3A5C50] w-5 h-5" />
                    <input
                        type="text"
                        value={search}
                        onChange={(e) => setSearch(e.target.value)}
                        onKeyPress={(e) => e.key === 'Enter' && loadNotifications()}
                        placeholder="Search by title or body..."
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
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">User</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Type</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Title & Body</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Status</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase">Timeline</th>
                            </tr>
                        </thead>
                        <tbody className="divide-y divide-primary-200 text-sm">
                            {notifications.map(notif => (
                                <tr key={notif.id} className="hover:bg-sand-50 transition-colors">
                                    <td className="px-6 py-4 whitespace-nowrap">
                                        <div className="font-medium text-[#122F26]">{notif.user_email || `User #${notif.user}`}</div>
                                    </td>
                                    <td className="px-6 py-4 whitespace-nowrap">
                                        {getTypeBadge(notif.notification_type)}
                                    </td>
                                    <td className="px-6 py-4 max-w-sm">
                                        <div className="font-medium text-[#122F26] truncate" title={notif.title}>{notif.title}</div>
                                        <div className="text-gray-500 mt-1 truncate" title={notif.body}>{notif.body}</div>
                                        {notif.error_message && (
                                            <div className="text-xs text-red-500 mt-1 italic truncate" title={notif.error_message}>
                                                Error: {notif.error_message}
                                            </div>
                                        )}
                                    </td>
                                    <td className="px-6 py-4 whitespace-nowrap">
                                        {getStatusBadge(notif.status)}
                                    </td>
                                    <td className="px-6 py-4 whitespace-nowrap text-xs text-[#3A5C50]">
                                        <div className="flex flex-col gap-1">
                                            <div><span className="font-medium">Created:</span> {new Date(notif.created_at).toLocaleString()}</div>
                                            {notif.sent_at && <div><span className="font-medium text-blue-600">Sent:</span> {new Date(notif.sent_at).toLocaleString()}</div>}
                                            {notif.read_at && <div><span className="font-medium text-green-600">Read:</span> {new Date(notif.read_at).toLocaleString()}</div>}
                                        </div>
                                    </td>
                                </tr>
                            ))}
                            {notifications.length === 0 && (
                                <tr>
                                    <td colSpan={5} className="px-6 py-8 text-center text-[#3A5C50] flex flex-col items-center">
                                        <Bell className="w-8 h-8 opacity-20 mb-2" />
                                        No notifications found in the global log.
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
                        Showing <span className="font-medium">{totalCount > 0 ? (page - 1) * ITEMS_PER_PAGE + 1 : 0}</span> to <span className="font-medium">{Math.min(page * ITEMS_PER_PAGE, totalCount)}</span> of <span className="font-medium">{totalCount}</span> notifications
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
        </div>
    );
}
