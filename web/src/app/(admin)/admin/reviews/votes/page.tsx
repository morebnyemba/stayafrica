'use client';

import { useState, useEffect } from 'react';
import { adminApi } from '@/lib/admin-api';
import toast from 'react-hot-toast';
import { ThumbsUp, ThumbsDown, Search, Trash2, Calendar } from 'lucide-react';
import ConfirmDialog from '@/components/admin/ConfirmDialog';

export default function ReviewVotesManagement() {
    const [votes, setVotes] = useState<any[]>([]);
    const [loading, setLoading] = useState(true);

    // Search & Pagination
    const [search, setSearch] = useState('');
    const [page, setPage] = useState(1);
    const [totalCount, setTotalCount] = useState(0);
    const ITEMS_PER_PAGE = 20;

    // Confirm dialog
    const [showConfirmDialog, setShowConfirmDialog] = useState(false);
    const [voteToDelete, setVoteToDelete] = useState<string | null>(null);

    useEffect(() => {
        const timer = setTimeout(() => {
            loadVotes();
        }, 500);
        return () => clearTimeout(timer);
    }, [search, page]);

    const loadVotes = async () => {
        try {
            setLoading(true);
            const data = await adminApi.getReviewVotes({ search: search.trim(), page, per_page: ITEMS_PER_PAGE });
            setVotes(data.results || []);
            setTotalCount(data.count || 0);
        } catch (err: any) {
            toast.error('Failed to load review votes');
            console.error(err);
        } finally {
            setLoading(false);
        }
    };

    const handleDeleteVote = (id: string) => {
        setVoteToDelete(id);
        setShowConfirmDialog(true);
    };

    const confirmDeleteVote = async () => {
        if (!voteToDelete) return;
        try {
            await adminApi.deleteReviewVote(voteToDelete);
            toast.success('Vote deleted mapping deleted');
            loadVotes();
        } catch (err: any) {
            toast.error('Failed to delete review vote');
            console.error(err);
        } finally {
            setShowConfirmDialog(false);
            setVoteToDelete(null);
        }
    };

    return (
        <div className="p-8">
            <div className="mb-8 flex justify-between items-center">
                <div>
                    <h1 className="text-3xl font-bold text-[#122F26]">Review Votes</h1>
                    <p className="text-[#3A5C50] mt-2">Track helpful/unhelpful votes on user reviews.</p>
                </div>
            </div>

            <div className="bg-white rounded-lg shadow p-6 mb-6">
                <div className="relative max-w-md">
                    <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-primary-300 w-5 h-5" />
                    <input
                        type="text"
                        value={search}
                        onChange={(e) => { setSearch(e.target.value); setPage(1); }}
                        placeholder="Search votes... (Not implemented by backend yet)"
                        disabled
                        className="w-full pl-10 pr-4 py-2 border border-[#3A5C50] rounded-lg focus:ring-2 focus:ring-[#D9B168] bg-sand-50"
                    />
                </div>
            </div>

            <div className="bg-white rounded-lg shadow overflow-hidden">
                {loading ? (
                    <div className="flex items-center justify-center p-12">
                        <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-[#D9B168]"></div>
                    </div>
                ) : (
                    <div className="overflow-x-auto">
                        <table className="min-w-full divide-y divide-primary-200">
                            <thead className="bg-[#F4F1EA]">
                                <tr>
                                    <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">ID</th>
                                    <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Review ID</th>
                                    <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">User ID</th>
                                    <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Vote Type</th>
                                    <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase">Created At</th>
                                    <th className="px-6 py-3 text-right text-xs font-medium text-[#122F26] uppercase">Actions</th>
                                </tr>
                            </thead>
                            <tbody className="divide-y divide-primary-200 bg-white">
                                {votes.map((vote) => (
                                    <tr key={vote.id} className="hover:bg-sand-50 transition">
                                        <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50]">
                                            #{vote.id}
                                        </td>
                                        <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-[#122F26]">
                                            Review #{vote.review}
                                        </td>
                                        <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50]">
                                            User #{vote.user}
                                        </td>
                                        <td className="px-6 py-4 whitespace-nowrap">
                                            {vote.vote_type === 'helpful' ? (
                                                <span className="inline-flex items-center gap-1 px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
                                                    <ThumbsUp className="w-3 h-3" /> Helpful
                                                </span>
                                            ) : (
                                                <span className="inline-flex items-center gap-1 px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
                                                    <ThumbsDown className="w-3 h-3" /> Unhelpful
                                                </span>
                                            )}
                                        </td>
                                        <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50]">
                                            <div className="flex items-center gap-1.5">
                                                <Calendar className="w-4 h-4" />
                                                {new Date(vote.created_at).toLocaleString()}
                                            </div>
                                        </td>
                                        <td className="px-6 py-4 whitespace-nowrap text-right text-sm">
                                            <button
                                                onClick={() => handleDeleteVote(vote.id)}
                                                className="text-red-500 hover:text-red-700"
                                                title="Delete vote"
                                            >
                                                <Trash2 className="w-5 h-5" />
                                            </button>
                                        </td>
                                    </tr>
                                ))}
                                {votes.length === 0 && (
                                    <tr>
                                        <td colSpan={6} className="px-6 py-8 text-center text-[#3A5C50]">
                                            No review votes found.
                                        </td>
                                    </tr>
                                )}
                            </tbody>
                        </table>
                    </div>
                )}

                {/* Pagination */}
                {!loading && totalCount > 0 && (
                    <div className="bg-sand-50 px-6 py-4 border-t flex items-center justify-between">
                        <div className="text-sm text-[#122F26]">
                            Showing {(page - 1) * ITEMS_PER_PAGE + 1} to {Math.min(page * ITEMS_PER_PAGE, totalCount)} of {totalCount} entries
                        </div>
                        <div className="flex space-x-2">
                            <button
                                onClick={() => setPage(p => Math.max(1, p - 1))}
                                disabled={page === 1}
                                className="px-4 py-2 border border-[#3A5C50] rounded-lg text-sm font-medium text-[#122F26] hover:bg-white disabled:opacity-50 disabled:cursor-not-allowed bg-transparent transition"
                            >
                                Previous
                            </button>
                            <button
                                onClick={() => setPage(p => p + 1)}
                                disabled={page * ITEMS_PER_PAGE >= totalCount}
                                className="px-4 py-2 border border-[#3A5C50] rounded-lg text-sm font-medium text-[#122F26] hover:bg-white disabled:opacity-50 disabled:cursor-not-allowed bg-transparent transition"
                            >
                                Next
                            </button>
                        </div>
                    </div>
                )}
            </div>

            <ConfirmDialog
                isOpen={showConfirmDialog}
                onClose={() => {
                    setShowConfirmDialog(false);
                    setVoteToDelete(null);
                }}
                onConfirm={confirmDeleteVote}
                title="Delete Review Vote"
                message="Are you sure you want to delete this vote mapping? This will remove the tracking record but may not recalculate the review's helpful\_count if handled separately by signals."
                variant="danger"
                confirmText="Delete Vote"
            />
        </div>
    );
}
