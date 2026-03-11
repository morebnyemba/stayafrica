'use client';

import { useEffect, useState } from 'react';
import { adminApi } from '@/lib/admin-api';
import { IdentityVerification, VerificationStats } from '@/types/admin-types';
import { CheckCircle, XCircle, Clock, AlertCircle, Eye, Search } from 'lucide-react';
import toast from 'react-hot-toast';
import VerificationModal from '@/components/admin/VerificationModal';
import Link from 'next/link';

export default function VerificationAttempts() {

    const [verifications, setVerifications] = useState<IdentityVerification[]>([]);
    const [loading, setLoading] = useState(true);
    const [page, setPage] = useState(1);
    const [totalCount, setTotalCount] = useState(0);
    const [stats, setStats] = useState<VerificationStats | null>(null);
    const [selectedVerification, setSelectedVerification] = useState<IdentityVerification | null>(null);
    const [showModal, setShowModal] = useState(false);
    const [search, setSearch] = useState('');
    const ITEMS_PER_PAGE = 15;

    useEffect(() => {
        loadVerifications();
        loadStats();
    }, [page, search]);

    const loadVerifications = async () => {
        try {
            setLoading(true);
            const data = await adminApi.getAllVerifications({
                page,
                per_page: ITEMS_PER_PAGE,
                search: search.trim() || undefined,
            });
            setVerifications(data.results || []);
            setTotalCount(data.count || 0);
        } catch (err: any) {
            const errorMsg = err?.response?.data?.detail || 'Failed to load historical verifications';
            toast.error(errorMsg);
            console.error('Verifications loading error:', err);
        } finally {
            setLoading(false);
        }
    };

    const loadStats = async () => {
        try {
            const statsData = await adminApi.getVerificationStats();
            setStats(statsData);
        } catch (err: any) {
            console.error('Stats load error:', err);
        }
    };

    const handleSearch = () => {
        setPage(1);
    };

    const handleViewDetails = async (id: string | number) => {
        try {
            const verification = await adminApi.getVerificationById(id);
            setSelectedVerification(verification);
            setShowModal(true);
        } catch (err: any) {
            const errorMsg = err?.response?.data?.detail || 'Failed to load verification details';
            toast.error(errorMsg);
        }
    };

    const dummyApprove = async (notes: string) => {
        // Usually these are historical and maybe already approved. 
        // In case they are still pending, we allow approval here.
        if (!selectedVerification) return;
        try {
            setLoading(true);
            await adminApi.approveVerification(selectedVerification.id, notes);
            toast.success('Verification approved');
            setShowModal(false);
            loadVerifications();
            loadStats();
        } catch (err: any) {
            toast.error(err?.response?.data?.detail || 'Approval failed');
        } finally {
            setLoading(false);
        }
    }

    const dummyReject = async (reason: string, notes: string) => {
        if (!selectedVerification) return;
        try {
            setLoading(true);
            await adminApi.rejectVerification(selectedVerification.id, reason, notes);
            toast.success('Verification rejected');
            setShowModal(false);
            loadVerifications();
            loadStats();
        } catch (err: any) {
            toast.error(err?.response?.data?.detail || 'Rejection failed');
        } finally {
            setLoading(false);
        }
    }

    const getStatusColor = (status: string) => {
        switch (status) {
            case 'approved': return 'text-green-600 bg-green-50';
            case 'rejected': return 'text-red-600 bg-red-50';
            case 'pending': return 'text-yellow-600 bg-yellow-50';
            case 'under_review': return 'text-secondary-600 dark:text-secondary-400 bg-blue-50';
            case 'expired': return 'text-primary-500 dark:text-sand-400 bg-sand-50 dark:bg-primary-900';
            default: return 'text-primary-500 dark:text-sand-400 bg-sand-50 dark:bg-primary-900';
        }
    };

    const getStatusIcon = (status: string) => {
        switch (status) {
            case 'approved': return <CheckCircle className="w-4 h-4" />;
            case 'rejected': return <XCircle className="w-4 h-4" />;
            case 'pending': return <Clock className="w-4 h-4" />;
            case 'under_review': return <AlertCircle className="w-4 h-4" />;
            case 'expired': return <AlertCircle className="w-4 h-4" />;
            default: return <Clock className="w-4 h-4" />;
        }
    };

    const formatDate = (dateString: string | undefined) => {
        if (!dateString) return '-';
        return new Date(dateString).toLocaleDateString('en-US', {
            year: 'numeric', month: 'short', day: 'numeric',
        });
    };

    return (
        <div className="p-8">
            <div className="mb-8 flex justify-between items-center">
                <div>
                    <h1 className="text-3xl font-bold text-[#122F26]">Verification Attempts History</h1>
                    <p className="text-[#3A5C50] mt-2">View all historical identity verifications across the platform</p>
                </div>
                <Link href="/admin/identity-verification">
                    <button className="px-4 py-2 border border-[#3A5C50] text-[#122F26] rounded-lg hover:bg-sand-50 transition-colors">
                        Back to Pending Reviews
                    </button>
                </Link>
            </div>

            <div className="bg-white rounded-lg shadow p-6 mb-6">
                <div className="w-full relative">
                    <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-primary-300 dark:text-primary-500 w-5 h-5" />
                    <input
                        type="text"
                        value={search}
                        onChange={(e) => setSearch(e.target.value)}
                        onKeyPress={(e) => e.key === 'Enter' && handleSearch()}
                        placeholder="Search by user name or email..."
                        className="w-full pl-10 pr-4 py-2 border border-[#3A5C50] rounded-lg focus:ring-2 focus:ring-[#D9B168] focus:border-transparent"
                    />
                </div>
            </div>

            <div className="bg-white rounded-lg shadow overflow-hidden">
                <div className="overflow-x-auto">
                    <table className="w-full">
                        <thead className="bg-[#F4F1EA]">
                            <tr>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase tracking-wider">User</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase tracking-wider">Document Type</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase tracking-wider">Status</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase tracking-wider">Submitted</th>
                                <th className="px-6 py-3 text-left text-xs font-medium text-[#122F26] uppercase tracking-wider">Actions</th>
                            </tr>
                        </thead>
                        <tbody className="divide-y divide-primary-200 dark:divide-primary-700">
                            {loading ? (
                                <tr><td colSpan={5} className="px-6 py-4 text-center">Loading...</td></tr>
                            ) : verifications.length === 0 ? (
                                <tr><td colSpan={5} className="px-6 py-4 text-center">No verification history found</td></tr>
                            ) : (
                                verifications.map((verification) => (
                                    <tr key={verification.id} className="hover:bg-sand-50 transition">
                                        <td className="px-6 py-4 whitespace-nowrap">
                                            <div>
                                                <p className="text-sm font-medium text-[#122F26]">
                                                    {verification.user.first_name} {verification.user.last_name}
                                                </p>
                                                <p className="text-sm text-[#3A5C50]">{verification.user.email}</p>
                                            </div>
                                        </td>
                                        <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50] capitalize">
                                            {verification.document_type.replace('_', ' ')}
                                        </td>
                                        <td className="px-6 py-4 whitespace-nowrap">
                                            <span className={`inline-flex items-center gap-2 px-3 py-1 rounded-full text-xs font-medium ${getStatusColor(verification.status)}`}>
                                                {getStatusIcon(verification.status)}
                                                {verification.status.replace('_', ' ')}
                                            </span>
                                        </td>
                                        <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50]">
                                            {formatDate(verification.submitted_at)}
                                        </td>
                                        <td className="px-6 py-4 whitespace-nowrap text-sm">
                                            <button
                                                onClick={() => handleViewDetails(verification.id)}
                                                className="inline-flex items-center gap-1 text-[#D9B168] hover:text-[#c9a158] transition"
                                            >
                                                <Eye className="w-4 h-4" /> View
                                            </button>
                                        </td>
                                    </tr>
                                ))
                            )}
                        </tbody>
                    </table>
                </div>

                {totalCount > ITEMS_PER_PAGE && (
                    <div className="px-6 py-4 border-t flex justify-between">
                        <p className="text-sm">Showing {(page - 1) * ITEMS_PER_PAGE + 1} to {Math.min(page * ITEMS_PER_PAGE, totalCount)} of {totalCount}</p>
                        <div className="flex gap-2">
                            <button
                                onClick={() => setPage(Math.max(1, page - 1))}
                                disabled={page === 1}
                                className="px-4 py-2 border rounded-lg text-sm bg-white hover:bg-gray-50 disabled:opacity-50"
                            >Previous</button>
                            <button
                                onClick={() => setPage(page + 1)}
                                disabled={page * ITEMS_PER_PAGE >= totalCount}
                                className="px-4 py-2 border rounded-lg text-sm bg-white hover:bg-gray-50 disabled:opacity-50"
                            >Next</button>
                        </div>
                    </div>
                )}
            </div>

            {showModal && selectedVerification && (
                <VerificationModal
                    verification={selectedVerification}
                    onClose={() => {
                        setShowModal(false);
                        setSelectedVerification(null);
                    }}
                    onApprove={dummyApprove}
                    onReject={dummyReject}
                    loading={loading}
                />
            )}
        </div>
    );
}
