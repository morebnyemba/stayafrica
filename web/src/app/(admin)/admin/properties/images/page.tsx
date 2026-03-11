'use client';

import { useState, useEffect } from 'react';
import { adminApi } from '@/lib/admin-api';
import { PropertyImage } from '@/types';
import toast from 'react-hot-toast';
import { Trash2, ExternalLink, Image as ImageIcon, RefreshCcw } from 'lucide-react';

export default function PropertyImagesManagement() {
    const [images, setImages] = useState<PropertyImage[]>([]);
    const [loading, setLoading] = useState(true);
    const [page, setPage] = useState(1);
    const [totalCount, setTotalCount] = useState(0);
    const ITEMS_PER_PAGE = 24;

    useEffect(() => {
        loadImages();
    }, [page]);

    const loadImages = async () => {
        try {
            setLoading(true);
            const data = await adminApi.getAllPropertyImages({
                page,
                per_page: ITEMS_PER_PAGE
            });
            setImages(data.results || []);
            setTotalCount(data.count || 0);
        } catch (err: any) {
            toast.error('Failed to load property images');
            console.error(err);
        } finally {
            setLoading(false);
        }
    };

    const handleDelete = async (id: string) => {
        if (!window.confirm('Are you sure you want to delete this property image? This cannot be undone.')) return;

        try {
            await adminApi.deletePropertyImage(id);
            toast.success('Image deleted from platform safely');
            loadImages();
        } catch (err: any) {
            toast.error('Failed to delete property image');
            console.error(err);
        }
    };

    return (
        <div className="p-8">
            <div className="mb-8 flex justify-between items-center">
                <div>
                    <h1 className="text-3xl font-bold text-[#122F26]">Property Images Moderation</h1>
                    <p className="text-[#3A5C50] mt-2">Review uploaded property images across the platform</p>
                </div>
                <button
                    onClick={loadImages}
                    className="flex items-center gap-2 px-4 py-2 bg-sand-100 text-[#122F26] rounded-lg hover:bg-sand-200 transition-colors"
                >
                    <RefreshCcw className={`w-5 h-5 ${loading ? 'animate-spin' : ''}`} /> Refresh
                </button>
            </div>

            <div className="bg-white rounded-lg shadow min-h-[500px] flex flex-col">
                {loading ? (
                    <div className="flex flex-1 items-center justify-center p-8">
                        <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-[#D9B168]"></div>
                    </div>
                ) : images.length === 0 ? (
                    <div className="flex flex-1 items-center justify-center flex-col text-sand-500">
                        <ImageIcon className="w-16 h-16 mb-4 text-sand-300" />
                        <p className="text-lg">No property images found.</p>
                    </div>
                ) : (
                    <div className="p-6 flex-1">
                        <div className="grid grid-cols-2 md:grid-cols-3 lg:grid-cols-4 xl:grid-cols-6 gap-6">
                            {images.map((img) => (
                                <div key={img.id} className="group relative rounded-lg border border-sand-200 overflow-hidden bg-sand-50 aspect-square flex items-center justify-center">
                                    {img.image_url ? (
                                        <img
                                            src={img.image_url}
                                            alt={`Property ${img.property_id}`}
                                            className="w-full h-full object-cover"
                                        />
                                    ) : (
                                        <div className="text-sand-400 flex flex-col items-center">
                                            <ImageIcon className="w-8 h-8 mb-2" />
                                            <span className="text-xs">No preview</span>
                                        </div>
                                    )}

                                    {/* Hover Overlay */}
                                    <div className="absolute inset-0 bg-black/60 opacity-0 group-hover:opacity-100 transition-opacity flex flex-col justify-between p-4">
                                        <div className="flex justify-end gap-2">
                                            <button
                                                onClick={() => window.open(img.image_url, '_blank')}
                                                className="p-2 bg-white/20 hover:bg-white/40 rounded-full text-white backdrop-blur-sm transition"
                                                title="View Full Size"
                                            >
                                                <ExternalLink className="w-4 h-4" />
                                            </button>
                                            <button
                                                onClick={() => handleDelete(img.id)}
                                                className="p-2 bg-red-500/80 hover:bg-red-600 rounded-full text-white backdrop-blur-sm transition"
                                                title="Delete Image"
                                            >
                                                <Trash2 className="w-4 h-4" />
                                            </button>
                                        </div>
                                        <div>
                                            <span className="text-white text-xs font-medium bg-black/50 px-2 py-1 rounded backdrop-blur-sm">
                                                Prop ID: {img.property_id || 'Unknown'}
                                            </span>
                                        </div>
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>
                )}

                {/* Pagination Info */}
                {!loading && totalCount > 0 && (
                    <div className="bg-sand-50 px-6 py-4 border-t flex items-center justify-between">
                        <div className="text-sm text-[#122F26]">
                            Showing {(page - 1) * ITEMS_PER_PAGE + 1} to {Math.min(page * ITEMS_PER_PAGE, totalCount)} of {totalCount} images
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
        </div>
    );
}
