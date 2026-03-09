'use client';

import { useState } from 'react';
import { useRouter, usePathname } from 'next/navigation';
import { Loader2, X, Calendar, Users, MessageSquare } from 'lucide-react';
import { toast } from 'react-hot-toast';
import { apiClient } from '@/services/api-client';

interface ContactHostModalProps {
    isOpen: boolean;
    onClose: () => void;
    host: {
        id: string;
        first_name: string;
        last_name: string;
    };
    propertyId?: string;
    userId?: string;
}

export function ContactHostModal({ isOpen, onClose, host, propertyId, userId }: ContactHostModalProps) {
    const router = useRouter();
    const pathname = usePathname();
    const [isSubmitting, setIsSubmitting] = useState(false);
    const [formData, setFormData] = useState({
        checkIn: '',
        checkOut: '',
        guests: 1,
        message: ''
    });

    if (!isOpen) return null;

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!userId) {
            toast.error('You must be logged in to perform this action');
            router.push(`/login?redirect=${encodeURIComponent(pathname || '/')}`);
            return;
        }

        if (!formData.checkIn || !formData.checkOut) {
            toast.error('Please provide travel dates to give the host context.');
            return;
        }
        if (!formData.message.trim()) {
            toast.error('Please include an initial message.');
            return;
        }

        // Safety regex check
        const emailRegex = /[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}/;
        const phoneRegex = /(?:\+?\d{1,3}[\s-]?)?\(?\d{3}\)?[\s-]?\d{3}[\s-]?\d{4}/;
        if (emailRegex.test(formData.message) || phoneRegex.test(formData.message)) {
            toast.error('For your safety and privacy, please do not share direct contact information before a booking is confirmed.');
            return;
        }

        setIsSubmitting(true);
        try {
            await apiClient.createConversation({
                participants: [parseInt(host.id), parseInt(userId)],
                property: propertyId ? parseInt(propertyId) : undefined,
                subject: `Inquiry from interested guest`,
                initial_message: formData.message,
                metadata: {
                    check_in: formData.checkIn,
                    check_out: formData.checkOut,
                    guests: formData.guests
                }
            });
            toast.success('Message sent to host!');
            onClose();
            router.push(`/messages`);
        } catch (error: any) {
            toast.error(error.response?.data?.error || 'Failed to start conversation');
        } finally {
            setIsSubmitting(false);
        }
    };

    return (
        <div className="fixed inset-0 z-50 flex items-center justify-center p-4 bg-black/50 backdrop-blur-sm animate-in fade-in duration-200">
            <div className="bg-white dark:bg-primary-900 rounded-2xl w-full max-w-md overflow-hidden shadow-2xl relative">
                {/* Header */}
                <div className="flex items-center justify-between p-5 border-b border-primary-100 dark:border-primary-800">
                    <h2 className="text-xl font-bold text-primary-900 dark:text-sand-50">
                        Contact {host.first_name}
                    </h2>
                    <button
                        onClick={onClose}
                        className="p-2 -mr-2 text-primary-500 hover:text-primary-700 dark:text-sand-400 dark:hover:text-sand-200 rounded-full hover:bg-primary-50 dark:hover:bg-primary-800 transition"
                    >
                        <X className="w-5 h-5" />
                    </button>
                </div>

                {/* Content */}
                <form onSubmit={handleSubmit} className="p-5 space-y-5">
                    <p className="text-sm text-primary-600 dark:text-sand-400">
                        Tell {host.first_name} a bit about your trip so they can better help you.
                    </p>

                    <div className="space-y-4">
                        {/* Dates */}
                        <div className="grid grid-cols-2 gap-3">
                            <div>
                                <label className="block text-xs font-semibold text-primary-700 dark:text-sand-300 uppercase tracking-wide mb-1.5">
                                    Check-in
                                </label>
                                <div className="relative">
                                    <Calendar className="absolute left-3 top-1/2 -translate-y-1/2 w-4 h-4 text-primary-400" />
                                    <input
                                        type="date"
                                        required
                                        min={new Date().toISOString().split('T')[0]}
                                        value={formData.checkIn}
                                        onChange={(e) => setFormData(p => ({ ...p, checkIn: e.target.value }))}
                                        className="w-full pl-9 pr-3 py-2.5 bg-primary-50 dark:bg-primary-800 border border-primary-200 dark:border-primary-700 rounded-lg text-sm focus:ring-2 focus:ring-secondary-500 text-primary-900 dark:text-sand-50"
                                    />
                                </div>
                            </div>
                            <div>
                                <label className="block text-xs font-semibold text-primary-700 dark:text-sand-300 uppercase tracking-wide mb-1.5">
                                    Check-out
                                </label>
                                <div className="relative">
                                    <Calendar className="absolute left-3 top-1/2 -translate-y-1/2 w-4 h-4 text-primary-400" />
                                    <input
                                        type="date"
                                        required
                                        min={formData.checkIn || new Date().toISOString().split('T')[0]}
                                        value={formData.checkOut}
                                        onChange={(e) => setFormData(p => ({ ...p, checkOut: e.target.value }))}
                                        className="w-full pl-9 pr-3 py-2.5 bg-primary-50 dark:bg-primary-800 border border-primary-200 dark:border-primary-700 rounded-lg text-sm focus:ring-2 focus:ring-secondary-500 text-primary-900 dark:text-sand-50"
                                    />
                                </div>
                            </div>
                        </div>

                        {/* Guests */}
                        <div>
                            <label className="block text-xs font-semibold text-primary-700 dark:text-sand-300 uppercase tracking-wide mb-1.5">
                                Guests
                            </label>
                            <div className="relative">
                                <Users className="absolute left-3 top-1/2 -translate-y-1/2 w-4 h-4 text-primary-400" />
                                <input
                                    type="number"
                                    min="1"
                                    max="20"
                                    required
                                    value={formData.guests}
                                    onChange={(e) => setFormData(p => ({ ...p, guests: parseInt(e.target.value) || 1 }))}
                                    className="w-full pl-9 pr-3 py-2.5 bg-primary-50 dark:bg-primary-800 border border-primary-200 dark:border-primary-700 rounded-lg text-sm focus:ring-2 focus:ring-secondary-500 text-primary-900 dark:text-sand-50"
                                />
                            </div>
                        </div>

                        {/* Message */}
                        <div>
                            <label className="block text-xs font-semibold text-primary-700 dark:text-sand-300 uppercase tracking-wide mb-1.5">
                                Message
                            </label>
                            <div className="relative">
                                <MessageSquare className="absolute left-3 top-3 w-4 h-4 text-primary-400" />
                                <textarea
                                    required
                                    rows={4}
                                    placeholder={`Hi ${host.first_name}, I'd like to ask about...`}
                                    value={formData.message}
                                    onChange={(e) => setFormData(p => ({ ...p, message: e.target.value }))}
                                    className="w-full pl-9 pr-3 py-2.5 bg-primary-50 dark:bg-primary-800 border border-primary-200 dark:border-primary-700 rounded-lg text-sm focus:ring-2 focus:ring-secondary-500 text-primary-900 dark:text-sand-50 resize-none"
                                />
                            </div>
                            <p className="text-xs text-primary-500 mt-2">
                                For your safety, do not share your phone number or email before a booking is confirmed.
                            </p>
                        </div>
                    </div>

                    <div className="pt-2">
                        <button
                            type="submit"
                            disabled={isSubmitting}
                            className="w-full flex justify-center items-center py-3 bg-secondary-600 hover:bg-secondary-700 text-white rounded-lg font-semibold transition disabled:opacity-70 disabled:cursor-not-allowed"
                        >
                            {isSubmitting ? <Loader2 className="w-5 h-5 animate-spin" /> : 'Send Message'}
                        </button>
                    </div>
                </form>
            </div>
        </div>
    );
}
