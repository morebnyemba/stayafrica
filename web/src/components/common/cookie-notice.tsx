'use client';

import { useState, useEffect } from 'react';
import Link from 'next/link';
import { X } from 'lucide-react';

const STORAGE_KEY = 'stayafrica_cookie_choice';

function saveCookieChoice(choice: 'accepted' | 'declined' | 'dismissed') {
    try {
        localStorage.setItem(STORAGE_KEY, choice);
    } catch {
        // localStorage unavailable (private browsing etc.) — fail silently
    }
}

export function CookieNotice() {
    // null = not yet checked (avoids SSR flash), false = hidden, true = visible
    const [isVisible, setIsVisible] = useState<boolean | null>(null);

    useEffect(() => {
        try {
            const saved = localStorage.getItem(STORAGE_KEY);
            if (saved) {
                setIsVisible(false);
            } else {
                // Show after a short delay so it doesn't flash on load
                const timer = setTimeout(() => setIsVisible(true), 1200);
                return () => clearTimeout(timer);
            }
        } catch {
            setIsVisible(false);
        }
    }, []);

    const hide = (choice: 'accepted' | 'declined' | 'dismissed') => {
        saveCookieChoice(choice);
        setIsVisible(false);
    };

    // Don't render anything until we've checked localStorage (avoids SSR mismatch)
    if (isVisible === null || !isVisible) return null;

    return (
        <div className="fixed bottom-0 left-0 right-0 z-[100] md:bottom-6 md:left-6 md:right-auto md:max-w-md w-full">
            <div className="bg-white dark:bg-primary-900 border-t border-sand-200 dark:border-primary-700 md:border md:rounded-2xl shadow-2xl p-5 md:p-6 animate-in slide-in-from-bottom-5 fade-in duration-500">
                <div className="flex justify-between items-start mb-3">
                    <h3 className="text-lg font-bold text-primary-900 dark:text-sand-100 flex items-center gap-2">
                        🍪 We use cookies
                    </h3>
                    <button
                        onClick={() => hide('dismissed')}
                        className="text-primary-400 hover:text-primary-600 dark:text-sand-500 dark:hover:text-sand-300 transition-colors"
                        aria-label="Close cookie notice"
                    >
                        <X className="w-5 h-5" />
                    </button>
                </div>

                <p className="text-sm text-primary-600 dark:text-sand-400 mb-5 leading-relaxed">
                    We use cookies to enhance your browsing experience, serve personalized content, and analyze our traffic. By clicking &quot;Accept All&quot;, you consent to our use of cookies.
                    <Link href="/cookies" className="text-accent-600 dark:text-accent-400 hover:underline ml-1 font-medium">
                        Read more
                    </Link>
                </p>

                <div className="flex flex-col sm:flex-row gap-3">
                    <button
                        onClick={() => hide('accepted')}
                        className="flex-1 px-4 py-2.5 bg-accent-600 hover:bg-accent-700 text-white rounded-xl text-sm font-semibold transition-colors"
                    >
                        Accept All
                    </button>
                    <button
                        onClick={() => hide('declined')}
                        className="flex-1 px-4 py-2.5 bg-white dark:bg-primary-800 border border-sand-300 dark:border-primary-600 text-primary-700 dark:text-sand-300 hover:bg-sand-50 dark:hover:bg-primary-700 rounded-xl text-sm font-medium transition-colors"
                    >
                        Decline
                    </button>
                </div>
            </div>
        </div>
    );
}

