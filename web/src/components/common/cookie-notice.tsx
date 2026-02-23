'use client';

import { useState, useEffect } from 'react';
import Link from 'next/link';
import { X } from 'lucide-react';

export function CookieNotice() {
    const [isVisible, setIsVisible] = useState(false);

    useEffect(() => {
        // Check if user has already accepted cookies
        const hasAccepted = localStorage.getItem('stayafrica_cookies_accepted');
        if (!hasAccepted) {
            // Show notice after a short delay
            const timer = setTimeout(() => setIsVisible(true), 1000);
            return () => clearTimeout(timer);
        }
    }, []);

    const acceptCookies = () => {
        localStorage.setItem('stayafrica_cookies_accepted', 'true');
        setIsVisible(false);
    };

    const declineCookies = () => {
        // Still hide the banner, but we could set a session storage instead
        // to ask again later, or just respect decline for functional cookies
        sessionStorage.setItem('stayafrica_cookies_declined', 'true');
        setIsVisible(false);
    };

    if (!isVisible) return null;

    return (
        <div className="fixed bottom-0 left-0 right-0 z-[100] md:bottom-6 md:left-6 md:right-auto md:max-w-md w-full">
            <div className="bg-white dark:bg-primary-900 border-t border-sand-200 dark:border-primary-700 md:border md:rounded-2xl shadow-2xl p-5 md:p-6 animate-in slide-in-from-bottom-5 fade-in duration-500">
                <div className="flex justify-between items-start mb-3">
                    <h3 className="text-lg font-bold text-primary-900 dark:text-sand-100 flex items-center gap-2">
                        🍪 We use cookies
                    </h3>
                    <button
                        onClick={() => setIsVisible(false)}
                        className="text-primary-400 hover:text-primary-600 dark:text-sand-500 dark:hover:text-sand-300 transition-colors"
                        aria-label="Close cookie notice"
                    >
                        <X className="w-5 h-5" />
                    </button>
                </div>

                <p className="text-sm text-primary-600 dark:text-sand-400 mb-5 leading-relaxed">
                    We use cookies to enhance your browsing experience, serve personalized ads or content, and analyze our traffic. By clicking &quot;Accept All&quot;, you consent to our use of cookies.
                    <Link href="/cookies" className="text-accent-600 dark:text-accent-400 hover:underline ml-1 font-medium">
                        Read more
                    </Link>
                </p>

                <div className="flex flex-col sm:flex-row gap-3">
                    <button
                        onClick={acceptCookies}
                        className="flex-1 px-4 py-2.5 bg-accent-600 hover:bg-accent-700 text-white rounded-xl text-sm font-semibold transition-colors"
                    >
                        Accept All
                    </button>
                    <button
                        onClick={declineCookies}
                        className="flex-1 px-4 py-2.5 bg-white dark:bg-primary-800 border border-sand-300 dark:border-primary-600 text-primary-700 dark:text-sand-300 hover:bg-sand-50 dark:hover:bg-primary-700 rounded-xl text-sm font-medium transition-colors"
                    >
                        Decline
                    </button>
                </div>
            </div>
        </div>
    );
}
