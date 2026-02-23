'use client';

import { useState } from 'react';
import { Search, ChevronDown, ChevronRight, Mail, MessageSquare, HelpCircle, Shield, CreditCard, Home, Calendar, Users, MapPin, ExternalLink } from 'lucide-react';

interface FaqItem {
    question: string;
    answer: string;
}

interface FaqSection {
    title: string;
    icon: any;
    items: FaqItem[];
}

const FAQ_SECTIONS: FaqSection[] = [
    {
        title: 'Getting Started',
        icon: HelpCircle,
        items: [
            {
                question: 'How do I create an account?',
                answer: 'Click "Sign Up" on the top right corner. You can register using your email address or sign in with Google, Facebook, or Apple. After registering, you\'ll receive a verification email to activate your account.',
            },
            {
                question: 'What countries does StayAfrica operate in?',
                answer: 'StayAfrica currently operates in Zimbabwe, South Africa, Botswana, Namibia, and Zambia. We\'re continuously expanding our reach across Africa.',
            },
            {
                question: 'Is there a mobile app available?',
                answer: 'Yes! StayAfrica is available on both iOS and Android. Download it from the App Store or Google Play to browse properties, manage bookings, and message hosts on the go.',
            },
        ],
    },
    {
        title: 'Booking & Reservations',
        icon: Calendar,
        items: [
            {
                question: 'How do I book a property?',
                answer: 'Search for your destination, select dates and number of guests, browse available properties, then click "Book Now" on your chosen property. Review the pricing breakdown, and complete your booking by selecting a payment method.',
            },
            {
                question: 'Can I cancel my booking?',
                answer: 'Yes, you can cancel from your Bookings page. Cancellation policies vary by property — check the property listing for specific refund terms. Most properties offer free cancellation up to 48 hours before check-in.',
            },
            {
                question: 'What is Instant Booking?',
                answer: 'Properties with Instant Booking enabled allow you to book immediately without waiting for host approval. The host may set requirements such as verified identity or a minimum number of previous reviews.',
            },
            {
                question: 'How do I contact the host?',
                answer: 'You can message any host directly from their property listing page by clicking "Contact Host." You can also access your conversations from the Messages page.',
            },
        ],
    },
    {
        title: 'Payments',
        icon: CreditCard,
        items: [
            {
                question: 'What payment methods are accepted?',
                answer: 'Payment methods depend on your country: Zimbabwe — Paynow (EcoCash, OneMoney) or Cash on Arrival; South Africa — PayFast or Ozow; International — Stripe (Visa, MasterCard). We also accept direct bank transfers for larger bookings.',
            },
            {
                question: 'How is pricing calculated?',
                answer: 'Your total includes: Nightly rate × Number of nights + Service fee ($3) + Cleaning fee (if applicable). StayAfrica charges a 7% commission which is deducted from the host\'s payout, not added to your bill.',
            },
            {
                question: 'When is the host paid?',
                answer: 'Host payouts are processed 24 hours after check-in to ensure a smooth experience. Hosts can view their earnings and request withdrawals from their Wallet page.',
            },
        ],
    },
    {
        title: 'Hosting',
        icon: Home,
        items: [
            {
                question: 'How do I become a host?',
                answer: 'Click "Become a Host" from the main menu, complete your profile, and submit your first property listing. Your listing will be reviewed by our team, and once approved, it will go live for guests to book.',
            },
            {
                question: 'What are the hosting requirements?',
                answer: 'Hosts must verify their identity, provide accurate property information and photos, maintain responsive communication, and uphold StayAfrica quality standards. Properties must comply with local regulations.',
            },
            {
                question: 'How do I manage my listings?',
                answer: 'Visit your Host Dashboard to manage all your properties, update pricing and availability calendars, respond to booking requests, and track your earnings.',
            },
            {
                question: 'How much does it cost to list on StayAfrica?',
                answer: 'Listing your property is free. StayAfrica charges a 7% commission on confirmed bookings, deducted from your payout. There are no upfront fees or monthly subscriptions.',
            },
        ],
    },
    {
        title: 'Safety & Security',
        icon: Shield,
        items: [
            {
                question: 'How does StayAfrica verify listings?',
                answer: 'All properties are reviewed before going live. We verify host identity, check property photos and descriptions for accuracy, and monitor guest reviews for quality assurance.',
            },
            {
                question: 'Is my payment information secure?',
                answer: 'Yes. We use industry-standard encryption and partner with trusted payment providers (Stripe, PayFast, Paynow). We never store your full card details on our servers.',
            },
            {
                question: 'How does the review system work?',
                answer: 'After checkout, both guests and hosts can leave reviews. Reviews are only visible after both parties have submitted theirs (or after 14 days), ensuring honest and unbiased feedback.',
            },
        ],
    },
];

function FaqAccordion({ item }: { item: FaqItem }) {
    const [isOpen, setIsOpen] = useState(false);

    return (
        <div className="border-b border-sand-200 dark:border-primary-700 last:border-0">
            <button
                onClick={() => setIsOpen(!isOpen)}
                className="w-full flex items-center justify-between py-4 text-left hover:text-accent-600 dark:hover:text-accent-400 transition-colors"
            >
                <span className="text-sm font-medium text-primary-800 dark:text-sand-200 pr-4">
                    {item.question}
                </span>
                {isOpen ? (
                    <ChevronDown className="w-4 h-4 text-primary-400 dark:text-sand-500 flex-shrink-0" />
                ) : (
                    <ChevronRight className="w-4 h-4 text-primary-400 dark:text-sand-500 flex-shrink-0" />
                )}
            </button>
            {isOpen && (
                <div className="pb-4">
                    <p className="text-sm text-primary-600 dark:text-sand-400 leading-relaxed">
                        {item.answer}
                    </p>
                </div>
            )}
        </div>
    );
}

export function HelpContent() {
    const [searchQuery, setSearchQuery] = useState('');

    const filteredSections = searchQuery.trim()
        ? FAQ_SECTIONS.map(section => ({
            ...section,
            items: section.items.filter(
                item =>
                    item.question.toLowerCase().includes(searchQuery.toLowerCase()) ||
                    item.answer.toLowerCase().includes(searchQuery.toLowerCase())
            ),
        })).filter(section => section.items.length > 0)
        : FAQ_SECTIONS;

    return (
        <div className="max-w-3xl mx-auto px-4 py-8">
            {/* Header */}
            <div className="text-center mb-8">
                <h1 className="text-3xl font-bold text-primary-900 dark:text-sand-100 mb-2">
                    How can we help?
                </h1>
                <p className="text-primary-500 dark:text-sand-400">
                    Find answers to common questions about StayAfrica
                </p>
            </div>

            {/* Search */}
            <div className="relative mb-8">
                <Search className="absolute left-4 top-1/2 -translate-y-1/2 w-5 h-5 text-primary-400 dark:text-sand-500" />
                <input
                    type="text"
                    placeholder="Search for help..."
                    value={searchQuery}
                    onChange={e => setSearchQuery(e.target.value)}
                    className="w-full pl-12 pr-4 py-3 rounded-xl border border-sand-300 dark:border-primary-600 bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-100 placeholder-primary-400 dark:placeholder-sand-500 focus:outline-none focus:ring-2 focus:ring-accent-500 focus:border-transparent transition-all"
                />
            </div>

            {/* FAQ Sections */}
            {filteredSections.length === 0 ? (
                <div className="text-center py-16">
                    <HelpCircle className="w-12 h-12 mx-auto text-sand-300 dark:text-primary-600 mb-4" />
                    <h3 className="text-lg font-medium text-primary-700 dark:text-sand-300">
                        No results found
                    </h3>
                    <p className="text-sm text-primary-500 dark:text-sand-400 mt-1">
                        Try different keywords or browse the sections below
                    </p>
                </div>
            ) : (
                <div className="space-y-8">
                    {filteredSections.map(section => {
                        const SectionIcon = section.icon;
                        return (
                            <div
                                key={section.title}
                                className="bg-white dark:bg-primary-800/40 rounded-2xl p-6 shadow-sm border border-sand-200/50 dark:border-primary-700/50"
                            >
                                <div className="flex items-center gap-3 mb-4">
                                    <div className="w-8 h-8 rounded-lg bg-accent-100 dark:bg-accent-900/30 flex items-center justify-center">
                                        <SectionIcon className="w-4 h-4 text-accent-600 dark:text-accent-400" />
                                    </div>
                                    <h2 className="text-lg font-semibold text-primary-900 dark:text-sand-100">
                                        {section.title}
                                    </h2>
                                </div>
                                <div>
                                    {section.items.map((item, i) => (
                                        <FaqAccordion key={i} item={item} />
                                    ))}
                                </div>
                            </div>
                        );
                    })}
                </div>
            )}

            {/* Contact Support */}
            <div className="mt-12 bg-gradient-to-br from-accent-50 to-sand-100 dark:from-accent-900/20 dark:to-primary-800/40 rounded-2xl p-8 text-center border border-accent-200/50 dark:border-accent-800/30">
                <h3 className="text-lg font-semibold text-primary-900 dark:text-sand-100 mb-2">
                    Still need help?
                </h3>
                <p className="text-sm text-primary-600 dark:text-sand-400 mb-6">
                    Can&apos;t find what you&apos;re looking for? Our support team is here to help.
                </p>
                <div className="flex flex-col sm:flex-row gap-3 justify-center">
                    <a
                        href="mailto:support@stayafrica.app"
                        className="inline-flex items-center justify-center gap-2 px-6 py-2.5 bg-accent-600 hover:bg-accent-700 text-white rounded-lg text-sm font-medium transition-colors"
                    >
                        <Mail className="w-4 h-4" />
                        Email Support
                    </a>
                    <a
                        href="/messages"
                        className="inline-flex items-center justify-center gap-2 px-6 py-2.5 bg-white dark:bg-primary-700 hover:bg-sand-50 dark:hover:bg-primary-600 text-primary-900 dark:text-sand-100 rounded-lg text-sm font-medium border border-sand-300 dark:border-primary-600 transition-colors"
                    >
                        <MessageSquare className="w-4 h-4" />
                        Live Chat
                    </a>
                </div>
            </div>
        </div>
    );
}
