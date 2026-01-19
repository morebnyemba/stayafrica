'use client';

import Link from 'next/link';
import { 
  User, 
  Bell, 
  CreditCard, 
  Shield, 
  Globe, 
  Lock,
  ChevronRight,
  Receipt,
  ShieldCheck,
} from 'lucide-react';

const settingsSections = [
  {
    title: 'Account',
    items: [
      {
        title: 'Personal Information',
        description: 'Update your name, email, and phone number',
        icon: User,
        link: '/profile',
      },
      {
        title: 'Identity Verification',
        description: 'Verify your identity for enhanced trust',
        icon: ShieldCheck,
        link: '/host/verification',
      },
      {
        title: 'Password & Security',
        description: 'Update password and security settings',
        icon: Lock,
        link: '/profile',
      },
    ],
  },
  {
    title: 'Payments',
    items: [
      {
        title: 'Payment Methods',
        description: 'Manage your payout and payment methods',
        icon: CreditCard,
        link: '/wallet',
      },
      {
        title: 'Tax Information',
        description: 'View tax documents and reports',
        icon: Receipt,
        link: '/host/tax-reports',
      },
    ],
  },
  {
    title: 'Preferences',
    items: [
      {
        title: 'Notifications',
        description: 'Choose what notifications you receive',
        icon: Bell,
        link: '/profile',
      },
      {
        title: 'Privacy',
        description: 'Manage your privacy settings',
        icon: Shield,
        link: '/profile',
      },
      {
        title: 'Language & Region',
        description: 'Set your language and currency preferences',
        icon: Globe,
        link: '/profile',
      },
    ],
  },
];

export default function SettingsPage() {
  return (
    <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
      <div className="max-w-4xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        <div className="mb-8">
          <h1 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-2">
            Settings
          </h1>
          <p className="text-lg text-primary-600 dark:text-sand-300">
            Manage your account and preferences
          </p>
        </div>

        <div className="space-y-8">
          {settingsSections.map((section) => (
            <div key={section.title}>
              <h2 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-4">
                {section.title}
              </h2>
              <div className="bg-white dark:bg-primary-800 rounded-lg shadow-sm border border-primary-200 dark:border-primary-700 divide-y divide-primary-200 dark:divide-primary-700">
                {section.items.map((item) => (
                  <Link
                    key={item.title}
                    href={item.link}
                    className="flex items-center justify-between p-4 hover:bg-primary-50 dark:hover:bg-primary-700/50 transition-colors"
                  >
                    <div className="flex items-center gap-4">
                      <div className="p-2 bg-primary-100 dark:bg-primary-700 rounded-lg">
                        <item.icon className="w-5 h-5 text-primary-600 dark:text-sand-300" />
                      </div>
                      <div>
                        <h3 className="font-medium text-primary-900 dark:text-sand-50">
                          {item.title}
                        </h3>
                        <p className="text-sm text-primary-600 dark:text-sand-400">
                          {item.description}
                        </p>
                      </div>
                    </div>
                    <ChevronRight className="w-5 h-5 text-primary-400 dark:text-sand-500" />
                  </Link>
                ))}
              </div>
            </div>
          ))}
        </div>
      </div>
    </div>
  );
}
