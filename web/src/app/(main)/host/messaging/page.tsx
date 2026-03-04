'use client';

import { useState } from 'react';
import Link from 'next/link';
import { 
  MessageSquare, 
  Settings, 
  Zap, 
  Clock, 
  FileText,
  ArrowLeft,
} from 'lucide-react';
import { AutoMessageSettings } from '@/components/automated-messaging/AutoMessageSettings';
import { QuickRepliesManager } from '@/components/automated-messaging/QuickRepliesManager';
import { ScheduledMessagesPanel } from '@/components/automated-messaging/ScheduledMessagesPanel';
import { MessageTemplateEditor } from '@/components/automated-messaging/MessageTemplateEditor';

type Tab = 'settings' | 'templates' | 'quick-replies' | 'scheduled';

const tabs = [
  { id: 'settings' as Tab, label: 'Auto-Reply Settings', icon: Settings, description: 'Configure automated responses' },
  { id: 'templates' as Tab, label: 'Message Templates', icon: FileText, description: 'Create reusable templates' },
  { id: 'quick-replies' as Tab, label: 'Quick Replies', icon: Zap, description: 'Manage quick reply shortcuts' },
  { id: 'scheduled' as Tab, label: 'Scheduled Messages', icon: Clock, description: 'View and manage scheduled sends' },
];

export default function HostMessagingPage() {
  const [activeTab, setActiveTab] = useState<Tab>('settings');

  return (
    <div className="min-h-screen bg-sand-50 dark:bg-primary-900">
      {/* Header */}
      <div className="bg-gradient-to-r from-forest to-forest/90 text-white">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          <Link
            href="/host/dashboard"
            className="inline-flex items-center text-sand-200 hover:text-white mb-4 transition-colors"
          >
            <ArrowLeft className="h-4 w-4 mr-2" />
            Back to Dashboard
          </Link>
          <div className="flex items-center gap-3">
            <MessageSquare className="h-8 w-8 text-gold" />
            <div>
              <h1 className="text-3xl font-bold">Messaging Automation</h1>
              <p className="text-sand-200 mt-1">
                Automate guest communication with templates, quick replies, and scheduled messages
              </p>
            </div>
          </div>
        </div>
      </div>

      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Tab Navigation */}
        <div className="grid grid-cols-2 lg:grid-cols-4 gap-3 mb-8">
          {tabs.map((tab) => {
            const Icon = tab.icon;
            const isActive = activeTab === tab.id;
            return (
              <button
                key={tab.id}
                onClick={() => setActiveTab(tab.id)}
                className={`flex items-center gap-3 p-4 rounded-xl border-2 transition-all text-left ${
                  isActive
                    ? 'border-forest bg-white dark:bg-primary-800 shadow-md'
                    : 'border-transparent bg-white/60 dark:bg-primary-800/60 hover:bg-white dark:hover:bg-primary-800 hover:border-sand-200/50 dark:hover:border-primary-700/50'
                }`}
              >
                <div className={`p-2 rounded-lg ${isActive ? 'bg-forest text-white' : 'bg-primary-100 dark:bg-primary-700 text-primary-400 dark:text-sand-500'}`}>
                  <Icon className="h-5 w-5" />
                </div>
                <div className="min-w-0">
                  <p className={`font-medium text-sm truncate ${isActive ? 'text-forest dark:text-white' : 'text-primary-700 dark:text-sand-200'}`}>
                    {tab.label}
                  </p>
                  <p className="text-xs text-primary-400 dark:text-sand-500 hidden sm:block truncate">
                    {tab.description}
                  </p>
                </div>
              </button>
            );
          })}
        </div>

        {/* Tab Content */}
        <div className="min-h-[500px]">
          {activeTab === 'settings' && <AutoMessageSettings />}
          {activeTab === 'templates' && <MessageTemplateEditor />}
          {activeTab === 'quick-replies' && <QuickRepliesManager />}
          {activeTab === 'scheduled' && <ScheduledMessagesPanel />}
        </div>
      </div>
    </div>
  );
}
