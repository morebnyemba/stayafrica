'use client';

import { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { ScheduledMessage } from '@/types/automated-messaging-types';
import { Calendar, Loader2, Plus, Trash2, X } from 'lucide-react';
import { format } from 'date-fns';

export const ScheduledMessagesPanel = () => {
  const queryClient = useQueryClient();
  const [isScheduling, setIsScheduling] = useState(false);
  const [formData, setFormData] = useState({
    conversation: '',
    message_text: '',
    scheduled_time: '',
  });

  const { data: messages = [], isLoading } = useQuery<ScheduledMessage[]>({
    queryKey: ['scheduled-messages'],
    queryFn: async () => {
      const response = await apiClient.get('/messaging/scheduled-messages/');
      return response.data.results || response.data;
    },
  });

  const createMutation = useMutation({
    mutationFn: async (data: { conversation: number; message_text: string; scheduled_time: string }) => {
      const response = await apiClient.post('/messaging/scheduled-messages/', data);
      return response.data;
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['scheduled-messages'] });
      resetForm();
    },
  });

  const cancelMutation = useMutation({
    mutationFn: async (id: number) => {
      const response = await apiClient.post(`/messaging/scheduled-messages/${id}/cancel/`);
      return response.data;
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['scheduled-messages'] });
    },
  });

  const resetForm = () => {
    setFormData({
      conversation: '',
      message_text: '',
      scheduled_time: '',
    });
    setIsScheduling(false);
  };

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();

    if (!formData.conversation.trim() || !formData.message_text.trim() || !formData.scheduled_time) {
      return;
    }

    createMutation.mutate({
      conversation: parseInt(formData.conversation),
      message_text: formData.message_text,
      scheduled_time: new Date(formData.scheduled_time).toISOString(),
    });
  };

  const handleCancel = (id: number) => {
    if (confirm('Are you sure you want to cancel this scheduled message?')) {
      cancelMutation.mutate(id);
    }
  };

  const getStatusColor = (status: ScheduledMessage['status']) => {
    switch (status) {
      case 'pending':
        return 'bg-yellow-100 text-yellow-800';
      case 'sent':
        return 'bg-green-100 text-green-800';
      case 'failed':
        return 'bg-red-100 text-red-800';
      case 'cancelled':
        return 'bg-primary-100 dark:bg-primary-800 text-primary-800 dark:text-sand-100';
    }
  };

  if (isLoading) {
    return (
      <div className="flex items-center justify-center p-8">
        <Loader2 className="h-8 w-8 animate-spin text-primary-300 dark:text-primary-500" />
      </div>
    );
  }

  return (
    <div className="bg-white dark:bg-primary-800/40 rounded-lg shadow-sm border">
      <div className="p-6 border-b">
        <div className="flex items-center justify-between">
          <div>
            <h2 className="text-xl font-semibold text-primary-900 dark:text-sand-50">Scheduled Messages</h2>
            <p className="text-sm text-primary-500 dark:text-sand-400 mt-1">
              Schedule messages to be sent at a specific time
            </p>
          </div>

          {!isScheduling && (
            <button
              onClick={() => setIsScheduling(true)}
              className="inline-flex items-center gap-2 px-4 py-2 bg-secondary-600 text-white rounded-lg hover:bg-secondary-700 transition-colors"
            >
              <Plus className="h-4 w-4" />
              <span>Schedule Message</span>
            </button>
          )}
        </div>
      </div>

      <div className="p-6 space-y-4">
        {isScheduling && (
          <form onSubmit={handleSubmit} className="border rounded-lg p-4 bg-sand-50 dark:bg-primary-900 space-y-4">
            <div>
              <label className="block text-sm font-medium text-primary-700 dark:text-sand-200 mb-2">
                Conversation ID *
              </label>
              <input
                type="number"
                value={formData.conversation}
                onChange={(e) => setFormData(prev => ({ ...prev, conversation: e.target.value }))}
                placeholder="Enter conversation ID"
                className="w-full px-3 py-2 border border-primary-300 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent"
                required
              />
            </div>

            <div>
              <label className="block text-sm font-medium text-primary-700 dark:text-sand-200 mb-2">
                Message *
              </label>
              <textarea
                value={formData.message_text}
                onChange={(e) => setFormData(prev => ({ ...prev, message_text: e.target.value }))}
                placeholder="Enter your message..."
                rows={4}
                className="w-full px-3 py-2 border border-primary-300 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent"
                required
              />
            </div>

            <div>
              <label className="block text-sm font-medium text-primary-700 dark:text-sand-200 mb-2">
                <Calendar className="h-4 w-4 inline mr-1" />
                Scheduled Date & Time *
              </label>
              <input
                type="datetime-local"
                value={formData.scheduled_time}
                onChange={(e) => setFormData(prev => ({ ...prev, scheduled_time: e.target.value }))}
                className="w-full px-3 py-2 border border-primary-300 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent"
                required
              />
            </div>

            <div className="flex gap-2">
              <button
                type="submit"
                disabled={createMutation.isPending}
                className="inline-flex items-center gap-2 px-4 py-2 bg-secondary-600 text-white rounded-lg hover:bg-secondary-700 disabled:opacity-50 transition-colors"
              >
                {createMutation.isPending ? (
                  <>
                    <Loader2 className="h-4 w-4 animate-spin" />
                    <span>Scheduling...</span>
                  </>
                ) : (
                  <>
                    <Calendar className="h-4 w-4" />
                    <span>Schedule</span>
                  </>
                )}
              </button>

              <button
                type="button"
                onClick={resetForm}
                className="inline-flex items-center gap-2 px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg hover:bg-sand-50 dark:hover:bg-primary-800 transition-colors"
              >
                <X className="h-4 w-4" />
                <span>Cancel</span>
              </button>
            </div>
          </form>
        )}

        {messages.length === 0 ? (
          <div className="text-center py-12 text-primary-400 dark:text-sand-500">
            <p>No scheduled messages. Schedule your first message!</p>
          </div>
        ) : (
          <div className="space-y-3">
            {messages.map((msg) => (
              <div key={msg.id} className="border rounded-lg p-4">
                <div className="flex items-start justify-between mb-3">
                  <div className="flex-1">
                    <div className="flex items-center gap-2 mb-1">
                      <span className="font-medium text-primary-900 dark:text-sand-50">
                        Conversation #{msg.conversation_id}
                      </span>
                      <span className={`px-2 py-0.5 text-xs rounded capitalize ${getStatusColor(msg.status)}`}>
                        {msg.status_display || msg.status}
                      </span>
                    </div>
                    <p className="text-sm text-primary-500 dark:text-sand-400">
                      {format(new Date(msg.scheduled_time), 'PPpp')}
                    </p>
                  </div>

                  {msg.status === 'pending' && (
                    <button
                      onClick={() => handleCancel(msg.id)}
                      disabled={cancelMutation.isPending}
                      className="p-2 text-red-600 hover:bg-red-50 rounded transition-colors disabled:opacity-50"
                      aria-label="Cancel"
                    >
                      <Trash2 className="h-4 w-4" />
                    </button>
                  )}
                </div>

                <p className="text-sm text-primary-700 dark:text-sand-200 whitespace-pre-wrap">
                  {msg.message_text}
                </p>

                {msg.sent_at && (
                  <p className="text-xs text-primary-400 dark:text-sand-500 mt-2">
                    Sent: {format(new Date(msg.sent_at), 'PPpp')}
                  </p>
                )}
              </div>
            ))}
          </div>
        )}
      </div>
    </div>
  );
};
