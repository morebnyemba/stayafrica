'use client';

import { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import axios from 'axios';
import { QuickReply } from '@/types/automated-messaging-types';
import { Plus, Edit2, Trash2, Loader2, Save, X } from 'lucide-react';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8000';

export const QuickRepliesManager = () => {
  const queryClient = useQueryClient();
  const [isAdding, setIsAdding] = useState(false);
  const [editingId, setEditingId] = useState<number | null>(null);
  const [formData, setFormData] = useState({ shortcut: '', message_text: '', category: '' });

  const { data: quickReplies = [], isLoading } = useQuery<QuickReply[]>({
    queryKey: ['quick-replies'],
    queryFn: async () => {
      const token = localStorage.getItem('access_token');
      const response = await axios.get(
        `${API_BASE_URL}/api/v1/messaging/quick-replies/`,
        {
          headers: {
            Authorization: `Bearer ${token}`,
          },
        }
      );
      return response.data.results || response.data;
    },
  });

  const createMutation = useMutation({
    mutationFn: async (data: { shortcut: string; message_text: string; category: string }) => {
      const token = localStorage.getItem('access_token');
      const response = await axios.post(
        `${API_BASE_URL}/api/v1/messaging/quick-replies/`,
        data,
        {
          headers: {
            Authorization: `Bearer ${token}`,
            'Content-Type': 'application/json',
          },
        }
      );
      return response.data;
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['quick-replies'] });
      resetForm();
    },
  });

  const updateMutation = useMutation({
    mutationFn: async ({ id, data }: { id: number; data: Partial<QuickReply> }) => {
      const token = localStorage.getItem('access_token');
      const response = await axios.patch(
        `${API_BASE_URL}/api/v1/messaging/quick-replies/${id}/`,
        data,
        {
          headers: {
            Authorization: `Bearer ${token}`,
            'Content-Type': 'application/json',
          },
        }
      );
      return response.data;
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['quick-replies'] });
      resetForm();
    },
  });

  const deleteMutation = useMutation({
    mutationFn: async (id: number) => {
      const token = localStorage.getItem('access_token');
      await axios.delete(
        `${API_BASE_URL}/api/v1/messaging/quick-replies/${id}/`,
        {
          headers: {
            Authorization: `Bearer ${token}`,
          },
        }
      );
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['quick-replies'] });
    },
  });

  const resetForm = () => {
    setFormData({ shortcut: '', message_text: '', category: '' });
    setIsAdding(false);
    setEditingId(null);
  };

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();

    if (!formData.shortcut.trim() || !formData.message_text.trim()) {
      return;
    }

    if (editingId) {
      updateMutation.mutate({ id: editingId, data: formData });
    } else {
      createMutation.mutate(formData);
    }
  };

  const handleEdit = (reply: QuickReply) => {
    setFormData({
      shortcut: reply.shortcut,
      message_text: reply.message_text,
      category: reply.category || '',
    });
    setEditingId(reply.id);
    setIsAdding(true);
  };

  const handleDelete = (id: number) => {
    if (confirm('Are you sure you want to delete this quick reply?')) {
      deleteMutation.mutate(id);
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
            <h2 className="text-xl font-semibold text-primary-900 dark:text-sand-50">Quick Replies</h2>
            <p className="text-sm text-primary-500 dark:text-sand-400 mt-1">
              Create shortcut responses for faster guest communication
            </p>
          </div>

          {!isAdding && (
            <button
              onClick={() => setIsAdding(true)}
              className="inline-flex items-center gap-2 px-4 py-2 bg-secondary-600 text-white rounded-lg hover:bg-secondary-700 transition-colors"
            >
              <Plus className="h-4 w-4" />
              <span>Add Quick Reply</span>
            </button>
          )}
        </div>
      </div>

      <div className="p-6 space-y-4">
        {isAdding && (
          <form onSubmit={handleSubmit} className="border rounded-lg p-4 bg-sand-50 dark:bg-primary-900 space-y-4">
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              <div>
                <label className="block text-sm font-medium text-primary-700 dark:text-sand-200 mb-2">
                  Shortcut *
                </label>
                <input
                  type="text"
                  value={formData.shortcut}
                  onChange={(e) => setFormData(prev => ({ ...prev, shortcut: e.target.value }))}
                  placeholder="e.g., /welcome or /wifi"
                  className="w-full px-3 py-2 border border-primary-300 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent"
                  required
                />
                <p className="text-xs text-primary-400 dark:text-sand-500 mt-1">
                  Type this shortcut in chat to insert the message
                </p>
              </div>

              <div>
                <label className="block text-sm font-medium text-primary-700 dark:text-sand-200 mb-2">
                  Category (Optional)
                </label>
                <input
                  type="text"
                  value={formData.category}
                  onChange={(e) => setFormData(prev => ({ ...prev, category: e.target.value }))}
                  placeholder="e.g., Booking, Support"
                  className="w-full px-3 py-2 border border-primary-300 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent"
                />
              </div>
            </div>

            <div>
              <label className="block text-sm font-medium text-primary-700 dark:text-sand-200 mb-2">
                Message *
              </label>
              <textarea
                value={formData.message_text}
                onChange={(e) => setFormData(prev => ({ ...prev, message_text: e.target.value }))}
                placeholder="Enter your quick reply message..."
                rows={4}
                className="w-full px-3 py-2 border border-primary-300 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent"
                required
              />
            </div>

            <div className="flex gap-2">
              <button
                type="submit"
                disabled={createMutation.isPending || updateMutation.isPending}
                className="inline-flex items-center gap-2 px-4 py-2 bg-secondary-600 text-white rounded-lg hover:bg-secondary-700 disabled:opacity-50 transition-colors"
              >
                {(createMutation.isPending || updateMutation.isPending) ? (
                  <>
                    <Loader2 className="h-4 w-4 animate-spin" />
                    <span>Saving...</span>
                  </>
                ) : (
                  <>
                    <Save className="h-4 w-4" />
                    <span>{editingId ? 'Update' : 'Create'}</span>
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

        {quickReplies.length === 0 ? (
          <div className="text-center py-12 text-primary-400 dark:text-sand-500">
            <p>No quick replies yet. Create your first one to get started!</p>
          </div>
        ) : (
          <div className="space-y-3">
            {quickReplies.map((reply) => (
              <div key={reply.id} className="border rounded-lg p-4 hover:bg-sand-50 dark:hover:bg-primary-800 transition-colors">
                <div className="flex items-start justify-between">
                  <div className="flex-1">
                    <div className="flex items-center gap-2 mb-2">
                      <span className="px-2 py-0.5 bg-blue-100 text-blue-700 text-xs rounded font-mono">
                        {reply.shortcut}
                      </span>
                      {reply.category && (
                        <span className="px-2 py-0.5 bg-primary-100 dark:bg-primary-800 text-primary-500 dark:text-sand-400 text-xs rounded">
                          {reply.category}
                        </span>
                      )}
                      {reply.use_count > 0 && (
                        <span className="text-xs text-primary-300 dark:text-primary-500">
                          Used {reply.use_count} times
                        </span>
                      )}
                    </div>
                    <p className="text-sm text-primary-500 dark:text-sand-400 whitespace-pre-wrap">
                      {reply.message_text}
                    </p>
                  </div>

                  <div className="flex gap-2 ml-4">
                    <button
                      onClick={() => handleEdit(reply)}
                      className="p-2 text-blue-600 hover:bg-blue-50 rounded transition-colors"
                      aria-label="Edit"
                    >
                      <Edit2 className="h-4 w-4" />
                    </button>

                    <button
                      onClick={() => handleDelete(reply.id)}
                      disabled={deleteMutation.isPending}
                      className="p-2 text-red-600 hover:bg-red-50 rounded transition-colors disabled:opacity-50"
                      aria-label="Delete"
                    >
                      <Trash2 className="h-4 w-4" />
                    </button>
                  </div>
                </div>
              </div>
            ))}
          </div>
        )}
      </div>
    </div>
  );
};
