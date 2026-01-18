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
  const [editingId, setEditingId] = useState<string | null>(null);
  const [formData, setFormData] = useState({ label: '', message: '', shortcut: '' });

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
    mutationFn: async (data: Omit<QuickReply, 'id' | 'user' | 'created_at' | 'updated_at'>) => {
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
    mutationFn: async ({ id, data }: { id: string; data: Partial<QuickReply> }) => {
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
    mutationFn: async (id: string) => {
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
    setFormData({ label: '', message: '', shortcut: '' });
    setIsAdding(false);
    setEditingId(null);
  };

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!formData.label.trim() || !formData.message.trim()) {
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
      label: reply.label,
      message: reply.message,
      shortcut: reply.shortcut || '',
    });
    setEditingId(reply.id);
    setIsAdding(true);
  };

  const handleDelete = (id: string) => {
    if (confirm('Are you sure you want to delete this quick reply?')) {
      deleteMutation.mutate(id);
    }
  };

  if (isLoading) {
    return (
      <div className="flex items-center justify-center p-8">
        <Loader2 className="h-8 w-8 animate-spin text-gray-400" />
      </div>
    );
  }

  return (
    <div className="bg-white rounded-lg shadow-sm border">
      <div className="p-6 border-b">
        <div className="flex items-center justify-between">
          <div>
            <h2 className="text-xl font-semibold text-gray-900">Quick Replies</h2>
            <p className="text-sm text-gray-600 mt-1">
              Create reusable message templates for faster responses
            </p>
          </div>

          {!isAdding && (
            <button
              onClick={() => setIsAdding(true)}
              className="inline-flex items-center gap-2 px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
            >
              <Plus className="h-4 w-4" />
              <span>Add Quick Reply</span>
            </button>
          )}
        </div>
      </div>

      <div className="p-6 space-y-4">
        {isAdding && (
          <form onSubmit={handleSubmit} className="border rounded-lg p-4 bg-gray-50 space-y-4">
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Label *
              </label>
              <input
                type="text"
                value={formData.label}
                onChange={(e) => setFormData(prev => ({ ...prev, label: e.target.value }))}
                placeholder="e.g., Welcome Message"
                className="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                required
              />
            </div>

            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Message *
              </label>
              <textarea
                value={formData.message}
                onChange={(e) => setFormData(prev => ({ ...prev, message: e.target.value }))}
                placeholder="Enter your quick reply message..."
                rows={4}
                className="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                required
              />
            </div>

            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Shortcut (Optional)
              </label>
              <input
                type="text"
                value={formData.shortcut}
                onChange={(e) => setFormData(prev => ({ ...prev, shortcut: e.target.value }))}
                placeholder="e.g., /welcome"
                className="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent"
              />
              <p className="text-xs text-gray-500 mt-1">
                Type this shortcut in chat to quickly insert this message
              </p>
            </div>

            <div className="flex gap-2">
              <button
                type="submit"
                disabled={createMutation.isPending || updateMutation.isPending}
                className="inline-flex items-center gap-2 px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 disabled:opacity-50 transition-colors"
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
                className="inline-flex items-center gap-2 px-4 py-2 border border-gray-300 rounded-lg hover:bg-gray-50 transition-colors"
              >
                <X className="h-4 w-4" />
                <span>Cancel</span>
              </button>
            </div>
          </form>
        )}

        {quickReplies.length === 0 ? (
          <div className="text-center py-12 text-gray-500">
            <p>No quick replies yet. Create your first one to get started!</p>
          </div>
        ) : (
          <div className="space-y-3">
            {quickReplies.map((reply) => (
              <div key={reply.id} className="border rounded-lg p-4 hover:bg-gray-50 transition-colors">
                <div className="flex items-start justify-between">
                  <div className="flex-1">
                    <div className="flex items-center gap-2 mb-2">
                      <h3 className="font-medium text-gray-900">{reply.label}</h3>
                      {reply.shortcut && (
                        <span className="px-2 py-0.5 bg-blue-100 text-blue-700 text-xs rounded font-mono">
                          {reply.shortcut}
                        </span>
                      )}
                    </div>
                    <p className="text-sm text-gray-600 whitespace-pre-wrap">
                      {reply.message}
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
