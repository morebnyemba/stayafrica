'use client';

import { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import axios from 'axios';
import { MessageTemplate } from '@/types/automated-messaging-types';
import { Plus, Edit2, Trash2, Loader2, Save, X, Eye } from 'lucide-react';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8000';

const extractVariables = (content: string): string[] => {
  const regex = /\{([^}]+)\}/g;
  const matches = content.matchAll(regex);
  return Array.from(new Set(Array.from(matches, m => m[1])));
};

export const MessageTemplateEditor = () => {
  const queryClient = useQueryClient();
  const [isCreating, setIsCreating] = useState(false);
  const [editingId, setEditingId] = useState<string | null>(null);
  const [previewMode, setPreviewMode] = useState(false);
  const [formData, setFormData] = useState({
    name: '',
    subject: '',
    content: '',
    category: '',
  });
  const [previewData, setPreviewData] = useState<Record<string, string>>({});

  const { data: templates = [], isLoading } = useQuery<MessageTemplate[]>({
    queryKey: ['message-templates'],
    queryFn: async () => {
      const token = localStorage.getItem('access_token');
      const response = await axios.get(
        `${API_BASE_URL}/api/v1/messaging/templates/`,
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
    mutationFn: async (data: typeof formData & { variables: string[] }) => {
      const token = localStorage.getItem('access_token');
      const response = await axios.post(
        `${API_BASE_URL}/api/v1/messaging/templates/`,
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
      queryClient.invalidateQueries({ queryKey: ['message-templates'] });
      resetForm();
    },
  });

  const updateMutation = useMutation({
    mutationFn: async ({ id, data }: { id: string; data: Partial<MessageTemplate> }) => {
      const token = localStorage.getItem('access_token');
      const response = await axios.patch(
        `${API_BASE_URL}/api/v1/messaging/templates/${id}/`,
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
      queryClient.invalidateQueries({ queryKey: ['message-templates'] });
      resetForm();
    },
  });

  const deleteMutation = useMutation({
    mutationFn: async (id: string) => {
      const token = localStorage.getItem('access_token');
      await axios.delete(
        `${API_BASE_URL}/api/v1/messaging/templates/${id}/`,
        {
          headers: {
            Authorization: `Bearer ${token}`,
          },
        }
      );
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['message-templates'] });
    },
  });

  const resetForm = () => {
    setFormData({ name: '', subject: '', content: '', category: '' });
    setPreviewData({});
    setIsCreating(false);
    setEditingId(null);
    setPreviewMode(false);
  };

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!formData.name.trim() || !formData.content.trim()) {
      return;
    }

    const variables = extractVariables(formData.content);

    if (editingId) {
      updateMutation.mutate({ 
        id: editingId, 
        data: { ...formData, variables } 
      });
    } else {
      createMutation.mutate({ ...formData, variables });
    }
  };

  const handleEdit = (template: MessageTemplate) => {
    setFormData({
      name: template.name,
      subject: template.subject || '',
      content: template.content,
      category: template.category || '',
    });
    
    // Initialize preview data with variable names
    const initialPreview: Record<string, string> = {};
    template.variables.forEach(v => {
      initialPreview[v] = '';
    });
    setPreviewData(initialPreview);
    
    setEditingId(template.id);
    setIsCreating(true);
  };

  const handleDelete = (id: string) => {
    if (confirm('Are you sure you want to delete this template?')) {
      deleteMutation.mutate(id);
    }
  };

  const renderPreview = () => {
    let preview = formData.content;
    Object.entries(previewData).forEach(([key, value]) => {
      preview = preview.replace(new RegExp(`\\{${key}\\}`, 'g'), value || `{${key}}`);
    });
    return preview;
  };

  const variables = extractVariables(formData.content);

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
            <h2 className="text-xl font-semibold text-gray-900">Message Templates</h2>
            <p className="text-sm text-gray-600 mt-1">
              Create reusable message templates with variable substitution
            </p>
          </div>

          {!isCreating && (
            <button
              onClick={() => setIsCreating(true)}
              className="inline-flex items-center gap-2 px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
            >
              <Plus className="h-4 w-4" />
              <span>New Template</span>
            </button>
          )}
        </div>
      </div>

      <div className="p-6 space-y-4">
        {isCreating && (
          <form onSubmit={handleSubmit} className="border rounded-lg p-4 bg-gray-50 space-y-4">
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Template Name *
                </label>
                <input
                  type="text"
                  value={formData.name}
                  onChange={(e) => setFormData(prev => ({ ...prev, name: e.target.value }))}
                  placeholder="e.g., Welcome Message"
                  className="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                  required
                />
              </div>

              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Category (Optional)
                </label>
                <input
                  type="text"
                  value={formData.category}
                  onChange={(e) => setFormData(prev => ({ ...prev, category: e.target.value }))}
                  placeholder="e.g., Booking, Support"
                  className="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                />
              </div>
            </div>

            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Subject (Optional)
              </label>
              <input
                type="text"
                value={formData.subject}
                onChange={(e) => setFormData(prev => ({ ...prev, subject: e.target.value }))}
                placeholder="Email subject (if applicable)"
                className="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent"
              />
            </div>

            <div>
              <div className="flex items-center justify-between mb-2">
                <label className="block text-sm font-medium text-gray-700">
                  Message Content *
                </label>
                <button
                  type="button"
                  onClick={() => setPreviewMode(!previewMode)}
                  className="text-sm text-blue-600 hover:text-blue-700 flex items-center gap-1"
                >
                  <Eye className="h-4 w-4" />
                  {previewMode ? 'Edit' : 'Preview'}
                </button>
              </div>

              {!previewMode ? (
                <>
                  <textarea
                    value={formData.content}
                    onChange={(e) => setFormData(prev => ({ ...prev, content: e.target.value }))}
                    placeholder="Enter template content... Use {variable_name} for variables"
                    rows={6}
                    className="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent font-mono text-sm"
                    required
                  />
                  <p className="text-xs text-gray-500 mt-1">
                    Use curly braces for variables: {'{guest_name}'}, {'{property_name}'}, etc.
                  </p>
                </>
              ) : (
                <div className="space-y-3">
                  {variables.map(variable => (
                    <div key={variable}>
                      <label className="block text-xs font-medium text-gray-600 mb-1">
                        {variable}
                      </label>
                      <input
                        type="text"
                        value={previewData[variable] || ''}
                        onChange={(e) => setPreviewData(prev => ({ ...prev, [variable]: e.target.value }))}
                        placeholder={`Enter ${variable}...`}
                        className="w-full px-3 py-2 border border-gray-300 rounded-lg text-sm"
                      />
                    </div>
                  ))}
                  <div className="bg-white border rounded-lg p-4 mt-3">
                    <p className="text-sm text-gray-500 mb-2">Preview:</p>
                    <p className="text-sm text-gray-900 whitespace-pre-wrap">
                      {renderPreview()}
                    </p>
                  </div>
                </div>
              )}
            </div>

            {variables.length > 0 && !previewMode && (
              <div className="bg-blue-50 border border-blue-200 rounded p-3">
                <p className="text-xs font-medium text-blue-900 mb-1">Detected Variables:</p>
                <div className="flex flex-wrap gap-2">
                  {variables.map(v => (
                    <span key={v} className="px-2 py-1 bg-blue-100 text-blue-800 text-xs rounded">
                      {'{' + v + '}'}
                    </span>
                  ))}
                </div>
              </div>
            )}

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

        {templates.length === 0 ? (
          <div className="text-center py-12 text-gray-500">
            <p>No templates yet. Create your first template!</p>
          </div>
        ) : (
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            {templates.map((template) => (
              <div key={template.id} className="border rounded-lg p-4 hover:bg-gray-50 transition-colors">
                <div className="flex items-start justify-between mb-3">
                  <div className="flex-1">
                    <h3 className="font-medium text-gray-900">{template.name}</h3>
                    {template.category && (
                      <span className="text-xs text-gray-500">{template.category}</span>
                    )}
                  </div>

                  <div className="flex gap-2">
                    <button
                      onClick={() => handleEdit(template)}
                      className="p-1.5 text-blue-600 hover:bg-blue-50 rounded transition-colors"
                      aria-label="Edit"
                    >
                      <Edit2 className="h-4 w-4" />
                    </button>

                    <button
                      onClick={() => handleDelete(template.id)}
                      disabled={deleteMutation.isPending}
                      className="p-1.5 text-red-600 hover:bg-red-50 rounded transition-colors disabled:opacity-50"
                      aria-label="Delete"
                    >
                      <Trash2 className="h-4 w-4" />
                    </button>
                  </div>
                </div>

                <p className="text-sm text-gray-600 line-clamp-3 mb-2">
                  {template.content}
                </p>

                {template.variables.length > 0 && (
                  <div className="flex flex-wrap gap-1 mt-2">
                    {template.variables.map(v => (
                      <span key={v} className="px-2 py-0.5 bg-gray-100 text-gray-700 text-xs rounded">
                        {'{' + v + '}'}
                      </span>
                    ))}
                  </div>
                )}
              </div>
            ))}
          </div>
        )}
      </div>
    </div>
  );
};
