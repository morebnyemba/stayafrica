'use client';

import { useAuth } from '@/store/auth-store';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { pricingApi } from '@/services/pricing-api';
import Link from 'next/link';
import { useState } from 'react';
import toast from 'react-hot-toast';
import type { PricingRule, PricingRuleFormData, PricingRuleType } from '@/types/pricing-types';
import { 
  TrendingUp, Calendar, DollarSign, Building, Percent,
  AlertCircle, Lightbulb, Plus, Pencil, Trash2, Power, X, Eye,
} from 'lucide-react';

const RULE_TYPE_OPTIONS: { value: PricingRuleType; label: string; icon: typeof TrendingUp; color: string; description: string }[] = [
  { value: 'seasonal', label: 'Seasonal', icon: TrendingUp, color: 'text-green-600 bg-green-100', description: 'Adjust prices during specific date ranges (holidays, peak seasons)' },
  { value: 'weekend', label: 'Weekend', icon: Calendar, color: 'text-blue-600 bg-blue-100', description: 'Automatically adjust rates for Friday–Sunday bookings' },
  { value: 'length_discount', label: 'Length of Stay', icon: Percent, color: 'text-purple-600 bg-purple-100', description: 'Offer discounts for bookings that meet minimum night requirements' },
  { value: 'early_bird', label: 'Early Bird', icon: DollarSign, color: 'text-amber-600 bg-amber-100', description: 'Reward guests who book well in advance' },
  { value: 'last_minute', label: 'Last Minute', icon: AlertCircle, color: 'text-red-600 bg-red-100', description: 'Fill vacancies with last-minute discounts' },
];

const EMPTY_FORM: PricingRuleFormData = {
  property: '',
  name: '',
  rule_type: 'seasonal',
  is_active: true,
  priority: 0,
  adjustment_type: 'percentage',
  adjustment_value: 10,
  start_date: null,
  end_date: null,
  min_nights: null,
  max_nights: null,
  min_days_advance: null,
  max_days_advance: null,
};

export default function DynamicPricingPage() {
  const { user, isAuthenticated } = useAuth();
  const queryClient = useQueryClient();
  const [showForm, setShowForm] = useState(false);
  const [editingRule, setEditingRule] = useState<PricingRule | null>(null);
  const [form, setForm] = useState<PricingRuleFormData>(EMPTY_FORM);
  const [filterProperty, setFilterProperty] = useState<string>('all');
  const [deleteConfirm, setDeleteConfirm] = useState<number | null>(null);

  const { data: propertiesData, isLoading: propertiesLoading } = useQuery({
    queryKey: ['host', 'properties'],
    queryFn: async () => {
      const response = await apiClient.getHostProperties();
      return response.data;
    },
    enabled: isAuthenticated && user?.role === 'host',
  });

  const { data: rules = [], isLoading: rulesLoading } = useQuery({
    queryKey: ['pricing-rules'],
    queryFn: () => pricingApi.getPricingRules(),
    enabled: isAuthenticated && user?.role === 'host',
  });

  const properties = propertiesData?.results || [];

  const filteredRules = filterProperty === 'all'
    ? rules
    : rules.filter((r: PricingRule) => String(r.property) === filterProperty);

  const propertyNameMap: Record<string, string> = {};
  properties.forEach((p: any) => { if (p?.id) propertyNameMap[String(p.id)] = p.title; });

  const createMutation = useMutation({
    mutationFn: (data: PricingRuleFormData) => pricingApi.createPricingRule(data),
    onSuccess: () => {
      toast.success('Pricing rule created');
      queryClient.invalidateQueries({ queryKey: ['pricing-rules'] });
      closeForm();
    },
    onError: (err: any) => toast.error(err?.response?.data?.detail || 'Failed to create rule'),
  });

  const updateMutation = useMutation({
    mutationFn: ({ id, data }: { id: number; data: Partial<PricingRuleFormData> }) =>
      pricingApi.updatePricingRule(id, data),
    onSuccess: () => {
      toast.success('Pricing rule updated');
      queryClient.invalidateQueries({ queryKey: ['pricing-rules'] });
      closeForm();
    },
    onError: (err: any) => toast.error(err?.response?.data?.detail || 'Failed to update rule'),
  });

  const deleteMutation = useMutation({
    mutationFn: (id: number) => pricingApi.deletePricingRule(id),
    onSuccess: () => {
      toast.success('Pricing rule deleted');
      queryClient.invalidateQueries({ queryKey: ['pricing-rules'] });
      setDeleteConfirm(null);
    },
    onError: (err: any) => toast.error(err?.response?.data?.detail || 'Failed to delete rule'),
  });

  const toggleMutation = useMutation({
    mutationFn: ({ id, is_active }: { id: number; is_active: boolean }) =>
      pricingApi.updatePricingRule(id, { is_active }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['pricing-rules'] });
    },
    onError: (err: any) => toast.error(err?.response?.data?.detail || 'Failed to toggle rule'),
  });

  function closeForm() {
    setShowForm(false);
    setEditingRule(null);
    setForm(EMPTY_FORM);
  }

  function openCreateForm() {
    setEditingRule(null);
    setForm({ ...EMPTY_FORM, property: properties[0]?.id || '' });
    setShowForm(true);
  }

  function openEditForm(rule: PricingRule) {
    setEditingRule(rule);
    setForm({
      property: rule.property,
      name: rule.name,
      rule_type: rule.rule_type,
      is_active: rule.is_active,
      priority: rule.priority,
      adjustment_type: rule.adjustment_type,
      adjustment_value: rule.adjustment_value,
      start_date: rule.start_date || null,
      end_date: rule.end_date || null,
      min_nights: rule.min_nights ?? null,
      max_nights: rule.max_nights ?? null,
      min_days_advance: rule.min_days_advance ?? null,
      max_days_advance: rule.max_days_advance ?? null,
    });
    setShowForm(true);
  }

  function handleSubmit(e: React.FormEvent) {
    e.preventDefault();
    if (!form.property) { toast.error('Select a property'); return; }
    if (!form.name.trim()) { toast.error('Enter a rule name'); return; }
    if (!form.adjustment_value || form.adjustment_value === 0) { toast.error('Adjustment value cannot be zero'); return; }

    const payload = { ...form };
    // Clean conditional fields
    if (form.rule_type !== 'seasonal') { payload.start_date = null; payload.end_date = null; }
    if (form.rule_type !== 'length_discount') { payload.min_nights = null; payload.max_nights = null; }
    if (form.rule_type !== 'early_bird' && form.rule_type !== 'last_minute') {
      payload.min_days_advance = null; payload.max_days_advance = null;
    }

    if (editingRule) {
      updateMutation.mutate({ id: editingRule.id, data: payload });
    } else {
      createMutation.mutate(payload);
    }
  }

  const ruleTypeInfo = RULE_TYPE_OPTIONS.reduce((acc, r) => { acc[r.value] = r; return acc; }, {} as Record<string, typeof RULE_TYPE_OPTIONS[0]>);

  const isSubmitting = createMutation.isPending || updateMutation.isPending;

  return (
    <div className="min-h-screen bg-sand-100">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Header */}
        <div className="flex flex-col sm:flex-row sm:items-center sm:justify-between gap-4 mb-8">
          <div>
            <h1 className="text-3xl md:text-4xl font-bold text-primary-900 mb-1">Dynamic Pricing</h1>
            <p className="text-lg text-primary-600">Create and manage pricing rules across your properties</p>
          </div>
          {properties.length > 0 && (
            <button
              onClick={openCreateForm}
              className="inline-flex items-center gap-2 px-5 py-2.5 bg-secondary-600 text-white rounded-lg hover:bg-secondary-700 transition-colors font-medium"
            >
              <Plus className="w-5 h-5" />
              Add Pricing Rule
            </button>
          )}
        </div>

        {/* Rule Type Cards */}
        <div className="grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-5 gap-4 mb-8">
          {RULE_TYPE_OPTIONS.map((rt) => {
            const Icon = rt.icon;
            const count = rules.filter((r: PricingRule) => r.rule_type === rt.value).length;
            return (
              <div key={rt.value} className="bg-white rounded-lg shadow-sm border border-primary-200 p-4 text-center">
                <div className={`w-10 h-10 rounded-lg flex items-center justify-center mx-auto mb-2 ${rt.color}`}>
                  <Icon className="w-5 h-5" />
                </div>
                <p className="font-medium text-primary-900 text-sm">{rt.label}</p>
                <p className="text-xs text-primary-500 mt-1">{count} rule{count !== 1 ? 's' : ''}</p>
              </div>
            );
          })}
        </div>

        {/* Rules Table */}
        <div className="bg-white rounded-lg shadow-sm border border-primary-200 mb-8">
          <div className="p-6 border-b border-primary-200 flex flex-col sm:flex-row sm:items-center sm:justify-between gap-4">
            <div>
              <h2 className="text-xl font-bold text-primary-900">Pricing Rules</h2>
              <p className="text-sm text-primary-600 mt-1">{filteredRules.length} rule{filteredRules.length !== 1 ? 's' : ''}</p>
            </div>
            <select
              value={filterProperty}
              onChange={(e) => setFilterProperty(e.target.value)}
              className="px-3 py-2 border border-primary-300 rounded-lg text-sm bg-white"
            >
              <option value="all">All Properties</option>
              {properties.filter((p: any) => p?.id).map((p: any) => (
                <option key={p.id} value={String(p.id)}>{p.title}</option>
              ))}
            </select>
          </div>

          {(propertiesLoading || rulesLoading) ? (
            <div className="p-6 space-y-3">
              {[1, 2, 3].map((i) => (
                <div key={i} className="animate-pulse h-16 bg-primary-100 rounded-lg" />
              ))}
            </div>
          ) : filteredRules.length === 0 ? (
            <div className="p-12 text-center">
              <TrendingUp className="w-12 h-12 text-primary-300 mx-auto mb-3" />
              <h3 className="text-lg font-semibold text-primary-900 mb-1">No Pricing Rules</h3>
              <p className="text-primary-600 mb-4 text-sm">
                {properties.length === 0
                  ? 'Add a property first, then create pricing rules.'
                  : 'Create your first pricing rule to optimize your earnings.'}
              </p>
              {properties.length > 0 && (
                <button onClick={openCreateForm} className="inline-flex items-center gap-2 px-4 py-2 bg-secondary-600 text-white rounded-lg hover:bg-secondary-700 transition text-sm">
                  <Plus className="w-4 h-4" /> Add Rule
                </button>
              )}
            </div>
          ) : (
            <div className="divide-y divide-primary-100">
              {filteredRules.map((rule: PricingRule) => {
                const rt = ruleTypeInfo[rule.rule_type];
                const Icon = rt?.icon || TrendingUp;
                return (
                  <div key={rule.id} className={`flex items-center justify-between p-4 sm:p-5 hover:bg-primary-50/50 transition ${!rule.is_active ? 'opacity-60' : ''}`}>
                    <div className="flex items-center gap-4 min-w-0 flex-1">
                      <div className={`w-10 h-10 rounded-lg flex items-center justify-center flex-shrink-0 ${rt?.color || 'text-primary-600 bg-primary-100'}`}>
                        <Icon className="w-5 h-5" />
                      </div>
                      <div className="min-w-0">
                        <div className="flex items-center gap-2 flex-wrap">
                          <h3 className="font-semibold text-primary-900 truncate">{rule.name}</h3>
                          <span className={`px-2 py-0.5 text-xs font-medium rounded-full ${rule.is_active ? 'bg-green-100 text-green-700' : 'bg-primary-100 text-primary-500'}`}>
                            {rule.is_active ? 'Active' : 'Inactive'}
                          </span>
                        </div>
                        <p className="text-sm text-primary-500 truncate">
                          {propertyNameMap[String(rule.property)] || `Property #${rule.property}`}
                          {' • '}
                          {rt?.label || rule.rule_type}
                          {' • '}
                          {rule.adjustment_type === 'percentage'
                            ? (['length_discount', 'early_bird', 'last_minute'].includes(rule.rule_type) ? `-${rule.adjustment_value}%` : `+${rule.adjustment_value}%`)
                            : (['length_discount', 'early_bird', 'last_minute'].includes(rule.rule_type) ? `-$${rule.adjustment_value}` : `+$${rule.adjustment_value}`)}
                          {rule.start_date && rule.end_date && ` • ${rule.start_date} → ${rule.end_date}`}
                          {rule.min_nights && ` • ${rule.min_nights}${rule.max_nights ? `–${rule.max_nights}` : '+'} nights`}
                          {rule.min_days_advance != null && ` • ${rule.min_days_advance}${rule.max_days_advance != null ? `–${rule.max_days_advance}` : '+'} days advance`}
                        </p>
                      </div>
                    </div>
                    <div className="flex items-center gap-1 flex-shrink-0 ml-4">
                      <button
                        onClick={() => toggleMutation.mutate({ id: rule.id, is_active: !rule.is_active })}
                        className={`p-2 rounded-lg transition ${rule.is_active ? 'text-green-600 hover:bg-green-50' : 'text-primary-400 hover:bg-primary-100'}`}
                        title={rule.is_active ? 'Deactivate' : 'Activate'}
                      >
                        <Power className="w-4 h-4" />
                      </button>
                      <button onClick={() => openEditForm(rule)} className="p-2 text-primary-500 hover:text-primary-900 hover:bg-primary-100 rounded-lg transition" title="Edit">
                        <Pencil className="w-4 h-4" />
                      </button>
                      <button
                        onClick={() => setDeleteConfirm(rule.id)}
                        className="p-2 text-red-400 hover:text-red-600 hover:bg-red-50 rounded-lg transition"
                        title="Delete"
                      >
                        <Trash2 className="w-4 h-4" />
                      </button>
                    </div>
                  </div>
                );
              })}
            </div>
          )}
        </div>

        {/* Property Pricing Calendars */}
        <div className="bg-white rounded-lg shadow-sm border border-primary-200 mb-8">
          <div className="p-6 border-b border-primary-200">
            <h2 className="text-xl font-bold text-primary-900">Property Pricing Calendars</h2>
            <p className="text-sm text-primary-600 mt-1">View how rules affect daily prices for each property</p>
          </div>
          {propertiesLoading ? (
            <div className="p-6 space-y-3">
              {[1, 2].map((i) => <div key={i} className="animate-pulse h-16 bg-primary-100 rounded-lg" />)}
            </div>
          ) : properties.length > 0 ? (
            <div className="divide-y divide-primary-100">
              {properties.filter((p: any) => p?.id).map((property: any) => {
                const ruleCount = rules.filter((r: PricingRule) => String(r.property) === String(property.id)).length;
                return (
                  <Link
                    key={property.id}
                    href={`/host/properties/${property.id}/pricing`}
                    className="flex items-center justify-between p-5 hover:bg-primary-50 transition-colors"
                  >
                    <div className="flex items-center gap-4">
                      <div className="p-3 bg-primary-100 rounded-lg">
                        <Building className="w-6 h-6 text-primary-600" />
                      </div>
                      <div>
                        <h3 className="font-semibold text-primary-900">{property.title}</h3>
                        <p className="text-sm text-primary-500">
                          {property.city}, {property.country} • {property.currency || 'USD'} {property.price_per_night}/night
                          <span className="ml-2 text-secondary-600 font-medium">{ruleCount} rule{ruleCount !== 1 ? 's' : ''}</span>
                        </p>
                      </div>
                    </div>
                    <div className="flex items-center gap-2">
                      <span className="text-xs text-primary-500 hidden sm:inline">View Calendar</span>
                      <Eye className="w-5 h-5 text-primary-400" />
                    </div>
                  </Link>
                );
              })}
            </div>
          ) : (
            <div className="p-12 text-center">
              <Building className="w-14 h-14 text-primary-300 mx-auto mb-3" />
              <h3 className="text-lg font-semibold text-primary-900 mb-2">No Properties Yet</h3>
              <p className="text-primary-600 mb-4">Add a property to start managing its pricing.</p>
              <Link href="/host/properties/new" className="inline-flex items-center gap-2 px-5 py-2.5 bg-secondary-600 text-white rounded-lg hover:bg-secondary-700 transition">
                Add Property
              </Link>
            </div>
          )}
        </div>

        {/* Tips */}
        <div className="bg-blue-50 border border-blue-200 rounded-lg p-6">
          <h3 className="font-semibold text-blue-900 mb-3 flex items-center gap-2">
            <Lightbulb className="h-5 w-5" /> Pricing Tips
          </h3>
          <ul className="space-y-2 text-sm text-blue-800">
            <li>• Use <strong>seasonal pricing</strong> to capture peak demand during holidays and events</li>
            <li>• <strong>Early bird</strong> discounts of 10–15% help secure bookings months in advance</li>
            <li>• <strong>Last minute</strong> discounts of 15–25% fill vacancies close to check-in</li>
            <li>• <strong>Weekend premiums</strong> of 10–20% are common in most African travel markets</li>
            <li>• <strong>Length of stay</strong> discounts (e.g., 10% for 7+ nights) encourage longer bookings</li>
            <li>• Higher <strong>priority</strong> rules are applied first when multiple rules match a booking</li>
          </ul>
        </div>
      </div>

      {/* Create/Edit Modal */}
      {showForm && (
        <div className="fixed inset-0 bg-black/40 z-50 flex items-center justify-center p-4" onClick={() => closeForm()}>
          <div className="bg-white rounded-xl shadow-xl w-full max-w-lg max-h-[90vh] overflow-y-auto" onClick={(e) => e.stopPropagation()}>
            <div className="flex items-center justify-between p-6 border-b border-primary-200">
              <h2 className="text-xl font-bold text-primary-900">
                {editingRule ? 'Edit Pricing Rule' : 'Create Pricing Rule'}
              </h2>
              <button onClick={closeForm} className="p-2 hover:bg-primary-100 rounded-lg transition">
                <X className="w-5 h-5 text-primary-500" />
              </button>
            </div>
            <form onSubmit={handleSubmit} className="p-6 space-y-5">
              {/* Property */}
              <div>
                <label className="block text-sm font-medium text-primary-700 mb-1">Property *</label>
                <select
                  value={String(form.property)}
                  onChange={(e) => setForm({ ...form, property: e.target.value })}
                  className="w-full px-3 py-2 border border-primary-300 rounded-lg text-sm"
                  required
                >
                  <option value="">Select property...</option>
                  {properties.filter((p: any) => p?.id).map((p: any) => (
                    <option key={p.id} value={String(p.id)}>{p.title}</option>
                  ))}
                </select>
              </div>

              {/* Rule Name */}
              <div>
                <label className="block text-sm font-medium text-primary-700 mb-1">Rule Name *</label>
                <input
                  type="text"
                  value={form.name}
                  onChange={(e) => setForm({ ...form, name: e.target.value })}
                  placeholder="e.g., Christmas Peak Season"
                  className="w-full px-3 py-2 border border-primary-300 rounded-lg text-sm"
                  required
                />
              </div>

              {/* Rule Type */}
              <div>
                <label className="block text-sm font-medium text-primary-700 mb-1">Rule Type *</label>
                <select
                  value={form.rule_type}
                  onChange={(e) => setForm({ ...form, rule_type: e.target.value as PricingRuleType })}
                  className="w-full px-3 py-2 border border-primary-300 rounded-lg text-sm"
                >
                  {RULE_TYPE_OPTIONS.map((rt) => (
                    <option key={rt.value} value={rt.value}>{rt.label} — {rt.description}</option>
                  ))}
                </select>
              </div>

              {/* Adjustment */}
              <div className="grid grid-cols-2 gap-4">
                <div>
                  <label className="block text-sm font-medium text-primary-700 mb-1">Adjustment Type</label>
                  <select
                    value={form.adjustment_type}
                    onChange={(e) => setForm({ ...form, adjustment_type: e.target.value as 'percentage' | 'fixed' })}
                    className="w-full px-3 py-2 border border-primary-300 rounded-lg text-sm"
                  >
                    <option value="percentage">Percentage (%)</option>
                    <option value="fixed">Fixed Amount ($)</option>
                  </select>
                </div>
                <div>
                  <label className="block text-sm font-medium text-primary-700 mb-1">
                    {['length_discount', 'early_bird', 'last_minute'].includes(form.rule_type) ? 'Discount' : 'Increase'}{' '}
                    {form.adjustment_type === 'percentage' ? '(%)' : '($)'} *
                  </label>
                  <input
                    type="number"
                    step="0.01"
                    min="0.01"
                    value={form.adjustment_value}
                    onChange={(e) => setForm({ ...form, adjustment_value: parseFloat(e.target.value) || 0 })}
                    className="w-full px-3 py-2 border border-primary-300 rounded-lg text-sm"
                    required
                  />
                  <p className="text-xs text-primary-500 mt-1">
                    {['length_discount', 'early_bird', 'last_minute'].includes(form.rule_type)
                      ? 'Enter the discount amount (e.g. 10 = 10% off)'
                      : 'Enter the surcharge amount (e.g. 20 = 20% more)'}
                  </p>
                </div>
              </div>

              {/* Seasonal: Date Range */}
              {form.rule_type === 'seasonal' && (
                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <label className="block text-sm font-medium text-primary-700 mb-1">Start Date</label>
                    <input
                      type="date"
                      lang="en-GB"
                      value={form.start_date || ''}
                      onChange={(e) => setForm({ ...form, start_date: e.target.value || null })}
                      className="w-full px-3 py-2 border border-primary-300 rounded-lg text-sm"
                    />
                  </div>
                  <div>
                    <label className="block text-sm font-medium text-primary-700 mb-1">End Date</label>
                    <input
                      type="date"
                      lang="en-GB"
                      value={form.end_date || ''}
                      onChange={(e) => setForm({ ...form, end_date: e.target.value || null })}
                      className="w-full px-3 py-2 border border-primary-300 rounded-lg text-sm"
                    />
                  </div>
                </div>
              )}

              {/* Length Discount: Min/Max Nights */}
              {form.rule_type === 'length_discount' && (
                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <label className="block text-sm font-medium text-primary-700 mb-1">Min Nights</label>
                    <input
                      type="number"
                      min="1"
                      value={form.min_nights ?? ''}
                      onChange={(e) => setForm({ ...form, min_nights: e.target.value ? parseInt(e.target.value) : null })}
                      placeholder="e.g., 7"
                      className="w-full px-3 py-2 border border-primary-300 rounded-lg text-sm"
                    />
                  </div>
                  <div>
                    <label className="block text-sm font-medium text-primary-700 mb-1">Max Nights</label>
                    <input
                      type="number"
                      min="1"
                      value={form.max_nights ?? ''}
                      onChange={(e) => setForm({ ...form, max_nights: e.target.value ? parseInt(e.target.value) : null })}
                      placeholder="e.g., 30"
                      className="w-full px-3 py-2 border border-primary-300 rounded-lg text-sm"
                    />
                  </div>
                </div>
              )}

              {/* Early Bird / Last Minute: Days Advance */}
              {(form.rule_type === 'early_bird' || form.rule_type === 'last_minute') && (
                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <label className="block text-sm font-medium text-primary-700 mb-1">Min Days Before Check-in</label>
                    <input
                      type="number"
                      min="0"
                      value={form.min_days_advance ?? ''}
                      onChange={(e) => setForm({ ...form, min_days_advance: e.target.value ? parseInt(e.target.value) : null })}
                      placeholder={form.rule_type === 'early_bird' ? 'e.g., 30' : 'e.g., 0'}
                      className="w-full px-3 py-2 border border-primary-300 rounded-lg text-sm"
                    />
                  </div>
                  <div>
                    <label className="block text-sm font-medium text-primary-700 mb-1">Max Days Before Check-in</label>
                    <input
                      type="number"
                      min="0"
                      value={form.max_days_advance ?? ''}
                      onChange={(e) => setForm({ ...form, max_days_advance: e.target.value ? parseInt(e.target.value) : null })}
                      placeholder={form.rule_type === 'early_bird' ? 'e.g., 90' : 'e.g., 7'}
                      className="w-full px-3 py-2 border border-primary-300 rounded-lg text-sm"
                    />
                  </div>
                </div>
              )}

              {/* Priority */}
              <div>
                <label className="block text-sm font-medium text-primary-700 mb-1">Priority</label>
                <input
                  type="number"
                  min="0"
                  value={form.priority}
                  onChange={(e) => setForm({ ...form, priority: parseInt(e.target.value) || 0 })}
                  className="w-full px-3 py-2 border border-primary-300 rounded-lg text-sm"
                />
                <p className="text-xs text-primary-500 mt-1">Higher priority rules are applied first</p>
              </div>

              {/* Active toggle */}
              <label className="flex items-center gap-3 cursor-pointer">
                <input
                  type="checkbox"
                  checked={form.is_active}
                  onChange={(e) => setForm({ ...form, is_active: e.target.checked })}
                  className="w-4 h-4 text-secondary-600 border-primary-300 rounded"
                />
                <span className="text-sm font-medium text-primary-700">Active</span>
              </label>

              {/* Submit */}
              <div className="flex justify-end gap-3 pt-2">
                <button type="button" onClick={closeForm} className="px-4 py-2 text-sm font-medium text-primary-700 bg-primary-100 rounded-lg hover:bg-primary-200 transition">
                  Cancel
                </button>
                <button
                  type="submit"
                  disabled={isSubmitting}
                  className="px-5 py-2 text-sm font-medium text-white bg-secondary-600 rounded-lg hover:bg-secondary-700 disabled:opacity-50 transition"
                >
                  {isSubmitting ? 'Saving...' : editingRule ? 'Update Rule' : 'Create Rule'}
                </button>
              </div>
            </form>
          </div>
        </div>
      )}

      {/* Delete Confirmation Modal */}
      {deleteConfirm !== null && (
        <div className="fixed inset-0 bg-black/40 z-50 flex items-center justify-center p-4" onClick={() => setDeleteConfirm(null)}>
          <div className="bg-white rounded-xl shadow-xl w-full max-w-sm p-6" onClick={(e) => e.stopPropagation()}>
            <h3 className="text-lg font-bold text-primary-900 mb-2">Delete Pricing Rule?</h3>
            <p className="text-sm text-primary-600 mb-6">This action cannot be undone. The rule will be permanently removed.</p>
            <div className="flex justify-end gap-3">
              <button onClick={() => setDeleteConfirm(null)} className="px-4 py-2 text-sm font-medium text-primary-700 bg-primary-100 rounded-lg hover:bg-primary-200 transition">
                Cancel
              </button>
              <button
                onClick={() => deleteMutation.mutate(deleteConfirm)}
                disabled={deleteMutation.isPending}
                className="px-4 py-2 text-sm font-medium text-white bg-red-600 rounded-lg hover:bg-red-700 disabled:opacity-50 transition"
              >
                {deleteMutation.isPending ? 'Deleting...' : 'Delete'}
              </button>
            </div>
          </div>
        </div>
      )}
    </div>
  );
}
