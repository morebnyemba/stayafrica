'use client';

import { useParams } from 'next/navigation';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { pricingApi } from '@/services/pricing-api';
import Link from 'next/link';
import { useState } from 'react';
import toast from 'react-hot-toast';
import { ArrowLeft, Plus, Pencil, Trash2, Power, TrendingUp, Calendar, Percent, DollarSign, AlertCircle } from 'lucide-react';
import PricingCalendar from '@/components/pricing/PricingCalendar';
import type { PricingRule, PricingRuleFormData, PricingRuleType } from '@/types/pricing-types';

const RULE_TYPE_META: Record<string, { label: string; color: string }> = {
  seasonal: { label: 'Seasonal', color: 'text-green-600 bg-green-100' },
  weekend: { label: 'Weekend', color: 'text-blue-600 bg-blue-100' },
  length_discount: { label: 'Length of Stay', color: 'text-purple-600 bg-purple-100' },
  early_bird: { label: 'Early Bird', color: 'text-amber-600 bg-amber-100' },
  last_minute: { label: 'Last Minute', color: 'text-red-600 bg-red-100' },
};

export default function PropertyPricingPage() {
  const params = useParams();
  const propertyId = params?.id as string;
  const queryClient = useQueryClient();

  const { data: property, isLoading } = useQuery({
    queryKey: ['property', propertyId],
    queryFn: async () => {
      const response = await apiClient.getPropertyById(propertyId);
      return response.data;
    },
    enabled: !!propertyId,
  });

  const { data: allRules = [] } = useQuery({
    queryKey: ['pricing-rules'],
    queryFn: () => pricingApi.getPricingRules(),
    enabled: !!propertyId,
  });

  const propertyRules = allRules.filter((r: PricingRule) => String(r.property) === String(propertyId));

  const deleteMutation = useMutation({
    mutationFn: (id: number) => pricingApi.deletePricingRule(id),
    onSuccess: () => {
      toast.success('Rule deleted');
      queryClient.invalidateQueries({ queryKey: ['pricing-rules'] });
    },
    onError: () => toast.error('Failed to delete rule'),
  });

  const toggleMutation = useMutation({
    mutationFn: ({ id, is_active }: { id: number; is_active: boolean }) =>
      pricingApi.updatePricingRule(id, { is_active }),
    onSuccess: () => queryClient.invalidateQueries({ queryKey: ['pricing-rules'] }),
    onError: () => toast.error('Failed to toggle rule'),
  });

  if (isLoading) {
    return (
      <div className="min-h-screen bg-sand-100">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          <div className="animate-pulse space-y-4">
            <div className="h-8 bg-primary-200 rounded w-1/3" />
            <div className="h-64 bg-primary-200 rounded" />
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-sand-100">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        <Link href="/host/pricing" className="inline-flex items-center gap-2 text-primary-600 hover:text-primary-900 mb-6 transition-colors">
          <ArrowLeft className="w-4 h-4" />
          <span>Back to Dynamic Pricing</span>
        </Link>

        <div className="flex flex-col sm:flex-row sm:items-center sm:justify-between gap-4 mb-8">
          <div>
            <h1 className="text-3xl md:text-4xl font-bold text-primary-900 mb-1">
              {property?.title || 'Property'} — Pricing
            </h1>
            <p className="text-lg text-primary-600">
              {property?.city}, {property?.country} • Base: {property?.currency || 'USD'} {property?.price_per_night}/night
            </p>
          </div>
          <Link
            href="/host/pricing"
            className="inline-flex items-center gap-2 px-4 py-2 bg-secondary-600 text-white rounded-lg hover:bg-secondary-700 transition text-sm font-medium"
          >
            <Plus className="w-4 h-4" /> Add Rule
          </Link>
        </div>

        {/* Active Rules for This Property */}
        {propertyRules.length > 0 && (
          <div className="bg-white rounded-lg shadow-sm border border-primary-200 mb-8">
            <div className="p-5 border-b border-primary-200">
              <h2 className="text-lg font-bold text-primary-900">Active Rules ({propertyRules.length})</h2>
            </div>
            <div className="divide-y divide-primary-100">
              {propertyRules.map((rule: PricingRule) => {
                const meta = RULE_TYPE_META[rule.rule_type] || { label: rule.rule_type, color: 'text-primary-600 bg-primary-100' };
                return (
                  <div key={rule.id} className={`flex items-center justify-between p-4 ${!rule.is_active ? 'opacity-50' : ''}`}>
                    <div className="flex items-center gap-3">
                      <span className={`px-2.5 py-1 text-xs font-semibold rounded-full ${meta.color}`}>{meta.label}</span>
                      <div>
                        <span className="font-medium text-primary-900">{rule.name}</span>
                        <span className="text-sm text-primary-500 ml-2">
                          {rule.adjustment_type === 'percentage' ? `${rule.adjustment_value > 0 ? '+' : ''}${rule.adjustment_value}%` : `$${rule.adjustment_value}`}
                          {rule.start_date && ` • ${rule.start_date} → ${rule.end_date}`}
                          {rule.min_nights && ` • ${rule.min_nights}+ nights`}
                        </span>
                      </div>
                    </div>
                    <div className="flex items-center gap-1">
                      <button onClick={() => toggleMutation.mutate({ id: rule.id, is_active: !rule.is_active })} className={`p-1.5 rounded-lg transition ${rule.is_active ? 'text-green-600 hover:bg-green-50' : 'text-primary-400 hover:bg-primary-100'}`}>
                        <Power className="w-4 h-4" />
                      </button>
                      <button onClick={() => { if (confirm('Delete this rule?')) deleteMutation.mutate(rule.id); }} className="p-1.5 text-red-400 hover:text-red-600 hover:bg-red-50 rounded-lg transition">
                        <Trash2 className="w-4 h-4" />
                      </button>
                    </div>
                  </div>
                );
              })}
            </div>
          </div>
        )}

        {/* Pricing Calendar */}
        <div className="mb-8">
          <PricingCalendar propertyId={propertyId} />
        </div>

        {/* Info */}
        <div className="bg-white rounded-lg shadow-sm border border-primary-200 p-6">
          <h2 className="text-xl font-bold text-primary-900 mb-4">How Dynamic Pricing Works</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div>
              <h3 className="font-semibold text-primary-900 mb-2">Price Adjustments</h3>
              <p className="text-sm text-primary-600">
                Pricing rules automatically adjust your nightly rate based on demand,
                seasonality, and booking patterns. Green indicates increased prices,
                red indicates discounted prices.
              </p>
            </div>
            <div>
              <h3 className="font-semibold text-primary-900 mb-2">Stacking Rules</h3>
              <p className="text-sm text-primary-600">
                Multiple pricing rules can apply to the same date. Rules are applied
                in priority order, with higher priority rules taking precedence.
              </p>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
