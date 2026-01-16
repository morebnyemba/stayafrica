'use client';

import React from 'react';
import { Sparkles, Calendar, Clock, TrendingDown, Zap } from 'lucide-react';
import { AppliedPricingRule } from '@/types/pricing-types';

interface PricingRuleIndicatorProps {
  rule: AppliedPricingRule;
  size?: 'sm' | 'md';
}

export default function PricingRuleIndicator({ rule, size = 'sm' }: PricingRuleIndicatorProps) {
  const isDiscount = rule.adjusted_amount < rule.original_amount;

  const getRuleIcon = () => {
    switch (rule.rule_type) {
      case 'seasonal':
        return <Calendar className={size === 'sm' ? 'w-3 h-3' : 'w-4 h-4'} />;
      case 'weekend':
        return <Sparkles className={size === 'sm' ? 'w-3 h-3' : 'w-4 h-4'} />;
      case 'length_of_stay':
        return <Clock className={size === 'sm' ? 'w-3 h-3' : 'w-4 h-4'} />;
      case 'early_bird':
        return <TrendingDown className={size === 'sm' ? 'w-3 h-3' : 'w-4 h-4'} />;
      case 'last_minute':
        return <Zap className={size === 'sm' ? 'w-3 h-3' : 'w-4 h-4'} />;
      default:
        return <Sparkles className={size === 'sm' ? 'w-3 h-3' : 'w-4 h-4'} />;
    }
  };

  const getRuleLabel = () => {
    const value = rule.adjustment_type === 'percentage'
      ? `${Math.abs(rule.adjustment_value)}%`
      : `$${Math.abs(rule.adjustment_value)}`;
    
    return isDiscount ? `${value} off` : `${value} extra`;
  };

  const colorClasses = isDiscount
    ? 'bg-green-50 text-green-700 border-green-200'
    : 'bg-orange-50 text-orange-700 border-orange-200';

  return (
    <div
      className={`
        inline-flex items-center gap-1.5 px-2.5 py-1 rounded-full border
        ${size === 'sm' ? 'text-xs' : 'text-sm'}
        font-medium ${colorClasses}
      `}
      title={`${rule.rule_name}: ${getRuleLabel()}`}
    >
      {getRuleIcon()}
      <span className="whitespace-nowrap">
        {rule.rule_name.replace(/_/g, ' ')}
      </span>
      <span className="font-bold">{getRuleLabel()}</span>
    </div>
  );
}
