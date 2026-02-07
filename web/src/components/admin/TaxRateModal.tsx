'use client';

import { useState, useEffect } from 'react';
import Modal from './Modal';

interface TaxRateModalProps {
  isOpen: boolean;
  onClose: () => void;
  onSave: (data: any) => Promise<void>;
  rate?: any | null;
  jurisdictions: any[];
}

export default function TaxRateModal({ isOpen, onClose, onSave, rate, jurisdictions }: TaxRateModalProps) {
  const [loading, setLoading] = useState(false);
  const [formData, setFormData] = useState({
    jurisdiction_id: '',
    name: '',
    tax_type: 'vat',
    rate: '',
    is_active: true,
    applies_to_accommodation: true,
    applies_to_cleaning_fee: false,
    applies_to_service_fee: false,
  });

  useEffect(() => {
    if (rate) {
      setFormData({
        jurisdiction_id: rate.jurisdiction?.id || '',
        name: rate.name || '',
        tax_type: rate.tax_type || 'vat',
        rate: rate.rate?.toString() || '',
        is_active: rate.is_active !== undefined ? rate.is_active : true,
        applies_to_accommodation: rate.applies_to_accommodation !== undefined ? rate.applies_to_accommodation : true,
        applies_to_cleaning_fee: rate.applies_to_cleaning_fee || false,
        applies_to_service_fee: rate.applies_to_service_fee || false,
      });
    } else {
      setFormData({
        jurisdiction_id: '',
        name: '',
        tax_type: 'vat',
        rate: '',
        is_active: true,
        applies_to_accommodation: true,
        applies_to_cleaning_fee: false,
        applies_to_service_fee: false,
      });
    }
  }, [rate, isOpen]);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setLoading(true);
    try {
      const dataToSave = {
        ...formData,
        rate: parseFloat(formData.rate),
      };
      await onSave(dataToSave);
      onClose();
    } catch (error) {
      console.error('Error saving tax rate:', error);
    } finally {
      setLoading(false);
    }
  };

  return (
    <Modal
      isOpen={isOpen}
      onClose={onClose}
      title={rate ? 'Edit Tax Rate' : 'Add Tax Rate'}
      size="lg"
    >
      <form onSubmit={handleSubmit} className="space-y-4">
        <div>
          <label className="block text-sm font-medium text-gray-700 mb-1">
            Jurisdiction <span className="text-red-500">*</span>
          </label>
          <select
            required
            value={formData.jurisdiction_id}
            onChange={(e) => setFormData({ ...formData, jurisdiction_id: e.target.value })}
            className="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-[#D9B168] focus:border-transparent"
          >
            <option value="">Select jurisdiction...</option>
            {jurisdictions.map((j) => (
              <option key={j.id} value={j.id}>
                {j.name} ({j.code})
              </option>
            ))}
          </select>
        </div>

        <div>
          <label className="block text-sm font-medium text-gray-700 mb-1">
            Name <span className="text-red-500">*</span>
          </label>
          <input
            type="text"
            required
            value={formData.name}
            onChange={(e) => setFormData({ ...formData, name: e.target.value })}
            placeholder="e.g., California Sales Tax"
            className="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-[#D9B168] focus:border-transparent"
          />
        </div>

        <div className="grid grid-cols-2 gap-4">
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">
              Tax Type <span className="text-red-500">*</span>
            </label>
            <select
              required
              value={formData.tax_type}
              onChange={(e) => setFormData({ ...formData, tax_type: e.target.value })}
              className="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-[#D9B168] focus:border-transparent"
            >
              <option value="vat">VAT</option>
              <option value="sales">Sales Tax</option>
              <option value="occupancy">Occupancy Tax / Hotel Tax</option>
              <option value="tourism">Tourism Tax</option>
              <option value="service">Service Tax</option>
              <option value="other">Other</option>
            </select>
          </div>

          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">
              Rate (%) <span className="text-red-500">*</span>
            </label>
            <input
              type="number"
              step="0.01"
              min="0"
              max="100"
              required
              value={formData.rate}
              onChange={(e) => setFormData({ ...formData, rate: e.target.value })}
              placeholder="e.g., 7.5"
              className="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-[#D9B168] focus:border-transparent"
            />
          </div>
        </div>

        <div className="space-y-2">
          <label className="block text-sm font-medium text-gray-700 mb-2">
            Applies To
          </label>
          
          <div className="flex items-center">
            <input
              type="checkbox"
              id="applies_to_accommodation"
              checked={formData.applies_to_accommodation}
              onChange={(e) => setFormData({ ...formData, applies_to_accommodation: e.target.checked })}
              className="w-4 h-4 text-[#D9B168] border-gray-300 rounded focus:ring-[#D9B168]"
            />
            <label htmlFor="applies_to_accommodation" className="ml-2 text-sm text-gray-700">
              Accommodation Fee
            </label>
          </div>

          <div className="flex items-center">
            <input
              type="checkbox"
              id="applies_to_cleaning_fee"
              checked={formData.applies_to_cleaning_fee}
              onChange={(e) => setFormData({ ...formData, applies_to_cleaning_fee: e.target.checked })}
              className="w-4 h-4 text-[#D9B168] border-gray-300 rounded focus:ring-[#D9B168]"
            />
            <label htmlFor="applies_to_cleaning_fee" className="ml-2 text-sm text-gray-700">
              Cleaning Fee
            </label>
          </div>

          <div className="flex items-center">
            <input
              type="checkbox"
              id="applies_to_service_fee"
              checked={formData.applies_to_service_fee}
              onChange={(e) => setFormData({ ...formData, applies_to_service_fee: e.target.checked })}
              className="w-4 h-4 text-[#D9B168] border-gray-300 rounded focus:ring-[#D9B168]"
            />
            <label htmlFor="applies_to_service_fee" className="ml-2 text-sm text-gray-700">
              Service Fee
            </label>
          </div>
        </div>

        <div className="flex items-center">
          <input
            type="checkbox"
            id="is_active"
            checked={formData.is_active}
            onChange={(e) => setFormData({ ...formData, is_active: e.target.checked })}
            className="w-4 h-4 text-[#D9B168] border-gray-300 rounded focus:ring-[#D9B168]"
          />
          <label htmlFor="is_active" className="ml-2 text-sm text-gray-700">
            Active
          </label>
        </div>

        <div className="flex justify-end space-x-3 pt-4 border-t">
          <button
            type="button"
            onClick={onClose}
            disabled={loading}
            className="px-4 py-2 border border-gray-300 rounded-lg text-gray-700 hover:bg-gray-50 transition-colors disabled:opacity-50"
          >
            Cancel
          </button>
          <button
            type="submit"
            disabled={loading}
            className="px-4 py-2 bg-[#D9B168] text-[#122F26] rounded-lg hover:bg-[#c9a158] transition-colors disabled:opacity-50"
          >
            {loading ? 'Saving...' : rate ? 'Update' : 'Create'}
          </button>
        </div>
      </form>
    </Modal>
  );
}
