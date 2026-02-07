'use client';

import { useState, useEffect } from 'react';
import Modal from './Modal';

interface TaxJurisdictionModalProps {
  isOpen: boolean;
  onClose: () => void;
  onSave: (data: any) => Promise<void>;
  jurisdiction?: any | null;
}

export default function TaxJurisdictionModal({ isOpen, onClose, onSave, jurisdiction }: TaxJurisdictionModalProps) {
  const [loading, setLoading] = useState(false);
  const [formData, setFormData] = useState({
    name: '',
    jurisdiction_type: 'country',
    code: '',
    country_code: '',
    state_province_code: '',
    city_name: '',
    is_active: true,
  });

  useEffect(() => {
    if (jurisdiction) {
      setFormData({
        name: jurisdiction.name || '',
        jurisdiction_type: jurisdiction.jurisdiction_type || 'country',
        code: jurisdiction.code || '',
        country_code: jurisdiction.country_code || '',
        state_province_code: jurisdiction.state_province_code || '',
        city_name: jurisdiction.city_name || '',
        is_active: jurisdiction.is_active !== undefined ? jurisdiction.is_active : true,
      });
    } else {
      setFormData({
        name: '',
        jurisdiction_type: 'country',
        code: '',
        country_code: '',
        state_province_code: '',
        city_name: '',
        is_active: true,
      });
    }
  }, [jurisdiction, isOpen]);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setLoading(true);
    try {
      await onSave(formData);
      onClose();
    } catch (error) {
      console.error('Error saving jurisdiction:', error);
    } finally {
      setLoading(false);
    }
  };

  return (
    <Modal
      isOpen={isOpen}
      onClose={onClose}
      title={jurisdiction ? 'Edit Tax Jurisdiction' : 'Add Tax Jurisdiction'}
      size="lg"
    >
      <form onSubmit={handleSubmit} className="space-y-4">
        <div>
          <label className="block text-sm font-medium text-gray-700 mb-1">
            Name <span className="text-red-500">*</span>
          </label>
          <input
            type="text"
            required
            value={formData.name}
            onChange={(e) => setFormData({ ...formData, name: e.target.value })}
            placeholder="e.g., California"
            className="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-[#D9B168] focus:border-transparent"
          />
        </div>

        <div className="grid grid-cols-2 gap-4">
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">
              Type <span className="text-red-500">*</span>
            </label>
            <select
              required
              value={formData.jurisdiction_type}
              onChange={(e) => setFormData({ ...formData, jurisdiction_type: e.target.value })}
              className="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-[#D9B168] focus:border-transparent"
            >
              <option value="country">Country</option>
              <option value="state">State/Province</option>
              <option value="city">City/Municipality</option>
              <option value="special">Special District</option>
            </select>
          </div>

          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">
              Code <span className="text-red-500">*</span>
            </label>
            <input
              type="text"
              required
              value={formData.code}
              onChange={(e) => setFormData({ ...formData, code: e.target.value })}
              placeholder="e.g., US-CA"
              className="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-[#D9B168] focus:border-transparent"
            />
          </div>
        </div>

        <div className="grid grid-cols-2 gap-4">
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">
              Country Code <span className="text-red-500">*</span>
            </label>
            <input
              type="text"
              required
              maxLength={2}
              value={formData.country_code}
              onChange={(e) => setFormData({ ...formData, country_code: e.target.value.toUpperCase() })}
              placeholder="e.g., US"
              className="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-[#D9B168] focus:border-transparent"
            />
          </div>

          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">
              State/Province Code
            </label>
            <input
              type="text"
              value={formData.state_province_code}
              onChange={(e) => setFormData({ ...formData, state_province_code: e.target.value })}
              placeholder="e.g., CA"
              className="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-[#D9B168] focus:border-transparent"
            />
          </div>
        </div>

        <div>
          <label className="block text-sm font-medium text-gray-700 mb-1">
            City Name
          </label>
          <input
            type="text"
            value={formData.city_name}
            onChange={(e) => setFormData({ ...formData, city_name: e.target.value })}
            placeholder="e.g., San Francisco"
            className="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-[#D9B168] focus:border-transparent"
          />
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
            {loading ? 'Saving...' : jurisdiction ? 'Update' : 'Create'}
          </button>
        </div>
      </form>
    </Modal>
  );
}
