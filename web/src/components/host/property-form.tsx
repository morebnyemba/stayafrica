'use client';

import { useState } from 'react';
import { useRouter } from 'next/navigation';
import { apiClient } from '@/services/api-client';
import { MapPin, DollarSign, Home, Image as ImageIcon, Search } from 'lucide-react';

interface PropertyFormProps {
  initialData?: any;
  isEdit?: boolean;
}

export function PropertyForm({ initialData, isEdit = false }: PropertyFormProps) {
  const router = useRouter();
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState('');
  const [searchingLocation, setSearchingLocation] = useState(false);
  const [locationSuggestions, setLocationSuggestions] = useState<any[]>([]);
  
  const [formData, setFormData] = useState({
    title: initialData?.title || '',
    description: initialData?.description || '',
    property_type: initialData?.property_type || 'house',
    address: initialData?.address || '',
    city: initialData?.city || '',
    suburb: initialData?.suburb || '',
    country: initialData?.country || '',
    latitude: initialData?.location?.coordinates?.[1] || '',
    longitude: initialData?.location?.coordinates?.[0] || '',
    price_per_night: initialData?.price_per_night || '',
    currency: initialData?.currency || 'USD',
    bedrooms: initialData?.bedrooms || 1,
    bathrooms: initialData?.bathrooms || 1,
    max_guests: initialData?.max_guests || 2,
  });

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement | HTMLTextAreaElement | HTMLSelectElement>) => {
    const { name, value } = e.target;
    setFormData(prev => ({ ...prev, [name]: value }));
  };

  const handleLocationSearch = async (query: string) => {
    if (query.length < 3) {
      setLocationSuggestions([]);
      return;
    }

    setSearchingLocation(true);
    try {
      const response = await apiClient.getLocationSuggestions(query, formData.country, 5);
      setLocationSuggestions(response.data?.suggestions || []);
    } catch (err) {
      console.error('Location search error:', err);
    } finally {
      setSearchingLocation(false);
    }
  };

  const handleSelectLocation = (suggestion: any) => {
    setFormData(prev => ({
      ...prev,
      address: suggestion.display_name,
      city: suggestion.city,
      country: suggestion.country,
      latitude: suggestion.latitude.toString(),
      longitude: suggestion.longitude.toString(),
    }));
    setLocationSuggestions([]);
  };

  const handleGeocodeAddress = async () => {
    if (!formData.address) return;

    setSearchingLocation(true);
    try {
      const response = await apiClient.geocodeAddress(formData.address, formData.country);
      if (response.data) {
        setFormData(prev => ({
          ...prev,
          latitude: response.data.latitude.toString(),
          longitude: response.data.longitude.toString(),
          city: response.data.city || prev.city,
          suburb: response.data.suburb || prev.suburb,
          country: response.data.country || prev.country,
        }));
      }
    } catch (err: any) {
      setError('Could not find location. Please check the address.');
    } finally {
      setSearchingLocation(false);
    }
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setLoading(true);
    setError('');

    try {
      // Validate required fields
      if (!formData.title || !formData.description || !formData.address) {
        setError('Please fill in all required fields');
        setLoading(false);
        return;
      }

      if (!formData.latitude || !formData.longitude) {
        setError('Please provide a valid location (use the search or geocode button)');
        setLoading(false);
        return;
      }

      const propertyData = {
        title: formData.title,
        description: formData.description,
        property_type: formData.property_type,
        address: formData.address,
        city: formData.city,
        suburb: formData.suburb,
        country: formData.country,
        location: {
          type: 'Point',
          coordinates: [parseFloat(formData.longitude), parseFloat(formData.latitude)]
        },
        price_per_night: parseFloat(formData.price_per_night),
        currency: formData.currency,
        bedrooms: Math.max(1, parseInt(String(formData.bedrooms)) || 1),
        bathrooms: Math.max(1, parseInt(String(formData.bathrooms)) || 1),
        max_guests: Math.max(1, parseInt(String(formData.max_guests)) || 1),
        status: 'pending_approval',
      };

      if (isEdit && initialData?.id) {
        await apiClient.updateProperty(initialData.id, propertyData);
      } else {
        await apiClient.createProperty(propertyData);
      }

      router.push('/host/properties');
    } catch (err: any) {
      setError(err.response?.data?.message || 'Failed to save property');
    } finally {
      setLoading(false);
    }
  };

  return (
    <form onSubmit={handleSubmit} className="space-y-6">
      {error && (
        <div className="bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 rounded-lg p-4 text-red-800 dark:text-red-200">
          {error}
        </div>
      )}

      {/* Basic Information */}
      <div className="card p-6">
        <h3 className="text-xl font-bold text-primary-900 dark:text-sand-50 mb-4 flex items-center gap-2">
          <Home className="w-5 h-5" />
          Basic Information
        </h3>
        
        <div className="space-y-4">
          <div>
            <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
              Property Title *
            </label>
            <input
              type="text"
              name="title"
              value={formData.title}
              onChange={handleInputChange}
              required
              className="w-full px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50"
              placeholder="e.g., Beautiful 3BR Villa in Harare"
            />
          </div>

          <div>
            <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
              Description *
            </label>
            <textarea
              name="description"
              value={formData.description}
              onChange={handleInputChange}
              required
              rows={4}
              className="w-full px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50"
              placeholder="Describe your property, its features, and what makes it special..."
            />
          </div>

          <div>
            <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
              Property Type *
            </label>
            <select
              name="property_type"
              value={formData.property_type}
              onChange={handleInputChange}
              required
              className="w-full px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50"
            >
              <option value="house">House</option>
              <option value="apartment">Apartment</option>
              <option value="villa">Villa</option>
              <option value="cottage">Cottage</option>
              <option value="lodge">Lodge</option>
              <option value="room">Room</option>
            </select>
          </div>

          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            <div>
              <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
                Bedrooms *
              </label>
              <input
                type="number"
                name="bedrooms"
                value={formData.bedrooms}
                onChange={handleInputChange}
                required
                min="1"
                className="w-full px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50"
              />
            </div>
            <div>
              <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
                Bathrooms *
              </label>
              <input
                type="number"
                name="bathrooms"
                value={formData.bathrooms}
                onChange={handleInputChange}
                required
                min="1"
                className="w-full px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50"
              />
            </div>
            <div>
              <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
                Max Guests *
              </label>
              <input
                type="number"
                name="max_guests"
                value={formData.max_guests}
                onChange={handleInputChange}
                required
                min="1"
                className="w-full px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50"
              />
            </div>
          </div>
        </div>
      </div>

      {/* Location */}
      <div className="card p-6">
        <h3 className="text-xl font-bold text-primary-900 dark:text-sand-50 mb-4 flex items-center gap-2">
          <MapPin className="w-5 h-5" />
          Location (GDAL-Powered)
        </h3>
        
        <div className="space-y-4">
          <div>
            <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
              Search Location
            </label>
            <div className="relative">
              <input
                type="text"
                placeholder="Search for your property location..."
                onChange={(e) => handleLocationSearch(e.target.value)}
                className="w-full px-4 py-2 pr-10 border border-primary-300 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50"
              />
              <Search className="absolute right-3 top-3 w-5 h-5 text-primary-400" />
              
              {locationSuggestions.length > 0 && (
                <div className="absolute z-10 w-full mt-1 bg-white dark:bg-primary-800 border border-primary-300 dark:border-primary-600 rounded-lg shadow-lg max-h-60 overflow-y-auto">
                  {locationSuggestions.map((suggestion, index) => (
                    <button
                      key={index}
                      type="button"
                      onClick={() => handleSelectLocation(suggestion)}
                      className="w-full text-left px-4 py-3 hover:bg-secondary-50 dark:hover:bg-secondary-900/10 border-b border-primary-100 dark:border-primary-700 last:border-b-0"
                    >
                      <div className="font-medium text-primary-900 dark:text-sand-50">
                        {suggestion.display_name}
                      </div>
                      <div className="text-sm text-primary-600 dark:text-sand-400">
                        {suggestion.type}
                      </div>
                    </button>
                  ))}
                </div>
              )}
            </div>
          </div>

          <div>
            <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
              Full Address *
            </label>
            <div className="flex gap-2">
              <input
                type="text"
                name="address"
                value={formData.address}
                onChange={handleInputChange}
                required
                className="flex-1 px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50"
                placeholder="123 Main Street, Harare"
              />
              <button
                type="button"
                onClick={handleGeocodeAddress}
                disabled={searchingLocation}
                className="btn-secondary px-4 py-2 whitespace-nowrap"
              >
                {searchingLocation ? 'Searching...' : 'Find Coordinates'}
              </button>
            </div>
          </div>

          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            <div>
              <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
                City *
              </label>
              <input
                type="text"
                name="city"
                value={formData.city}
                onChange={handleInputChange}
                required
                className="w-full px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50"
              />
            </div>
            <div>
              <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
                Suburb
              </label>
              <input
                type="text"
                name="suburb"
                value={formData.suburb}
                onChange={handleInputChange}
                className="w-full px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50"
              />
            </div>
            <div>
              <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
                Country *
              </label>
              <select
                name="country"
                value={formData.country}
                onChange={handleInputChange}
                required
                className="w-full px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50"
              >
                <option value="">Select Country</option>
                <option value="Zimbabwe">Zimbabwe</option>
                <option value="South Africa">South Africa</option>
                <option value="Botswana">Botswana</option>
                <option value="Namibia">Namibia</option>
                <option value="Zambia">Zambia</option>
              </select>
            </div>
          </div>

          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <div>
              <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
                Latitude *
              </label>
              <input
                type="text"
                name="latitude"
                value={formData.latitude}
                onChange={handleInputChange}
                required
                className="w-full px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50"
                placeholder="-17.8252"
              />
            </div>
            <div>
              <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
                Longitude *
              </label>
              <input
                type="text"
                name="longitude"
                value={formData.longitude}
                onChange={handleInputChange}
                required
                className="w-full px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50"
                placeholder="31.0335"
              />
            </div>
          </div>

          <div className="bg-blue-50 dark:bg-blue-900/20 border border-blue-200 dark:border-blue-800 rounded-lg p-4 text-sm text-blue-800 dark:text-blue-200">
            <p className="font-medium mb-1">📍 Location powered by GDAL/PostGIS</p>
            <p>Search for your property or enter the address, then click "Find Coordinates" to automatically geocode the location with high accuracy.</p>
          </div>
        </div>
      </div>

      {/* Pricing */}
      <div className="card p-6">
        <h3 className="text-xl font-bold text-primary-900 dark:text-sand-50 mb-4 flex items-center gap-2">
          <DollarSign className="w-5 h-5" />
          Pricing
        </h3>
        
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div>
            <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
              Price per Night *
            </label>
            <input
              type="number"
              name="price_per_night"
              value={formData.price_per_night}
              onChange={handleInputChange}
              required
              min="1"
              step="0.01"
              className="w-full px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50"
              placeholder="50.00"
            />
          </div>
          <div>
            <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
              Currency *
            </label>
            <select
              name="currency"
              value={formData.currency}
              onChange={handleInputChange}
              required
              className="w-full px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50"
            >
              <option value="USD">USD ($)</option>
              <option value="ZWL">ZWL (Z$)</option>
              <option value="ZAR">ZAR (R)</option>
              <option value="BWP">BWP (P)</option>
            </select>
          </div>
        </div>
      </div>

      {/* Submit */}
      <div className="flex gap-4">
        <button
          type="button"
          onClick={() => router.back()}
          className="btn-secondary px-6 py-3"
          disabled={loading}
        >
          Cancel
        </button>
        <button
          type="submit"
          className="btn-primary px-6 py-3 flex-1"
          disabled={loading}
        >
          {loading ? 'Saving...' : isEdit ? 'Update Property' : 'Create Property'}
        </button>
      </div>
    </form>
  );
}
