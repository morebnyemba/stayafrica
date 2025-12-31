'use client';

import { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { apiClient } from '@/services/api-client';
import { MapPin, DollarSign, Home, Search, Image as ImageIcon, X, Upload } from 'lucide-react';

interface PropertyFormProps {
  initialData?: any;
  isEdit?: boolean;
  propertyId?: string;
  onSuccess?: () => void;
}

export function PropertyForm({ initialData, isEdit = false, propertyId, onSuccess }: PropertyFormProps) {
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

  const [imageFiles, setImageFiles] = useState<File[]>([]);
  const [imagePreviews, setImagePreviews] = useState<string[]>([]);
  const [existingImages, setExistingImages] = useState<any[]>([]);

  // Load existing images when editing
  useEffect(() => {
    if (isEdit && initialData?.images && Array.isArray(initialData.images)) {
      setExistingImages(initialData.images);
    }
  }, [isEdit, initialData]);

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement | HTMLTextAreaElement | HTMLSelectElement>) => {
    const { name, value } = e.target;
    setFormData(prev => ({ ...prev, [name]: value }));
  };

  const handleImageSelection = (e: React.ChangeEvent<HTMLInputElement>) => {
    const files = Array.from(e.target.files || []);
    if (files.length === 0) return;

    const newFiles = [...imageFiles, ...files];
    const maxImages = 10;

    if (newFiles.length > maxImages) {
      setError(`Maximum ${maxImages} images allowed`);
      return;
    }

    setImageFiles(newFiles);
    setError('');

    // Generate previews
    const newPreviews = files.map(file => URL.createObjectURL(file));
    setImagePreviews([...imagePreviews, ...newPreviews]);
  };

  const removeImage = (index: number) => {
    setImageFiles(prev => prev.filter((_, i) => i !== index));
    setImagePreviews(prev => {
      URL.revokeObjectURL(prev[index]);
      return prev.filter((_, i) => i !== index);
    });
  };

  const removeExistingImage = async (imageId: number) => {
    if (!window.confirm('Are you sure you want to delete this image?')) return;
    
    try {
      // You may need to implement this endpoint on the backend
      // await apiClient.deletePropertyImage(propertyId, imageId);
      setExistingImages(prev => prev.filter(img => img.id !== imageId));
    } catch (err) {
      console.error('Failed to delete image:', err);
      setError('Failed to delete image');
    }
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

      let propertyId_local = propertyId || initialData?.id;
      
      if (isEdit && propertyId_local) {
        await apiClient.updateProperty(propertyId_local, propertyData);
      } else {
        const response = await apiClient.createProperty(propertyData);
        propertyId_local = response.data?.id;
      }

      // Upload images if provided
      if (imageFiles.length > 0 && propertyId_local) {
        const formDataImages = new FormData();
        imageFiles.forEach((file, index) => {
          formDataImages.append('images', file);
          formDataImages.append(`image_order_${index}`, index.toString());
        });

        try {
          await apiClient.uploadPropertyImages(propertyId_local, formDataImages);
        } catch (imgErr) {
          console.error('Error uploading images:', imgErr);
          // Don't fail the entire property creation if image upload fails
        }
      }

      // Call onSuccess callback if provided, otherwise redirect
      if (onSuccess) {
        onSuccess();
      } else {
        router.push('/host/properties');
      }
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

      {/* Images */}
      <div className="card p-6">
        <h3 className="text-xl font-bold text-primary-900 dark:text-sand-50 mb-4 flex items-center gap-2">
          <ImageIcon className="w-5 h-5" />
          Property Images
        </h3>
        
        <div className="space-y-4">
          <p className="text-sm text-primary-600 dark:text-sand-400">
            Add up to 10 images of your property. The first image will be used as the main thumbnail.
          </p>

          {/* Image Upload Area */}
          <div className="border-2 border-dashed border-primary-300 dark:border-primary-600 rounded-lg p-8 text-center hover:border-secondary-500 dark:hover:border-secondary-400 transition cursor-pointer"
            onClick={() => document.getElementById('image-input')?.click()}
          >
            <input
              id="image-input"
              type="file"
              multiple
              accept="image/*"
              onChange={handleImageSelection}
              className="hidden"
              disabled={imageFiles.length >= 10}
            />
            <Upload className="w-8 h-8 text-primary-400 mx-auto mb-2" />
            <p className="font-medium text-primary-900 dark:text-sand-50 mb-1">
              Click to upload images
            </p>
            <p className="text-sm text-primary-600 dark:text-sand-400">
              or drag and drop (JPG, PNG, WebP up to 5MB each)
            </p>
            <p className="text-xs text-primary-500 dark:text-sand-400 mt-2">
              {imageFiles.length}/10 images added
            </p>
          </div>

          {/* Existing Images (when editing) */}
          {existingImages.length > 0 && (
            <div className="mb-4">
              <p className="text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
                Current Images ({existingImages.length})
              </p>
              <div className="grid grid-cols-2 sm:grid-cols-3 md:grid-cols-4 gap-4">
                {existingImages.map((image, index) => (
                  <div key={image.id} className="relative group">
                    <img
                      src={image.image_url || image.image}
                      alt={`Current ${index + 1}`}
                      className="w-full h-32 object-cover rounded-lg border border-primary-200 dark:border-primary-700"
                    />
                    {index === 0 && (
                      <span className="absolute top-2 left-2 bg-secondary-600 text-white text-xs font-semibold px-2 py-1 rounded">
                        Main
                      </span>
                    )}
                    <button
                      type="button"
                      onClick={() => removeExistingImage(image.id)}
                      className="absolute top-2 right-2 bg-red-600 hover:bg-red-700 text-white p-1 rounded-full opacity-0 group-hover:opacity-100 transition"
                      title="Delete image"
                    >
                      <X className="w-4 h-4" />
                    </button>
                  </div>
                ))}
              </div>
            </div>
          )}

          {/* New Image Previews */}
          {imagePreviews.length > 0 && (
            <div>
              <p className="text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
                New Images ({imagePreviews.length})
              </p>
              <div className="grid grid-cols-2 sm:grid-cols-3 md:grid-cols-4 gap-4">
                {imagePreviews.map((preview, index) => (
                  <div key={index} className="relative group">
                    <img
                      src={preview}
                      alt={`Preview ${index + 1}`}
                      className="w-full h-32 object-cover rounded-lg border border-primary-200 dark:border-primary-700"
                    />
                    <button
                      type="button"
                      onClick={() => removeImage(index)}
                      className="absolute top-2 right-2 bg-red-600 hover:bg-red-700 text-white p-1 rounded-full opacity-0 group-hover:opacity-100 transition"
                    >
                      <X className="w-4 h-4" />
                    </button>
                  </div>
                ))}
              </div>
            </div>
          )}
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
