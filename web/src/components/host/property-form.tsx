'use client';

import { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { apiClient } from '@/services/api-client';
import { Input, Button } from '@/components/ui';
import { MapPin, DollarSign, Home, Image as ImageIcon, X, Upload } from 'lucide-react';

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
  const [usingCurrentLocation, setUsingCurrentLocation] = useState(false);
  const [premisesStatus, setPremisesStatus] = useState<{ isOnPremises: boolean | null; distanceKm: number | null; message: string }>(
    { isOnPremises: null, distanceKm: null, message: '' }
  );
  
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

    // Reset premises status when selecting from suggestions
    setPremisesStatus({ isOnPremises: null, distanceKm: null, message: '' });
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

        // If we already captured current location, update premises status against geocoded coords
        if (premisesStatus.distanceKm !== null) {
          const dist = haversineDistance(
            currentLatLng.lat,
            currentLatLng.lng,
            Number(response.data.latitude),
            Number(response.data.longitude)
          );
          const isOnPrem = dist <= 0.1; // within 100m considered on-premises
          setPremisesStatus({
            isOnPremises: isOnPrem,
            distanceKm: Number(dist.toFixed(3)),
            message: isOnPrem
              ? 'You appear to be on the property premises.'
              : `You are approximately ${dist.toFixed(3)} km from the entered property location.`
          });
        }
      }
    } catch (err: any) {
      setError('Could not find location. Please check the address.');
    } finally {
      setSearchingLocation(false);
    }
  };

  // Track current captured coordinates for premises calculation
  const [currentLatLng, setCurrentLatLng] = useState<{ lat: number; lng: number }>({ lat: 0, lng: 0 });

  // Haversine distance in kilometers
  const haversineDistance = (lat1: number, lon1: number, lat2: number, lon2: number) => {
    const toRad = (deg: number) => (deg * Math.PI) / 180;
    const R = 6371; // Earth radius in km
    const dLat = toRad(lat2 - lat1);
    const dLon = toRad(lon2 - lon1);
    const a = Math.sin(dLat / 2) * Math.sin(dLat / 2) +
              Math.cos(toRad(lat1)) * Math.cos(toRad(lat2)) *
              Math.sin(dLon / 2) * Math.sin(dLon / 2);
    const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
    return R * c;
  };

  const handleUseCurrentLocation = async () => {
    if (!navigator.geolocation) {
      setError('Geolocation is not supported by your browser.');
      return;
    }

    setUsingCurrentLocation(true);
    setError('');
    setPremisesStatus({ isOnPremises: null, distanceKm: null, message: '' });

    navigator.geolocation.getCurrentPosition(
      async (pos) => {
        const lat = pos.coords.latitude;
        const lng = pos.coords.longitude;
        setCurrentLatLng({ lat, lng });

        // If form already has coordinates, compute distance; otherwise fill them
        const existingLat = Number(formData.latitude);
        const existingLng = Number(formData.longitude);
        if (!isNaN(existingLat) && !isNaN(existingLng) && formData.latitude && formData.longitude) {
          const dist = haversineDistance(lat, lng, existingLat, existingLng);
          const isOnPrem = dist <= 0.1; // within 100m considered on-premises
          setPremisesStatus({
            isOnPremises: isOnPrem,
            distanceKm: Number(dist.toFixed(3)),
            message: isOnPrem
              ? 'You appear to be on the property premises.'
              : `You are approximately ${dist.toFixed(3)} km from the entered property location.`
          });
        } else {
          setFormData(prev => ({
            ...prev,
            latitude: lat.toString(),
            longitude: lng.toString(),
          }));

          // Reverse geocode to fill address/city/suburb/country if available
          try {
            const resp = await apiClient.reverseGeocode(lat, lng);
            const data = resp.data || {};
            setFormData(prev => ({
              ...prev,
              address: data.address || prev.address,
              city: data.city || prev.city,
              suburb: data.suburb || prev.suburb,
              country: data.country || prev.country,
            }));
          } catch (_) {
            // Non-blocking if reverse geocode fails
          }

          // If we filled from current location, we are on premises by definition
          setPremisesStatus({
            isOnPremises: true,
            distanceKm: 0,
            message: 'Based on your current location, you are on the property premises.'
          });
        }
        setUsingCurrentLocation(false);
      },
      (err) => {
        setUsingCurrentLocation(false);
        if (err.code === err.PERMISSION_DENIED) {
          setError('Location permission denied. Please allow access to use current location.');
        } else {
          setError('Unable to retrieve your location.');
        }
      },
      { enableHighAccuracy: true, timeout: 10000 }
    );
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
            <Input
              label="Property Title *"
              type="text"
              name="title"
              value={formData.title}
              onChange={handleInputChange}
              required
              placeholder="e.g., Beautiful 3BR Villa in Harare"
            />
          </div>

          <div>
            <Input
              label="Description *"
              multiline
              rows={4}
              name="description"
              value={formData.description}
              onChange={handleInputChange}
              required
              placeholder="Describe your property, its features, and what makes it special..."
            />
          </div>

          <div>
            <Input
              label="Property Type *"
              select
              name="property_type"
              value={formData.property_type}
              onChange={handleInputChange}
              required
              options={[
                { value: 'house', label: 'House' },
                { value: 'apartment', label: 'Apartment' },
                { value: 'villa', label: 'Villa' },
                { value: 'cottage', label: 'Cottage' },
                { value: 'lodge', label: 'Lodge' },
                { value: 'room', label: 'Room' },
              ]}
            />
          </div>

          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            <div>
              <Input
                label="Bedrooms *"
                type="number"
                name="bedrooms"
                value={formData.bedrooms}
                onChange={handleInputChange}
                required
                min="1"
              />
            </div>
            <div>
              <Input
                label="Bathrooms *"
                type="number"
                name="bathrooms"
                value={formData.bathrooms}
                onChange={handleInputChange}
                required
                min="1"
              />
            </div>
            <div>
              <Input
                label="Max Guests *"
                type="number"
                name="max_guests"
                value={formData.max_guests}
                onChange={handleInputChange}
                required
                min="1"
              />
            </div>
          </div>
        </div>
      </div>

      {/* Location */}
      <div className="card p-4 sm:p-6">
        <h3 className="text-lg sm:text-xl font-bold text-primary-900 dark:text-sand-50 mb-4 flex items-center gap-2">
          <MapPin className="w-5 h-5" />
          <span>Location <span className="hidden sm:inline">(GDAL-Powered)</span></span>
        </h3>
        
        <div className="space-y-4">
          <div className="relative">
            <Input
              label="Search Location"
              type="text"
              placeholder="Search for your property location..."
              onChange={(e) => handleLocationSearch(e.target.value)}
            />
            
            {locationSuggestions.length > 0 && (
              <div className="absolute z-10 w-full mt-1 bg-white dark:bg-primary-800 border border-primary-300 dark:border-primary-600 rounded-lg shadow-lg max-h-60 overflow-y-auto">
                {locationSuggestions.map((suggestion, index) => (
                  <button
                    key={index}
                    type="button"
                    onClick={() => handleSelectLocation(suggestion)}
                    className="w-full text-left px-3 sm:px-4 py-2 sm:py-3 hover:bg-secondary-50 dark:hover:bg-secondary-900/10 border-b border-primary-100 dark:border-primary-700 last:border-b-0"
                  >
                    <div className="font-medium text-primary-900 dark:text-sand-50 text-sm sm:text-base break-words">
                      {suggestion.display_name}
                    </div>
                    <div className="text-xs sm:text-sm text-primary-600 dark:text-sand-400">
                      {suggestion.type}
                    </div>
                  </button>
                ))}
              </div>
            )}
          </div>

          <div className="space-y-3">
            <div>
              <Input
                label="Full Address *"
                type="text"
                name="address"
                value={formData.address}
                onChange={handleInputChange}
                required
                placeholder="123 Main Street, Harare"
              />
            </div>
            <div className="flex flex-col sm:flex-row gap-2">
              <Button
                type="button"
                onClick={handleGeocodeAddress}
                disabled={searchingLocation}
                variant="secondary"
                className="w-full sm:w-auto"
              >
                {searchingLocation ? 'Searching...' : 'Find Coordinates'}
              </Button>
              <Button
                type="button"
                onClick={handleUseCurrentLocation}
                disabled={usingCurrentLocation}
                variant="secondary"
                className="w-full sm:w-auto"
              >
                {usingCurrentLocation ? 'Locating...' : 'Use Current Location'}
              </Button>
            </div>
          </div>

          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            <div>
              <Input
                label="City *"
                type="text"
                name="city"
                value={formData.city}
                onChange={handleInputChange}
                required
              />
            </div>
            <div>
              <Input
                label="Suburb"
                type="text"
                name="suburb"
                value={formData.suburb}
                onChange={handleInputChange}
              />
            </div>
            <div>
              <Input
                label="Country *"
                select
                name="country"
                value={formData.country}
                onChange={handleInputChange}
                required
                options={[
                  { value: '', label: 'Select Country' },
                  { value: 'Zimbabwe', label: 'Zimbabwe' },
                  { value: 'South Africa', label: 'South Africa' },
                  { value: 'Botswana', label: 'Botswana' },
                  { value: 'Namibia', label: 'Namibia' },
                  { value: 'Zambia', label: 'Zambia' },
                ]}
              />
            </div>
          </div>

          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <div>
              <Input
                label="Latitude *"
                type="text"
                name="latitude"
                value={formData.latitude}
                onChange={handleInputChange}
                required
                placeholder="-17.8252"
              />
            </div>
            <div>
              <Input
                label="Longitude *"
                type="text"
                name="longitude"
                value={formData.longitude}
                onChange={handleInputChange}
                required
                placeholder="31.0335"
              />
            </div>
          </div>

          <div className="space-y-3">
            <div className="bg-blue-50 dark:bg-blue-900/20 border border-blue-200 dark:border-blue-800 rounded-lg p-3 sm:p-4 text-xs sm:text-sm text-blue-800 dark:text-blue-200">
              <p className="font-medium mb-1">📍 Location powered by GDAL/PostGIS</p>
              <p className="leading-relaxed">Search for your property or enter the address, then click "Find Coordinates" or "Use Current Location" to populate coordinates.</p>
            </div>
            {premisesStatus.isOnPremises !== null && (
              <div className={`rounded-lg p-3 sm:p-4 text-xs sm:text-sm border ${premisesStatus.isOnPremises ? 'bg-green-50 border-green-200 text-green-800 dark:bg-green-900/20 dark:border-green-800 dark:text-green-200' : 'bg-yellow-50 border-yellow-200 text-yellow-800 dark:bg-yellow-900/20 dark:border-yellow-800 dark:text-yellow-200'}`}>
                <p className="font-semibold flex items-center gap-2">
                  <span className="text-base sm:text-lg">{premisesStatus.isOnPremises ? '✅' : '⚠️'}</span>
                  Premises Check
                </p>
                <p className="mt-2 leading-relaxed">{premisesStatus.message}</p>
                {premisesStatus.distanceKm !== null && !premisesStatus.isOnPremises && (
                  <p className="mt-2 text-xs sm:text-sm font-medium bg-white/50 dark:bg-black/20 px-2 py-1 rounded inline-block">
                    Distance: {premisesStatus.distanceKm} km
                  </p>
                )}
              </div>
            )}
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
            <Input
              label="Price per Night *"
              type="number"
              name="price_per_night"
              value={formData.price_per_night}
              onChange={handleInputChange}
              required
              min="1"
              step="0.01"
              placeholder="50.00"
            />
          </div>
          <div>
            <Input
              label="Currency *"
              select
              name="currency"
              value={formData.currency}
              onChange={handleInputChange}
              required
              options={[
                { value: 'USD', label: 'USD ($)' },
                { value: 'ZWL', label: 'ZWL (Z$)' },
                { value: 'ZAR', label: 'ZAR (R)' },
                { value: 'BWP', label: 'BWP (P)' },
              ]}
            />
          </div>
        </div>
      </div>

      {/* Submit */}
      <div className="flex gap-4">
        <Button
          type="button"
          onClick={() => router.back()}
          variant="secondary"
          disabled={loading}
        >
          Cancel
        </Button>
        <Button
          type="submit"
          disabled={loading}
          className="flex-1"
        >
          {loading ? 'Saving...' : isEdit ? 'Update Property' : 'Create Property'}
        </Button>
      </div>
    </form>
  );
}
