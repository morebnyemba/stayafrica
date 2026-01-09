'use client';

import { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { apiClient } from '@/services/api-client';
import { Input, Button } from '@/components/ui';
import { MapPin, DollarSign, Home, Image as ImageIcon, X, Upload, ChevronRight, ChevronLeft } from 'lucide-react';
import { MapboxLocationPicker } from '@/components/common/mapbox-location-picker';

interface PropertyFormProps {
  initialData?: any;
  isEdit?: boolean;
  propertyId?: string;
  onSuccess?: () => void;
}

type FormStep = 'basic' | 'location' | 'images' | 'pricing';

const STEPS: { id: FormStep; label: string; description: string }[] = [
  { id: 'basic', label: 'Basic Info', description: 'Property details' },
  { id: 'location', label: 'Location', description: 'Pin your property' },
  { id: 'images', label: 'Images', description: 'Add photos' },
  { id: 'pricing', label: 'Pricing', description: 'Set rates' },
];

export function PropertyForm({ initialData, isEdit = false, propertyId, onSuccess }: PropertyFormProps) {
  const router = useRouter();
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState('');
  const [currentStep, setCurrentStep] = useState<FormStep>('basic');
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

  const handleMapLocationSelect = (locationData: { lat: number; lng: number; address?: string }) => {
    setFormData(prev => ({
      ...prev,
      latitude: locationData.lat.toString(),
      longitude: locationData.lng.toString(),
      address: locationData.address || prev.address,
    }));
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

  // Validation for each step
  const validateStep = (step: FormStep): boolean => {
    switch (step) {
      case 'basic':
        if (!formData.title || !formData.description || !formData.property_type) {
          setError('Please fill in all required basic information fields');
          return false;
        }
        return true;
      case 'location':
        if (!formData.address || !formData.city || !formData.country) {
          setError('Please fill in address, city, and country');
          return false;
        }
        if (!formData.latitude || !formData.longitude) {
          setError('Please pin the location on the map or use the search');
          return false;
        }
        return true;
      case 'images':
        // Images are optional
        return true;
      case 'pricing':
        if (!formData.price_per_night || !formData.currency) {
          setError('Please fill in pricing information');
          return false;
        }
        return true;
      default:
        return true;
    }
  };

  const goToNextStep = () => {
    setError('');
    if (!validateStep(currentStep)) return;

    const stepIndex = STEPS.findIndex(s => s.id === currentStep);
    if (stepIndex < STEPS.length - 1) {
      setCurrentStep(STEPS[stepIndex + 1].id);
    }
  };

  const goToPreviousStep = () => {
    const stepIndex = STEPS.findIndex(s => s.id === currentStep);
    if (stepIndex > 0) {
      setCurrentStep(STEPS[stepIndex - 1].id);
    }
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setLoading(true);
    setError('');

    try {
      if (!validateStep('pricing')) {
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
        }
      }

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

  // Step indicators
  const currentStepIndex = STEPS.findIndex(s => s.id === currentStep);
  const isLastStep = currentStepIndex === STEPS.length - 1;

  return (
    <form onSubmit={handleSubmit} className="space-y-6">
      {error && (
        <div className="bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 rounded-lg p-3 sm:p-4 text-xs sm:text-sm text-red-800 dark:text-red-200">
          {error}
        </div>
      )}

      {/* Step Indicator */}
      <div className="flex flex-col sm:flex-row justify-between gap-2 sm:gap-4">
        {STEPS.map((step, index) => (
          <div key={step.id} className="flex-1">
            <div className={`flex items-center gap-2 mb-1 ${index <= currentStepIndex ? 'opacity-100' : 'opacity-50'}`}>
              <div className={`w-6 h-6 sm:w-8 sm:h-8 rounded-full flex items-center justify-center text-xs sm:text-sm font-semibold ${
                index < currentStepIndex ? 'bg-green-500 text-white' :
                index === currentStepIndex ? 'bg-primary-600 text-white' :
                'bg-primary-200 dark:bg-primary-700 text-primary-900 dark:text-sand-50'
              }`}>
                {index < currentStepIndex ? '✓' : index + 1}
              </div>
              <div className="hidden sm:block">
                <p className="text-xs sm:text-sm font-medium text-primary-900 dark:text-sand-50">{step.label}</p>
                <p className="text-xs text-primary-600 dark:text-sand-400">{step.description}</p>
              </div>
            </div>
            {index < STEPS.length - 1 && (
              <div className={`h-0.5 mx-3 sm:mx-4 mt-2 ${index < currentStepIndex ? 'bg-green-500' : 'bg-primary-200 dark:bg-primary-700'}`} />
            )}
          </div>
        ))}
      </div>

      {/* Step Content */}
      {currentStep === 'basic' && (
        <div className="card p-4 sm:p-6 space-y-4">
          <h3 className="text-lg sm:text-xl font-bold text-primary-900 dark:text-sand-50 flex items-center gap-2">
            <Home className="w-4 h-4 sm:w-5 sm:h-5" />
            Basic Information
          </h3>
          
          <div className="space-y-4">
            <Input
              label="Property Title *"
              type="text"
              name="title"
              value={formData.title}
              onChange={handleInputChange}
              required
              placeholder="e.g., Beautiful 3BR Villa in Harare"
            />

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

            <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
              <Input
                label="Bedrooms *"
                type="number"
                name="bedrooms"
                value={formData.bedrooms}
                onChange={handleInputChange}
                required
                min="1"
              />
              <Input
                label="Bathrooms *"
                type="number"
                name="bathrooms"
                value={formData.bathrooms}
                onChange={handleInputChange}
                required
                min="1"
              />
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
      )}

      {currentStep === 'location' && (
        <div className="card p-4 sm:p-6 space-y-4">
          <h3 className="text-lg sm:text-xl font-bold text-primary-900 dark:text-sand-50 flex items-center gap-2">
            <MapPin className="w-4 h-4 sm:w-5 sm:h-5" />
            <span>Location <span className="hidden sm:inline">(Pin on Map)</span></span>
          </h3>

          <div className="space-y-4">
            {/* Address Search */}
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

            {/* Address Input with Geocode Button */}
            <div className="space-y-3">
              <Input
                label="Full Address *"
                type="text"
                name="address"
                value={formData.address}
                onChange={handleInputChange}
                required
                placeholder="123 Main Street, Harare"
              />
              <Button
                type="button"
                onClick={handleGeocodeAddress}
                disabled={searchingLocation}
                variant="secondary"
                className="w-full"
              >
                {searchingLocation ? 'Geocoding...' : 'Fill from Address'}
              </Button>
            </div>

            {/* Mapbox Map */}
            <div>
              <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
                Pin Your Property on Map *
              </label>
              <MapboxLocationPicker
                onLocationSelect={handleMapLocationSelect}
                initialLat={formData.latitude ? parseFloat(formData.latitude) : -17.8252}
                initialLng={formData.longitude ? parseFloat(formData.longitude) : 31.0335}
              />
            </div>

            {/* Location Fields */}
            <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
              <Input
                label="City *"
                type="text"
                name="city"
                value={formData.city}
                onChange={handleInputChange}
                required
              />
              <Input
                label="Suburb"
                type="text"
                name="suburb"
                value={formData.suburb}
                onChange={handleInputChange}
              />
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
          </div>
        </div>
      )}

      {currentStep === 'images' && (
        <div className="card p-4 sm:p-6 space-y-4">
          <h3 className="text-lg sm:text-xl font-bold text-primary-900 dark:text-sand-50 flex items-center gap-2">
            <ImageIcon className="w-4 h-4 sm:w-5 sm:h-5" />
            Property Images
          </h3>
          
          <div className="space-y-4">
            <p className="text-xs sm:text-sm text-primary-600 dark:text-sand-400">
              Add up to 10 images of your property. The first image will be used as the main thumbnail.
            </p>

            {/* Image Upload Area */}
            <div className="border-2 border-dashed border-primary-300 dark:border-primary-600 rounded-lg p-4 sm:p-8 text-center hover:border-secondary-500 dark:hover:border-secondary-400 transition cursor-pointer"
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
              <Upload className="w-6 h-6 sm:w-8 sm:h-8 text-primary-400 mx-auto mb-2" />
              <p className="font-medium text-sm sm:text-base text-primary-900 dark:text-sand-50 mb-1">
                Click to upload images
              </p>
              <p className="text-xs sm:text-sm text-primary-600 dark:text-sand-400">
                or drag and drop (JPG, PNG, WebP up to 5MB each)
              </p>
              <p className="text-xs text-primary-500 dark:text-sand-400 mt-2">
                {imageFiles.length}/10 images added
              </p>
            </div>

            {/* Existing Images */}
            {existingImages.length > 0 && (
              <div className="mb-4">
                <p className="text-xs sm:text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
                  Current Images ({existingImages.length})
                </p>
                <div className="grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-4 gap-2 sm:gap-4">
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
                <p className="text-xs sm:text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
                  New Images ({imagePreviews.length})
                </p>
                <div className="grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-4 gap-2 sm:gap-4">
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
      )}

      {currentStep === 'pricing' && (
        <div className="card p-4 sm:p-6 space-y-4">
          <h3 className="text-lg sm:text-xl font-bold text-primary-900 dark:text-sand-50 flex items-center gap-2">
            <DollarSign className="w-4 h-4 sm:w-5 sm:h-5" />
            Pricing
          </h3>
          
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
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

          <div className="bg-green-50 dark:bg-green-900/20 border border-green-200 dark:border-green-800 rounded-lg p-3 sm:p-4 text-xs sm:text-sm text-green-800 dark:text-green-200">
            <p className="font-medium">Ready to submit!</p>
            <p className="mt-1">Review your information and click "Create Property" to complete the listing.</p>
          </div>
        </div>
      )}

      {/* Navigation Buttons */}
      <div className="flex flex-col sm:flex-row gap-3 sm:gap-4">
        {currentStepIndex > 0 && (
          <Button
            type="button"
            onClick={goToPreviousStep}
            variant="secondary"
            className="w-full sm:w-auto flex items-center justify-center gap-2"
          >
            <ChevronLeft className="w-4 h-4" />
            Previous
          </Button>
        )}
        
        {!isLastStep ? (
          <Button
            type="button"
            onClick={goToNextStep}
            disabled={loading}
            className="w-full sm:flex-1 flex items-center justify-center gap-2"
          >
            Next
            <ChevronRight className="w-4 h-4" />
          </Button>
        ) : (
          <Button
            type="submit"
            disabled={loading}
            className="w-full sm:flex-1"
          >
            {loading ? 'Creating...' : isEdit ? 'Update Property' : 'Create Property'}
          </Button>
        )}
      </div>
    </form>
  );
}
