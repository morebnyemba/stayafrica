'use client';

import { useState } from 'react';
import { useQuery, useMutation } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { useRouter } from 'next/navigation';
import { Input, Button } from '@/components/ui';
import {
  MapPin,
  Clock,
  Users,
  DollarSign,
  FileText,
  Tag,
  Save,
  ImageIcon,
  X,
} from 'lucide-react';
import { toast } from 'react-hot-toast';
import type { Experience, ExperienceCategory } from '@/types';

interface ExperienceFormProps {
  experience?: Experience;
  mode: 'create' | 'edit';
}

export function ExperienceForm({ experience, mode }: ExperienceFormProps) {
  const router = useRouter();
  const [imagePreview, setImagePreview] = useState<string | null>(experience?.main_image ?? null);
  const [imageFile, setImageFile] = useState<File | null>(null);

  const [formData, setFormData] = useState({
    title: experience?.title || '',
    description: experience?.description || '',
    category: experience?.category?.toString() || '',
    country: experience?.country || '',
    city: experience?.city || '',
    address: experience?.address || '',
    latitude: experience?.latitude?.toString() || '',
    longitude: experience?.longitude?.toString() || '',
    price_per_person: experience?.price_per_person?.toString() || '',
    currency: experience?.currency || 'USD',
    duration: experience?.duration || 'half_day',
    duration_hours: experience?.duration_hours?.toString() || '',
    difficulty: experience?.difficulty || 'easy',
    min_participants: experience?.min_participants?.toString() || '1',
    max_participants: experience?.max_participants?.toString() || '10',
    included_items: experience?.included_items || '',
    requirements: experience?.requirements || '',
    cancellation_policy: experience?.cancellation_policy || '',
  });

  const [errors, setErrors] = useState<Record<string, string>>({});

  const { data: categoriesData } = useQuery({
    queryKey: ['experience-categories'],
    queryFn: async () => {
      const response = await apiClient.getExperienceCategories();
      return response.data as ExperienceCategory[];
    },
  });

  const categories = categoriesData || [];

  const validate = (): boolean => {
    const newErrors: Record<string, string> = {};

    if (!formData.title.trim()) newErrors.title = 'Title is required';
    if (!formData.description.trim()) newErrors.description = 'Description is required';
    if (!formData.country.trim()) newErrors.country = 'Country is required';
    if (!formData.city.trim()) newErrors.city = 'City is required';
    if (!formData.latitude || isNaN(Number(formData.latitude)))
      newErrors.latitude = 'Valid latitude is required';
    if (!formData.longitude || isNaN(Number(formData.longitude)))
      newErrors.longitude = 'Valid longitude is required';
    if (!formData.price_per_person || Number(formData.price_per_person) <= 0)
      newErrors.price_per_person = 'Price must be greater than 0';
    if (!formData.duration_hours || Number(formData.duration_hours) <= 0)
      newErrors.duration_hours = 'Duration in hours is required';
    if (Number(formData.min_participants) < 1)
      newErrors.min_participants = 'Minimum 1 participant';
    if (Number(formData.max_participants) < Number(formData.min_participants))
      newErrors.max_participants = 'Must be ≥ min participants';

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const mutation = useMutation({
    mutationFn: async () => {
      const payload = new FormData();
      payload.append('title', formData.title);
      payload.append('description', formData.description);
      if (formData.category) payload.append('category', formData.category);
      payload.append('country', formData.country);
      payload.append('city', formData.city);
      payload.append('address', formData.address);
      payload.append('latitude', formData.latitude);
      payload.append('longitude', formData.longitude);
      payload.append('price_per_person', formData.price_per_person);
      payload.append('currency', formData.currency);
      payload.append('duration', formData.duration);
      payload.append('duration_hours', formData.duration_hours);
      payload.append('difficulty', formData.difficulty);
      payload.append('min_participants', formData.min_participants);
      payload.append('max_participants', formData.max_participants);
      payload.append('included_items', formData.included_items);
      payload.append('requirements', formData.requirements);
      payload.append('cancellation_policy', formData.cancellation_policy);

      if (imageFile) {
        payload.append('main_image', imageFile);
      }

      if (mode === 'edit' && experience) {
        return apiClient.updateExperience(experience.id, payload);
      }
      return apiClient.createExperience(payload);
    },
    onSuccess: () => {
      toast.success(mode === 'create' ? 'Experience created!' : 'Experience updated!');
      router.push('/host/experiences');
    },
    onError: (err: any) => {
      const msg = err?.response?.data?.detail || err?.message || 'Something went wrong';
      toast.error(msg);
    },
  });

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    if (!validate()) return;
    mutation.mutate();
  };

  const handleImageChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const file = e.target.files?.[0];
    if (file) {
      setImageFile(file);
      setImagePreview(URL.createObjectURL(file));
    }
  };

  const update = (field: string, value: string) => {
    setFormData((prev) => ({ ...prev, [field]: value }));
    if (errors[field]) setErrors((prev) => ({ ...prev, [field]: '' }));
  };

  return (
        <form onSubmit={handleSubmit} className="space-y-8">
          {/* Basic Info */}
          <section className="card p-6">
            <h2 className="text-lg font-semibold text-primary-900 dark:text-sand-50 mb-4 flex items-center gap-2">
              <FileText className="w-5 h-5 text-secondary-500" />
              Basic Information
            </h2>
            <div className="space-y-4">
              <Input
                label="Title"
                type="text"
                value={formData.title}
                onChange={(e) => update('title', e.target.value)}
                placeholder="e.g. Sunset Safari Drive"
                error={errors.title}
                required
              />

              <Input
                label="Description"
                multiline
                rows={4}
                value={formData.description}
                onChange={(e) => update('description', e.target.value)}
                placeholder="Describe your experience in detail..."
                error={errors.description}
                required
              />

              <Input
                label="Category"
                select
                options={[
                  { value: '', label: 'Select a category' },
                  ...categories.map((c) => ({ value: c.id, label: c.name })),
                ]}
                value={formData.category}
                onChange={(e) => update('category', e.target.value)}
                icon={<Tag className="w-5 h-5" />}
              />
            </div>
          </section>

          {/* Location */}
          <section className="card p-6">
            <h2 className="text-lg font-semibold text-primary-900 dark:text-sand-50 mb-4 flex items-center gap-2">
              <MapPin className="w-5 h-5 text-secondary-500" />
              Location
            </h2>
            <div className="space-y-4">
              <div className="grid grid-cols-1 sm:grid-cols-2 gap-4">
                <Input
                  label="Country"
                  type="text"
                  value={formData.country}
                  onChange={(e) => update('country', e.target.value)}
                  placeholder="e.g. Kenya"
                  error={errors.country}
                  required
                />
                <Input
                  label="City"
                  type="text"
                  value={formData.city}
                  onChange={(e) => update('city', e.target.value)}
                  placeholder="e.g. Nairobi"
                  error={errors.city}
                  required
                />
              </div>

              <Input
                label="Address"
                type="text"
                value={formData.address}
                onChange={(e) => update('address', e.target.value)}
                placeholder="Detailed address (optional)"
              />

              <div className="grid grid-cols-1 sm:grid-cols-2 gap-4">
                <Input
                  label="Latitude"
                  type="number"
                  step="any"
                  value={formData.latitude}
                  onChange={(e) => update('latitude', e.target.value)}
                  placeholder="-1.2921"
                  error={errors.latitude}
                  required
                />
                <Input
                  label="Longitude"
                  type="number"
                  step="any"
                  value={formData.longitude}
                  onChange={(e) => update('longitude', e.target.value)}
                  placeholder="36.8219"
                  error={errors.longitude}
                  required
                />
              </div>
            </div>
          </section>

          {/* Pricing & Duration */}
          <section className="card p-6">
            <h2 className="text-lg font-semibold text-primary-900 dark:text-sand-50 mb-4 flex items-center gap-2">
              <DollarSign className="w-5 h-5 text-secondary-500" />
              Pricing & Duration
            </h2>
            <div className="space-y-4">
              <div className="grid grid-cols-1 sm:grid-cols-2 gap-4">
                <Input
                  label="Price per Person"
                  type="number"
                  step="any"
                  min="0"
                  value={formData.price_per_person}
                  onChange={(e) => update('price_per_person', e.target.value)}
                  placeholder="50.00"
                  error={errors.price_per_person}
                  icon={<DollarSign className="w-5 h-5" />}
                  required
                />
                <Input
                  label="Currency"
                  select
                  options={[
                    { value: 'USD', label: 'USD' },
                    { value: 'EUR', label: 'EUR' },
                    { value: 'GBP', label: 'GBP' },
                    { value: 'ZAR', label: 'ZAR' },
                    { value: 'KES', label: 'KES' },
                    { value: 'NGN', label: 'NGN' },
                    { value: 'ZWL', label: 'ZWL' },
                  ]}
                  value={formData.currency}
                  onChange={(e) => update('currency', e.target.value)}
                />
              </div>

              <div className="grid grid-cols-1 sm:grid-cols-2 gap-4">
                <Input
                  label="Duration Type"
                  select
                  options={[
                    { value: 'hourly', label: 'Hourly' },
                    { value: 'half_day', label: 'Half Day (2-4 hours)' },
                    { value: 'full_day', label: 'Full Day (5-8 hours)' },
                    { value: 'multi_day', label: 'Multi-Day' },
                  ]}
                  value={formData.duration}
                  onChange={(e) => update('duration', e.target.value)}
                  icon={<Clock className="w-5 h-5" />}
                />
                <Input
                  label="Duration (hours)"
                  type="number"
                  step="any"
                  min="0"
                  value={formData.duration_hours}
                  onChange={(e) => update('duration_hours', e.target.value)}
                  placeholder="4"
                  error={errors.duration_hours}
                  required
                />
              </div>

              <Input
                label="Difficulty"
                select
                options={[
                  { value: 'easy', label: 'Easy' },
                  { value: 'moderate', label: 'Moderate' },
                  { value: 'challenging', label: 'Challenging' },
                  { value: 'expert', label: 'Expert' },
                ]}
                value={formData.difficulty}
                onChange={(e) => update('difficulty', e.target.value)}
              />
            </div>
          </section>

          {/* Capacity */}
          <section className="card p-6">
            <h2 className="text-lg font-semibold text-primary-900 dark:text-sand-50 mb-4 flex items-center gap-2">
              <Users className="w-5 h-5 text-secondary-500" />
              Capacity
            </h2>
            <div className="grid grid-cols-1 sm:grid-cols-2 gap-4">
              <Input
                label="Minimum Participants"
                type="number"
                value={formData.min_participants}
                onChange={(e) => update('min_participants', e.target.value)}
                error={errors.min_participants}
                required
              />
              <Input
                label="Maximum Participants"
                type="number"
                value={formData.max_participants}
                onChange={(e) => update('max_participants', e.target.value)}
                error={errors.max_participants}
                required
              />
            </div>
          </section>

          {/* Image */}
          <section className="card p-6">
            <h2 className="text-lg font-semibold text-primary-900 dark:text-sand-50 mb-4 flex items-center gap-2">
              <ImageIcon className="w-5 h-5 text-secondary-500" />
              Cover Image
            </h2>
            <div className="space-y-4">
              {imagePreview ? (
                <div className="relative w-full h-48 rounded-lg overflow-hidden">
                  <img src={imagePreview} alt="Preview" className="w-full h-full object-cover" />
                  <button
                    type="button"
                    onClick={() => {
                      setImagePreview(null);
                      setImageFile(null);
                    }}
                    className="absolute top-2 right-2 p-1 bg-black/50 rounded-full text-white hover:bg-black/70"
                  >
                    <X className="w-5 h-5" />
                  </button>
                </div>
              ) : (
                <label className="w-full h-48 border-2 border-dashed border-primary-300 dark:border-primary-600 rounded-lg cursor-pointer hover:border-secondary-500 transition-colors flex items-center justify-center">
                  <div className="text-center">
                    <ImageIcon className="w-10 h-10 text-primary-400 dark:text-sand-500 mx-auto mb-2" />
                    <p className="text-sm text-primary-600 dark:text-sand-300">
                      Click to upload a cover image
                    </p>
                  </div>
                  <input
                    type="file"
                    accept="image/*"
                    onChange={handleImageChange}
                    className="hidden"
                  />
                </label>
              )}
            </div>
          </section>

          {/* Additional Details */}
          <section className="card p-6">
            <h2 className="text-lg font-semibold text-primary-900 dark:text-sand-50 mb-4">
              Additional Details
            </h2>
            <div className="space-y-4">
              <Input
                label="What's Included"
                multiline
                rows={3}
                value={formData.included_items}
                onChange={(e) => update('included_items', e.target.value)}
                placeholder="One item per line, e.g.&#10;Transport&#10;Lunch&#10;Equipment"
                helpText="List what's included for participants (one per line)"
              />

              <Input
                label="Requirements"
                multiline
                rows={3}
                value={formData.requirements}
                onChange={(e) => update('requirements', e.target.value)}
                placeholder="e.g. Minimum age 12, comfortable walking shoes required"
                helpText="Any requirements for participants"
              />

              <Input
                label="Cancellation Policy"
                multiline
                rows={2}
                value={formData.cancellation_policy}
                onChange={(e) => update('cancellation_policy', e.target.value)}
                placeholder="e.g. Free cancellation up to 48 hours before"
              />
            </div>
          </section>

          {/* Submit */}
          <div className="flex flex-col sm:flex-row gap-4">
            <Button
              type="button"
              variant="outline"
              size="lg"
              onClick={() => router.back()}
              className="flex-1 sm:flex-none"
            >
              Cancel
            </Button>
            <Button
              type="submit"
              size="lg"
              isLoading={mutation.isPending}
              disabled={mutation.isPending}
              className="flex-1"
            >
              <Save className="w-5 h-5 mr-2" />
              {mode === 'create' ? 'Create Experience' : 'Save Changes'}
            </Button>
          </div>
        </form>
  );
}
