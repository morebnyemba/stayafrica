'use client';

import { useState, useEffect } from 'react';
import { useAuth } from '@/store/auth-store';
import dynamic from 'next/dynamic';
const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });
import { User, Mail, Phone, MapPin, CreditCard, Shield, Bell, Camera, Save, Loader2, Heart } from 'lucide-react';
import { toast } from 'react-hot-toast';
import { WORLD_COUNTRIES } from '@/lib/countries';
import { Button } from '@/components/ui/Button';
import TwoFactorSettings from '@/components/settings/TwoFactorSettings';
import { apiClient } from '@/services/api-client';
import { useQuery, useQueryClient } from '@tanstack/react-query';

type Tab = 'profile' | 'security' | 'payments' | 'notifications' | 'preferences';

export function ProfileContent() {
  const { user, updateProfile } = useAuth();
  const [activeTab, setActiveTab] = useState<Tab>('profile');
  const [isLoading, setIsLoading] = useState(false);
  const [formData, setFormData] = useState({
    first_name: user?.first_name || '',
    last_name: user?.last_name || '',
    email: user?.email || '',
    phone_number: user?.phone_number || '',
    country_of_residence: user?.country_of_residence || '',
  });

  // Sync form data when user data loads or changes
  useEffect(() => {
    if (user) {
      setFormData({
        first_name: user.first_name || '',
        last_name: user.last_name || '',
        email: user.email || '',
        phone_number: user.phone_number || '',
        country_of_residence: user.country_of_residence || '',
      });
    }
  }, [user]);

  const handleUpdateProfile = async (e: React.FormEvent) => {
    e.preventDefault();
    setIsLoading(true);

    try {
      await updateProfile(formData);
      toast.success('Profile updated successfully!');
    } catch (error) {
      toast.error('Failed to update profile');
      console.error('Update error:', error);
    } finally {
      setIsLoading(false);
    }
  };

  const tabs = [
    { id: 'profile' as Tab, label: 'Profile', icon: User },
    { id: 'security' as Tab, label: 'Security', icon: Shield },
    { id: 'payments' as Tab, label: 'Payments', icon: CreditCard },
    { id: 'notifications' as Tab, label: 'Notifications', icon: Bell },
    { id: 'preferences' as Tab, label: 'Preferences', icon: Heart },
  ];

  return (
    <ProtectedRoute>
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
          {/* Header */}
          <div className="mb-8">
            <h1 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-2">
              Account Settings
            </h1>
            <p className="text-lg text-primary-600 dark:text-sand-200">
              Manage your profile and preferences
            </p>
          </div>

          <div className="grid grid-cols-1 lg:grid-cols-4 gap-8">
            {/* Sidebar Tabs */}
            <div className="lg:col-span-1">
              <div className="card p-4">
                <nav className="space-y-2">
                  {tabs.map((tab) => (
                    <button
                      key={tab.id}
                      onClick={() => setActiveTab(tab.id)}
                      className={`w-full flex items-center gap-3 px-4 py-3 rounded-lg transition ${activeTab === tab.id
                        ? 'bg-secondary-500 text-primary-900 font-semibold'
                        : 'text-primary-700 dark:text-sand-200 hover:bg-primary-100 dark:hover:bg-primary-700'
                        }`}
                    >
                      <tab.icon className="w-5 h-5" />
                      <span>{tab.label}</span>
                    </button>
                  ))}
                </nav>
              </div>
            </div>

            {/* Main Content */}
            <div className="lg:col-span-3">
              {/* Profile Tab */}
              {activeTab === 'profile' && (
                <div className="card p-4 sm:p-8">
                  <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-6">
                    Profile Information
                  </h2>

                  {/* Profile Picture */}
                  <div className="flex flex-col sm:flex-row items-center gap-4 sm:gap-6 mb-8 pb-8 border-b border-primary-200 dark:border-primary-700">
                    <div className="relative flex-shrink-0">
                      {user?.profile_picture ? (
                        <img
                          src={user.profile_picture}
                          alt={user.first_name}
                          className="w-20 h-20 sm:w-24 sm:h-24 rounded-full object-cover"
                        />
                      ) : (
                        <div className="w-20 h-20 sm:w-24 sm:h-24 rounded-full bg-secondary-500 flex items-center justify-center">
                          <User className="w-10 h-10 sm:w-12 sm:h-12 text-primary-900" />
                        </div>
                      )}
                      <button className="absolute bottom-0 right-0 p-2 bg-white dark:bg-primary-800 rounded-full border-2 border-primary-200 dark:border-primary-700 hover:bg-primary-50 dark:hover:bg-primary-700 transition">
                        <Camera className="w-4 h-4 text-primary-900 dark:text-sand-100" />
                      </button>
                    </div>
                    <div className="text-center sm:text-left">
                      <h3 className="text-lg sm:text-xl font-semibold text-primary-900 dark:text-sand-50 mb-1">
                        {user?.first_name} {user?.last_name}
                      </h3>
                      <p className="text-primary-600 dark:text-sand-300 mb-2">
                        {user?.email}
                      </p>
                      <span className={`px-3 py-1 rounded-full text-xs font-semibold ${user?.is_verified
                        ? 'bg-green-100 dark:bg-green-900/30 text-green-800 dark:text-green-300'
                        : 'bg-yellow-100 dark:bg-yellow-900/30 text-yellow-800 dark:text-yellow-300'
                        }`}>
                        {user?.is_verified ? 'Verified' : 'Unverified'}
                      </span>
                    </div>
                  </div>

                  {/* Profile Form */}
                  <form onSubmit={handleUpdateProfile} className="space-y-6">
                    <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                      <div>
                        <label htmlFor="first_name" className="block text-sm font-medium text-primary-900 dark:text-sand-100 mb-2">
                          First Name
                        </label>
                        <div className="relative">
                          <User className="absolute left-3 top-1/2 -translate-y-1/2 w-5 h-5 text-primary-400 dark:text-sand-400" />
                          <input
                            id="first_name"
                            type="text"
                            value={formData.first_name}
                            onChange={(e) => setFormData({ ...formData, first_name: e.target.value })}
                            className="w-full pl-10 pr-4 py-3 bg-sand-50 dark:bg-primary-700 border border-primary-200 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent text-primary-900 dark:text-sand-100"
                          />
                        </div>
                      </div>

                      <div>
                        <label htmlFor="last_name" className="block text-sm font-medium text-primary-900 dark:text-sand-100 mb-2">
                          Last Name
                        </label>
                        <div className="relative">
                          <User className="absolute left-3 top-1/2 -translate-y-1/2 w-5 h-5 text-primary-400 dark:text-sand-400" />
                          <input
                            id="last_name"
                            type="text"
                            value={formData.last_name}
                            onChange={(e) => setFormData({ ...formData, last_name: e.target.value })}
                            className="w-full pl-10 pr-4 py-3 bg-sand-50 dark:bg-primary-700 border border-primary-200 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent text-primary-900 dark:text-sand-100"
                          />
                        </div>
                      </div>
                    </div>

                    <div>
                      <label htmlFor="email" className="block text-sm font-medium text-primary-900 dark:text-sand-100 mb-2">
                        Email Address
                      </label>
                      <div className="relative">
                        <Mail className="absolute left-3 top-1/2 -translate-y-1/2 w-5 h-5 text-primary-400 dark:text-sand-400" />
                        <input
                          id="email"
                          type="email"
                          value={formData.email}
                          onChange={(e) => setFormData({ ...formData, email: e.target.value })}
                          className="w-full pl-10 pr-4 py-3 bg-sand-50 dark:bg-primary-700 border border-primary-200 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent text-primary-900 dark:text-sand-100"
                        />
                      </div>
                    </div>

                    <div>
                      <label htmlFor="phone_number" className="block text-sm font-medium text-primary-900 dark:text-sand-100 mb-2">
                        Phone Number
                      </label>
                      <div className="relative">
                        <Phone className="absolute left-3 top-1/2 -translate-y-1/2 w-5 h-5 text-primary-400 dark:text-sand-400" />
                        <input
                          id="phone_number"
                          type="tel"
                          value={formData.phone_number}
                          onChange={(e) => setFormData({ ...formData, phone_number: e.target.value })}
                          className="w-full pl-10 pr-4 py-3 bg-sand-50 dark:bg-primary-700 border border-primary-200 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent text-primary-900 dark:text-sand-100"
                        />
                      </div>
                    </div>

                    <div>
                      <label htmlFor="country_of_residence" className="block text-sm font-medium text-primary-900 dark:text-sand-100 mb-2">
                        Country of Residence
                      </label>
                      <div className="relative">
                        <MapPin className="absolute left-3 top-1/2 -translate-y-1/2 w-5 h-5 text-primary-400 dark:text-sand-400" />
                        <select
                          id="country_of_residence"
                          value={formData.country_of_residence}
                          onChange={(e) => setFormData({ ...formData, country_of_residence: e.target.value })}
                          className="w-full pl-10 pr-4 py-3 bg-sand-50 dark:bg-primary-700 border border-primary-200 dark:border-primary-600 rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent text-primary-900 dark:text-sand-100"
                        >
                          <option value="">Select your country</option>
                          {WORLD_COUNTRIES.map((country) => (
                            <option key={country.code} value={country.name}>
                              {country.name}
                            </option>
                          ))}
                        </select>
                      </div>
                    </div>

                    <div className="flex gap-4 pt-6">
                      <Button
                        type="submit"
                        variant="primary"
                        size="lg"
                        disabled={isLoading}
                        className="gap-2"
                      >
                        {isLoading ? (
                          <>
                            <Loader2 className="w-5 h-5 animate-spin" />
                            <span>Saving...</span>
                          </>
                        ) : (
                          <>
                            <Save className="w-5 h-5" />
                            <span>Save Changes</span>
                          </>
                        )}
                      </Button>
                      <Button type="button" variant="secondary" size="lg">
                        Cancel
                      </Button>
                    </div>
                  </form>
                </div>
              )}

              {/* Security Tab */}
              {activeTab === 'security' && (
                <div className="card p-4 sm:p-8">
                  <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-6">
                    Security Settings
                  </h2>
                  <div className="space-y-6">
                    <div className="p-6 bg-sand-50 dark:bg-primary-700 rounded-lg">
                      <h3 className="font-semibold text-primary-900 dark:text-sand-50 mb-2">
                        Change Password
                      </h3>
                      <p className="text-sm text-primary-600 dark:text-sand-300 mb-4">
                        Update your password to keep your account secure
                      </p>
                      <Button variant="secondary" size="sm">
                        Update Password
                      </Button>
                    </div>

                    <div className="p-6 bg-sand-50 dark:bg-primary-700 rounded-lg">
                      <h3 className="font-semibold text-primary-900 dark:text-sand-50 mb-2">
                        Two-Factor Authentication
                      </h3>
                      <p className="text-sm text-primary-600 dark:text-sand-300 mb-4">
                        Add an extra layer of security to your account
                      </p>
                      <TwoFactorSettings />
                    </div>
                  </div>
                </div>
              )}

              {/* Payments Tab */}
              {activeTab === 'payments' && (
                <div className="card p-4 sm:p-8">
                  <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-6">
                    Payment Methods
                  </h2>
                  <div className="text-center py-12">
                    <CreditCard className="w-16 h-16 text-primary-300 dark:text-primary-700 mx-auto mb-4" />
                    <h3 className="text-lg font-semibold text-primary-900 dark:text-sand-50 mb-2">
                      No Payment Methods
                    </h3>
                    <p className="text-primary-600 dark:text-sand-300 mb-6">
                      Add a payment method to make bookings easier
                    </p>
                    <Button variant="primary" size="lg">
                      Add Payment Method
                    </Button>
                  </div>
                </div>
              )}

              {/* Notifications Tab */}
              {activeTab === 'notifications' && (
                <div className="card p-4 sm:p-8">
                  <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-6">
                    Notification Preferences
                  </h2>
                  <div className="space-y-6">
                    <div className="flex items-center justify-between p-4 border border-primary-200 dark:border-primary-700 rounded-lg">
                      <div>
                        <h3 className="font-semibold text-primary-900 dark:text-sand-50 mb-1">
                          Email Notifications
                        </h3>
                        <p className="text-sm text-primary-600 dark:text-sand-300">
                          Receive updates about your bookings
                        </p>
                      </div>
                      <label className="relative inline-flex items-center cursor-pointer">
                        <input type="checkbox" className="sr-only peer" defaultChecked />
                        <div className="w-11 h-6 bg-primary-300 peer-focus:outline-none peer-focus:ring-4 peer-focus:ring-secondary-300 dark:peer-focus:ring-secondary-800 rounded-full peer dark:bg-primary-700 peer-checked:after:translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:left-[2px] after:bg-white after:border-primary-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all dark:border-primary-600 peer-checked:bg-secondary-500"></div>
                      </label>
                    </div>

                    <div className="flex items-center justify-between p-4 border border-primary-200 dark:border-primary-700 rounded-lg">
                      <div>
                        <h3 className="font-semibold text-primary-900 dark:text-sand-50 mb-1">
                          SMS Notifications
                        </h3>
                        <p className="text-sm text-primary-600 dark:text-sand-300">
                          Get text messages for important updates
                        </p>
                      </div>
                      <label className="relative inline-flex items-center cursor-pointer">
                        <input type="checkbox" className="sr-only peer" />
                        <div className="w-11 h-6 bg-primary-300 peer-focus:outline-none peer-focus:ring-4 peer-focus:ring-secondary-300 dark:peer-focus:ring-secondary-800 rounded-full peer dark:bg-primary-700 peer-checked:after:translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:left-[2px] after:bg-white after:border-primary-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all dark:border-primary-600 peer-checked:bg-secondary-500"></div>
                      </label>
                    </div>

                    <div className="flex items-center justify-between p-4 border border-primary-200 dark:border-primary-700 rounded-lg">
                      <div>
                        <h3 className="font-semibold text-primary-900 dark:text-sand-50 mb-1">
                          Marketing Emails
                        </h3>
                        <p className="text-sm text-primary-600 dark:text-sand-300">
                          Receive newsletters and promotions
                        </p>
                      </div>
                      <label className="relative inline-flex items-center cursor-pointer">
                        <input type="checkbox" className="sr-only peer" defaultChecked />
                        <div className="w-11 h-6 bg-primary-300 peer-focus:outline-none peer-focus:ring-4 peer-focus:ring-secondary-300 dark:peer-focus:ring-secondary-800 rounded-full peer dark:bg-primary-700 peer-checked:after:translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:left-[2px] after:bg-white after:border-primary-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all dark:border-primary-600 peer-checked:bg-secondary-500"></div>
                      </label>
                    </div>
                  </div>
                </div>
              )}

              {/* Preferences Tab */}
              {activeTab === 'preferences' && <PreferencesTab />}
            </div>
          </div>
        </div>
      </div>
    </ProtectedRoute>
  );
}

const PROPERTY_TYPES = ['lodge', 'cottage', 'room', 'apartment', 'house', 'villa', 'cosy_room'];

function PreferencesTab() {
  const queryClient = useQueryClient();
  const [saving, setSaving] = useState(false);

  const { data: prefsRes, isLoading } = useQuery({
    queryKey: ['user-preferences'],
    queryFn: () => apiClient.getUserPreferences(),
  });
  const prefs = prefsRes?.data;

  const [form, setForm] = useState({
    preferred_property_types: [] as string[],
    preferred_min_price: '',
    preferred_max_price: '',
    preferred_cities: [] as string[],
    usual_guest_count: '',
    preferred_amenities: [] as string[],
  });
  const [cityInput, setCityInput] = useState('');
  const [amenityInput, setAmenityInput] = useState('');

  useEffect(() => {
    if (prefs) {
      setForm({
        preferred_property_types: prefs.preferred_property_types || [],
        preferred_min_price: prefs.preferred_min_price || '',
        preferred_max_price: prefs.preferred_max_price || '',
        preferred_cities: prefs.preferred_cities || [],
        usual_guest_count: prefs.usual_guest_count?.toString() || '',
        preferred_amenities: prefs.preferred_amenities || [],
      });
    }
  }, [prefs]);

  const handleSave = async () => {
    setSaving(true);
    try {
      await apiClient.updateUserPreferences({
        preferred_property_types: form.preferred_property_types,
        preferred_min_price: form.preferred_min_price ? parseFloat(form.preferred_min_price as string) : null,
        preferred_max_price: form.preferred_max_price ? parseFloat(form.preferred_max_price as string) : null,
        preferred_cities: form.preferred_cities,
        usual_guest_count: form.usual_guest_count ? parseInt(form.usual_guest_count as string) : null,
        preferred_amenities: form.preferred_amenities,
      });
      queryClient.invalidateQueries({ queryKey: ['user-preferences'] });
      toast.success('Preferences saved');
    } catch {
      toast.error('Failed to save preferences');
    } finally {
      setSaving(false);
    }
  };

  const togglePropertyType = (type: string) => {
    setForm(f => ({
      ...f,
      preferred_property_types: f.preferred_property_types.includes(type)
        ? f.preferred_property_types.filter(t => t !== type)
        : [...f.preferred_property_types, type],
    }));
  };

  const addCity = () => {
    const city = cityInput.trim();
    if (city && !form.preferred_cities.includes(city)) {
      setForm(f => ({ ...f, preferred_cities: [...f.preferred_cities, city] }));
      setCityInput('');
    }
  };

  const addAmenity = () => {
    const amenity = amenityInput.trim();
    if (amenity && !form.preferred_amenities.includes(amenity)) {
      setForm(f => ({ ...f, preferred_amenities: [...f.preferred_amenities, amenity] }));
      setAmenityInput('');
    }
  };

  if (isLoading) {
    return (
      <div className="card p-4 sm:p-8 text-center">
        <Loader2 className="w-6 h-6 animate-spin mx-auto text-secondary-500" />
      </div>
    );
  }

  return (
    <div className="card p-4 sm:p-8">
      <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-6">
        Travel Preferences
      </h2>
      <div className="space-y-6">
        {/* Property Types */}
        <div>
          <label className="block text-sm font-medium text-primary-700 dark:text-sand-300 mb-2">
            Preferred Property Types
          </label>
          <div className="flex flex-wrap gap-2">
            {PROPERTY_TYPES.map(type => (
              <button
                key={type}
                onClick={() => togglePropertyType(type)}
                className={`px-4 py-2 rounded-full text-sm font-medium border transition-colors ${form.preferred_property_types.includes(type)
                  ? 'bg-secondary-500 text-white border-secondary-500'
                  : 'border-primary-300 dark:border-primary-600 text-primary-700 dark:text-sand-300 hover:border-secondary-400'
                  }`}
              >
                {type.split('_').map(w => w.charAt(0).toUpperCase() + w.slice(1)).join(' ')}
              </button>
            ))}
          </div>
        </div>

        {/* Price Range */}
        <div>
          <label className="block text-sm font-medium text-primary-700 dark:text-sand-300 mb-2">
            Price Range (per night)
          </label>
          <div className="flex flex-wrap items-center gap-3">
            <input
              type="number"
              placeholder="Min"
              value={form.preferred_min_price}
              onChange={e => setForm(f => ({ ...f, preferred_min_price: e.target.value }))}
              className="w-24 sm:w-32 px-3 py-2 border border-primary-300 dark:border-primary-600 rounded-lg bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50"
            />
            <span className="text-primary-600 dark:text-sand-400">—</span>
            <input
              type="number"
              placeholder="Max"
              value={form.preferred_max_price}
              onChange={e => setForm(f => ({ ...f, preferred_max_price: e.target.value }))}
              className="w-24 sm:w-32 px-3 py-2 border border-primary-300 dark:border-primary-600 rounded-lg bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50"
            />
          </div>
        </div>

        {/* Guest Count */}
        <div>
          <label className="block text-sm font-medium text-primary-700 dark:text-sand-300 mb-2">
            Usual Number of Guests
          </label>
          <input
            type="number"
            min="1"
            max="20"
            value={form.usual_guest_count}
            onChange={e => setForm(f => ({ ...f, usual_guest_count: e.target.value }))}
            className="w-24 sm:w-32 px-3 py-2 border border-primary-300 dark:border-primary-600 rounded-lg bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50"
          />
        </div>

        {/* Preferred Cities */}
        <div>
          <label className="block text-sm font-medium text-primary-700 dark:text-sand-300 mb-2">
            Preferred Cities
          </label>
          <div className="flex gap-2 mb-2">
            <input
              type="text"
              placeholder="Add a city…"
              value={cityInput}
              onChange={e => setCityInput(e.target.value)}
              onKeyDown={e => { if (e.key === 'Enter') { e.preventDefault(); addCity(); } }}
              className="flex-1 px-3 py-2 border border-primary-300 dark:border-primary-600 rounded-lg bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50"
            />
            <Button onClick={addCity} size="sm" variant="outline">Add</Button>
          </div>
          <div className="flex flex-wrap gap-2">
            {form.preferred_cities.map(city => (
              <span key={city} className="inline-flex items-center gap-1 bg-primary-100 dark:bg-primary-700 text-primary-800 dark:text-sand-200 px-3 py-1 rounded-full text-sm">
                {city}
                <button onClick={() => setForm(f => ({ ...f, preferred_cities: f.preferred_cities.filter(c => c !== city) }))} className="hover:text-red-500">×</button>
              </span>
            ))}
          </div>
        </div>

        {/* Preferred Amenities */}
        <div>
          <label className="block text-sm font-medium text-primary-700 dark:text-sand-300 mb-2">
            Preferred Amenities
          </label>
          <div className="flex gap-2 mb-2">
            <input
              type="text"
              placeholder="Add an amenity…"
              value={amenityInput}
              onChange={e => setAmenityInput(e.target.value)}
              onKeyDown={e => { if (e.key === 'Enter') { e.preventDefault(); addAmenity(); } }}
              className="flex-1 px-3 py-2 border border-primary-300 dark:border-primary-600 rounded-lg bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50"
            />
            <Button onClick={addAmenity} size="sm" variant="outline">Add</Button>
          </div>
          <div className="flex flex-wrap gap-2">
            {form.preferred_amenities.map(amenity => (
              <span key={amenity} className="inline-flex items-center gap-1 bg-primary-100 dark:bg-primary-700 text-primary-800 dark:text-sand-200 px-3 py-1 rounded-full text-sm">
                {amenity}
                <button onClick={() => setForm(f => ({ ...f, preferred_amenities: f.preferred_amenities.filter(a => a !== amenity) }))} className="hover:text-red-500">×</button>
              </span>
            ))}
          </div>
        </div>

        <Button onClick={handleSave} disabled={saving} className="mt-4">
          {saving ? <><Loader2 className="w-4 h-4 animate-spin mr-2" /> Saving…</> : <><Save className="w-4 h-4 mr-2" /> Save Preferences</>}
        </Button>
      </div>
    </div>
  );
}
