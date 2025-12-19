'use client';

import { useState } from 'react';
import { useAuth } from '@/context/auth-context';
import dynamic from 'next/dynamic';
const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });
import { User, Mail, Phone, MapPin, CreditCard, Shield, Bell, Camera, Save, Loader2 } from 'lucide-react';
import { toast } from 'react-hot-toast';
import { WORLD_COUNTRIES } from '@/lib/countries';

type Tab = 'profile' | 'security' | 'payments' | 'notifications';

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
                      className={`w-full flex items-center gap-3 px-4 py-3 rounded-lg transition ${
                        activeTab === tab.id
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
                <div className="card p-8">
                  <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-6">
                    Profile Information
                  </h2>

                  {/* Profile Picture */}
                  <div className="flex items-center gap-6 mb-8 pb-8 border-b border-primary-200 dark:border-primary-700">
                    <div className="relative">
                      {user?.profile_picture ? (
                        <img
                          src={user.profile_picture}
                          alt={user.first_name}
                          className="w-24 h-24 rounded-full object-cover"
                        />
                      ) : (
                        <div className="w-24 h-24 rounded-full bg-secondary-500 flex items-center justify-center">
                          <User className="w-12 h-12 text-primary-900" />
                        </div>
                      )}
                      <button className="absolute bottom-0 right-0 p-2 bg-white dark:bg-primary-800 rounded-full border-2 border-primary-200 dark:border-primary-700 hover:bg-primary-50 dark:hover:bg-primary-700 transition">
                        <Camera className="w-4 h-4 text-primary-900 dark:text-sand-100" />
                      </button>
                    </div>
                    <div>
                      <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-1">
                        {user?.first_name} {user?.last_name}
                      </h3>
                      <p className="text-primary-600 dark:text-sand-300 mb-2">
                        {user?.email}
                      </p>
                      <span className={`px-3 py-1 rounded-full text-xs font-semibold ${
                        user?.is_verified
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
                            <option key={country} value={country}>
                              {country}
                            </option>
                          ))}
                        </select>
                      </div>
                    </div>

                    <div className="flex gap-4 pt-6">
                      <button
                        type="submit"
                        disabled={isLoading}
                        className="btn-primary px-6 py-3 flex items-center gap-2 disabled:opacity-50 disabled:cursor-not-allowed"
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
                      </button>
                      <button
                        type="button"
                        className="btn-secondary px-6 py-3"
                      >
                        Cancel
                      </button>
                    </div>
                  </form>
                </div>
              )}

              {/* Security Tab */}
              {activeTab === 'security' && (
                <div className="card p-8">
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
                      <button className="btn-secondary px-4 py-2">
                        Update Password
                      </button>
                    </div>

                    <div className="p-6 bg-sand-50 dark:bg-primary-700 rounded-lg">
                      <h3 className="font-semibold text-primary-900 dark:text-sand-50 mb-2">
                        Two-Factor Authentication
                      </h3>
                      <p className="text-sm text-primary-600 dark:text-sand-300 mb-4">
                        Add an extra layer of security to your account
                      </p>
                      <button className="btn-primary px-4 py-2">
                        Enable 2FA
                      </button>
                    </div>
                  </div>
                </div>
              )}

              {/* Payments Tab */}
              {activeTab === 'payments' && (
                <div className="card p-8">
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
                    <button className="btn-primary px-6 py-3">
                      Add Payment Method
                    </button>
                  </div>
                </div>
              )}

              {/* Notifications Tab */}
              {activeTab === 'notifications' && (
                <div className="card p-8">
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
            </div>
          </div>
        </div>
      </div>
    </ProtectedRoute>
  );
}
