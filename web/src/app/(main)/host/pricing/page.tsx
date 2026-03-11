'use client';

import { useAuth } from '@/store/auth-store';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import Link from 'next/link';
import { 
  TrendingUp, 
  Calendar, 
  DollarSign,
  ChevronRight,
  Building,
  Percent,
  AlertCircle,
  Lightbulb,
} from 'lucide-react';

export default function DynamicPricingPage() {
  const { user, isAuthenticated } = useAuth();

  // Fetch host properties
  const { data: propertiesData, isLoading } = useQuery({
    queryKey: ['host', 'properties'],
    queryFn: async () => {
      const response = await apiClient.getHostProperties();
      return response.data;
    },
    enabled: isAuthenticated && user?.role === 'host',
  });

  const properties = propertiesData?.results || [];

  return (
    <div className="min-h-screen bg-sand-100">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Header */}
        <div className="mb-8">
          <h1 className="text-3xl md:text-4xl font-bold text-primary-900 mb-2">
            Dynamic Pricing
          </h1>
          <p className="text-lg text-primary-600">
            Manage pricing rules to maximize your earnings
          </p>
        </div>

        {/* Pricing Overview Cards */}
        <div className="grid grid-cols-1 md:grid-cols-3 gap-6 mb-8">
          <div className="bg-white rounded-lg shadow-sm border border-primary-200 p-6">
            <div className="flex items-center gap-3 mb-4">
              <div className="p-3 bg-green-100 rounded-lg">
                <TrendingUp className="w-6 h-6 text-green-600" />
              </div>
              <h3 className="font-semibold text-primary-900">Seasonal Pricing</h3>
            </div>
            <p className="text-sm text-primary-600">
              Increase prices during peak seasons and holidays for maximum revenue.
            </p>
          </div>

          <div className="bg-white rounded-lg shadow-sm border border-primary-200 p-6">
            <div className="flex items-center gap-3 mb-4">
              <div className="p-3 bg-blue-100 rounded-lg">
                <Calendar className="w-6 h-6 text-blue-600" />
              </div>
              <h3 className="font-semibold text-primary-900">Weekend Premium</h3>
            </div>
            <p className="text-sm text-primary-600">
              Automatically apply higher rates for weekend bookings.
            </p>
          </div>

          <div className="bg-white rounded-lg shadow-sm border border-primary-200 p-6">
            <div className="flex items-center gap-3 mb-4">
              <div className="p-3 bg-purple-100 rounded-lg">
                <Percent className="w-6 h-6 text-purple-600" />
              </div>
              <h3 className="font-semibold text-primary-900">Length Discounts</h3>
            </div>
            <p className="text-sm text-primary-600">
              Offer discounts for longer stays to increase booking duration.
            </p>
          </div>
        </div>

        {/* Available Rule Types */}
        <div className="bg-white rounded-lg shadow-sm border border-primary-200 p-6 mb-8">
          <h2 className="text-xl font-bold text-primary-900 mb-4">
            Available Pricing Rules
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-5 gap-4">
            <div className="p-4 bg-primary-50 rounded-lg text-center">
              <TrendingUp className="w-8 h-8 text-green-600 mx-auto mb-2" />
              <p className="font-medium text-primary-900 text-sm">Seasonal</p>
            </div>
            <div className="p-4 bg-primary-50 rounded-lg text-center">
              <Calendar className="w-8 h-8 text-blue-600 mx-auto mb-2" />
              <p className="font-medium text-primary-900 text-sm">Weekend</p>
            </div>
            <div className="p-4 bg-primary-50 rounded-lg text-center">
              <Percent className="w-8 h-8 text-purple-600 mx-auto mb-2" />
              <p className="font-medium text-primary-900 text-sm">Length of Stay</p>
            </div>
            <div className="p-4 bg-primary-50 rounded-lg text-center">
              <DollarSign className="w-8 h-8 text-amber-600 mx-auto mb-2" />
              <p className="font-medium text-primary-900 text-sm">Early Bird</p>
            </div>
            <div className="p-4 bg-primary-50 rounded-lg text-center">
              <AlertCircle className="w-8 h-8 text-red-600 mx-auto mb-2" />
              <p className="font-medium text-primary-900 text-sm">Last Minute</p>
            </div>
          </div>
        </div>

        {/* Property Pricing List */}
        <div className="bg-white rounded-lg shadow-sm border border-primary-200">
          <div className="p-6 border-b border-primary-200">
            <h2 className="text-xl font-bold text-primary-900">
              Property Pricing
            </h2>
            <p className="text-sm text-primary-600 mt-1">
              Select a property to manage its pricing rules
            </p>
          </div>

          {isLoading ? (
            <div className="p-6 space-y-4">
              {[1, 2, 3].map((i) => (
                <div key={i} className="animate-pulse">
                  <div className="h-20 bg-primary-200 rounded-lg"></div>
                </div>
              ))}
            </div>
          ) : properties.length > 0 ? (
            <div className="divide-y divide-primary-200">
              {properties.filter((p: any) => p?.id).map((property: any) => (
                <Link
                  key={property.id}
                  href={`/host/properties/${property.id}/pricing`}
                  className="flex items-center justify-between p-6 hover:bg-primary-50 transition-colors"
                >
                  <div className="flex items-center gap-4">
                    <div className="p-3 bg-primary-100 rounded-lg">
                      <Building className="w-6 h-6 text-primary-600" />
                    </div>
                    <div>
                      <h3 className="font-semibold text-primary-900">
                        {property.title}
                      </h3>
                      <p className="text-sm text-primary-600">
                        {property.city}, {property.country}
                      </p>
                      <div className="flex items-center gap-2 mt-1">
                        <span className="text-sm font-medium text-secondary-600">
                          ${property.price_per_night}/night
                        </span>
                        <span className={`px-2 py-0.5 text-xs font-medium rounded-full ${
                          property.status === 'active'
                            ? 'bg-green-100 text-green-800'
                            : 'bg-primary-100 text-primary-800'
                        }`}>
                          {property.status}
                        </span>
                      </div>
                    </div>
                  </div>
                  <ChevronRight className="w-5 h-5 text-primary-400" />
                </Link>
              ))}
            </div>
          ) : (
            <div className="p-12 text-center">
              <Building className="w-16 h-16 text-primary-300 mx-auto mb-4" />
              <h3 className="text-lg font-semibold text-primary-900 mb-2">
                No Properties Yet
              </h3>
              <p className="text-primary-600 mb-6">
                Add a property to start managing its pricing rules.
              </p>
              <Link
                href="/host/properties/new"
                className="inline-flex items-center gap-2 px-6 py-3 bg-secondary-600 text-white rounded-lg hover:bg-secondary-700 transition-colors"
              >
                Add Property
              </Link>
            </div>
          )}
        </div>

        {/* Tips Section */}
        <div className="mt-8 bg-blue-50 border border-blue-200 rounded-lg p-6">
          <h3 className="font-semibold text-blue-900 mb-3 flex items-center gap-2">
            <Lightbulb className="h-5 w-5" /> Pricing Tips
          </h3>
          <ul className="space-y-2 text-sm text-blue-800">
            <li>• Use seasonal pricing to capture peak demand during holidays</li>
            <li>• Offer early bird discounts (10-15%) to secure bookings in advance</li>
            <li>• Apply last-minute discounts to fill vacant dates</li>
            <li>• Weekend premiums of 10-20% are common in most markets</li>
            <li>• Length of stay discounts encourage longer bookings</li>
          </ul>
        </div>
      </div>
    </div>
  );
}
