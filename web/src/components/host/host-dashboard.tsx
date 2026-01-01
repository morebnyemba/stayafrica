'use client';

import { useAuth } from '@/store/auth-store';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import Link from 'next/link';
import {
  Building,
  DollarSign,
  Calendar,
  Clock,
  AlertCircle,
  Star,
  MessageSquare,
  BarChart3,
} from 'lucide-react';
import dynamic from 'next/dynamic';
const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });

export function HostDashboard() {
  const { user, isAuthenticated } = useAuth();

  // Redirect if not a host
  if (user && user.role !== 'host') {
    return (
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900 flex items-center justify-center">
        <div className="card p-8 text-center max-w-md">
          <AlertCircle className="w-16 h-16 text-yellow-500 mx-auto mb-4" />
          <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-2">
            Host Access Required
          </h2>
          <p className="text-primary-600 dark:text-sand-300 mb-6">
            You need to have a host account to access this dashboard.
          </p>
          <Link href="/host" className="btn-primary px-6 py-2">
            Become a Host
          </Link>
        </div>
      </div>
    );
  }

  // Fetch host analytics
  const { data: analytics, isLoading: loadingAnalytics } = useQuery({
    queryKey: ['host', 'analytics'],
    queryFn: async () => {
      const response = await apiClient.getHostAnalytics();
      return response.data;
    },
    enabled: isAuthenticated && user?.role === 'host',
  });

  // Fetch host properties
  const { data: propertiesData, isLoading: loadingProperties } = useQuery({
    queryKey: ['host', 'properties'],
    queryFn: async () => {
      const response = await apiClient.getHostProperties();
      return response.data;
    },
    enabled: isAuthenticated && user?.role === 'host',
  });

  // Fetch upcoming check-ins
  const { data: checkinsData } = useQuery({
    queryKey: ['host', 'checkins'],
    queryFn: async () => {
      const response = await apiClient.getUpcomingCheckins(7);
      return response.data?.upcoming_checkins || [];
    },
    enabled: isAuthenticated && user?.role === 'host',
  });

  // Fetch pending actions
  const { data: pendingData } = useQuery({
    queryKey: ['host', 'pending'],
    queryFn: async () => {
      const response = await apiClient.getPendingActions();
      return response.data;
    },
    enabled: isAuthenticated && user?.role === 'host',
    refetchInterval: 30000, // Refresh every 30 seconds
  });

  // Fetch property performance
  const { data: performanceData } = useQuery({
    queryKey: ['host', 'performance'],
    queryFn: async () => {
      const response = await apiClient.getPropertyPerformance();
      return response.data?.properties || [];
    },
    enabled: isAuthenticated && user?.role === 'host',
  });

  const stats = [
    {
      title: 'Active Properties',
      value: analytics?.active_properties || 0,
      icon: Building,
      color: 'text-blue-600 dark:text-blue-400',
      bgColor: 'bg-blue-100 dark:bg-blue-900/30',
      link: '/host/properties',
    },
    {
      title: 'Total Earnings',
      value: `$${analytics?.total_earnings?.toFixed(2) || '0.00'}`,
      icon: DollarSign,
      color: 'text-green-600 dark:text-green-400',
      bgColor: 'bg-green-100 dark:bg-green-900/30',
      link: '/host/earnings',
    },
    {
      title: 'Pending Bookings',
      value: analytics?.pending_bookings || 0,
      icon: Clock,
      color: 'text-yellow-600 dark:text-yellow-400',
      bgColor: 'bg-yellow-100 dark:bg-yellow-900/30',
      link: '/host/bookings?status=pending',
    },
    {
      title: 'Avg Rating',
      value: analytics?.average_rating?.toFixed(1) || '0.0',
      icon: Star,
      color: 'text-purple-600 dark:text-purple-400',
      bgColor: 'bg-purple-100 dark:bg-purple-900/30',
      link: '#reviews',
    },
  ];

  const quickActions = [
    {
      title: 'Add New Property',
      description: 'List a new property',
      icon: Building,
      link: '/host/properties/new',
      color: 'bg-secondary-500',
    },
    {
      title: 'Manage Properties',
      description: 'Edit existing listings',
      icon: Building,
      link: '/host/properties',
      color: 'bg-blue-500',
    },
    {
      title: 'View Bookings',
      description: 'Manage reservations',
      icon: Calendar,
      link: '/host/bookings',
      color: 'bg-green-500',
    },
    {
      title: 'Earnings',
      description: 'Track your revenue',
      icon: DollarSign,
      link: '/host/earnings',
      color: 'bg-purple-500',
    },
    {
      title: 'View Analytics',
      description: 'Performance insights',
      icon: BarChart3,
      link: '/host/earnings',
      color: 'bg-pink-500',
    },
    {
      title: 'Messages',
      description: 'Communicate with guests',
      icon: MessageSquare,
      link: '/messages',
      color: 'bg-orange-500',
    },
  ];

  return (
    <ProtectedRoute>
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          {/* Welcome Section */}
          <div className="mb-8">
            <h1 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-2">
              Welcome back, {user?.first_name}! 🏠
            </h1>
            <p className="text-lg text-primary-600 dark:text-sand-300">
              Here&apos;s how your properties are performing
            </p>
          </div>

          {/* Pending Actions Alert */}
          {pendingData && pendingData.total_pending > 0 && (
            <div className="bg-yellow-50 dark:bg-yellow-900/20 border border-yellow-200 dark:border-yellow-800 rounded-lg p-4 mb-8">
              <div className="flex items-start gap-3">
                <AlertCircle className="w-5 h-5 text-yellow-600 dark:text-yellow-400 flex-shrink-0 mt-0.5" />
                <div className="flex-1">
                  <h3 className="font-semibold text-yellow-900 dark:text-yellow-100 mb-1">
                    You have {pendingData.total_pending} pending action(s)
                  </h3>
                  <ul className="text-sm text-yellow-800 dark:text-yellow-200 space-y-1">
                    {pendingData.pending_bookings > 0 && (
                      <li>• {pendingData.pending_bookings} booking request(s) awaiting response</li>
                    )}
                    {pendingData.pending_properties > 0 && (
                      <li>• {pendingData.pending_properties} property(ies) pending approval</li>
                    )}
                    {pendingData.unread_messages > 0 && (
                      <li>• {pendingData.unread_messages} unread message(s)</li>
                    )}
                    {pendingData.needs_completion > 0 && (
                      <li>• {pendingData.needs_completion} booking(s) need completion</li>
                    )}
                  </ul>
                </div>
              </div>
            </div>
          )}

          {/* Stats Grid */}
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
            {loadingAnalytics ? (
              Array.from({ length: 4 }).map((_, i) => (
                <div key={i} className="card p-6 animate-pulse">
                  <div className="h-20 bg-primary-200 dark:bg-primary-700 rounded"></div>
                </div>
              ))
            ) : (
              stats.map((stat, index) => (
                <Link
                  key={index}
                  href={stat.link}
                  className="card p-6 hover:shadow-lg transition-shadow"
                >
                  <div className="flex items-center justify-between">
                    <div>
                      <p className="text-sm text-primary-600 dark:text-sand-400 mb-1">
                        {stat.title}
                      </p>
                      <p className="text-3xl font-bold text-primary-900 dark:text-sand-50">
                        {stat.value}
                      </p>
                    </div>
                    <div className={`${stat.bgColor} ${stat.color} p-3 rounded-full`}>
                      <stat.icon className="w-6 h-6" />
                    </div>
                  </div>
                </Link>
              ))
            )}
          </div>

          <div className="grid grid-cols-1 lg:grid-cols-3 gap-8 mb-8">
            {/* Recent Properties */}
            <div className="lg:col-span-2">
              <div className="card p-6">
                <div className="flex items-center justify-between mb-6">
                  <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50">
                    Your Properties
                  </h2>
                  <Link
                    href="/host/properties"
                    className="text-secondary-600 dark:text-secondary-400 hover:underline text-sm font-medium"
                  >
                    View All
                  </Link>
                </div>

                {loadingProperties ? (
                  <div className="space-y-4">
                    {[1, 2, 3].map((i) => (
                      <div key={i} className="animate-pulse">
                        <div className="h-24 bg-primary-200 dark:bg-primary-700 rounded-lg"></div>
                      </div>
                    ))}
                  </div>
                ) : propertiesData?.results && propertiesData.results.filter((p: any) => p?.id).length > 0 ? (
                  <div className="space-y-4">
                    {propertiesData.results
                      .filter((p: any) => p?.id)
                      .slice(0, 3)
                      .map((property: any) => (
                      <div
                        key={property.id}
                        className="p-4 border border-primary-200 dark:border-primary-700 rounded-lg hover:border-secondary-500 transition"
                      >
                        <div className="flex items-start justify-between">
                          <div className="flex-1">
                            <h3 className="font-semibold text-primary-900 dark:text-sand-50 mb-1">
                              {property.title}
                            </h3>
                            <p className="text-sm text-primary-600 dark:text-sand-300">
                              {property.city}, {property.country}
                            </p>
                            <p className="text-sm text-primary-700 dark:text-sand-200 mt-1">
                              ${property.price_per_night}/night
                            </p>
                          </div>
                          <div className="text-right">
                            <span
                              className={`px-3 py-1 text-xs font-semibold rounded-full ${
                                property.status === 'active'
                                  ? 'bg-green-100 dark:bg-green-900/30 text-green-800 dark:text-green-300'
                                  : property.status === 'pending_approval'
                                  ? 'bg-yellow-100 dark:bg-yellow-900/30 text-yellow-800 dark:text-yellow-300'
                                  : 'bg-gray-100 dark:bg-gray-900/30 text-gray-800 dark:text-gray-300'
                              }`}
                            >
                              {property.status.replace('_', ' ')}
                            </span>
                          </div>
                        </div>
                      </div>
                    ))}
                  </div>
                ) : (
                  <div className="text-center py-12">
                    <Building className="w-16 h-16 text-primary-300 dark:text-primary-700 mx-auto mb-4" />
                    <h3 className="text-lg font-semibold text-primary-900 dark:text-sand-50 mb-2">
                      No Properties Yet
                    </h3>
                    <p className="text-primary-600 dark:text-sand-300 mb-4">
                      Start earning by listing your first property!
                    </p>
                    <Link href="/host/properties/new" className="btn-primary px-6 py-2">
                      Add Property
                    </Link>
                  </div>
                )}
              </div>
            </div>

            {/* Quick Actions */}
            <div className="lg:col-span-1">
              <div className="card p-6">
                <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-6">
                  Quick Actions
                </h2>
                <div className="space-y-3">
                  {quickActions.map((action, index) => (
                    <Link
                      key={index}
                      href={action.link}
                      className="flex items-center gap-4 p-4 border border-primary-200 dark:border-primary-700 rounded-lg hover:border-secondary-500 hover:bg-secondary-50 dark:hover:bg-secondary-900/10 transition group"
                    >
                      <div
                        className={`${action.color} p-2 rounded-lg text-white group-hover:scale-110 transition-transform`}
                      >
                        <action.icon className="w-5 h-5" />
                      </div>
                      <div>
                        <div className="font-semibold text-primary-900 dark:text-sand-50 text-sm">
                          {action.title}
                        </div>
                        <div className="text-xs text-primary-600 dark:text-sand-400">
                          {action.description}
                        </div>
                      </div>
                    </Link>
                  ))}
                </div>
              </div>
            </div>
          </div>

          {/* Upcoming Check-ins */}
          {checkinsData && checkinsData.length > 0 && (
            <div className="card p-6 mb-8">
              <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-6">
                Upcoming Check-ins (Next 7 Days)
              </h2>
              <div className="space-y-4">
                {checkinsData.map((checkin: any) => (
                  <div
                    key={checkin.booking_id}
                    className="p-4 border border-primary-200 dark:border-primary-700 rounded-lg"
                  >
                    <div className="flex items-start justify-between">
                      <div className="flex-1">
                        <h3 className="font-semibold text-primary-900 dark:text-sand-50 mb-1">
                          {checkin.property_title}
                        </h3>
                        <p className="text-sm text-primary-600 dark:text-sand-300">
                          Guest: {checkin.guest_name}
                        </p>
                        <div className="flex items-center gap-4 text-sm mt-2">
                          <div className="flex items-center gap-1">
                            <Calendar className="w-4 h-4 text-primary-400" />
                            <span className="text-primary-700 dark:text-sand-200">
                              Check-in: {new Date(checkin.check_in).toLocaleDateString()}
                            </span>
                          </div>
                          <span className="text-primary-400">→</span>
                          <div className="flex items-center gap-1">
                            <Calendar className="w-4 h-4 text-primary-400" />
                            <span className="text-primary-700 dark:text-sand-200">
                              Check-out: {new Date(checkin.check_out).toLocaleDateString()}
                            </span>
                          </div>
                        </div>
                      </div>
                      <Link
                        href={`/host/bookings`}
                        className="text-secondary-600 dark:text-secondary-400 hover:underline text-sm font-medium"
                      >
                        View Details
                      </Link>
                    </div>
                  </div>
                ))}
              </div>
            </div>
          )}

          {/* Performance Overview */}
          {performanceData && performanceData.length > 0 && (
            <div className="card p-6">
              <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-6">
                Property Performance
              </h2>
              <div className="overflow-x-auto">
                <table className="w-full">
                  <thead>
                    <tr className="border-b border-primary-200 dark:border-primary-700">
                      <th className="text-left py-3 px-4 text-primary-900 dark:text-sand-50">
                        Property
                      </th>
                      <th className="text-center py-3 px-4 text-primary-900 dark:text-sand-50">
                        Bookings
                      </th>
                      <th className="text-center py-3 px-4 text-primary-900 dark:text-sand-50">
                        Earnings
                      </th>
                      <th className="text-center py-3 px-4 text-primary-900 dark:text-sand-50">
                        Rating
                      </th>
                      <th className="text-center py-3 px-4 text-primary-900 dark:text-sand-50">
                        Status
                      </th>
                    </tr>
                  </thead>
                  <tbody>
                    {performanceData.slice(0, 5).map((prop: any) => (
                      <tr
                        key={prop.property_id}
                        className="border-b border-primary-100 dark:border-primary-800"
                      >
                        <td className="py-3 px-4 text-primary-900 dark:text-sand-50">
                          {prop.property_title}
                        </td>
                        <td className="py-3 px-4 text-center text-primary-700 dark:text-sand-200">
                          {prop.total_bookings}
                        </td>
                        <td className="py-3 px-4 text-center text-primary-700 dark:text-sand-200">
                          ${prop.total_earnings.toFixed(2)}
                        </td>
                        <td className="py-3 px-4 text-center">
                          <div className="flex items-center justify-center gap-1">
                            <Star className="w-4 h-4 text-yellow-500 fill-current" />
                            <span className="text-primary-700 dark:text-sand-200">
                              {prop.average_rating.toFixed(1)}
                            </span>
                          </div>
                        </td>
                        <td className="py-3 px-4 text-center">
                          <span
                            className={`px-2 py-1 text-xs font-semibold rounded-full ${
                              prop.status === 'active'
                                ? 'bg-green-100 dark:bg-green-900/30 text-green-800 dark:text-green-300'
                                : 'bg-gray-100 dark:bg-gray-900/30 text-gray-800 dark:text-gray-300'
                            }`}
                          >
                            {prop.status}
                          </span>
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </div>
          )}
        </div>
      </div>
    </ProtectedRoute>
  );
}
