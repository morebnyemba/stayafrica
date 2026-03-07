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
  ShieldCheck,
  Receipt,
  Settings,
  TrendingUp,
  Home,
  Zap,
  ArrowRight,
  Users,
  MapPin,
} from 'lucide-react';
import { AnalyticsDashboard } from '@/components/analytics';
import { VerificationStatus } from '@/components/verification/VerificationStatus';
import { useState } from 'react';
import dynamic from 'next/dynamic';
const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });

export function HostDashboard() {
  const { user, isAuthenticated } = useAuth();
  const [activeTab, setActiveTab] = useState<'overview' | 'analytics'>('overview');

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
          <Link href="/host" className="inline-flex items-center gap-2 bg-secondary-600 hover:bg-secondary-700 text-white font-semibold py-3 px-6 rounded-lg transition">
            Become a Host
          </Link>
        </div>
      </div>
    );
  }

  const { data: analytics, isLoading: loadingAnalytics } = useQuery({
    queryKey: ['host', 'analytics'],
    queryFn: async () => {
      const response = await apiClient.getHostAnalytics();
      return response.data;
    },
    enabled: isAuthenticated && user?.role === 'host',
  });

  const { data: propertiesData, isLoading: loadingProperties } = useQuery({
    queryKey: ['host', 'properties'],
    queryFn: async () => {
      const response = await apiClient.getHostProperties();
      return response.data;
    },
    enabled: isAuthenticated && user?.role === 'host',
  });

  const { data: checkinsData } = useQuery({
    queryKey: ['host', 'checkins'],
    queryFn: async () => {
      const response = await apiClient.getUpcomingCheckins(7);
      return response.data?.upcoming_checkins || [];
    },
    enabled: isAuthenticated && user?.role === 'host',
  });

  const { data: pendingData } = useQuery({
    queryKey: ['host', 'pending'],
    queryFn: async () => {
      const response = await apiClient.getPendingActions();
      return response.data;
    },
    enabled: isAuthenticated && user?.role === 'host',
    refetchInterval: 30000,
  });

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
      badge: (analytics?.pending_bookings || 0) > 0,
    },
    {
      title: 'Avg Rating',
      value: analytics?.average_rating?.toFixed(1) || '—',
      icon: Star,
      color: 'text-secondary-600 dark:text-secondary-400',
      bgColor: 'bg-secondary-100 dark:bg-secondary-900/30',
      link: '/host/reviews',
    },
  ];

  const formatDate = (d: string) =>
    new Date(d).toLocaleDateString('en-US', { month: 'short', day: 'numeric' });

  // Organized quick actions in logical groups
  const managementLinks = [
    { title: 'Properties', icon: Building, link: '/host/properties', color: 'bg-blue-500' },
    { title: 'Add New', icon: Building, link: '/host/properties/new', color: 'bg-secondary-500' },
    { title: 'Bookings', icon: Calendar, link: '/host/bookings', color: 'bg-green-500' },
    { title: 'Messages', icon: MessageSquare, link: '/messages', color: 'bg-orange-500' },
  ];

  const financeLinks = [
    { title: 'Earnings', icon: DollarSign, link: '/host/earnings', color: 'bg-green-500' },
    { title: 'Analytics', icon: BarChart3, link: '/host/earnings', color: 'bg-pink-500' },
    { title: 'Pricing', icon: TrendingUp, link: '/host/pricing', color: 'bg-amber-500' },
    { title: 'Tax Reports', icon: Receipt, link: '/host/tax-reports', color: 'bg-teal-500' },
  ];

  const moreLinks = [
    { title: 'Auto-Messaging', icon: Zap, link: '/host/messaging', color: 'bg-violet-500' },
    ...(!user?.is_verified ? [{ title: 'Verify ID', icon: ShieldCheck, link: '/host/verification', color: 'bg-indigo-500' }] : []),
    { title: 'Settings', icon: Settings, link: '/host/settings', color: 'bg-primary-500' },
  ];

  return (
    <ProtectedRoute>
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-4 sm:py-6 lg:py-8">

          {/* Header with tabs */}
          <div className="dashboard-header mb-6">
            <div className="flex flex-col sm:flex-row sm:items-center sm:justify-between gap-4">
              <div>
                <h1 className="text-2xl sm:text-3xl font-bold text-primary-900 dark:text-sand-50 mb-1">
                  Welcome back, {user?.first_name}! <Home className="inline-block h-6 w-6 ml-1" />
                </h1>
                <p className="text-primary-600 dark:text-sand-300">
                  Here&apos;s how your properties are performing
                </p>
              </div>
              <div className="flex bg-primary-100 dark:bg-primary-800 rounded-lg p-1 self-start sm:self-auto">
                <button
                  onClick={() => setActiveTab('overview')}
                  className={`px-4 py-2 rounded-md text-sm font-medium transition-all ${
                    activeTab === 'overview'
                      ? 'bg-white dark:bg-primary-700 text-primary-900 dark:text-sand-50 shadow-sm'
                      : 'text-primary-600 dark:text-sand-300 hover:text-primary-900 dark:hover:text-sand-50'
                  }`}
                >
                  Overview
                </button>
                <button
                  onClick={() => setActiveTab('analytics')}
                  className={`px-4 py-2 rounded-md text-sm font-medium transition-all flex items-center gap-1.5 ${
                    activeTab === 'analytics'
                      ? 'bg-white dark:bg-primary-700 text-primary-900 dark:text-sand-50 shadow-sm'
                      : 'text-primary-600 dark:text-sand-300 hover:text-primary-900 dark:hover:text-sand-50'
                  }`}
                >
                  <BarChart3 className="w-4 h-4" />
                  Analytics
                </button>
              </div>
            </div>
          </div>

          {activeTab === 'overview' ? (
            <>
              {/* Verification banner */}
              {!user?.is_verified && (
                <div className="mb-6">
                  <VerificationStatus />
                </div>
              )}

              {/* Pending actions alert */}
              {pendingData && pendingData.total_pending > 0 && (
                <div className="bg-yellow-50 dark:bg-yellow-900/20 border border-yellow-200 dark:border-yellow-800 rounded-xl p-4 mb-6">
                  <div className="flex items-start gap-3">
                    <AlertCircle className="w-5 h-5 text-yellow-600 dark:text-yellow-400 flex-shrink-0 mt-0.5" />
                    <div className="flex-1">
                      <h3 className="font-semibold text-yellow-900 dark:text-yellow-100 text-sm">
                        {pendingData.total_pending} pending action{pendingData.total_pending > 1 ? 's' : ''}
                      </h3>
                      <div className="flex flex-wrap gap-3 mt-1.5 text-sm text-yellow-800 dark:text-yellow-200">
                        {pendingData.pending_bookings > 0 && (
                          <Link href="/host/bookings?status=pending" className="hover:underline">
                            {pendingData.pending_bookings} booking request{pendingData.pending_bookings > 1 ? 's' : ''}
                          </Link>
                        )}
                        {pendingData.pending_properties > 0 && (
                          <span>{pendingData.pending_properties} pending approval</span>
                        )}
                        {pendingData.unread_messages > 0 && (
                          <Link href="/messages" className="hover:underline">
                            {pendingData.unread_messages} unread message{pendingData.unread_messages > 1 ? 's' : ''}
                          </Link>
                        )}
                        {pendingData.needs_completion > 0 && (
                          <span>{pendingData.needs_completion} need completion</span>
                        )}
                      </div>
                    </div>
                  </div>
                </div>
              )}

              {/* Stats */}
              <div className="grid grid-cols-2 lg:grid-cols-4 gap-3 sm:gap-4 mb-6 sm:mb-8">
                {loadingAnalytics ? (
                  Array.from({ length: 4 }).map((_, i) => (
                    <div key={i} className="stats-card animate-pulse">
                      <div className="h-16 bg-primary-200 dark:bg-primary-700 rounded" />
                    </div>
                  ))
                ) : (
                  stats.map((stat, i) => (
                    <Link key={i} href={stat.link} className="stats-card group relative">
                      <div className="flex items-center justify-between">
                        <div>
                          <p className="text-xs sm:text-sm text-primary-500 dark:text-sand-400">{stat.title}</p>
                          <p className="text-xl sm:text-2xl lg:text-3xl font-bold text-primary-900 dark:text-sand-50 mt-1">
                            {stat.value}
                          </p>
                        </div>
                        <div className={`${stat.bgColor} ${stat.color} p-2.5 sm:p-3 rounded-xl group-hover:scale-110 transition-transform`}>
                          <stat.icon className="w-5 h-5 sm:w-6 sm:h-6" />
                        </div>
                      </div>
                      {'badge' in stat && stat.badge && (
                        <span className="absolute top-2 right-2 w-2.5 h-2.5 bg-yellow-500 rounded-full animate-pulse" />
                      )}
                    </Link>
                  ))
                )}
              </div>

              {/* Main: Properties + Check-ins | Quick actions */}
              <div className="grid grid-cols-1 lg:grid-cols-3 gap-6 mb-6 sm:mb-8">

                {/* Left: Properties + Check-ins */}
                <div className="lg:col-span-2 space-y-6">

                  {/* Your Properties */}
                  <div className="card-gradient p-5 sm:p-6">
                    <div className="flex items-center justify-between mb-5">
                      <h2 className="text-lg sm:text-xl font-bold text-primary-900 dark:text-sand-50">
                        Your Properties
                      </h2>
                      <Link href="/host/properties" className="text-secondary-600 dark:text-secondary-400 hover:underline text-sm font-medium flex items-center gap-1">
                        View all <ArrowRight className="w-3.5 h-3.5" />
                      </Link>
                    </div>

                    {loadingProperties ? (
                      <div className="space-y-3">
                        {[1, 2, 3].map((i) => (
                          <div key={i} className="animate-pulse h-20 bg-primary-200 dark:bg-primary-700 rounded-xl" />
                        ))}
                      </div>
                    ) : propertiesData?.results && propertiesData.results.filter((p: any) => p?.id).length > 0 ? (
                      <div className="space-y-3">
                        {propertiesData.results
                          .filter((p: any) => p?.id)
                          .slice(0, 4)
                          .map((property: any) => {
                            const img = property.images?.[0]?.image || property.images?.[0]?.image_url || property.main_image_url;
                            return (
                              <Link
                                key={property.id}
                                href={`/property/${property.id}`}
                                className="flex gap-4 p-3 rounded-xl border border-primary-200 dark:border-primary-700 hover:border-secondary-500 dark:hover:border-secondary-600 transition group"
                              >
                                <div className="w-16 h-16 sm:w-20 sm:h-16 rounded-lg overflow-hidden flex-shrink-0 bg-primary-200 dark:bg-primary-700">
                                  {img ? (
                                    <img src={img} alt="" className="w-full h-full object-cover" />
                                  ) : (
                                    <div className="w-full h-full flex items-center justify-center">
                                      <Building className="w-5 h-5 text-primary-400 dark:text-primary-600" />
                                    </div>
                                  )}
                                </div>
                                <div className="flex-1 min-w-0">
                                  <h3 className="font-semibold text-sm text-primary-900 dark:text-sand-50 truncate group-hover:text-secondary-700 dark:group-hover:text-secondary-400 transition">
                                    {property.title}
                                  </h3>
                                  <p className="text-xs text-primary-500 dark:text-sand-400 flex items-center gap-1 mt-0.5">
                                    <MapPin className="w-3 h-3" />
                                    {property.city}, {property.country}
                                  </p>
                                  <div className="flex items-center gap-3 mt-1.5 text-xs">
                                    <span className="font-semibold text-secondary-700 dark:text-secondary-400">
                                      ${property.price_per_night}/night
                                    </span>
                                    <span className={`px-1.5 py-0.5 rounded-full text-[10px] font-semibold ${
                                      property.status === 'active'
                                        ? 'bg-green-100 dark:bg-green-900/30 text-green-700 dark:text-green-300'
                                        : property.status === 'pending_approval'
                                        ? 'bg-yellow-100 dark:bg-yellow-900/30 text-yellow-700 dark:text-yellow-300'
                                        : 'bg-primary-100 dark:bg-primary-900/30 text-primary-600 dark:text-primary-400'
                                    }`}>
                                      {property.status?.replace('_', ' ')}
                                    </span>
                                  </div>
                                </div>
                              </Link>
                            );
                          })}
                      </div>
                    ) : (
                      <div className="text-center py-10">
                        <div className="w-16 h-16 mx-auto rounded-full bg-primary-100 dark:bg-primary-800 flex items-center justify-center mb-4">
                          <Building className="w-8 h-8 text-primary-400 dark:text-primary-600" />
                        </div>
                        <h3 className="font-semibold text-primary-900 dark:text-sand-50 mb-1">No properties yet</h3>
                        <p className="text-sm text-primary-500 dark:text-sand-400 mb-4">Start earning by listing your first property!</p>
                        <Link
                          href="/host/properties/new"
                          className="inline-flex items-center gap-2 bg-secondary-600 hover:bg-secondary-700 text-white font-semibold text-sm py-2.5 px-5 rounded-lg transition"
                        >
                          <Building className="w-4 h-4" /> Add property
                        </Link>
                      </div>
                    )}
                  </div>

                  {/* Upcoming Check-ins */}
                  {checkinsData && checkinsData.length > 0 && (
                    <div className="card-gradient p-5 sm:p-6">
                      <div className="flex items-center justify-between mb-4">
                        <h2 className="text-lg sm:text-xl font-bold text-primary-900 dark:text-sand-50">
                          Upcoming Check-ins
                        </h2>
                        <span className="text-xs font-medium px-2 py-1 rounded-full bg-blue-100 dark:bg-blue-900/30 text-blue-700 dark:text-blue-300">
                          Next 7 days
                        </span>
                      </div>
                      <div className="space-y-2.5">
                        {checkinsData.map((checkin: any) => (
                          <div
                            key={checkin.booking_id}
                            className="flex items-center gap-4 p-3 rounded-xl border border-primary-200 dark:border-primary-700"
                          >
                            <div className="w-10 h-10 rounded-full bg-blue-100 dark:bg-blue-900/30 flex items-center justify-center flex-shrink-0">
                              <Users className="w-5 h-5 text-blue-600 dark:text-blue-400" />
                            </div>
                            <div className="flex-1 min-w-0">
                              <p className="text-sm font-medium text-primary-900 dark:text-sand-50 truncate">
                                {checkin.guest_name}
                              </p>
                              <p className="text-xs text-primary-500 dark:text-sand-400 truncate">
                                {checkin.property_title}
                              </p>
                            </div>
                            <div className="text-right flex-shrink-0">
                              <p className="text-sm font-semibold text-primary-900 dark:text-sand-50">
                                {formatDate(checkin.check_in)}
                              </p>
                              <p className="text-xs text-primary-500 dark:text-sand-400">
                                → {formatDate(checkin.check_out)}
                              </p>
                            </div>
                          </div>
                        ))}
                      </div>
                    </div>
                  )}
                </div>

                {/* Right sidebar: Quick actions grouped */}
                <div className="space-y-6">
                  {/* Management */}
                  <div className="card-gradient p-5">
                    <h3 className="text-sm font-semibold text-primary-500 dark:text-sand-400 uppercase tracking-wider mb-3">
                      Management
                    </h3>
                    <div className="grid grid-cols-2 gap-2">
                      {managementLinks.map((item, i) => (
                        <Link
                          key={i}
                          href={item.link}
                          className="flex items-center gap-2.5 p-2.5 rounded-lg border border-primary-100 dark:border-primary-700/50 hover:border-secondary-500 dark:hover:border-secondary-600 transition group"
                        >
                          <div className={`${item.color} p-1.5 rounded-md text-white group-hover:scale-110 transition-transform`}>
                            <item.icon className="w-3.5 h-3.5" />
                          </div>
                          <span className="text-sm font-medium text-primary-800 dark:text-sand-100 truncate">{item.title}</span>
                        </Link>
                      ))}
                    </div>
                  </div>

                  {/* Finance */}
                  <div className="card-gradient p-5">
                    <h3 className="text-sm font-semibold text-primary-500 dark:text-sand-400 uppercase tracking-wider mb-3">
                      Finance & Analytics
                    </h3>
                    <div className="grid grid-cols-2 gap-2">
                      {financeLinks.map((item, i) => (
                        <Link
                          key={i}
                          href={item.link}
                          className="flex items-center gap-2.5 p-2.5 rounded-lg border border-primary-100 dark:border-primary-700/50 hover:border-secondary-500 dark:hover:border-secondary-600 transition group"
                        >
                          <div className={`${item.color} p-1.5 rounded-md text-white group-hover:scale-110 transition-transform`}>
                            <item.icon className="w-3.5 h-3.5" />
                          </div>
                          <span className="text-sm font-medium text-primary-800 dark:text-sand-100 truncate">{item.title}</span>
                        </Link>
                      ))}
                    </div>
                  </div>

                  {/* More */}
                  <div className="card-gradient p-5">
                    <h3 className="text-sm font-semibold text-primary-500 dark:text-sand-400 uppercase tracking-wider mb-3">
                      More
                    </h3>
                    <div className="grid grid-cols-2 gap-2">
                      {moreLinks.map((item, i) => (
                        <Link
                          key={i}
                          href={item.link}
                          className="flex items-center gap-2.5 p-2.5 rounded-lg border border-primary-100 dark:border-primary-700/50 hover:border-secondary-500 dark:hover:border-secondary-600 transition group"
                        >
                          <div className={`${item.color} p-1.5 rounded-md text-white group-hover:scale-110 transition-transform`}>
                            <item.icon className="w-3.5 h-3.5" />
                          </div>
                          <span className="text-sm font-medium text-primary-800 dark:text-sand-100 truncate">{item.title}</span>
                        </Link>
                      ))}
                    </div>
                  </div>
                </div>
              </div>

              {/* Performance table */}
              {performanceData && performanceData.length > 0 && (
                <div className="card-gradient p-5 sm:p-6">
                  <div className="flex items-center justify-between mb-4">
                    <h2 className="text-lg sm:text-xl font-bold text-primary-900 dark:text-sand-50">
                      Property Performance
                    </h2>
                    <Link href="/host/earnings" className="text-secondary-600 dark:text-secondary-400 hover:underline text-sm font-medium flex items-center gap-1">
                      Full report <ArrowRight className="w-3.5 h-3.5" />
                    </Link>
                  </div>
                  <div className="overflow-x-auto -mx-5 sm:mx-0">
                    <table className="w-full min-w-[500px]">
                      <thead>
                        <tr className="border-b border-primary-200 dark:border-primary-700">
                          <th className="text-left py-2.5 px-3 text-xs font-semibold uppercase tracking-wider text-primary-500 dark:text-sand-400">Property</th>
                          <th className="text-center py-2.5 px-2 text-xs font-semibold uppercase tracking-wider text-primary-500 dark:text-sand-400">Bookings</th>
                          <th className="text-center py-2.5 px-2 text-xs font-semibold uppercase tracking-wider text-primary-500 dark:text-sand-400">Earnings</th>
                          <th className="text-center py-2.5 px-2 text-xs font-semibold uppercase tracking-wider text-primary-500 dark:text-sand-400">Rating</th>
                          <th className="text-center py-2.5 px-2 text-xs font-semibold uppercase tracking-wider text-primary-500 dark:text-sand-400">Status</th>
                        </tr>
                      </thead>
                      <tbody>
                        {performanceData.slice(0, 5).map((prop: any) => (
                          <tr key={prop.property_id} className="border-b border-primary-100 dark:border-primary-800 hover:bg-sand-50 dark:hover:bg-primary-800/50 transition">
                            <td className="py-3 px-3 text-sm text-primary-900 dark:text-sand-50 max-w-[180px] truncate font-medium">
                              {prop.property_title}
                            </td>
                            <td className="py-3 px-2 text-center text-sm text-primary-700 dark:text-sand-200">
                              {prop.total_bookings ?? 0}
                            </td>
                            <td className="py-3 px-2 text-center text-sm font-semibold text-green-700 dark:text-green-400">
                              ${(prop.total_earnings ?? 0).toFixed(2)}
                            </td>
                            <td className="py-3 px-2 text-center">
                              <span className="inline-flex items-center gap-1 text-sm text-primary-700 dark:text-sand-200">
                                <Star className="w-3.5 h-3.5 text-secondary-500 fill-secondary-500" />
                                {(prop.average_rating ?? 0).toFixed(1)}
                              </span>
                            </td>
                            <td className="py-3 px-2 text-center">
                              <span className={`px-2 py-0.5 text-xs font-semibold rounded-full ${
                                prop.status === 'active'
                                  ? 'bg-green-100 dark:bg-green-900/30 text-green-700 dark:text-green-300'
                                  : 'bg-primary-100 dark:bg-primary-900/30 text-primary-600 dark:text-primary-400'
                              }`}>
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
            </>
          ) : (
            <AnalyticsDashboard />
          )}
        </div>
      </div>
    </ProtectedRoute>
  );
}
