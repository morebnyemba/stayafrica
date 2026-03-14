'use client';

import { useAuth } from '@/store/auth-store';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import Link from 'next/link';
import {
  Calendar,
  Heart,
  MapPin,
  User,
  CreditCard,
  Clock,
  CheckCircle,
  XCircle,
  Home,
  MessageSquare,
  Star,
  ShieldCheck,
  ArrowRight,
  Compass,
} from 'lucide-react';
import { VerificationStatus } from '@/components/verification/VerificationStatus';
import dynamic from 'next/dynamic';
const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });

export function DashboardContent() {
  const { user, isAuthenticated } = useAuth();

  const { data: upcomingBookings, isLoading: loadingBookings } = useQuery({
    queryKey: ['bookings', 'upcoming'],
    queryFn: async () => {
      const response = await apiClient.getBookings({ status: 'CONFIRMED' });
      return response.data?.results || [];
    },
    enabled: isAuthenticated,
  });

  const { data: totalBookingsData } = useQuery({
    queryKey: ['bookings', 'total'],
    queryFn: async () => {
      const response = await apiClient.getBookings({});
      return response.data || { results: [], count: 0 };
    },
    enabled: isAuthenticated,
  });

  const { data: recentBookings } = useQuery({
    queryKey: ['bookings', 'recent'],
    queryFn: async () => {
      const response = await apiClient.getBookings({});
      return response.data?.results?.map((b: any) => ({
        type: 'booking',
        id: b.id,
        status: b.status,
        property: b.property,
        created_at: b.created_at,
      })) || [];
    },
    enabled: isAuthenticated,
  });

  const { data: recentReviews } = useQuery({
    queryKey: ['reviews', 'recent'],
    queryFn: async () => {
      const response = await apiClient.getReviews({ written_by_user: true });
      return response.data?.results?.map((r: any) => ({
        type: 'review',
        id: r.id,
        rating: r.rating,
        text: r.text,
        created_at: r.created_at,
        property: r.property,
      })) || [];
    },
    enabled: isAuthenticated,
  });

  const { data: recentPayments } = useQuery({
    queryKey: ['payments', 'recent'],
    queryFn: async () => {
      const response = await apiClient.getPaymentHistory();
      return response.data?.results?.map((p: any) => ({
        type: 'payment',
        id: p.id,
        status: p.status,
        amount: p.amount,
        provider: p.provider,
        created_at: p.created_at,
        booking: p.booking,
      })) || [];
    },
    enabled: isAuthenticated,
  });

  const { data: recentWishlist } = useQuery({
    queryKey: ['wishlist', 'recent'],
    queryFn: async () => {
      const response = await apiClient.getSavedProperties();
      return response.data?.results?.map((w: any) => ({
        type: 'wishlist',
        id: w.id,
        property: w.property,
        created_at: w.created_at,
      })) || [];
    },
    enabled: isAuthenticated,
  });

  const allActivities = [
    ...(recentBookings || []),
    ...(recentReviews || []),
    ...(recentPayments || []),
    ...(recentWishlist || []),
  ].sort((a, b) => new Date(b.created_at).getTime() - new Date(a.created_at).getTime()).slice(0, 6);

  const { data: unreadMessages } = useQuery({
    queryKey: ['messages', 'unread'],
    queryFn: async () => {
      const response = await apiClient.getUnreadCount();
      return response.data?.unread_count || 0;
    },
    enabled: isAuthenticated,
    refetchInterval: 30000,
  });

  const { data: savedPropertiesData } = useQuery({
    queryKey: ['properties', 'saved'],
    queryFn: async () => {
      try {
        const response = await apiClient.getSavedProperties();
        return response.data?.results || [];
      } catch {
        return [];
      }
    },
    enabled: isAuthenticated,
  });

  const stats = [
    {
      title: 'Upcoming Trips',
      value: upcomingBookings?.length || 0,
      icon: Calendar,
      color: 'text-blue-600',
      bgColor: 'bg-blue-100',
      link: '/bookings',
    },
    {
      title: 'Saved Places',
      value: savedPropertiesData?.length || 0,
      icon: Heart,
      color: 'text-red-600',
      bgColor: 'bg-red-100',
      link: '/wishlist',
    },
    {
      title: 'Total Bookings',
      value: totalBookingsData?.count || totalBookingsData?.results?.length || 0,
      icon: CheckCircle,
      color: 'text-green-600',
      bgColor: 'bg-green-100',
      link: '/bookings',
    },
    {
      title: 'Messages',
      value: unreadMessages || 0,
      icon: MessageSquare,
      color: 'text-purple-600',
      bgColor: 'bg-purple-100',
      link: '/messages',
      badge: unreadMessages && unreadMessages > 0,
    },
  ];

  const formatDate = (d: string) =>
    new Date(d).toLocaleDateString('en-US', { month: 'short', day: 'numeric' });

  const getActivityIcon = (activity: any) => {
    if (activity.type === 'booking') {
      return activity.status === 'CONFIRMED' ? CheckCircle
        : activity.status === 'PENDING' ? Clock : XCircle;
    }
    if (activity.type === 'review') return Star;
    if (activity.type === 'payment') return CreditCard;
    return Heart;
  };

  const getActivityColor = (activity: any) => {
    if (activity.type === 'booking') {
      return activity.status === 'CONFIRMED'
        ? 'bg-green-100 text-green-600'
        : activity.status === 'PENDING'
        ? 'bg-yellow-100 text-yellow-600'
        : 'bg-red-100 text-red-600';
    }
    if (activity.type === 'review') return 'bg-pink-100 text-pink-600';
    if (activity.type === 'payment') return 'bg-blue-100 text-blue-600';
    return 'bg-purple-100 text-purple-600';
  };

  const getActivityLabel = (activity: any) => {
    if (activity.type === 'booking') return `Booking ${activity.status?.toLowerCase()} · ${activity.property?.title || 'Property'}`;
    if (activity.type === 'review') return `Reviewed ${activity.property?.title || 'Property'} · ${activity.rating}★`;
    if (activity.type === 'payment') return `Payment ${activity.status} · $${activity.amount}`;
    return `Saved · ${activity.property?.title || 'Property'}`;
  };

  const getActivityLink = (activity: any) => {
    if (activity.type === 'booking') return '/bookings';
    if (activity.type === 'review') return '/reviews';
    if (activity.type === 'payment') return '/payments';
    return '/wishlist';
  };

  return (
    <ProtectedRoute>
      <div className="min-h-screen bg-sand-100">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-4 sm:py-6 lg:py-8">

          {/* Welcome */}
          <div className="dashboard-header">
            <h1 className="text-2xl sm:text-3xl font-bold text-primary-900 mb-1">
              Welcome back, {user?.first_name || 'Traveler'}! 👋
            </h1>
            <p className="text-primary-600">
              Here&apos;s what&apos;s happening with your travels
            </p>
          </div>

          {/* Verification */}
          <div className="mb-6">
            <VerificationStatus />
          </div>

          {/* Stats */}
          <div className="grid grid-cols-2 lg:grid-cols-4 gap-3 sm:gap-4 mb-6 sm:mb-8">
            {stats.map((stat, i) => (
              <Link key={i} href={stat.link} className="stats-card group relative">
                <div className="flex items-center justify-between">
                  <div>
                    <p className="text-xs sm:text-sm text-primary-500">{stat.title}</p>
                    <p className="text-2xl sm:text-3xl font-bold text-primary-900 mt-1">{stat.value}</p>
                  </div>
                  <div className={`${stat.bgColor} ${stat.color} p-2.5 sm:p-3 rounded-xl group-hover:scale-110 transition-transform`}>
                    <stat.icon className="w-5 h-5 sm:w-6 sm:h-6" />
                  </div>
                </div>
                {'badge' in stat && stat.badge && (
                  <span className="absolute top-2 right-2 w-2.5 h-2.5 bg-red-500 rounded-full animate-pulse" />
                )}
              </Link>
            ))}
          </div>

          {/* Main content: Trips + Sidebar */}
          <div className="grid grid-cols-1 lg:grid-cols-3 gap-6 mb-6 sm:mb-8">

            {/* Upcoming trips — wider column */}
            <div className="lg:col-span-2 card-gradient p-5 sm:p-6">
              <div className="flex items-center justify-between mb-5">
                <h2 className="text-lg sm:text-xl font-bold text-primary-900">
                  Upcoming Trips
                </h2>
                <Link href="/bookings" className="text-secondary-600 hover:underline text-sm font-medium flex items-center gap-1">
                  View all <ArrowRight className="w-3.5 h-3.5" />
                </Link>
              </div>

              {loadingBookings ? (
                <div className="space-y-3">
                  {[1, 2].map((i) => (
                    <div key={i} className="animate-pulse h-24 bg-primary-200 rounded-xl" />
                  ))}
                </div>
              ) : upcomingBookings && upcomingBookings.length > 0 ? (
                <div className="space-y-3">
                  {upcomingBookings.slice(0, 3).map((booking: any) => {
                    const img = booking.property?.images?.[0]?.image || booking.property?.images?.[0]?.image_url || booking.property?.main_image_url;
                    return (
                      <Link
                        key={booking.id}
                        href="/bookings"
                        className="flex gap-4 p-3 rounded-xl border border-primary-200 hover:border-secondary-500 transition group"
                      >
                        {/* Property thumbnail */}
                        <div className="w-20 h-20 sm:w-24 sm:h-20 rounded-lg overflow-hidden flex-shrink-0 bg-primary-200">
                          {img ? (
                            <img src={img} alt="" className="w-full h-full object-cover" />
                          ) : (
                            <div className="w-full h-full flex items-center justify-center">
                              <Home className="w-6 h-6 text-primary-400" />
                            </div>
                          )}
                        </div>
                        <div className="flex-1 min-w-0">
                          <h3 className="font-semibold text-sm sm:text-base text-primary-900 truncate group-hover:text-secondary-700 transition">
                            {booking.property?.title || booking.property_title || 'Property'}
                          </h3>
                          <p className="text-xs sm:text-sm text-primary-500 flex items-center gap-1 mt-0.5">
                            <MapPin className="w-3 h-3 flex-shrink-0" />
                            <span className="truncate">{booking.property?.city || 'Unknown'}, {booking.property?.country || ''}</span>
                          </p>
                          <div className="flex items-center gap-3 mt-2 text-xs text-primary-600">
                            <span className="flex items-center gap-1">
                              <Calendar className="w-3 h-3" />
                              {formatDate(booking.check_in)}
                            </span>
                            <span className="text-primary-400">→</span>
                            <span>{formatDate(booking.check_out)}</span>
                            <span className="ml-auto px-2 py-0.5 bg-green-100 text-green-700 text-xs font-medium rounded-full">
                              Confirmed
                            </span>
                          </div>
                        </div>
                      </Link>
                    );
                  })}
                </div>
              ) : (
                <div className="text-center py-10">
                  <div className="w-16 h-16 mx-auto rounded-full bg-primary-100 flex items-center justify-center mb-4">
                    <Compass className="w-8 h-8 text-primary-400" />
                  </div>
                  <h3 className="font-semibold text-primary-900 mb-1">No upcoming trips</h3>
                  <p className="text-sm text-primary-500 mb-4">Time to plan your next adventure!</p>
                  <Link
                    href="/explore"
                    className="inline-flex items-center gap-2 bg-secondary-600 hover:bg-secondary-700 text-white font-semibold text-sm py-2.5 px-5 rounded-lg transition"
                  >
                    <Compass className="w-4 h-4" /> Explore properties
                  </Link>
                </div>
              )}
            </div>

            {/* Sidebar: Quick links + activity */}
            <div className="space-y-6">
              {/* Quick links — compact */}
              <div className="card-gradient p-5">
                <h2 className="text-lg font-bold text-primary-900 mb-4">Quick Links</h2>
                <div className="grid grid-cols-2 gap-2">
                  {[
                    { title: 'Explore', icon: Compass, link: '/explore', color: 'bg-secondary-500' },
                    { title: 'Bookings', icon: Calendar, link: '/bookings', color: 'bg-blue-500' },
                    { title: 'Wishlist', icon: Heart, link: '/wishlist', color: 'bg-red-500' },
                    { title: 'My Reviews', icon: Star, link: '/reviews', color: 'bg-pink-500' },
                    { title: 'Profile', icon: User, link: '/profile', color: 'bg-green-500' },
                    { title: 'Payments', icon: CreditCard, link: '/profile?tab=payments', color: 'bg-purple-500' },
                    { title: 'Verify ID', icon: ShieldCheck, link: '/profile/verification', color: 'bg-indigo-500' },
                  ].map((item, i) => (
                    <Link
                      key={i}
                      href={item.link}
                      className="flex items-center gap-2.5 p-2.5 rounded-lg border border-primary-100 hover:border-secondary-500 transition group"
                    >
                      <div className={`${item.color} p-1.5 rounded-md text-white group-hover:scale-110 transition-transform`}>
                        <item.icon className="w-3.5 h-3.5" />
                      </div>
                      <span className="text-sm font-medium text-primary-800 truncate">{item.title}</span>
                    </Link>
                  ))}
                </div>
              </div>

              {/* Recent activity */}
              <div className="card-gradient p-5">
                <h2 className="text-lg font-bold text-primary-900 mb-4">Recent Activity</h2>
                {allActivities.length > 0 ? (
                  <div className="space-y-2.5">
                    {allActivities.map((activity: any) => {
                      const Icon = getActivityIcon(activity);
                      return (
                        <Link
                          key={activity.type + '-' + activity.id}
                          href={getActivityLink(activity)}
                          className="flex items-center gap-3 p-2 rounded-lg hover:bg-sand-50 transition -mx-2"
                        >
                          <div className={`p-1.5 rounded-full flex-shrink-0 ${getActivityColor(activity)}`}>
                            <Icon className="w-3.5 h-3.5" />
                          </div>
                          <div className="flex-1 min-w-0">
                            <p className="text-sm text-primary-800 truncate">
                              {getActivityLabel(activity)}
                            </p>
                            <p className="text-xs text-primary-500">
                              {formatDate(activity.created_at)}
                            </p>
                          </div>
                        </Link>
                      );
                    })}
                  </div>
                ) : (
                  <p className="text-sm text-primary-500 text-center py-4">
                    No recent activity
                  </p>
                )}
              </div>
            </div>
          </div>
        </div>
      </div>
    </ProtectedRoute>
  );
}
