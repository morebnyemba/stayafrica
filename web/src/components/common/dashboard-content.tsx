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
} from 'lucide-react';
import dynamic from 'next/dynamic';
const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });

export function DashboardContent() {
  const { user, isAuthenticated } = useAuth();

  // Fetch upcoming bookings
  const { data: upcomingBookings, isLoading: loadingBookings } = useQuery({
    queryKey: ['bookings', 'upcoming'],
    queryFn: async () => {
      const response = await apiClient.getBookings({ status: 'CONFIRMED' });
      return response.data?.results || [];
    },
    enabled: isAuthenticated,
  });

  // Fetch total bookings count
  const { data: totalBookingsData } = useQuery({
    queryKey: ['bookings', 'total'],
    queryFn: async () => {
      const response = await apiClient.getBookings({});
      return response.data || { results: [], count: 0 };
    },
    enabled: isAuthenticated,
  });


  // Fetch recent bookings
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

  // Fetch recent reviews written
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

  // Fetch recent payments
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

  // Fetch recent wishlist changes (saved/unsaved)
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

  // Merge and sort all activities by date
  const allActivities = [
    ...(recentBookings || []),
    ...(recentReviews || []),
    ...(recentPayments || []),
    ...(recentWishlist || []),
  ].sort((a, b) => new Date(b.created_at).getTime() - new Date(a.created_at).getTime()).slice(0, 8);

  // Fetch unread messages count
  const { data: unreadMessages } = useQuery({
    queryKey: ['messages', 'unread'],
    queryFn: async () => {
      const response = await apiClient.getUnreadCount();
      return response.data?.unread_count || 0;
    },
    enabled: isAuthenticated,
    refetchInterval: 30000, // Refetch every 30 seconds
  });

  // Fetch saved properties count
  const { data: savedPropertiesData } = useQuery({
    queryKey: ['properties', 'saved'],
    queryFn: async () => {
      try {
        const response = await apiClient.getSavedProperties();
        return response.data?.results || [];
      } catch (error) {
        // If endpoint not implemented yet, return empty array
        console.warn('Saved properties endpoint not available yet');
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
      color: 'text-blue-600 dark:text-blue-400',
      bgColor: 'bg-blue-100 dark:bg-blue-900/30',
      link: '/bookings',
    },
    {
      title: 'Saved Properties',
      value: savedPropertiesData?.length || 0,
      icon: Heart,
      color: 'text-red-600 dark:text-red-400',
      bgColor: 'bg-red-100 dark:bg-red-900/30',
      link: '/wishlist',
    },
    {
      title: 'Total Bookings',
      value: totalBookingsData?.count || totalBookingsData?.results?.length || 0,
      icon: CheckCircle,
      color: 'text-green-600 dark:text-green-400',
      bgColor: 'bg-green-100 dark:bg-green-900/30',
      link: '/bookings',
    },
    {
      title: 'Messages',
      value: unreadMessages || 0,
      icon: MessageSquare,
      color: 'text-purple-600 dark:text-purple-400',
      bgColor: 'bg-purple-100 dark:bg-purple-900/30',
      link: '/messages',
    },
  ];

  const quickActions = [
    {
      title: 'Explore Properties',
      description: 'Discover new places to stay',
      icon: Home,
      link: '/explore',
      color: 'bg-secondary-500',
    },
    {
      title: 'View Bookings',
      description: 'Manage your reservations',
      icon: Calendar,
      link: '/bookings',
      color: 'bg-blue-500',
    },
    {
      title: 'My Profile',
      description: 'Update your information',
      icon: User,
      link: '/profile',
      color: 'bg-green-500',
    },
    {
      title: 'Payment Methods',
      description: 'Manage payment options',
      icon: CreditCard,
      link: '/profile?tab=payments',
      color: 'bg-purple-500',
    },
    {
      title: 'Payment History',
      description: 'View all your payments',
      icon: CreditCard,
      link: '/payments',
      color: 'bg-yellow-500',
    },
    {
      title: 'My Reviews',
      description: 'See reviews you wrote and received',
      icon: Star,
      link: '/reviews',
      color: 'bg-pink-500',
    },
  ];

  return (
    <ProtectedRoute>
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          {/* Welcome Section */}
          <div className="mb-8">
            <h1 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-2">
              Welcome back, {user?.first_name}! 👋
            </h1>
            <p className="text-lg text-primary-600 dark:text-sand-300">
              Here&apos;s what&apos;s happening with your travels
            </p>
          </div>

          {/* Stats Grid */}
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
            {stats.map((stat, index) => (
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
            ))}
          </div>

          <div className="grid grid-cols-1 lg:grid-cols-3 gap-8 mb-8">
            {/* Upcoming Trips */}
            <div className="lg:col-span-2">
              <div className="card p-6">
                <div className="flex items-center justify-between mb-6">
                  <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50">
                    Upcoming Trips
                  </h2>
                  <Link
                    href="/bookings"
                    className="text-secondary-600 dark:text-secondary-400 hover:underline text-sm font-medium"
                  >
                    View All
                  </Link>
                </div>

                {loadingBookings ? (
                  <div className="space-y-4">
                    {[1, 2].map((i) => (
                      <div key={i} className="animate-pulse">
                        <div className="h-24 bg-primary-200 dark:bg-primary-700 rounded-lg"></div>
                      </div>
                    ))}
                  </div>
                ) : upcomingBookings && upcomingBookings.length > 0 ? (
                  <div className="space-y-4">
                    {upcomingBookings.slice(0, 3).map((booking: any) => (
                      <div
                        key={booking.id}
                        className="p-4 border border-primary-200 dark:border-primary-700 rounded-lg hover:border-secondary-500 transition"
                      >
                        <div className="flex items-start justify-between">
                          <div className="flex-1">
                            <h3 className="font-semibold text-primary-900 dark:text-sand-50 mb-1">
                              {booking.property?.title || booking.property_title || 'Property'}
                            </h3>
                            <div className="flex items-center gap-2 text-sm text-primary-600 dark:text-sand-300 mb-2">
                              <MapPin className="w-4 h-4" />
                              <span>
                                {booking.property?.city || 'Unknown'}, {booking.property?.country || 'Unknown'}
                              </span>
                            </div>
                            <div className="flex items-center gap-4 text-sm">
                              <div className="flex items-center gap-1">
                                <Calendar className="w-4 h-4 text-primary-400" />
                                <span className="text-primary-700 dark:text-sand-200">
                                  {new Date(booking.check_in).toLocaleDateString()}
                                </span>
                              </div>
                              <span className="text-primary-400">→</span>
                              <div className="flex items-center gap-1">
                                <Calendar className="w-4 h-4 text-primary-400" />
                                <span className="text-primary-700 dark:text-sand-200">
                                  {new Date(booking.check_out).toLocaleDateString()}
                                </span>
                              </div>
                            </div>
                          </div>
                          <div className="text-right">
                            <span className="px-3 py-1 bg-green-100 dark:bg-green-900/30 text-green-800 dark:text-green-300 text-xs font-semibold rounded-full">
                              Confirmed
                            </span>
                          </div>
                        </div>
                      </div>
                    ))}
                  </div>
                ) : (
                  <div className="text-center py-12">
                    <Calendar className="w-16 h-16 text-primary-300 dark:text-primary-700 mx-auto mb-4" />
                    <h3 className="text-lg font-semibold text-primary-900 dark:text-sand-50 mb-2">
                      No Upcoming Trips
                    </h3>
                    <p className="text-primary-600 dark:text-sand-300 mb-4">
                      Time to plan your next adventure!
                    </p>
                    <Link href="/explore" className="btn-primary px-6 py-2">
                      Explore Properties
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
                      <div className={`${action.color} p-2 rounded-lg text-white group-hover:scale-110 transition-transform`}>
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

          {/* Recent Activity */}
          <div className="card p-6">
            <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-6">
              Recent Activity
            </h2>
            {allActivities.length > 0 ? (
              <div className="space-y-4">
                {allActivities.map((activity: any) => (
                  <div
                    key={activity.type + '-' + activity.id}
                    className="flex items-center gap-4 p-4 border border-primary-200 dark:border-primary-700 rounded-lg"
                  >
                    <div className={`p-2 rounded-full ${
                      activity.type === 'booking' ?
                        activity.status === 'CONFIRMED'
                          ? 'bg-green-100 dark:bg-green-900/30 text-green-600 dark:text-green-400'
                          : activity.status === 'PENDING'
                          ? 'bg-yellow-100 dark:bg-yellow-900/30 text-yellow-600 dark:text-yellow-400'
                          : 'bg-red-100 dark:bg-red-900/30 text-red-600 dark:text-red-400'
                      : activity.type === 'review' ? 'bg-pink-100 dark:bg-pink-900/30 text-pink-600 dark:text-pink-400'
                      : activity.type === 'payment' ?
                        activity.status === 'success'
                          ? 'bg-blue-100 dark:bg-blue-900/30 text-blue-600 dark:text-blue-400'
                          : activity.status === 'pending'
                          ? 'bg-yellow-100 dark:bg-yellow-900/30 text-yellow-600 dark:text-yellow-400'
                          : 'bg-red-100 dark:bg-red-900/30 text-red-600 dark:text-red-400'
                      : 'bg-purple-100 dark:bg-purple-900/30 text-purple-600 dark:text-purple-400'
                    }`}>
                      {activity.type === 'booking' ? (
                        activity.status === 'CONFIRMED' ? <CheckCircle className="w-5 h-5" />
                        : activity.status === 'PENDING' ? <Clock className="w-5 h-5" />
                        : <XCircle className="w-5 h-5" />
                      ) : activity.type === 'review' ? (
                        <Star className="w-5 h-5" />
                      ) : activity.type === 'payment' ? (
                        activity.status === 'success' ? <CheckCircle className="w-5 h-5" />
                        : activity.status === 'pending' ? <Clock className="w-5 h-5" />
                        : <XCircle className="w-5 h-5" />
                      ) : (
                        <Heart className="w-5 h-5" />
                      )}
                    </div>
                    <div className="flex-1">
                      {activity.type === 'booking' ? (
                        <p className="font-semibold text-primary-900 dark:text-sand-50">
                          Booking {activity.status.toLowerCase()} - {activity.property?.title}
                        </p>
                      ) : activity.type === 'review' ? (
                        <p className="font-semibold text-primary-900 dark:text-sand-50">
                          Review {activity.rating}★ - {activity.property?.title}
                        </p>
                      ) : activity.type === 'payment' ? (
                        <p className="font-semibold text-primary-900 dark:text-sand-50">
                          Payment {activity.status} - {activity.provider} ${activity.amount}
                        </p>
                      ) : (
                        <p className="font-semibold text-primary-900 dark:text-sand-50">
                          Saved property - {activity.property?.title}
                        </p>
                      )}
                      <p className="text-sm text-primary-600 dark:text-sand-400">
                        {new Date(activity.created_at).toLocaleDateString()} at {new Date(activity.created_at).toLocaleTimeString()}
                      </p>
                    </div>
                    {activity.type === 'booking' ? (
                      <Link href={`/bookings`} className="text-secondary-600 dark:text-secondary-400 hover:underline text-sm font-medium">View Details</Link>
                    ) : activity.type === 'review' ? (
                      <Link href={`/reviews`} className="text-secondary-600 dark:text-secondary-400 hover:underline text-sm font-medium">View Reviews</Link>
                    ) : activity.type === 'payment' ? (
                      <Link href={`/payments`} className="text-secondary-600 dark:text-secondary-400 hover:underline text-sm font-medium">View Payments</Link>
                    ) : (
                      <Link href={`/wishlist`} className="text-secondary-600 dark:text-secondary-400 hover:underline text-sm font-medium">View Wishlist</Link>
                    )}
                  </div>
                ))}
              </div>
            ) : (
              <div className="text-center py-8 text-primary-600 dark:text-sand-400">
                No recent activity
              </div>
            )}
          </div>
        </div>
      </div>
    </ProtectedRoute>
  );
}
