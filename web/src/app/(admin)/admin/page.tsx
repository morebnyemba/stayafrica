'use client';

import { useEffect, useState } from 'react';
import { adminApi } from '@/lib/admin-api';
import { AdminStats, BookingAnalytics, RevenueAnalytics, AuditLog } from '@/types/admin-types';
import { 
  Users, 
  Home, 
  Calendar, 
  DollarSign,
  Activity,
  RefreshCw,
} from 'lucide-react';
import { Bar, Line, Pie, Cell } from 'recharts';
import {
  BarChart,
  LineChart,
  PieChart,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  Legend,
  ResponsiveContainer,
} from 'recharts';

const PIE_COLORS = ['#D9B168', '#122F26', '#3A5C50', '#10b981', '#6366f1', '#f59e0b'];

function timeAgo(dateString: string): string {
  const now = new Date();
  const date = new Date(dateString);
  const diffMs = now.getTime() - date.getTime();
  const diffMins = Math.floor(diffMs / 60000);
  if (diffMins < 1) return 'just now';
  if (diffMins < 60) return `${diffMins}m ago`;
  const diffHours = Math.floor(diffMins / 60);
  if (diffHours < 24) return `${diffHours}h ago`;
  const diffDays = Math.floor(diffHours / 24);
  return `${diffDays}d ago`;
}

export default function AdminDashboard() {
  const [stats, setStats] = useState<AdminStats | null>(null);
  const [bookingAnalytics, setBookingAnalytics] = useState<BookingAnalytics | null>(null);
  const [revenueAnalytics, setRevenueAnalytics] = useState<RevenueAnalytics | null>(null);
  const [recentActivity, setRecentActivity] = useState<AuditLog[]>([]);
  const [propertyTypes, setPropertyTypes] = useState<{ name: string; value: number }[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    loadDashboard();
  }, []);

  const loadDashboard = async () => {
    try {
      setLoading(true);
      setError(null);

      const [statsData, bookingData, revenueData, activityData] = await Promise.allSettled([
        adminApi.getStats(),
        adminApi.getBookingAnalytics({ period: 'monthly' }),
        adminApi.getRevenueAnalytics({ period: 'monthly' }),
        adminApi.getAuditLogs({ per_page: 8 }),
      ]);

      if (statsData.status === 'fulfilled') setStats(statsData.value);
      else setError('Failed to load dashboard stats');

      if (bookingData.status === 'fulfilled') setBookingAnalytics(bookingData.value);
      if (revenueData.status === 'fulfilled') setRevenueAnalytics(revenueData.value);
      if (activityData.status === 'fulfilled') setRecentActivity(activityData.value.results || []);

      // Load property type distribution from properties endpoint
      try {
        const propsData = await adminApi.getProperties({ per_page: 100 });
        const typeCounts: Record<string, number> = {};
        (propsData.results || []).forEach((p: any) => {
          const pType = p.property_type || 'Other';
          typeCounts[pType] = (typeCounts[pType] || 0) + 1;
        });
        setPropertyTypes(Object.entries(typeCounts).map(([name, value]) => ({ name, value })));
      } catch {
        // Non-critical, leave empty
      }
    } catch (err) {
      setError('Failed to load dashboard');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  if (loading) {
    return (
      <div className="flex flex-col items-center justify-center h-full">
        <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-[#D9B168]"></div>
        <p className="mt-4 text-gray-500">Loading dashboard...</p>
      </div>
    );
  }

  if (error && !stats) {
    return (
      <div className="p-8">
        <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded">
          {error}
          <button onClick={loadDashboard} className="ml-4 underline">Retry</button>
        </div>
      </div>
    );
  }

  const statCards = stats ? [
    {
      label: 'Total Revenue',
      value: `$${parseFloat(stats.total_revenue?.toString() || '0').toLocaleString()}`,
      icon: DollarSign,
      color: 'bg-green-500',
    },
    {
      label: 'Total Users',
      value: (stats.total_users || 0).toLocaleString(),
      icon: Users,
      color: 'bg-blue-500',
    },
    {
      label: 'Active Hosts',
      value: (stats.active_hosts || 0).toLocaleString(),
      icon: Activity,
      color: 'bg-purple-500',
    },
    {
      label: 'Total Properties',
      value: (stats.total_properties || 0).toLocaleString(),
      icon: Home,
      color: 'bg-[#D9B168]',
    },
    {
      label: 'Total Bookings',
      value: (stats.total_bookings || 0).toLocaleString(),
      icon: Calendar,
      color: 'bg-pink-500',
    },
  ] : [];

  // Build chart data from real analytics
  const chartData = bookingAnalytics?.bookings_by_month?.map((item) => ({
    month: item.month,
    bookings: item.count,
    revenue: item.revenue,
  })) || [];

  const revenueChartData = revenueAnalytics?.revenue_by_month?.map((item) => ({
    month: item.month,
    revenue: item.revenue,
  })) || chartData;

  const activityColors: Record<string, string> = {
    register_user: 'bg-purple-500',
    create_booking: 'bg-blue-500',
    create_property: 'bg-green-500',
    create_payment: 'bg-[#D9B168]',
    approve_property: 'bg-emerald-500',
    cancel_booking: 'bg-red-500',
  };

  return (
    <div className="p-8 max-w-7xl mx-auto">
      <div className="mb-8 flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold text-[#122F26]">Admin Dashboard</h1>
          <p className="text-[#3A5C50] mt-2">Overview of your platform&apos;s performance</p>
        </div>
        <button
          onClick={loadDashboard}
          className="flex items-center px-4 py-2 bg-white border border-gray-300 rounded-lg text-sm font-medium text-gray-700 hover:bg-gray-50"
        >
          <RefreshCw className={`w-4 h-4 mr-2 ${loading ? 'animate-spin' : ''}`} />
          Refresh
        </button>
      </div>

      {/* Stats Cards */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-5 gap-6 mb-8">
        {statCards.map((stat, index) => (
          <div key={index} className="bg-white rounded-xl shadow-sm border border-gray-200 p-6">
            <div className="flex items-center justify-between mb-4">
              <div className={`${stat.color} p-3 rounded-lg`}>
                <stat.icon className="w-6 h-6 text-white" />
              </div>
            </div>
            <h3 className="text-gray-500 text-sm font-medium">{stat.label}</h3>
            <p className="text-2xl font-bold text-[#122F26] mt-1">{stat.value}</p>
          </div>
        ))}
      </div>

      {/* Charts Row */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-8">
        {/* Bookings Trend */}
        <div className="bg-white rounded-xl shadow-sm border border-gray-200 p-6">
          <h3 className="text-lg font-semibold text-[#122F26] mb-4">Bookings Trend</h3>
          {chartData.length > 0 ? (
            <ResponsiveContainer width="100%" height={300}>
              <LineChart data={chartData}>
                <CartesianGrid strokeDasharray="3 3" />
                <XAxis dataKey="month" />
                <YAxis />
                <Tooltip />
                <Legend />
                <Line type="monotone" dataKey="bookings" stroke="#D9B168" strokeWidth={2} />
              </LineChart>
            </ResponsiveContainer>
          ) : (
            <div className="flex items-center justify-center h-[300px] text-gray-400">
              No booking data available yet
            </div>
          )}
        </div>

        {/* Revenue Trend */}
        <div className="bg-white rounded-xl shadow-sm border border-gray-200 p-6">
          <h3 className="text-lg font-semibold text-[#122F26] mb-4">Revenue Trend</h3>
          {revenueChartData.length > 0 ? (
            <ResponsiveContainer width="100%" height={300}>
              <BarChart data={revenueChartData}>
                <CartesianGrid strokeDasharray="3 3" />
                <XAxis dataKey="month" />
                <YAxis />
                <Tooltip formatter={(value: number) => `$${value.toLocaleString()}`} />
                <Legend />
                <Bar dataKey="revenue" fill="#10b981" />
              </BarChart>
            </ResponsiveContainer>
          ) : (
            <div className="flex items-center justify-center h-[300px] text-gray-400">
              No revenue data available yet
            </div>
          )}
        </div>
      </div>

      {/* Property Types and Recent Activity */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Property Distribution */}
        <div className="bg-white rounded-xl shadow-sm border border-gray-200 p-6">
          <h3 className="text-lg font-semibold text-[#122F26] mb-4">Property Types Distribution</h3>
          {propertyTypes.length > 0 ? (
            <ResponsiveContainer width="100%" height={300}>
              <PieChart>
                <Pie
                  data={propertyTypes}
                  cx="50%"
                  cy="50%"
                  labelLine={false}
                  label={({ name, percent }) => `${name} ${(percent * 100).toFixed(0)}%`}
                  outerRadius={80}
                  fill="#8884d8"
                  dataKey="value"
                >
                  {propertyTypes.map((_, index) => (
                    <Cell key={`cell-${index}`} fill={PIE_COLORS[index % PIE_COLORS.length]} />
                  ))}
                </Pie>
                <Tooltip />
              </PieChart>
            </ResponsiveContainer>
          ) : (
            <div className="flex items-center justify-center h-[300px] text-gray-400">
              No property data available yet
            </div>
          )}
        </div>

        {/* Recent Activity - from real audit logs */}
        <div className="bg-white rounded-xl shadow-sm border border-gray-200 p-6">
          <h3 className="text-lg font-semibold text-[#122F26] mb-4">Recent Activity</h3>
          {recentActivity.length > 0 ? (
            <div className="space-y-4 max-h-[300px] overflow-y-auto">
              {recentActivity.map((log) => (
                <div key={log.id} className="flex items-start space-x-3 pb-3 border-b border-gray-100 last:border-0">
                  <div className={`w-2 h-2 rounded-full mt-2 ${activityColors[log.action] || 'bg-gray-400'}`}></div>
                  <div className="flex-1 min-w-0">
                    <p className="text-sm font-medium text-[#122F26] truncate">
                      {log.action?.replace(/_/g, ' ').replace(/\b\w/g, (c) => c.toUpperCase())}
                    </p>
                    <p className="text-xs text-[#3A5C50]">
                      {log.user_first_name || log.user_last_name
                        ? `${log.user_first_name} ${log.user_last_name}`.trim()
                        : log.user_email || 'System'} • {timeAgo(log.timestamp)}
                    </p>
                  </div>
                </div>
              ))}
            </div>
          ) : (
            <div className="flex items-center justify-center h-[300px] text-gray-400">
              No recent activity
            </div>
          )}
        </div>
      </div>
    </div>
  );
}
