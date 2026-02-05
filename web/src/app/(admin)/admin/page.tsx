'use client';

import { useEffect, useState } from 'react';
import { adminApi } from '@/lib/admin-api';
import { AdminStats } from '@/types/admin-types';
import { 
  Users, 
  Home, 
  Calendar, 
  DollarSign,
  Activity,
} from 'lucide-react';
import { Bar, Line, Pie } from 'recharts';
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

export default function AdminDashboard() {
  const [stats, setStats] = useState<AdminStats | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    loadStats();
  }, []);

  const loadStats = async () => {
    try {
      setLoading(true);
      const data = await adminApi.getDashboardStats();
      setStats(data);
      setError(null);
    } catch (err) {
      setError('Failed to load dashboard stats');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center h-full">
        <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-orange-600"></div>
      </div>
    );
  }

  if (error || !stats) {
    return (
      <div className="p-8">
        <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded">
          {error || 'Failed to load dashboard'}
        </div>
      </div>
    );
  }

  const statCards = [
    {
      label: 'Total Revenue',
      value: `$${parseFloat(stats.total_revenue.toString()).toLocaleString()}`,
      icon: DollarSign,
      color: 'bg-green-500',
      change: '+12.5%',
    },
    {
      label: 'Total Users',
      value: stats.total_users.toLocaleString(),
      icon: Users,
      color: 'bg-blue-500',
      change: '+8.2%',
    },
    {
      label: 'Active Hosts',
      value: stats.active_hosts.toLocaleString(),
      icon: Activity,
      color: 'bg-purple-500',
      change: '+5.3%',
    },
    {
      label: 'Total Properties',
      value: stats.total_properties.toLocaleString(),
      icon: Home,
      color: 'bg-orange-500',
      change: '+15.7%',
    },
    {
      label: 'Total Bookings',
      value: stats.total_bookings.toLocaleString(),
      icon: Calendar,
      color: 'bg-pink-500',
      change: '+10.1%',
    },
  ];

  // Mock data for charts (in real implementation, fetch from API)
  const monthlyBookings = [
    { month: 'Jan', bookings: 45, revenue: 12500 },
    { month: 'Feb', bookings: 52, revenue: 15200 },
    { month: 'Mar', bookings: 68, revenue: 18900 },
    { month: 'Apr', bookings: 71, revenue: 21300 },
    { month: 'May', bookings: 85, revenue: 24700 },
    { month: 'Jun', bookings: 92, revenue: 28100 },
  ];

  const propertyTypes = [
    { name: 'Apartment', value: 45 },
    { name: 'House', value: 30 },
    { name: 'Villa', value: 15 },
    { name: 'Cottage', value: 10 },
  ];

  return (
    <div className="p-8">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-gray-900">Admin Dashboard</h1>
        <p className="text-gray-600 mt-2">Overview of your platform's performance</p>
      </div>

      {/* Stats Cards */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
        {statCards.map((stat, index) => (
          <div key={index} className="bg-white rounded-lg shadow p-6">
            <div className="flex items-center justify-between mb-4">
              <div className={`${stat.color} p-3 rounded-lg`}>
                <stat.icon className="w-6 h-6 text-white" />
              </div>
              <span className="text-sm font-medium text-green-600">{stat.change}</span>
            </div>
            <h3 className="text-gray-600 text-sm font-medium">{stat.label}</h3>
            <p className="text-2xl font-bold text-gray-900 mt-1">{stat.value}</p>
          </div>
        ))}
      </div>

      {/* Charts Row */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-8">
        {/* Bookings Trend */}
        <div className="bg-white rounded-lg shadow p-6">
          <h3 className="text-lg font-semibold text-gray-900 mb-4">Bookings Trend</h3>
          <ResponsiveContainer width="100%" height={300}>
            <LineChart data={monthlyBookings}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis dataKey="month" />
              <YAxis />
              <Tooltip />
              <Legend />
              <Line type="monotone" dataKey="bookings" stroke="#f97316" strokeWidth={2} />
            </LineChart>
          </ResponsiveContainer>
        </div>

        {/* Revenue Trend */}
        <div className="bg-white rounded-lg shadow p-6">
          <h3 className="text-lg font-semibold text-gray-900 mb-4">Revenue Trend</h3>
          <ResponsiveContainer width="100%" height={300}>
            <BarChart data={monthlyBookings}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis dataKey="month" />
              <YAxis />
              <Tooltip />
              <Legend />
              <Bar dataKey="revenue" fill="#10b981" />
            </BarChart>
          </ResponsiveContainer>
        </div>
      </div>

      {/* Property Types and Recent Activity */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Property Distribution */}
        <div className="bg-white rounded-lg shadow p-6">
          <h3 className="text-lg font-semibold text-gray-900 mb-4">Property Types Distribution</h3>
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
              />
              <Tooltip />
            </PieChart>
          </ResponsiveContainer>
        </div>

        {/* Recent Activity */}
        <div className="bg-white rounded-lg shadow p-6">
          <h3 className="text-lg font-semibold text-gray-900 mb-4">Recent Activity</h3>
          <div className="space-y-4">
            <div className="flex items-start space-x-3 pb-3 border-b">
              <div className="w-2 h-2 rounded-full bg-green-500 mt-2"></div>
              <div>
                <p className="text-sm font-medium text-gray-900">New property listed</p>
                <p className="text-xs text-gray-600">Luxury Villa in Cape Town • 2 hours ago</p>
              </div>
            </div>
            <div className="flex items-start space-x-3 pb-3 border-b">
              <div className="w-2 h-2 rounded-full bg-blue-500 mt-2"></div>
              <div>
                <p className="text-sm font-medium text-gray-900">New booking confirmed</p>
                <p className="text-xs text-gray-600">Booking #12345 • 3 hours ago</p>
              </div>
            </div>
            <div className="flex items-start space-x-3 pb-3 border-b">
              <div className="w-2 h-2 rounded-full bg-purple-500 mt-2"></div>
              <div>
                <p className="text-sm font-medium text-gray-900">New user registered</p>
                <p className="text-xs text-gray-600">John Doe joined as host • 5 hours ago</p>
              </div>
            </div>
            <div className="flex items-start space-x-3 pb-3 border-b">
              <div className="w-2 h-2 rounded-full bg-orange-500 mt-2"></div>
              <div>
                <p className="text-sm font-medium text-gray-900">Payment processed</p>
                <p className="text-xs text-gray-600">$1,250 via Stripe • 6 hours ago</p>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
