'use client';

import { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { useAuth } from '@/store/auth-store';
import Link from 'next/link';
import Image from 'next/image';
import {
  Plus,
  Edit,
  MapPin,
  Clock,
  Users,
  DollarSign,
  Eye,
  EyeOff,
  Search,
  Filter,
  MoreVertical,
} from 'lucide-react';
import { Button } from '@/components/ui';
import { toast } from 'react-hot-toast';
import type { Experience, ExperienceStatus } from '@/types';

const statusColors: Record<ExperienceStatus, string> = {
  active: 'bg-green-100 text-green-800 dark:bg-green-900/30 dark:text-green-300',
  inactive: 'bg-gray-100 text-gray-800 dark:bg-gray-700 dark:text-gray-300',
  pending_approval: 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900/30 dark:text-yellow-300',
};

const statusLabels: Record<ExperienceStatus, string> = {
  active: 'Active',
  inactive: 'Inactive',
  pending_approval: 'Pending Approval',
};

const durationLabels: Record<string, string> = {
  half_day: 'Half Day',
  full_day: 'Full Day',
  multi_day: 'Multi-Day',
  hourly: 'Hourly',
};

export function HostExperiencesContent() {
  const { user, isAuthenticated } = useAuth();
  const queryClient = useQueryClient();
  const [searchTerm, setSearchTerm] = useState('');
  const [statusFilter, setStatusFilter] = useState<string>('');
  const [showFilters, setShowFilters] = useState(false);
  const [menuOpen, setMenuOpen] = useState<string | null>(null);

  const { data, isLoading, error } = useQuery({
    queryKey: ['host-experiences', searchTerm, statusFilter],
    queryFn: async () => {
      const params: Record<string, string> = {};
      if (searchTerm) params.search = searchTerm;
      if (statusFilter) params.status = statusFilter;
      const response = await apiClient.getHostExperiences(params);
      return response.data;
    },
    enabled: isAuthenticated && user?.role === 'host',
  });

  const deleteMutation = useMutation({
    mutationFn: (id: string) => apiClient.deleteExperience(id),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['host-experiences'] });
      toast.success('Experience deleted');
    },
    onError: () => toast.error('Failed to delete experience'),
  });

  const toggleStatusMutation = useMutation({
    mutationFn: ({ id, status }: { id: string; status: ExperienceStatus }) =>
      apiClient.updateExperience(id, { status }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['host-experiences'] });
      toast.success('Status updated');
    },
    onError: () => toast.error('Failed to update status'),
  });

  // Filter experiences owned by this host
  const allExperiences: Experience[] = data?.results || [];
  const experiences = allExperiences.filter((exp) => {
    if (statusFilter && exp.status !== statusFilter) return false;
    return true;
  });

  const stats = {
    total: allExperiences.length,
    active: allExperiences.filter((e) => e.status === 'active').length,
    pending: allExperiences.filter((e) => e.status === 'pending_approval').length,
    inactive: allExperiences.filter((e) => e.status === 'inactive').length,
  };

  return (
    <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Header */}
        <div className="flex flex-col sm:flex-row sm:items-center sm:justify-between gap-4 mb-8">
          <div>
            <h1 className="text-2xl sm:text-3xl font-bold text-primary-900 dark:text-sand-50">
              My Experiences
            </h1>
            <p className="text-primary-600 dark:text-sand-300 mt-1">
              Manage your activities and adventures
            </p>
          </div>
          <Link href="/host/experiences/new">
            <Button size="lg">
              <Plus className="w-5 h-5 mr-2" />
              Create Experience
            </Button>
          </Link>
        </div>

        {/* Stats */}
        <div className="grid grid-cols-2 sm:grid-cols-4 gap-4 mb-8">
          {[
            { label: 'Total', value: stats.total, color: 'text-primary-900 dark:text-sand-50' },
            { label: 'Active', value: stats.active, color: 'text-green-600 dark:text-green-400' },
            { label: 'Pending', value: stats.pending, color: 'text-yellow-600 dark:text-yellow-400' },
            { label: 'Inactive', value: stats.inactive, color: 'text-gray-600 dark:text-gray-400' },
          ].map((stat) => (
            <div key={stat.label} className="card p-4 text-center">
              <p className={`text-2xl font-bold ${stat.color}`}>{stat.value}</p>
              <p className="text-sm text-primary-600 dark:text-sand-400">{stat.label}</p>
            </div>
          ))}
        </div>

        {/* Search & Filter */}
        <div className="card p-4 mb-6">
          <div className="flex flex-col sm:flex-row gap-3">
            <div className="flex-1 relative">
              <Search className="absolute left-3 top-1/2 -translate-y-1/2 text-primary-400 w-5 h-5" />
              <input
                type="text"
                placeholder="Search your experiences..."
                value={searchTerm}
                onChange={(e) => setSearchTerm(e.target.value)}
                className="w-full pl-10 pr-4 py-2 border border-primary-200 dark:border-primary-600 rounded-lg bg-sand-50 dark:bg-primary-700 text-primary-900 dark:text-sand-100 focus:ring-2 focus:ring-secondary-500 focus:border-transparent"
              />
            </div>
            <Button variant="outline" onClick={() => setShowFilters(!showFilters)}>
              <Filter className="w-4 h-4 mr-2" />
              Filters
            </Button>
          </div>

          {showFilters && (
            <div className="mt-4 pt-4 border-t border-primary-200 dark:border-primary-700">
              <div className="flex flex-wrap gap-2">
                {(['', 'active', 'pending_approval', 'inactive'] as const).map((s) => (
                  <button
                    key={s || 'all'}
                    onClick={() => setStatusFilter(s)}
                    className={`px-3 py-1.5 rounded-full text-sm font-medium transition ${
                      statusFilter === s
                        ? 'bg-secondary-500 text-neutral-900'
                        : 'bg-primary-100 dark:bg-primary-700 text-primary-700 dark:text-sand-200 hover:bg-primary-200 dark:hover:bg-primary-600'
                    }`}
                  >
                    {s ? statusLabels[s] : 'All'}
                  </button>
                ))}
              </div>
            </div>
          )}
        </div>

        {/* Experiences List */}
        {isLoading ? (
          <div className="space-y-4">
            {[1, 2, 3].map((i) => (
              <div key={i} className="card p-6 animate-pulse">
                <div className="flex gap-4">
                  <div className="w-32 h-24 bg-primary-200 dark:bg-primary-700 rounded-lg" />
                  <div className="flex-1 space-y-3">
                    <div className="h-5 bg-primary-200 dark:bg-primary-700 rounded w-1/3" />
                    <div className="h-4 bg-primary-200 dark:bg-primary-700 rounded w-1/2" />
                    <div className="h-4 bg-primary-200 dark:bg-primary-700 rounded w-1/4" />
                  </div>
                </div>
              </div>
            ))}
          </div>
        ) : error ? (
          <div className="card p-12 text-center">
            <p className="text-primary-600 dark:text-sand-300">
              Failed to load experiences. Please try again.
            </p>
          </div>
        ) : experiences.length === 0 ? (
          <div className="card p-12 text-center">
            <div className="flex justify-center mb-4">
              <MapPin className="w-16 h-16 text-primary-300 dark:text-primary-600" />
            </div>
            <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-2">
              {allExperiences.length === 0 ? 'No Experiences Yet' : 'No Matching Experiences'}
            </h3>
            <p className="text-primary-600 dark:text-sand-300 mb-6">
              {allExperiences.length === 0
                ? 'Create your first experience to offer guests unique activities and adventures.'
                : 'Try adjusting your search or filters.'}
            </p>
            {allExperiences.length === 0 && (
              <Link href="/host/experiences/new">
                <Button>
                  <Plus className="w-5 h-5 mr-2" />
                  Create Your First Experience
                </Button>
              </Link>
            )}
          </div>
        ) : (
          <div className="space-y-4">
            {experiences.map((exp) => (
              <div
                key={exp.id}
                className="card p-4 sm:p-6 hover:shadow-md transition-shadow"
              >
                <div className="flex flex-col sm:flex-row gap-4">
                  {/* Image */}
                  <div className="relative w-full sm:w-40 h-32 sm:h-28 rounded-lg overflow-hidden bg-primary-200 dark:bg-primary-700 flex-shrink-0">
                    {exp.main_image ? (
                      <Image
                        src={exp.main_image}
                        alt={exp.title}
                        fill
                        className="object-cover"
                      />
                    ) : (
                      <div className="w-full h-full flex items-center justify-center text-primary-400">
                        <MapPin className="w-10 h-10" />
                      </div>
                    )}
                  </div>

                  {/* Content */}
                  <div className="flex-1 min-w-0">
                    <div className="flex items-start justify-between gap-2">
                      <div>
                        <h3 className="text-lg font-semibold text-primary-900 dark:text-sand-50 truncate">
                          {exp.title}
                        </h3>
                        <div className="flex items-center gap-1 text-sm text-primary-600 dark:text-sand-300 mt-1">
                          <MapPin className="w-4 h-4" />
                          <span>{exp.city}, {exp.country}</span>
                        </div>
                      </div>
                      <span className={`px-2.5 py-1 rounded-full text-xs font-semibold whitespace-nowrap ${statusColors[exp.status]}`}>
                        {statusLabels[exp.status]}
                      </span>
                    </div>

                    <p className="text-sm text-primary-700 dark:text-sand-300 mt-2 line-clamp-1">
                      {exp.description}
                    </p>

                    <div className="flex flex-wrap items-center gap-4 mt-3 text-sm text-primary-600 dark:text-sand-400">
                      <span className="flex items-center gap-1">
                        <DollarSign className="w-4 h-4" />
                        {exp.currency} {exp.price_per_person}/person
                      </span>
                      <span className="flex items-center gap-1">
                        <Clock className="w-4 h-4" />
                        {durationLabels[exp.duration] || exp.duration}
                      </span>
                      <span className="flex items-center gap-1">
                        <Users className="w-4 h-4" />
                        {exp.min_participants}–{exp.max_participants} guests
                      </span>
                    </div>
                  </div>

                  {/* Actions */}
                  <div className="flex sm:flex-col items-center gap-2 flex-shrink-0 relative">
                    <Link href={`/host/experiences/${exp.id}/edit`}>
                      <Button variant="outline" size="sm">
                        <Edit className="w-4 h-4" />
                      </Button>
                    </Link>
                    <Button
                      variant="outline"
                      size="sm"
                      onClick={() =>
                        toggleStatusMutation.mutate({
                          id: exp.id,
                          status: exp.status === 'active' ? 'inactive' : 'active',
                        })
                      }
                      title={exp.status === 'active' ? 'Deactivate' : 'Activate'}
                    >
                      {exp.status === 'active' ? (
                        <EyeOff className="w-4 h-4" />
                      ) : (
                        <Eye className="w-4 h-4" />
                      )}
                    </Button>
                    <div className="relative">
                      <Button
                        variant="outline"
                        size="sm"
                        onClick={() => setMenuOpen(menuOpen === exp.id ? null : exp.id)}
                      >
                        <MoreVertical className="w-4 h-4" />
                      </Button>
                      {menuOpen === exp.id && (
                        <div className="absolute right-0 top-full mt-1 bg-white dark:bg-primary-800 rounded-lg shadow-lg border border-primary-200 dark:border-primary-700 z-10 min-w-[140px]">
                          <Link
                            href={`/experiences/${exp.id}`}
                            className="block px-4 py-2 text-sm text-primary-700 dark:text-sand-200 hover:bg-primary-50 dark:hover:bg-primary-700 rounded-t-lg"
                            onClick={() => setMenuOpen(null)}
                          >
                            View Public Page
                          </Link>
                          <button
                            onClick={() => {
                              if (confirm('Are you sure you want to delete this experience?')) {
                                deleteMutation.mutate(exp.id);
                              }
                              setMenuOpen(null);
                            }}
                            className="block w-full text-left px-4 py-2 text-sm text-red-600 dark:text-red-400 hover:bg-red-50 dark:hover:bg-red-900/20 rounded-b-lg"
                          >
                            Delete
                          </button>
                        </div>
                      )}
                    </div>
                  </div>
                </div>
              </div>
            ))}
          </div>
        )}
      </div>
    </div>
  );
}
