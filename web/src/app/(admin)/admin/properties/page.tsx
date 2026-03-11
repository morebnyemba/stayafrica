'use client';

import { useEffect, useState } from 'react';
import { adminApi } from '@/lib/admin-api';
import { Search, CheckCircle, XCircle, Eye, ChevronDown, Ban, Power, Home, ExternalLink } from 'lucide-react';
import toast from 'react-hot-toast';
import ConfirmDialog from '@/components/admin/ConfirmDialog';

const STATUS_COLORS: Record<string, string> = {
  active: 'bg-green-100 text-green-800',
  pending_approval: 'bg-yellow-100 text-yellow-800',
  inactive: 'bg-red-100 text-red-800',
  rejected: 'bg-gray-100 text-gray-800',
};

const STATUS_LABELS: Record<string, string> = {
  active: 'Active',
  pending_approval: 'Pending',
  inactive: 'Inactive',
  rejected: 'Rejected',
};

export default function PropertiesManagement() {
  const [properties, setProperties] = useState<any[]>([]);
  const [loading, setLoading] = useState(true);
  const [search, setSearch] = useState('');
  const [statusFilter, setStatusFilter] = useState<string>('');
  const [page, setPage] = useState(1);
  const [totalCount, setTotalCount] = useState(0);
  const [selectedProperties, setSelectedProperties] = useState<string[]>([]);
  const [showBulkActions, setShowBulkActions] = useState(false);
  const [showConfirmDialog, setShowConfirmDialog] = useState(false);
  const [confirmAction, setConfirmAction] = useState<{ type: string; propertyId?: string } | null>(null);
  const [showRejectDialog, setShowRejectDialog] = useState(false);
  const [rejectPropertyId, setRejectPropertyId] = useState<string | null>(null);
  const [rejectReason, setRejectReason] = useState('');
  const ITEMS_PER_PAGE = 20;

  useEffect(() => {
    loadProperties();
  }, [page, statusFilter, search]);

  const loadProperties = async () => {
    try {
      setLoading(true);
      const data = await adminApi.getProperties({ 
        page, 
        status: statusFilter || undefined,
        search: search.trim() || undefined,
        per_page: ITEMS_PER_PAGE,
      });
      setProperties(data.results || []);
      setTotalCount(data.count || 0);
    } catch (err: any) {
      const errorMsg = err?.response?.data?.detail || 'Failed to load properties';
      toast.error(errorMsg);
      console.error('Properties load error:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleSearch = () => {
    setPage(1);
  };

  const handleApprove = (propertyId: string) => {
    setConfirmAction({ type: 'approve', propertyId });
    setShowConfirmDialog(true);
  };

  const handleReject = (propertyId: string) => {
    setRejectPropertyId(propertyId);
    setRejectReason('');
    setShowRejectDialog(true);
  };

  const submitReject = async () => {
    if (!rejectPropertyId) return;
    try {
      await adminApi.rejectProperty(rejectPropertyId, rejectReason || 'Rejected by admin');
      toast.success('Property rejected');
      setShowRejectDialog(false);
      setRejectPropertyId(null);
      setRejectReason('');
      loadProperties();
    } catch (err: any) {
      toast.error(err?.response?.data?.detail || 'Failed to reject property');
      console.error(err);
    }
  };

  const handleSuspend = (propertyId: string) => {
    setConfirmAction({ type: 'suspend', propertyId });
    setShowConfirmDialog(true);
  };

  const handleActivate = (propertyId: string) => {
    setConfirmAction({ type: 'activate', propertyId });
    setShowConfirmDialog(true);
  };

  const handleBulkAction = async (action: string) => {
    if (selectedProperties.length === 0) {
      toast.error('No properties selected');
      return;
    }

    try {
      switch (action) {
        case 'approve':
          await Promise.all(selectedProperties.map(id => adminApi.approveProperty(id)));
          toast.success(`${selectedProperties.length} properties approved`);
          break;
        case 'reject':
          await Promise.all(selectedProperties.map(id => adminApi.rejectProperty(id, 'Bulk rejected by admin')));
          toast.success(`${selectedProperties.length} properties rejected`);
          break;
        case 'activate':
          await Promise.all(selectedProperties.map(id => adminApi.activateProperty(id)));
          toast.success(`${selectedProperties.length} properties activated`);
          break;
        case 'deactivate':
          await Promise.all(selectedProperties.map(id => adminApi.deactivateProperty(id)));
          toast.success(`${selectedProperties.length} properties deactivated`);
          break;
        default:
          toast.error('Unknown action');
          return;
      }
      setSelectedProperties([]);
      setShowBulkActions(false);
      loadProperties();
    } catch (err) {
      toast.error('Failed to perform bulk action');
      console.error(err);
    }
  };

  const handleConfirm = async () => {
    if (!confirmAction || !confirmAction.propertyId) return;

    try {
      switch (confirmAction.type) {
        case 'approve':
          await adminApi.approveProperty(confirmAction.propertyId);
          toast.success('Property approved successfully');
          break;
        case 'suspend':
          await adminApi.deactivateProperty(confirmAction.propertyId);
          toast.success('Property suspended successfully');
          break;
        case 'activate':
          await adminApi.activateProperty(confirmAction.propertyId);
          toast.success('Property activated successfully');
          break;
      }
      loadProperties();
    } catch (err) {
      toast.error(`Failed to ${confirmAction.type} property`);
      console.error(err);
    }
  };

  const toggleSelectProperty = (propertyId: string) => {
    setSelectedProperties(prev => 
      prev.includes(propertyId) 
        ? prev.filter(id => id !== propertyId)
        : [...prev, propertyId]
    );
  };

  // Helper to safely get city/country from either flat or nested location
  const getCity = (p: any) => p.city || p.location?.city || '';
  const getCountry = (p: any) => p.country || p.location?.country || '';

  const totalPages = Math.ceil(totalCount / ITEMS_PER_PAGE);

  return (
    <div className="p-8">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-[#122F26]">Property Management</h1>
        <p className="text-[#3A5C50] mt-2">Review and manage property listings</p>
      </div>

      {/* Filters and Search */}
      <div className="bg-white rounded-lg shadow p-6 mb-6">
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
          <div className="md:col-span-2">
            <label className="block text-sm font-medium text-[#122F26] mb-2">
              Search Properties
            </label>
            <div className="flex space-x-2">
              <div className="flex-1 relative">
                <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-[#3A5C50] w-5 h-5" />
                <input
                  type="text"
                  value={search}
                  onChange={(e) => setSearch(e.target.value)}
                  onKeyPress={(e) => e.key === 'Enter' && handleSearch()}
                  placeholder="Search by title, location..."
                  className="w-full pl-10 pr-4 py-2 border border-[#3A5C50] rounded-lg focus:ring-2 focus:ring-[#D9B168] focus:border-transparent"
                />
              </div>
              <button
                onClick={handleSearch}
                className="px-6 py-2 bg-[#D9B168] text-[#122F26] rounded-lg hover:bg-[#c9a158] transition-colors"
              >
                Search
              </button>
            </div>
          </div>
          <div>
            <label className="block text-sm font-medium text-[#122F26] mb-2">
              Filter by Status
            </label>
            <select
              value={statusFilter}
              onChange={(e) => {
                setStatusFilter(e.target.value);
                setPage(1);
              }}
              className="w-full px-4 py-2 border border-[#3A5C50] rounded-lg focus:ring-2 focus:ring-[#D9B168] focus:border-transparent"
            >
              <option value="">All Status</option>
              <option value="active">Active</option>
              <option value="pending_approval">Pending Approval</option>
              <option value="inactive">Inactive</option>
              <option value="rejected">Rejected</option>
            </select>
          </div>
        </div>

        {selectedProperties.length > 0 && (
          <div className="mt-4 flex items-center justify-between bg-[#F4F1EA] px-4 py-3 rounded-lg">
            <p className="text-sm text-[#122F26]">
              {selectedProperties.length} properties selected
            </p>
            <div className="relative">
              <button
                onClick={() => setShowBulkActions(!showBulkActions)}
                className="flex items-center space-x-2 px-4 py-2 bg-[#D9B168] text-[#122F26] font-medium rounded-lg hover:bg-[#c9a158] transition-colors"
              >
                <span>Bulk Actions</span>
                <ChevronDown className="w-4 h-4" />
              </button>
              {showBulkActions && (
                <div className="absolute right-0 mt-2 w-48 bg-white rounded-lg shadow-lg border border-sand-200/50 z-10">
                  <button
                    onClick={() => handleBulkAction('approve')}
                    className="w-full text-left px-4 py-2 text-sm text-primary-700 hover:bg-primary-100 first:rounded-t-lg"
                  >
                    Approve Properties
                  </button>
                  <button
                    onClick={() => handleBulkAction('reject')}
                    className="w-full text-left px-4 py-2 text-sm text-primary-700 hover:bg-primary-100"
                  >
                    Reject Properties
                  </button>
                  <button
                    onClick={() => handleBulkAction('activate')}
                    className="w-full text-left px-4 py-2 text-sm text-primary-700 hover:bg-primary-100"
                  >
                    Activate Properties
                  </button>
                  <button
                    onClick={() => handleBulkAction('deactivate')}
                    className="w-full text-left px-4 py-2 text-sm text-primary-700 hover:bg-primary-100 last:rounded-b-lg"
                  >
                    Deactivate Properties
                  </button>
                </div>
              )}
            </div>
          </div>
        )}
      </div>

      {/* Stats Cards */}
      <div className="grid grid-cols-2 md:grid-cols-5 gap-4 mb-6">
        <div className="bg-white rounded-lg shadow p-4">
          <p className="text-sm text-[#3A5C50]">Total</p>
          <p className="text-2xl font-bold text-[#122F26]">{totalCount}</p>
        </div>
        {['active', 'pending_approval', 'inactive', 'rejected'].map((s) => {
          const count = properties.filter(p => p.status === s).length;
          const colors: Record<string, string> = {
            active: 'text-green-600', pending_approval: 'text-yellow-600',
            inactive: 'text-red-600', rejected: 'text-gray-600',
          };
          return (
            <div key={s} className="bg-white rounded-lg shadow p-4">
              <p className="text-sm text-[#3A5C50]">{STATUS_LABELS[s]}</p>
              <p className={`text-2xl font-bold ${colors[s]}`}>{count}</p>
              <p className="text-xs text-gray-400">on this page</p>
            </div>
          );
        })}
      </div>

      {/* Properties Table */}
      <div className="bg-white rounded-lg shadow overflow-hidden">
        {loading ? (
          <div className="flex items-center justify-center h-64">
            <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-[#D9B168]"></div>
          </div>
        ) : properties.length === 0 ? (
          <div className="flex flex-col items-center justify-center h-64 text-[#3A5C50]">
            <Home className="w-12 h-12 mb-3 opacity-30" />
            <p className="text-lg font-medium">No properties found</p>
            <p className="text-sm mt-1">
              {search || statusFilter
                ? 'Try adjusting your search or filters'
                : 'No properties have been created yet'}
            </p>
          </div>
        ) : (
          <>
            <div className="overflow-x-auto">
              <table className="min-w-full divide-y divide-primary-200">
                <thead className="bg-sand-50">
                  <tr>
                    <th className="px-4 py-3 text-left">
                      <input
                        type="checkbox"
                        onChange={(e) => {
                          if (e.target.checked) {
                            setSelectedProperties(properties.map(p => p.id));
                          } else {
                            setSelectedProperties([]);
                          }
                        }}
                        checked={selectedProperties.length === properties.length && properties.length > 0}
                        className="rounded border-primary-300"
                      />
                    </th>
                    <th className="px-4 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Property
                    </th>
                    <th className="px-4 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Host
                    </th>
                    <th className="px-4 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Location
                    </th>
                    <th className="px-4 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Type
                    </th>
                    <th className="px-4 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Price/Night
                    </th>
                    <th className="px-4 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Status
                    </th>
                    <th className="px-4 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Created
                    </th>
                    <th className="px-4 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                      Actions
                    </th>
                  </tr>
                </thead>
                <tbody className="bg-white divide-y divide-primary-200">
                  {properties.map((property) => (
                    <tr key={property.id} className="hover:bg-sand-50">
                      <td className="px-4 py-4">
                        <input
                          type="checkbox"
                          checked={selectedProperties.includes(property.id)}
                          onChange={() => toggleSelectProperty(property.id)}
                          className="rounded border-primary-300"
                        />
                      </td>
                      <td className="px-4 py-4">
                        <div className="flex items-center">
                          {property.images?.[0] ? (
                            <img
                              src={property.images[0].image_url}
                              alt={property.title}
                              className="h-12 w-12 rounded object-cover flex-shrink-0"
                            />
                          ) : property.main_image_url ? (
                            <img
                              src={property.main_image_url}
                              alt={property.title}
                              className="h-12 w-12 rounded object-cover flex-shrink-0"
                            />
                          ) : (
                            <div className="h-12 w-12 rounded bg-primary-200 flex items-center justify-center flex-shrink-0">
                              <Home className="w-6 h-6 text-primary-400" />
                            </div>
                          )}
                          <div className="ml-3 min-w-0">
                            <div className="text-sm font-medium text-[#122F26] truncate max-w-[200px]" title={property.title}>
                              {property.title}
                            </div>
                            <div className="text-xs text-[#3A5C50]">
                              {property.bedrooms} bed • {property.bathrooms} bath • {property.max_guests} guests
                            </div>
                          </div>
                        </div>
                      </td>
                      <td className="px-4 py-4 whitespace-nowrap">
                        <div className="text-sm text-[#122F26] truncate max-w-[120px]" title={property.host_name || property.host_email}>
                          {property.host_name || `Host #${property.host}`}
                        </div>
                        {property.host_email && (
                          <div className="text-xs text-[#3A5C50] truncate max-w-[120px]" title={property.host_email}>
                            {property.host_email}
                          </div>
                        )}
                      </td>
                      <td className="px-4 py-4 whitespace-nowrap">
                        <div className="text-sm text-[#122F26]">{getCity(property)}</div>
                        <div className="text-xs text-[#3A5C50]">{getCountry(property)}</div>
                      </td>
                      <td className="px-4 py-4 whitespace-nowrap text-sm text-[#122F26] capitalize">
                        {(property.property_type || '').replace('_', ' ')}
                      </td>
                      <td className="px-4 py-4 whitespace-nowrap text-sm text-[#122F26]">
                        {property.currency || '$'}{property.price_per_night}
                      </td>
                      <td className="px-4 py-4 whitespace-nowrap">
                        <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${STATUS_COLORS[property.status] || 'bg-gray-100 text-gray-800'}`}>
                          {STATUS_LABELS[property.status] || property.status}
                        </span>
                      </td>
                      <td className="px-4 py-4 whitespace-nowrap text-xs text-[#3A5C50]">
                        {property.created_at
                          ? new Date(property.created_at).toLocaleDateString()
                          : '—'}
                      </td>
                      <td className="px-4 py-4 whitespace-nowrap text-sm font-medium">
                        <div className="flex items-center space-x-1">
                          {property.status === 'pending_approval' && (
                            <>
                              <button
                                onClick={() => handleApprove(property.id)}
                                className="p-1 text-green-600 hover:text-green-900 hover:bg-green-50 rounded"
                                title="Approve"
                              >
                                <CheckCircle className="w-5 h-5" />
                              </button>
                              <button
                                onClick={() => handleReject(property.id)}
                                className="p-1 text-red-600 hover:text-red-900 hover:bg-red-50 rounded"
                                title="Reject"
                              >
                                <XCircle className="w-5 h-5" />
                              </button>
                            </>
                          )}
                          {property.status === 'active' && (
                            <button
                              onClick={() => handleSuspend(property.id)}
                              className="p-1 text-yellow-600 hover:text-yellow-900 hover:bg-yellow-50 rounded"
                              title="Suspend"
                            >
                              <Ban className="w-5 h-5" />
                            </button>
                          )}
                          {(property.status === 'inactive' || property.status === 'rejected') && (
                            <button
                              onClick={() => handleActivate(property.id)}
                              className="p-1 text-green-600 hover:text-green-900 hover:bg-green-50 rounded"
                              title="Reactivate"
                            >
                              <Power className="w-5 h-5" />
                            </button>
                          )}
                          <button
                            onClick={() => window.open(`/admin/properties/${property.id}`, '_blank')}
                            className="p-1 text-[#3A5C50] hover:text-[#122F26] hover:bg-sand-100 rounded"
                            title="Admin detail"
                          >
                            <Eye className="w-5 h-5" />
                          </button>
                          <button
                            onClick={() => window.open(`/properties/${property.id}`, '_blank')}
                            className="p-1 text-[#D9B168] hover:text-[#c9a158] hover:bg-yellow-50 rounded"
                            title="Public page"
                          >
                            <ExternalLink className="w-4 h-4" />
                          </button>
                        </div>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>

            {/* Pagination */}
            <div className="bg-sand-50 px-6 py-4 flex items-center justify-between border-t">
              <div className="text-sm text-[#122F26]">
                Showing {totalCount > 0 ? (page - 1) * ITEMS_PER_PAGE + 1 : 0} to {Math.min(page * ITEMS_PER_PAGE, totalCount)} of {totalCount} properties
                {totalPages > 1 && <span className="ml-2 text-[#3A5C50]">(Page {page} of {totalPages})</span>}
              </div>
              <div className="flex space-x-2">
                <button
                  onClick={() => setPage(p => Math.max(1, p - 1))}
                  disabled={page === 1}
                  className="px-4 py-2 border border-[#3A5C50] rounded-lg text-sm font-medium text-[#122F26] hover:bg-sand-50 disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  Previous
                </button>
                <button
                  onClick={() => setPage(p => p + 1)}
                  disabled={page * ITEMS_PER_PAGE >= totalCount}
                  className="px-4 py-2 border border-[#3A5C50] rounded-lg text-sm font-medium text-[#122F26] hover:bg-sand-50 disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  Next
                </button>
              </div>
            </div>
          </>
        )}
      </div>

      {/* Confirm Dialog */}
      <ConfirmDialog
        isOpen={showConfirmDialog}
        onClose={() => setShowConfirmDialog(false)}
        onConfirm={handleConfirm}
        title={
          confirmAction?.type === 'approve' ? 'Approve Property' :
          confirmAction?.type === 'activate' ? 'Activate Property' : 'Suspend Property'
        }
        message={
          confirmAction?.type === 'approve'
            ? 'Are you sure you want to approve this property? It will become visible to all users.'
            : confirmAction?.type === 'activate'
            ? 'Are you sure you want to reactivate this property?'
            : 'Are you sure you want to suspend this property? It will be marked as inactive.'
        }
        variant={confirmAction?.type === 'suspend' ? 'warning' : 'info'}
        confirmText={
          confirmAction?.type === 'approve' ? 'Approve' :
          confirmAction?.type === 'activate' ? 'Activate' : 'Suspend'
        }
      />

      {/* Reject Dialog with Reason */}
      {showRejectDialog && (
        <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/50">
          <div className="bg-white rounded-xl shadow-xl p-6 w-full max-w-md mx-4">
            <h3 className="text-lg font-semibold text-[#122F26] mb-2">Reject Property</h3>
            <p className="text-sm text-[#3A5C50] mb-4">
              This property will be marked as rejected and hidden from public listings.
            </p>
            <div className="mb-4">
              <label className="block text-sm font-medium text-[#122F26] mb-1">
                Reason for rejection
              </label>
              <textarea
                value={rejectReason}
                onChange={(e) => setRejectReason(e.target.value)}
                rows={3}
                placeholder="E.g., Incomplete listing, inappropriate content, duplicate..."
                className="w-full px-3 py-2 border border-[#3A5C50] rounded-lg focus:ring-2 focus:ring-[#D9B168] focus:border-transparent text-sm"
              />
            </div>
            <div className="flex justify-end gap-3">
              <button
                onClick={() => {
                  setShowRejectDialog(false);
                  setRejectPropertyId(null);
                  setRejectReason('');
                }}
                className="px-4 py-2 border border-[#3A5C50] rounded-lg text-sm font-medium text-[#122F26] hover:bg-sand-50"
              >
                Cancel
              </button>
              <button
                onClick={submitReject}
                className="px-4 py-2 bg-red-600 text-white rounded-lg text-sm font-medium hover:bg-red-700"
              >
                Reject Property
              </button>
            </div>
          </div>
        </div>
      )}
    </div>
  );
}
