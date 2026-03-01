'use client';

import { useEffect, useState } from 'react';
import { adminApi } from '@/lib/admin-api';
import { Search, MapPin, Tag, CheckCircle, XCircle, Trash2 } from 'lucide-react';
import toast from 'react-hot-toast';
import ConfirmDialog from '@/components/admin/ConfirmDialog';

type POICategory = {
  id: string;
  name: string;
  icon?: string;
  description?: string;
};

type PointOfInterest = {
  id: string;
  name: string;
  category: POICategory;
  poi_type: string;
  city?: string;
  country: string;
  address?: string;
  latitude?: number;
  longitude?: number;
  is_active: boolean;
  created_at: string;
};

export default function POIManagement() {
  const [activeTab, setActiveTab] = useState<'pois' | 'categories'>('pois');
  const [pois, setPOIs] = useState<PointOfInterest[]>([]);
  const [categories, setCategories] = useState<POICategory[]>([]);
  const [loading, setLoading] = useState(true);
  const [search, setSearch] = useState('');
  const [page, setPage] = useState(1);
  const [totalCount, setTotalCount] = useState(0);
  const [selectedPOIs, setSelectedPOIs] = useState<string[]>([]);
  const [showConfirmDialog, setShowConfirmDialog] = useState(false);
  const [confirmAction, setConfirmAction] = useState<{ type: string; ids: string[] } | null>(null);
  const [showDeleteDialog, setShowDeleteDialog] = useState(false);
  const [deleteTarget, setDeleteTarget] = useState<{ type: 'poi' | 'category'; id: string } | null>(null);
  const ITEMS_PER_PAGE = 20;

  useEffect(() => {
    if (activeTab === 'pois') {
      loadPOIs();
    } else {
      loadCategories();
    }
  }, [activeTab, page, search]);

  const loadPOIs = async () => {
    try {
      setLoading(true);
      const data = await adminApi.getPOIs({ 
        page, 
        search: search.trim() || undefined,
        per_page: ITEMS_PER_PAGE,
      });
      setPOIs(data.results || []);
      setTotalCount(data.count || 0);
    } catch (err: any) {
      const errorMsg = err?.response?.data?.detail || 'Failed to load POIs';
      toast.error(errorMsg);
      console.error('POIs load error:', err);
    } finally {
      setLoading(false);
    }
  };

  const loadCategories = async () => {
    try {
      setLoading(true);
      const data = await adminApi.getPOICategories({ 
        page, 
        search: search.trim() || undefined,
        per_page: ITEMS_PER_PAGE,
      });
      setCategories(data.results || []);
      setTotalCount(data.count || 0);
    } catch (err: any) {
      const errorMsg = err?.response?.data?.detail || 'Failed to load categories';
      toast.error(errorMsg);
      console.error('Categories load error:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleSearch = () => {
    setPage(1);
  };

  const handleDeleteItem = async () => {
    if (!deleteTarget) return;
    try {
      if (deleteTarget.type === 'poi') {
        await adminApi.deletePOI(deleteTarget.id);
        toast.success('POI deleted');
        loadPOIs();
      } else {
        await adminApi.deletePOICategory(deleteTarget.id);
        toast.success('Category deleted');
        loadCategories();
      }
    } catch (err: any) {
      toast.error(err?.response?.data?.detail || 'Failed to delete');
      console.error(err);
    }
  };

  const toggleSelectPOI = (poiId: string) => {
    setSelectedPOIs(prev => 
      prev.includes(poiId) 
        ? prev.filter(id => id !== poiId)
        : [...prev, poiId]
    );
  };

  const handleBulkVerify = () => {
    if (selectedPOIs.length === 0) {
      toast.error('No POIs selected');
      return;
    }
    setConfirmAction({ type: 'verify', ids: selectedPOIs });
    setShowConfirmDialog(true);
  };

  const handleBulkUnverify = () => {
    if (selectedPOIs.length === 0) {
      toast.error('No POIs selected');
      return;
    }
    setConfirmAction({ type: 'unverify', ids: selectedPOIs });
    setShowConfirmDialog(true);
  };

  const handleConfirm = async () => {
    if (!confirmAction) return;

    try {
      if (confirmAction.type === 'verify') {
        await Promise.all(confirmAction.ids.map(id => adminApi.verifyPOI(id)));
        toast.success(`${confirmAction.ids.length} POI(s) verified`);
      } else if (confirmAction.type === 'unverify') {
        await Promise.all(confirmAction.ids.map(id => adminApi.unverifyPOI(id)));
        toast.success(`${confirmAction.ids.length} POI(s) unverified`);
      }
      setSelectedPOIs([]);
      loadPOIs();
    } catch (err) {
      toast.error('Failed to perform bulk action');
      console.error(err);
    }
  };

  const getPOITypeBadge = (type: string) => {
    const badges: Record<string, string> = {
      restaurant: 'bg-orange-100 text-orange-800',
      attraction: 'bg-purple-100 text-purple-800',
      transport: 'bg-blue-100 text-blue-800',
      shopping: 'bg-pink-100 text-pink-800',
      healthcare: 'bg-red-100 text-red-800',
      education: 'bg-green-100 text-green-800',
    };
    return badges[type] || 'bg-gray-100 text-gray-800';
  };

  return (
    <div className="p-8">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-[#122F26]">POI Management</h1>
        <p className="text-[#3A5C50] mt-2">Manage points of interest and categories</p>
      </div>

      {/* Tabs */}
      <div className="mb-6 border-b border-gray-200">
        <div className="flex space-x-8">
          <button
            onClick={() => {
              setActiveTab('pois');
              setPage(1);
            }}
            className={`pb-4 px-1 border-b-2 font-medium text-sm transition-colors ${
              activeTab === 'pois'
                ? 'border-[#D9B168] text-[#D9B168]'
                : 'border-transparent text-gray-500 hover:text-gray-700'
            }`}
          >
            <div className="flex items-center space-x-2">
              <MapPin className="w-4 h-4" />
              <span>Points of Interest</span>
            </div>
          </button>
          <button
            onClick={() => {
              setActiveTab('categories');
              setPage(1);
            }}
            className={`pb-4 px-1 border-b-2 font-medium text-sm transition-colors ${
              activeTab === 'categories'
                ? 'border-[#D9B168] text-[#D9B168]'
                : 'border-transparent text-gray-500 hover:text-gray-700'
            }`}
          >
            <div className="flex items-center space-x-2">
              <Tag className="w-4 h-4" />
              <span>POI Categories</span>
            </div>
          </button>
        </div>
      </div>

      {/* Search Bar and Bulk Actions */}
      <div className="bg-white rounded-lg shadow p-6 mb-6">
        <div className="flex space-x-2">
          <div className="flex-1 relative">
            <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-[#3A5C50] w-5 h-5" />
            <input
              type="text"
              value={search}
              onChange={(e) => setSearch(e.target.value)}
              onKeyPress={(e) => e.key === 'Enter' && handleSearch()}
              placeholder={`Search ${activeTab}...`}
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

        {/* Bulk Actions for POIs */}
        {activeTab === 'pois' && selectedPOIs.length > 0 && (
          <div className="mt-4 flex items-center justify-between bg-[#F4F1EA] px-4 py-3 rounded-lg">
            <p className="text-sm text-[#122F26] font-medium">
              {selectedPOIs.length} POI{selectedPOIs.length > 1 ? 's' : ''} selected
            </p>
            <div className="flex space-x-2">
              <button
                onClick={handleBulkVerify}
                className="px-4 py-2 bg-green-600 text-white rounded-lg hover:bg-green-700 transition-colors text-sm"
              >
                Verify Selected
              </button>
              <button
                onClick={handleBulkUnverify}
                className="px-4 py-2 bg-yellow-600 text-white rounded-lg hover:bg-yellow-700 transition-colors text-sm"
              >
                Unverify Selected
              </button>
            </div>
          </div>
        )}
      </div>

      {/* Content */}
      <div className="bg-white rounded-lg shadow overflow-hidden">
        {loading ? (
          <div className="flex items-center justify-center h-64">
            <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-[#D9B168]"></div>
          </div>
        ) : (
          <>
            {activeTab === 'pois' && (
              <div className="overflow-x-auto">
                <table className="min-w-full divide-y divide-gray-200">
                  <thead className="bg-gray-50">
                    <tr>
                      <th className="px-6 py-3 text-left">
                        <input
                          type="checkbox"
                          onChange={(e) => {
                            if (e.target.checked) {
                              setSelectedPOIs(pois.map(p => p.id));
                            } else {
                              setSelectedPOIs([]);
                            }
                          }}
                          checked={selectedPOIs.length === pois.length && pois.length > 0}
                          className="rounded border-gray-300"
                        />
                      </th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                        Name
                      </th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                        Category
                      </th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                        Type
                      </th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                        Location
                      </th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                        Verified
                      </th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                        Created
                      </th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                        Actions
                      </th>
                    </tr>
                  </thead>
                  <tbody className="bg-white divide-y divide-gray-200">
                    {pois.map((poi) => (
                      <tr key={poi.id} className="hover:bg-gray-50">
                        <td className="px-6 py-4">
                          <input
                            type="checkbox"
                            checked={selectedPOIs.includes(poi.id)}
                            onChange={() => toggleSelectPOI(poi.id)}
                            className="rounded border-gray-300"
                          />
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap">
                          <div className="text-sm font-medium text-[#122F26]">
                            {poi.name}
                          </div>
                          {poi.address && (
                            <div className="text-xs text-[#3A5C50]">
                              {poi.address}
                            </div>
                          )}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-[#122F26]">
                          {poi.category.name}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap">
                          <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${getPOITypeBadge(poi.poi_type)}`}>
                            {poi.poi_type}
                          </span>
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap">
                          <div className="text-sm text-[#122F26]">
                            {poi.city && `${poi.city}, `}
                            {poi.country}
                          </div>
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap">
                          <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${
                            poi.is_active ? 'bg-green-100 text-green-800' : 'bg-yellow-100 text-yellow-800'
                          }`}>
                            {poi.is_active ? (
                              <>
                                <CheckCircle className="w-3 h-3 mr-1" />
                                Verified
                              </>
                            ) : (
                              <>
                                <XCircle className="w-3 h-3 mr-1" />
                                Unverified
                              </>
                            )}
                          </span>
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-[#3A5C50]">
                          {new Date(poi.created_at).toLocaleDateString()}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm font-medium">
                          <button
                            onClick={() => { setDeleteTarget({ type: 'poi', id: poi.id }); setShowDeleteDialog(true); }}
                            className="text-red-600 hover:text-red-900"
                            title="Delete POI"
                          >
                            <Trash2 className="w-4 h-4" />
                          </button>
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            )}

            {activeTab === 'categories' && (
              <div className="overflow-x-auto">
                <table className="min-w-full divide-y divide-gray-200">
                  <thead className="bg-gray-50">
                    <tr>
                      <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                        Name
                      </th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                        Icon
                      </th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                        Description
                      </th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-[#3A5C50] uppercase tracking-wider">
                        Actions
                      </th>
                    </tr>
                  </thead>
                  <tbody className="bg-white divide-y divide-gray-200">
                    {categories.map((category) => (
                      <tr key={category.id} className="hover:bg-gray-50">
                        <td className="px-6 py-4 whitespace-nowrap">
                          <div className="text-sm font-medium text-[#122F26]">
                            {category.name}
                          </div>
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-[#122F26]">
                          {category.icon || '-'}
                        </td>
                        <td className="px-6 py-4">
                          <div className="text-sm text-[#3A5C50]">
                            {category.description || '-'}
                          </div>
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm font-medium">
                          <button
                            onClick={() => { setDeleteTarget({ type: 'category', id: category.id }); setShowDeleteDialog(true); }}
                            className="text-red-600 hover:text-red-900"
                            title="Delete category"
                          >
                            <Trash2 className="w-4 h-4" />
                          </button>
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            )}

            {/* Pagination */}
            <div className="bg-gray-50 px-6 py-4 flex items-center justify-between border-t">
              <div className="text-sm text-[#122F26]">
                Showing {totalCount > 0 ? (page - 1) * ITEMS_PER_PAGE + 1 : 0} to {Math.min(page * ITEMS_PER_PAGE, totalCount)} of {totalCount} items
              </div>
              <div className="flex space-x-2">
                <button
                  onClick={() => setPage(p => Math.max(1, p - 1))}
                  disabled={page === 1}
                  className="px-4 py-2 border border-[#3A5C50] rounded-lg text-sm font-medium text-[#122F26] hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  Previous
                </button>
                <button
                  onClick={() => setPage(p => p + 1)}
                  disabled={page * ITEMS_PER_PAGE >= totalCount}
                  className="px-4 py-2 border border-[#3A5C50] rounded-lg text-sm font-medium text-[#122F26] hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  Next
                </button>
              </div>
            </div>
          </>
        )}
      </div>

      {/* Verify Confirm Dialog */}
      <ConfirmDialog
        isOpen={showConfirmDialog}
        onClose={() => setShowConfirmDialog(false)}
        onConfirm={handleConfirm}
        title={confirmAction?.type === 'verify' ? 'Verify POIs' : 'Unverify POIs'}
        message={`Are you sure you want to ${confirmAction?.type} ${confirmAction?.ids.length} POI(s)?`}
        variant={confirmAction?.type === 'verify' ? 'info' : 'warning'}
        confirmText={confirmAction?.type === 'verify' ? 'Verify' : 'Unverify'}
      />

      {/* Delete Confirm Dialog */}
      <ConfirmDialog
        isOpen={showDeleteDialog}
        onClose={() => { setShowDeleteDialog(false); setDeleteTarget(null); }}
        onConfirm={handleDeleteItem}
        title={`Delete ${deleteTarget?.type === 'category' ? 'Category' : 'POI'}`}
        message="Are you sure you want to delete this item? This action cannot be undone."
        variant="danger"
        confirmText="Delete"
      />
    </div>
  );
}
