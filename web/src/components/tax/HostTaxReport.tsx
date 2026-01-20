'use client';

import { useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import axios from 'axios';
import { HostTaxReport as TaxReportType } from '@/types/tax-types';
import { Loader2, Download, Calendar } from 'lucide-react';
import { format, subMonths, startOfMonth, endOfMonth } from 'date-fns';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8000';

// Transform backend response to frontend expected format
const transformTaxReport = (data: any): TaxReportType | null => {
  if (!data) return null;
  
  // Build tax breakdown from by_jurisdiction
  const taxBreakdown: TaxReportType['tax_breakdown'] = [];
  if (data.by_jurisdiction) {
    for (const [code, info] of Object.entries(data.by_jurisdiction as Record<string, any>)) {
      taxBreakdown.push({
        jurisdiction: info.name || code,
        tax_type: 'Tax', // Default type since backend groups by jurisdiction
        total_amount: parseFloat(info.total_tax) || 0,
      });
    }
  }
  
  return {
    period_start: data.period?.start || '',
    period_end: data.period?.end || '',
    total_bookings: data.summary?.total_bookings || 0,
    total_taxes_collected: parseFloat(data.summary?.total_tax_collected) || 0,
    tax_breakdown: taxBreakdown,
  };
};

export const HostTaxReport = () => {
  const [periodStart, setPeriodStart] = useState(
    format(startOfMonth(subMonths(new Date(), 1)), 'yyyy-MM-dd')
  );
  const [periodEnd, setPeriodEnd] = useState(
    format(endOfMonth(subMonths(new Date(), 1)), 'yyyy-MM-dd')
  );

  const { data: report, isLoading, error } = useQuery<TaxReportType | null>({
    queryKey: ['host-tax-report', periodStart, periodEnd],
    queryFn: async () => {
      const token = localStorage.getItem('access_token');
      const response = await axios.get(
        `${API_BASE_URL}/api/v1/payments/tax/reports/host_summary/`,
        {
          params: {
            period_start: periodStart,
            period_end: periodEnd,
          },
          headers: {
            Authorization: `Bearer ${token}`,
          },
        }
      );
      return transformTaxReport(response.data);
    },
  });

  const exportToCSV = () => {
    if (!report) return;

    const headers = ['Jurisdiction', 'Tax Type', 'Amount'];
    const rows = report.tax_breakdown.map(item => [
      item.jurisdiction,
      item.tax_type,
      item.total_amount.toFixed(2),
    ]);

    const csvRows = [
      headers.join(','),
      ...rows.map(row => row.join(',')),
      '',
    ];
    
    if (report.total_revenue !== undefined) {
      csvRows.push(`Total Revenue,${report.total_revenue.toFixed(2)}`);
    }
    csvRows.push(`Total Taxes Collected,${report.total_taxes_collected.toFixed(2)}`);
    csvRows.push(`Total Bookings,${report.total_bookings}`);
    csvRows.push(`Period,${periodStart} to ${periodEnd}`);

    const csvContent = csvRows.join('\n');

    const blob = new Blob([csvContent], { type: 'text/csv' });
    const url = window.URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = `tax-report-${periodStart}-${periodEnd}.csv`;
    link.click();
    window.URL.revokeObjectURL(url);
  };

  return (
    <div className="bg-white rounded-lg shadow-sm border">
      <div className="p-6 border-b">
        <div className="flex items-center justify-between mb-4">
          <h2 className="text-xl font-semibold text-gray-900">Tax Report</h2>
          
          {report && (
            <button
              onClick={exportToCSV}
              className="inline-flex items-center gap-2 px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
            >
              <Download className="h-4 w-4" />
              <span>Export CSV</span>
            </button>
          )}
        </div>

        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              <Calendar className="h-4 w-4 inline mr-1" />
              Period Start
            </label>
            <input
              type="date"
              value={periodStart}
              onChange={(e) => setPeriodStart(e.target.value)}
              className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent"
            />
          </div>

          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              <Calendar className="h-4 w-4 inline mr-1" />
              Period End
            </label>
            <input
              type="date"
              value={periodEnd}
              onChange={(e) => setPeriodEnd(e.target.value)}
              className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent"
            />
          </div>
        </div>
      </div>

      <div className="p-6">
        {isLoading ? (
          <div className="flex items-center justify-center py-12">
            <Loader2 className="h-8 w-8 animate-spin text-gray-400" />
          </div>
        ) : error ? (
          <div className="bg-red-50 border border-red-200 rounded-lg p-4">
            <p className="text-sm text-red-800">
              Failed to load tax report. Please try again.
            </p>
          </div>
        ) : report ? (
          <div className="space-y-6">
            {/* Summary Cards */}
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              {report.total_revenue !== undefined && (
                <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
                  <p className="text-sm text-blue-600 font-medium mb-1">Total Revenue</p>
                  <p className="text-2xl font-bold text-blue-900">
                    ${report.total_revenue.toFixed(2)}
                  </p>
                </div>
              )}

              <div className="bg-green-50 border border-green-200 rounded-lg p-4">
                <p className="text-sm text-green-600 font-medium mb-1">Taxes Collected</p>
                <p className="text-2xl font-bold text-green-900">
                  ${report.total_taxes_collected.toFixed(2)}
                </p>
              </div>

              <div className="bg-purple-50 border border-purple-200 rounded-lg p-4">
                <p className="text-sm text-purple-600 font-medium mb-1">Total Bookings</p>
                <p className="text-2xl font-bold text-purple-900">
                  {report.total_bookings}
                </p>
              </div>
            </div>

            {/* Tax Breakdown Table */}
            {report.tax_breakdown && report.tax_breakdown.length > 0 ? (
              <div>
                <h3 className="text-lg font-semibold text-gray-900 mb-4">Tax Breakdown</h3>
                <div className="overflow-x-auto">
                  <table className="w-full">
                    <thead className="bg-gray-50 border-b">
                      <tr>
                        <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                          Jurisdiction
                        </th>
                        <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                          Tax Type
                        </th>
                        <th className="px-4 py-3 text-right text-xs font-medium text-gray-500 uppercase tracking-wider">
                          Total Amount
                        </th>
                      </tr>
                    </thead>
                    <tbody className="divide-y divide-gray-200">
                      {report.tax_breakdown.map((item, index) => (
                        <tr key={index} className="hover:bg-gray-50">
                          <td className="px-4 py-3 text-sm text-gray-900">
                            {item.jurisdiction}
                          </td>
                          <td className="px-4 py-3 text-sm text-gray-700">
                            {item.tax_type}
                          </td>
                          <td className="px-4 py-3 text-sm text-gray-900 text-right font-medium">
                            ${item.total_amount.toFixed(2)}
                          </td>
                        </tr>
                      ))}
                    </tbody>
                    <tfoot className="bg-gray-50 border-t-2">
                      <tr>
                        <td colSpan={2} className="px-4 py-3 text-sm font-semibold text-gray-900">
                          Total
                        </td>
                        <td className="px-4 py-3 text-sm font-bold text-gray-900 text-right">
                          ${report.total_taxes_collected.toFixed(2)}
                        </td>
                      </tr>
                    </tfoot>
                  </table>
                </div>
              </div>
            ) : (
              <div className="text-center py-8 text-gray-500">
                No tax data available for the selected period.
              </div>
            )}
          </div>
        ) : null}
      </div>
    </div>
  );
};
