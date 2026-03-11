'use client';

import React, { useState } from 'react';
import { Download, FileText, FileSpreadsheet, Loader2 } from 'lucide-react';
import { useExportAnalytics } from '@/hooks/useAnalytics';
import toast from 'react-hot-toast';

interface ExportButtonProps {
  data: any;
  dataType?: 'dashboard' | 'revenue' | 'occupancy' | 'bookings' | 'performance';
  filename?: string;
  className?: string;
}

export const ExportButton: React.FC<ExportButtonProps> = ({
  data,
  dataType = 'dashboard',
  filename,
  className = '',
}) => {
  const [isOpen, setIsOpen] = useState(false);
  const { mutate: exportData, isPending } = useExportAnalytics();

  const handleExport = (format: 'csv' | 'pdf') => {
    if (!data) {
      toast.error('No data available to export');
      return;
    }

    const exportFilename = filename || `analytics-${dataType}-${new Date().toISOString().split('T')[0]}`;

    exportData(
      { format, data },
      {
        onSuccess: (blob) => {
          // Create download link
          const url = window.URL.createObjectURL(blob);
          const link = document.createElement('a');
          link.href = url;
          link.download = `${exportFilename}.${format}`;
          document.body.appendChild(link);
          link.click();
          document.body.removeChild(link);
          window.URL.revokeObjectURL(url);

          toast.success(`Report exported as ${format.toUpperCase()}`);
          setIsOpen(false);
        },
        onError: (error: any) => {
          toast.error(error.message || 'Failed to export report');
        },
      }
    );
  };

  return (
    <div className={`relative ${className}`}>
      {/* Trigger Button */}
      <button
        onClick={() => setIsOpen(!isOpen)}
        disabled={isPending || !data}
        className="flex items-center space-x-2 px-4 py-2 bg-secondary-600 hover:bg-secondary-700 disabled:bg-primary-300 text-white rounded-lg transition-colors"
      >
        {isPending ? (
          <Loader2 className="h-4 w-4 animate-spin" />
        ) : (
          <Download className="h-4 w-4" />
        )}
        <span className="text-sm font-medium">Export</span>
      </button>

      {/* Dropdown Menu */}
      {isOpen && !isPending && (
        <>
          {/* Backdrop */}
          <div
            className="fixed inset-0 z-40"
            onClick={() => setIsOpen(false)}
          ></div>

          {/* Menu */}
          <div className="absolute right-0 mt-2 z-50 w-48 bg-white rounded-lg shadow-xl border border-sand-200/50 py-2">
            <button
              onClick={() => handleExport('csv')}
              className="w-full flex items-center space-x-3 px-4 py-2 hover:bg-sand-50 transition-colors"
            >
              <FileSpreadsheet className="h-4 w-4 text-green-600" />
              <div className="text-left">
                <p className="text-sm font-medium text-primary-900">
                  Export as CSV
                </p>
                <p className="text-xs text-primary-400">
                  Spreadsheet format
                </p>
              </div>
            </button>

            <button
              onClick={() => handleExport('pdf')}
              className="w-full flex items-center space-x-3 px-4 py-2 hover:bg-sand-50 transition-colors"
            >
              <FileText className="h-4 w-4 text-red-600" />
              <div className="text-left">
                <p className="text-sm font-medium text-primary-900">
                  Export as PDF
                </p>
                <p className="text-xs text-primary-400">
                  Printable report
                </p>
              </div>
            </button>
          </div>
        </>
      )}
    </div>
  );
};
