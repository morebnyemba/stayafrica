"use client";

import { useState } from 'react';
import { useAuth } from '@/store/auth-store';
import { Bug, X, Camera, AlertCircle, ChevronDown } from 'lucide-react';
import toast from 'react-hot-toast';
import html2canvas from 'html2canvas';

export const BugReportButton = () => {
  const { isAuthenticated, isLoading } = useAuth();

  const [isOpen, setIsOpen] = useState(false);
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [isCapturing, setIsCapturing] = useState(false);
  const [title, setTitle] = useState('');
  const [description, setDescription] = useState('');
  const [steps, setSteps] = useState('');
  const [severity, setSeverity] = useState('minor');
  const [screenshot, setScreenshot] = useState<string | null>(null);

  const inputStyle = {
    background: 'rgba(255,255,255,0.07)',
    border: '1px solid rgba(212,177,88,0.2)',
    color: 'white',
  };

  const handleCaptureScreen = async () => {
    setIsCapturing(true);
    try {
      setIsOpen(false);
      await new Promise(resolve => setTimeout(resolve, 100));
      const canvas = await html2canvas(document.body, {
        useCORS: true, logging: false,
        ignoreElements: (el) => el.id === 'support-widget-container'
      });
      setScreenshot(canvas.toDataURL('image/png'));
    } catch {
      toast.error('Could not capture screen. You can still submit without it.');
    } finally {
      setIsCapturing(false);
      setIsOpen(true);
    }
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!title || !description) return;
    setIsSubmitting(true);
    try {
      const accessToken = typeof window !== 'undefined' ? localStorage.getItem('access_token') : '';
      const res = await fetch('/api/support/bugs/', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json', 'Authorization': `Bearer ${accessToken}` },
        body: JSON.stringify({
          title, description, steps_to_reproduce: steps, severity, status: 'new',
          page_url: window.location.href,
          browser_info: {
            userAgent: navigator.userAgent,
            language: navigator.language,
            screenResolution: `${screen.width}x${screen.height}`,
            viewport: `${window.innerWidth}x${window.innerHeight}`,
          },
          screenshot_base64: screenshot
        })
      });
      if (!res.ok) throw new Error();
      toast.success('Bug reported! Our team will review it shortly.');
      setIsOpen(false);
      setTitle(''); setDescription(''); setSteps(''); setScreenshot(null);
    } catch {
      toast.error('Failed to submit report. Please try again.');
    } finally {
      setIsSubmitting(false);
    }
  };

  if (isLoading || !isAuthenticated) return null;

  return (
    <>
      {/* Floating Bug Button */}
      <button
        onClick={() => setIsOpen(true)}
        className="fixed bottom-6 left-6 z-40 flex items-center gap-2 px-3 h-10 rounded-full text-xs font-medium transition-all hover:scale-105"
        style={{
          background: 'rgba(15, 25, 10, 0.7)',
          backdropFilter: 'blur(12px)',
          WebkitBackdropFilter: 'blur(12px)',
          border: '1px solid rgba(212,177,88,0.3)',
          color: '#d4b158',
          boxShadow: '0 4px 16px rgba(0,0,0,0.3)',
        }}
      >
        <Bug className="h-3.5 w-3.5" />
        Report Issue
      </button>

      {/* Modal Overlay */}
      {isOpen && (
        <div
          className="fixed inset-0 z-50 flex items-center justify-center p-4"
          style={{ background: 'rgba(0,0,0,0.6)', backdropFilter: 'blur(4px)' }}
        >
          <div
            className="w-full max-w-lg rounded-2xl overflow-hidden flex flex-col max-h-[90vh]"
            style={{
              background: 'rgba(12, 22, 6, 0.88)',
              backdropFilter: 'blur(24px)',
              WebkitBackdropFilter: 'blur(24px)',
              border: '1px solid rgba(212,177,88,0.2)',
              boxShadow: '0 32px 80px rgba(0,0,0,0.6), inset 0 1px 0 rgba(212,177,88,0.1)',
            }}
          >
            {/* Header */}
            <div
              className="p-4 flex justify-between items-center shrink-0"
              style={{ borderBottom: '1px solid rgba(212,177,88,0.15)', background: 'rgba(45,80,22,0.3)' }}
            >
              <div className="flex items-center gap-2">
                <Bug className="h-4 w-4" style={{ color: '#d4b158' }} />
                <h2 className="font-semibold text-white text-sm">Report a Bug</h2>
              </div>
              <button
                onClick={() => setIsOpen(false)}
                className="p-1.5 rounded-full transition-colors"
                style={{ color: 'rgba(255,255,255,0.5)' }}
                onMouseEnter={e => (e.currentTarget.style.background = 'rgba(212,177,88,0.1)')}
                onMouseLeave={e => (e.currentTarget.style.background = 'transparent')}
              >
                <X className="h-4 w-4" />
              </button>
            </div>

            {/* Body */}
            <div className="p-5 overflow-y-auto flex-1" style={{ scrollbarWidth: 'thin', scrollbarColor: 'rgba(212,177,88,0.2) transparent' }}>
              <div
                className="text-xs p-3 rounded-xl mb-5 flex gap-2"
                style={{ background: 'rgba(212,177,88,0.08)', border: '1px solid rgba(212,177,88,0.15)', color: 'rgba(255,255,255,0.6)' }}
              >
                <AlertCircle className="h-4 w-4 shrink-0 mt-0.5" style={{ color: '#d4b158' }} />
                <p>Provide as much detail as possible. Browser info and current URL are captured automatically.</p>
              </div>

              <form id="bug-report-form" onSubmit={handleSubmit} className="space-y-4">
                <div>
                  <label className="text-xs font-medium block mb-1" style={{ color: 'rgba(255,255,255,0.6)' }}>Issue Title</label>
                  <input
                    placeholder="e.g. Broken checkout button"
                    value={title}
                    onChange={(e) => setTitle(e.target.value)}
                    required
                    className="w-full h-10 rounded-xl px-3 text-sm placeholder-white/30 outline-none"
                    style={inputStyle}
                    onFocus={e => e.target.style.borderColor = 'rgba(212,177,88,0.5)'}
                    onBlur={e => e.target.style.borderColor = 'rgba(212,177,88,0.2)'}
                  />
                </div>

                <div>
                  <label className="text-xs font-medium block mb-1" style={{ color: 'rgba(255,255,255,0.6)' }}>Severity</label>
                  <div className="relative">
                    <select
                      value={severity}
                      onChange={(e) => setSeverity(e.target.value)}
                      className="w-full h-10 rounded-xl px-3 text-sm appearance-none pr-8 text-white outline-none"
                      style={inputStyle}
                    >
                      <option value="critical" style={{ background: '#0f1a07' }}>Critical (Crash / Data Loss)</option>
                      <option value="major" style={{ background: '#0f1a07' }}>Major (Feature Broken)</option>
                      <option value="minor" style={{ background: '#0f1a07' }}>Minor (Workaround Exists)</option>
                      <option value="cosmetic" style={{ background: '#0f1a07' }}>Cosmetic (UI Glitch)</option>
                    </select>
                    <ChevronDown className="h-4 w-4 absolute right-2.5 top-3 pointer-events-none" style={{ color: 'rgba(212,177,88,0.6)' }} />
                  </div>
                </div>

                <div>
                  <label className="text-xs font-medium block mb-1" style={{ color: 'rgba(255,255,255,0.6)' }}>Description</label>
                  <textarea
                    placeholder="What went wrong?"
                    value={description}
                    onChange={(e) => setDescription(e.target.value)}
                    required
                    rows={3}
                    className="w-full rounded-xl px-3 py-2 text-sm placeholder-white/30 outline-none resize-none"
                    style={inputStyle}
                    onFocus={e => e.target.style.borderColor = 'rgba(212,177,88,0.5)'}
                    onBlur={e => e.target.style.borderColor = 'rgba(212,177,88,0.2)'}
                  />
                </div>

                <div>
                  <label className="text-xs font-medium block mb-1" style={{ color: 'rgba(255,255,255,0.6)' }}>Steps to Reproduce <span style={{ color: 'rgba(255,255,255,0.3)' }}>(Optional)</span></label>
                  <textarea
                    placeholder="1. Go to...   2. Click..."
                    value={steps}
                    onChange={(e) => setSteps(e.target.value)}
                    rows={2}
                    className="w-full rounded-xl px-3 py-2 text-sm placeholder-white/30 outline-none resize-none"
                    style={inputStyle}
                    onFocus={e => e.target.style.borderColor = 'rgba(212,177,88,0.5)'}
                    onBlur={e => e.target.style.borderColor = 'rgba(212,177,88,0.2)'}
                  />
                </div>

                <div>
                  <p className="text-xs font-medium mb-2" style={{ color: 'rgba(255,255,255,0.6)' }}>Screenshot Evidence</p>
                  {screenshot ? (
                    <div className="relative rounded-xl overflow-hidden group">
                      <img src={screenshot} alt="Captured" className="w-full object-cover max-h-[140px] opacity-80" />
                      <div className="absolute inset-0 bg-black/50 flex items-center justify-center opacity-0 group-hover:opacity-100 transition-opacity">
                        <button
                          type="button"
                          onClick={() => setScreenshot(null)}
                          className="px-3 py-1.5 rounded-lg text-xs font-medium"
                          style={{ background: 'rgba(220,38,38,0.8)', color: 'white' }}
                        >
                          Remove
                        </button>
                      </div>
                    </div>
                  ) : (
                    <button
                      type="button"
                      onClick={handleCaptureScreen}
                      disabled={isCapturing}
                      className="w-full py-6 rounded-xl text-sm transition-colors flex items-center justify-center gap-2"
                      style={{
                        background: 'rgba(255,255,255,0.04)',
                        border: '1px dashed rgba(212,177,88,0.25)',
                        color: 'rgba(255,255,255,0.4)',
                      }}
                    >
                      {isCapturing ? (
                        <span className="flex items-center gap-2">
                          <span className="animate-spin w-4 h-4 border-2 rounded-full" style={{ borderColor: '#d4b158', borderTopColor: 'transparent' }} />
                          Capturing...
                        </span>
                      ) : (
                        <>
                          <Camera className="h-4 w-4" style={{ color: '#d4b158' }} />
                          Capture Current Screen
                        </>
                      )}
                    </button>
                  )}
                </div>
              </form>
            </div>

            {/* Footer */}
            <div
              className="p-4 flex justify-end gap-3 shrink-0"
              style={{ borderTop: '1px solid rgba(212,177,88,0.1)', background: 'rgba(0,0,0,0.2)' }}
            >
              <button
                type="button"
                onClick={() => setIsOpen(false)}
                className="px-4 h-9 rounded-xl text-sm transition-colors"
                style={{ color: 'rgba(255,255,255,0.5)', background: 'rgba(255,255,255,0.05)' }}
              >
                Cancel
              </button>
              <button
                type="submit"
                form="bug-report-form"
                disabled={isSubmitting || !title || !description}
                className="px-5 h-9 rounded-xl text-sm font-semibold transition-all disabled:opacity-40 disabled:cursor-not-allowed"
                style={{ background: 'linear-gradient(135deg, #d4b158, #b8952e)', color: '#0f1a07' }}
              >
                {isSubmitting ? 'Submitting...' : 'Submit Report'}
              </button>
            </div>
          </div>
        </div>
      )}
    </>
  );
};
