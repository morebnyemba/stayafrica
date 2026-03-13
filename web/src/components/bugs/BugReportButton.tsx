"use client";

import React, { useState } from 'react';
import { useAuth } from '@/context/auth-context';
import { Bug, X, Camera, AlertCircle } from 'lucide-react';
import { Button, Input, Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui';
import { useToast } from '@/hooks/use-toast';
import html2canvas from 'html2canvas';

export const BugReportButton = () => {
  const { isAuthenticated } = useAuth();
  const { toast } = useToast();
  
  const [isOpen, setIsOpen] = useState(false);
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [isCapturing, setIsCapturing] = useState(false);
  
  // Form State
  const [title, setTitle] = useState('');
  const [description, setDescription] = useState('');
  const [steps, setSteps] = useState('');
  const [severity, setSeverity] = useState('minor');
  const [screenshot, setScreenshot] = useState<string | null>(null);

  const handleCaptureScreen = async () => {
    setIsCapturing(true);
    try {
      // Hide the modal temporarily so it's not in the screenshot
      setIsOpen(false);
      
      // Wait a tiny bit for React to unmount the modal
      await new Promise(resolve => setTimeout(resolve, 100));
      
      const canvas = await html2canvas(document.body, {
        useCORS: true,
        logging: false,
        ignoreElements: (element) => element.id === 'support-widget-container'
      });
      
      const image = canvas.toDataURL('image/png');
      setScreenshot(image);
      
    } catch (error) {
      toast({
        title: 'Screen Capture Failed',
        description: 'We could not capture your screen. You can still submit the bug without it.',
        variant: 'destructive',
      });
    } finally {
      setIsCapturing(false);
      setIsOpen(true);
    }
  };

  const getBrowserInfo = () => {
    if (typeof window === 'undefined') return {};
    return {
      userAgent: window.navigator.userAgent,
      language: window.navigator.language,
      platform: window.navigator.platform,
      screenResolution: `${window.screen.width}x${window.screen.height}`,
      viewport: `${window.innerWidth}x${window.innerHeight}`,
    };
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!title || !description) return;
    
    setIsSubmitting(true);
    
    // In a real scenario you would upload the base64 screenshot to S3/Cloudinary first
    // Since our backend expects an ImageField, we'd normally send formData.
    // Assuming backend is adjusted or we are just storing base64 temporarily:

    try {
      const accessToken = typeof window !== 'undefined' ? localStorage.getItem('access_token') : '';
      const response = await fetch('/api/support/bugs/', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${accessToken}`
        },
        body: JSON.stringify({
          title,
          description,
          steps_to_reproduce: steps,
          severity,
          status: 'new',
          page_url: window.location.href,
          browser_info: getBrowserInfo(),
          screenshot_base64: screenshot // Only if backend accepts it this way
        })
      });

      if (!response.ok) throw new Error('Failed to submit bug report');
      
      toast({
        title: 'Bug Reported',
        description: 'Thank you for your report! Our engineering team will review it.',
      });
      
      setIsOpen(false);
      // Reset form
      setTitle('');
      setDescription('');
      setSteps('');
      setScreenshot(null);
      
    } catch (error) {
      toast({
        title: 'Error',
        description: 'Failed to submit the report. Please try again.',
        variant: 'destructive'
      });
    } finally {
      setIsSubmitting(false);
    }
  };

  if (!isAuthenticated) return null;

  return (
    <>
      <Button 
        variant="outline" 
        size="sm" 
        className="fixed bottom-6 left-6 z-40 bg-background/80 backdrop-blur-sm border-dashed"
        onClick={() => setIsOpen(true)}
      >
        <Bug className="h-4 w-4 mr-2 text-primary" />
        Report Issue
      </Button>

      {isOpen && (
        <div className="fixed inset-0 z-50 bg-background/80 backdrop-blur-sm flex items-center justify-center p-4">
          <div className="bg-card w-full max-w-lg rounded-xl shadow-2xl border overflow-hidden flex flex-col max-h-[90vh]">
            <div className="p-4 border-b flex justify-between items-center bg-muted/30">
              <div className="flex items-center gap-2">
                <Bug className="h-5 w-5 text-primary" />
                <h2 className="font-semibold text-lg">Report a Bug</h2>
              </div>
              <Button variant="ghost" size="icon" onClick={() => setIsOpen(false)} className="h-8 w-8 rounded-full">
                <X className="h-4 w-4" />
              </Button>
            </div>

            <div className="p-6 overflow-y-auto flex-1">
              <div className="bg-yellow-100/50 text-yellow-800 text-sm p-3 rounded-lg mb-6 flex gap-2">
                <AlertCircle className="h-4 w-4 shrink-0 mt-0.5" />
                <p>Please provide as much detail as possible. Technical context like your browser version and current URL will be included automatically.</p>
              </div>

              <form id="bug-report-form" onSubmit={handleSubmit} className="space-y-4">
                <div className="grid grid-cols-2 gap-4">
                  <div className="col-span-2">
                    <label className="text-xs font-medium mb-1 inline-block">Issue Title</label>
                    <Input 
                      placeholder="e.g. Broken checkout button" 
                      value={title}
                      onChange={(e) => setTitle(e.target.value)}
                      required
                    />
                  </div>
                  
                  <div className="col-span-2">
                    <label className="text-xs font-medium mb-1 inline-block">Severity</label>
                    <Select value={severity} onValueChange={setSeverity}>
                      <SelectTrigger>
                        <SelectValue />
                      </SelectTrigger>
                      <SelectContent>
                        <SelectItem value="critical">Critical (Crash/Data Loss)</SelectItem>
                        <SelectItem value="major">Major (Feature Broken)</SelectItem>
                        <SelectItem value="minor">Minor (Workaround Exists)</SelectItem>
                        <SelectItem value="cosmetic">Cosmetic (UI Glitch)</SelectItem>
                      </SelectContent>
                    </Select>
                  </div>

                  <div className="col-span-2">
                    <label className="text-xs font-medium mb-1 inline-block">Description</label>
                    <textarea 
                      className="flex min-h-[80px] w-full rounded-md border border-input bg-transparent px-3 py-2 text-sm shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
                      placeholder="What went wrong?"
                      value={description}
                      onChange={(e) => setDescription(e.target.value)}
                      required
                    />
                  </div>

                  <div className="col-span-2">
                    <label className="text-xs font-medium mb-1 inline-block">Steps to Reproduce (Optional)</label>
                    <textarea 
                      className="flex min-h-[80px] w-full rounded-md border border-input bg-transparent px-3 py-2 text-sm shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
                      placeholder="1. Go to... 2. Click..."
                      value={steps}
                      onChange={(e) => setSteps(e.target.value)}
                    />
                  </div>

                  <div className="col-span-2 mt-2">
                    <p className="text-xs font-medium mb-2">Screenshot Evidence</p>
                    {screenshot ? (
                      <div className="relative border rounded-lg overflow-hidden group">
                        <img src={screenshot} alt="Captured screen" className="w-full object-cover max-h-[150px] opacity-80" />
                        <div className="absolute inset-0 bg-black/40 flex items-center justify-center opacity-0 group-hover:opacity-100 transition-opacity">
                          <Button 
                            variant="outline" 
                            className="bg-red-500 text-white hover:bg-red-600 border-red-600"
                            size="sm" 
                            onClick={() => setScreenshot(null)}
                          >
                            Remove
                          </Button>
                        </div>
                      </div>
                    ) : (
                      <Button 
                        type="button" 
                        variant="outline" 
                        className="w-full border-dashed py-8 bg-muted/20 hover:bg-muted/50 transition-colors"
                        onClick={handleCaptureScreen}
                        disabled={isCapturing}
                      >
                        {isCapturing ? (
                          <span className="flex items-center"><span className="animate-spin w-4 h-4 border-2 border-primary border-t-transparent rounded-full mr-2"></span> Capturing...</span>
                        ) : (
                          <span className="flex items-center text-muted-foreground"><Camera className="h-5 w-5 mr-2" /> Capture Current Screen</span>
                        )}
                      </Button>
                    )}
                  </div>
                </div>
              </form>
            </div>
            
            <div className="p-4 border-t bg-muted/10 flex justify-end gap-3">
              <Button type="button" variant="ghost" onClick={() => setIsOpen(false)}>
                Cancel
              </Button>
              <Button type="submit" form="bug-report-form" disabled={isSubmitting || !title || !description}>
                {isSubmitting ? 'Submitting...' : 'Submit Report'}
              </Button>
            </div>
          </div>
        </div>
      )}
    </>
  );
};
