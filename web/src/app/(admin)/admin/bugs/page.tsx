"use client";

import { useState, useEffect } from 'react';
import { useAuth } from '@/store/auth-store';
import { useRouter } from 'next/navigation';
import { Card, CardBody, Button, Badge, Select, SelectContent, SelectItem, SelectTrigger, SelectValue, Table, TableBody, TableCell, TableHead, TableHeader, TableRow, Dialog, DialogContent, DialogHeader, DialogTitle, DialogTrigger } from '@/components/ui';
import { BugReport } from '@/types/support-types';
import { AlertCircle, ExternalLink, ImageIcon } from 'lucide-react';
import toast from 'react-hot-toast';
import { formatDistanceToNow } from 'date-fns';

export default function AdminBugDashboard() {
  const { isAuthenticated } = useAuth();
  const router = useRouter();
  
  const [bugs, setBugs] = useState<BugReport[]>([]);
  const [isLoading, setIsLoading] = useState(true);
  const [filterStatus, setFilterStatus] = useState<string>('all');
  
  const fetchBugs = async () => {
    if (typeof window === 'undefined') return;
    const accessToken = localStorage.getItem('access_token');
    if (!accessToken) return;
    
    setIsLoading(true);
    try {
      let url = '/api/support/bugs/';
      if (filterStatus !== 'all') {
        url += `?status=${filterStatus}`;
      }

      const res = await fetch(url, {
        headers: {
          'Authorization': `Bearer ${accessToken}`
        }
      });
      
      if (!res.ok) throw new Error('Failed to fetch bugs');
      const data = await res.json();
      setBugs(data);
    } catch (error) {
      toast.error('Could not load bug reports');
    } finally {
      setIsLoading(false);
    }
  };

  useEffect(() => {
    fetchBugs();
  }, [isAuthenticated, filterStatus]);

  const handleEscalateToTicket = async (bugId: number) => {
    try {
      const res = await fetch(`/api/support/bugs/${bugId}/escalate/`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${typeof window !== 'undefined' ? localStorage.getItem('access_token') : ''}`
        }
      });
      
      if (!res.ok) throw new Error('Escalation failed');
      const data = await res.json();
      toast.success(`Bug escalated to Support Ticket #${data.ticket_id}`);
      fetchBugs();
    } catch (error) {
      toast.error('Could not escalate bug.');
    }
  };

  const severityColors: Record<string, string> = {
    cosmetic: 'text-gray-500',
    minor: 'text-blue-500',
    major: 'text-orange-500 font-bold',
    critical: 'text-red-600 font-bold',
  };

  const statusColors: Record<string, string> = {
    new: 'bg-red-100 text-red-800',
    triaged: 'bg-blue-100 text-blue-800',
    in_progress: 'bg-yellow-100 text-yellow-800',
    fixed: 'bg-green-100 text-green-800',
    wont_fix: 'bg-gray-100 text-gray-800',
    duplicate: 'bg-gray-300 text-gray-900',
  };

  return (
    <div className="p-6 space-y-6 max-w-7xl mx-auto">
      <div className="flex items-center justify-between">
        <h1 className="text-3xl font-bold tracking-tight flex items-center gap-2">
          <AlertCircle className="h-8 w-8 text-primary" />
          Bug Triage
        </h1>
        
        <div className="flex items-center gap-4">
          <Select value={filterStatus} onValueChange={setFilterStatus}>
            <SelectTrigger className="w-[180px]">
              <SelectValue placeholder="Status" />
            </SelectTrigger>
            <SelectContent>
              <SelectItem value="all">All Statuses</SelectItem>
              <SelectItem value="new">New</SelectItem>
              <SelectItem value="triaged">Triaged</SelectItem>
              <SelectItem value="in_progress">In Progress</SelectItem>
              <SelectItem value="fixed">Fixed</SelectItem>
            </SelectContent>
          </Select>
          <Button onClick={fetchBugs} variant="outline">Refresh</Button>
        </div>
      </div>

      <Card>
        <CardBody className="p-0">
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead>ID</TableHead>
                <TableHead>Title</TableHead>
                <TableHead>Reporter</TableHead>
                <TableHead>Severity</TableHead>
                <TableHead>Status</TableHead>
                <TableHead>Created</TableHead>
                <TableHead className="text-right">Actions</TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {isLoading ? (
                <TableRow>
                  <TableCell colSpan={7} className="text-center h-32">Loading bugs...</TableCell>
                </TableRow>
              ) : bugs.length === 0 ? (
                <TableRow>
                  <TableCell colSpan={7} className="text-center h-32 text-muted-foreground">
                    No bugs found matching your filters.
                  </TableCell>
                </TableRow>
              ) : (
                bugs.map((bug) => (
                  <TableRow key={bug.id}>
                    <TableCell className="font-medium">#{bug.id}</TableCell>
                    <TableCell className="max-w-[300px] truncate">{bug.title}</TableCell>
                    <TableCell>{bug.reporter_name}</TableCell>
                    <TableCell className={severityColors[bug.severity]}>
                      {bug.severity.charAt(0).toUpperCase() + bug.severity.slice(1)}
                    </TableCell>
                    <TableCell>
                      <Badge className={statusColors[bug.status]} variant="secondary">
                        {bug.status.replace('_', ' ').toUpperCase()}
                      </Badge>
                    </TableCell>
                    <TableCell className="text-muted-foreground text-sm">
                      {formatDistanceToNow(new Date(bug.created_at), { addSuffix: true })}
                    </TableCell>
                    <TableCell className="text-right space-x-2">
                      <Dialog>
                        <DialogTrigger asChild>
                          <Button variant="outline" size="sm">View Details</Button>
                        </DialogTrigger>
                        <DialogContent className="max-w-2xl max-h-[80vh] overflow-y-auto">
                          <DialogHeader>
                            <DialogTitle className="flex justify-between items-center pr-6">
                              <span>Bug #{bug.id}: {bug.title}</span>
                              <Badge className={severityColors[bug.severity]} variant="secondary">
                                {bug.severity.toUpperCase()}
                              </Badge>
                            </DialogTitle>
                          </DialogHeader>
                          
                          <div className="space-y-4 pt-4">
                            <div>
                              <h4 className="text-sm font-semibold mb-1">Description</h4>
                              <p className="text-sm bg-muted/30 p-3 rounded-md">{bug.description}</p>
                            </div>
                            
                            {bug.steps_to_reproduce && (
                              <div>
                                <h4 className="text-sm font-semibold mb-1">Steps to Reproduce</h4>
                                <p className="text-sm bg-muted/30 p-3 rounded-md whitespace-pre-wrap">{bug.steps_to_reproduce}</p>
                              </div>
                            )}

                            <div className="grid grid-cols-2 gap-4">
                              <div>
                                <h4 className="text-sm font-semibold mb-1 flex items-center gap-1">
                                  <ExternalLink className="h-3 w-3" /> URL
                                </h4>
                                <a href={bug.page_url} target="_blank" rel="noreferrer" className="text-xs text-blue-600 hover:underline break-all">
                                  {bug.page_url}
                                </a>
                              </div>
                              <div>
                                <h4 className="text-sm font-semibold mb-1">Browser</h4>
                                <p className="text-xs text-muted-foreground truncate" title={bug.browser_info.userAgent}>
                                  {bug.browser_info.userAgent || 'Unknown'}
                                </p>
                              </div>
                            </div>

                            {bug.screenshot && (
                              <div>
                                <h4 className="text-sm font-semibold mb-1 flex items-center gap-1">
                                  <ImageIcon className="h-4 w-4" /> Attached Screenshot
                                </h4>
                                <div className="border rounded-md overflow-hidden">
                                  <img src={bug.screenshot} alt="Bug screenshot" className="w-full h-auto" />
                                </div>
                              </div>
                            )}

                            {!bug.support_ticket && (
                              <div className="pt-4 flex justify-end">
                                <Button onClick={() => handleEscalateToTicket(bug.id)}>
                                  Escalate to Support Ticket
                                </Button>
                              </div>
                            )}
                            
                            {bug.support_ticket && (
                              <div className="pt-4 flex justify-end">
                                <Button variant="outline" onClick={() => router.push(`/admin/support/${bug.support_ticket}`)}>  
                                  View Associated Ticket
                                </Button>
                              </div>
                            )}
                          </div>
                        </DialogContent>
                      </Dialog>
                    </TableCell>
                  </TableRow>
                ))
              )}
            </TableBody>
          </Table>
        </CardBody>
      </Card>
    </div>
  );
}
