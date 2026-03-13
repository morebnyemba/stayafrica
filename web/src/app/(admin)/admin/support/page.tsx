"use client";

import { useState, useEffect } from 'react';
import { useAuth } from '@/store/auth-store';
import { useRouter } from 'next/navigation';
import { Card, CardBody, Button, Badge, Select, SelectContent, SelectItem, SelectTrigger, SelectValue, Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from '@/components/ui';
import { SupportTicket } from '@/types/support-types';
import toast from 'react-hot-toast';
import { formatDistanceToNow } from 'date-fns';

export default function AdminSupportDashboard() {
  const { isAuthenticated } = useAuth();
  const router = useRouter();
  
  const [tickets, setTickets] = useState<SupportTicket[]>([]);
  const [isLoading, setIsLoading] = useState(true);
  const [filterStatus, setFilterStatus] = useState<string>('all');
  const [filterAssignee, setFilterAssignee] = useState<string>('all');
  
  // Quick fetch
  const fetchTickets = async () => {
    if (typeof window === 'undefined') return;
    const accessToken = localStorage.getItem('access_token');
    if (!accessToken) return;
    
    setIsLoading(true);
    try {
      let url = '/api/support/tickets/';
      const params = new URLSearchParams();
      if (filterStatus !== 'all') params.append('status', filterStatus);
      if (filterAssignee !== 'all') params.append('assigned_to', filterAssignee);
      
      const queryString = params.toString();
      if (queryString) url += `?${queryString}`;

      const res = await fetch(url, {
        headers: {
          'Authorization': `Bearer ${accessToken}`
        }
      });
      
      if (!res.ok) throw new Error('Failed to fetch tickets');
      const data = await res.json();
      setTickets(Array.isArray(data) ? data : (data.results ?? []));
    } catch (error) {
      toast.error('Could not load tickets');
    } finally {
      setIsLoading(false);
    }
  };

  useEffect(() => {
    fetchTickets();
  }, [isAuthenticated, filterStatus, filterAssignee]);

  const handleAssignToMe = async (ticketId: number) => {
    try {
      const res = await fetch(`/api/support/tickets/${ticketId}/assign/`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${typeof window !== 'undefined' ? localStorage.getItem('access_token') : ''}`
        },
        body: JSON.stringify({ user_id: 'me' })
      });
      
      if (!res.ok) throw new Error('Assignment failed');
      toast.success('Ticket assigned to you.');
      fetchTickets();
    } catch (error) {
      toast.error('Could not assign ticket.');
    }
  };

  const statusColors: Record<string, string> = {
    open: 'bg-green-100 text-green-800',
    assigned: 'bg-blue-100 text-blue-800',
    in_progress: 'bg-yellow-100 text-yellow-800',
    waiting_customer: 'bg-orange-100 text-orange-800',
    resolved: 'bg-gray-100 text-gray-800',
    closed: 'bg-gray-300 text-gray-900',
  };

  const priorityColors: Record<string, string> = {
    low: 'text-gray-500',
    medium: 'text-blue-500',
    high: 'text-orange-500 font-bold',
    urgent: 'text-red-600 font-bold animate-pulse',
  };

  return (
    <div className="p-6 space-y-6 max-w-7xl mx-auto">
      <div className="flex items-center justify-between">
        <h1 className="text-3xl font-bold tracking-tight">Support Desk</h1>
        
        <div className="flex items-center gap-4">
          <Select value={filterAssignee} onValueChange={setFilterAssignee}>
            <SelectTrigger className="w-[180px]">
              <SelectValue placeholder="Assignee" />
            </SelectTrigger>
            <SelectContent>
              <SelectItem value="all">Every Agent</SelectItem>
              <SelectItem value="me">Assigned to Me</SelectItem>
              <SelectItem value="unassigned">Unassigned</SelectItem>
            </SelectContent>
          </Select>

          <Select value={filterStatus} onValueChange={setFilterStatus}>
            <SelectTrigger className="w-[180px]">
              <SelectValue placeholder="Status" />
            </SelectTrigger>
            <SelectContent>
              <SelectItem value="all">All Statuses</SelectItem>
              <SelectItem value="open">Open</SelectItem>
              <SelectItem value="assigned">Assigned</SelectItem>
              <SelectItem value="in_progress">In Progress</SelectItem>
              <SelectItem value="resolved">Resolved</SelectItem>
            </SelectContent>
          </Select>
          
          <Button onClick={fetchTickets} variant="outline">Refresh</Button>
        </div>
      </div>

      <Card>
        <CardBody className="p-0">
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead>ID</TableHead>
                <TableHead>Subject</TableHead>
                <TableHead>Requester</TableHead>
                <TableHead>Priority</TableHead>
                <TableHead>Status</TableHead>
                <TableHead>Age</TableHead>
                <TableHead>Agent</TableHead>
                <TableHead className="text-right">Actions</TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {isLoading ? (
                <TableRow>
                  <TableCell colSpan={8} className="text-center h-32">Loading tickets...</TableCell>
                </TableRow>
              ) : tickets.length === 0 ? (
                <TableRow>
                  <TableCell colSpan={8} className="text-center h-32 text-muted-foreground">
                    No tickets found matching your filters.
                  </TableCell>
                </TableRow>
              ) : (
                tickets.map((ticket) => (
                  <TableRow key={ticket.id}>
                    <TableCell className="font-medium">#{ticket.id}</TableCell>
                    <TableCell className="max-w-[300px] truncate">{ticket.subject}</TableCell>
                    <TableCell>{ticket.requester_name}</TableCell>
                    <TableCell className={priorityColors[ticket.priority]}>
                      {ticket.priority.charAt(0).toUpperCase() + ticket.priority.slice(1)}
                    </TableCell>
                    <TableCell>
                      <Badge className={statusColors[ticket.status]} variant="secondary">
                        {ticket.status.replace('_', ' ').toUpperCase()}
                      </Badge>
                    </TableCell>
                    <TableCell className="text-muted-foreground">
                      {formatDistanceToNow(new Date(ticket.created_at), { addSuffix: true })}
                    </TableCell>
                    <TableCell>
                      {ticket.assigned_agent_name || <span className="text-muted-foreground italic">Unassigned</span>}
                    </TableCell>
                    <TableCell className="text-right space-x-2">
                      {!ticket.assigned_agent && (
                        <Button variant="secondary" size="sm" onClick={() => handleAssignToMe(ticket.id)}>
                          Claim
                        </Button>
                      )}
                      <Button variant="outline" size="sm" onClick={() => router.push(`/admin/support/${ticket.id}`)}>  
                        View
                      </Button>
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
