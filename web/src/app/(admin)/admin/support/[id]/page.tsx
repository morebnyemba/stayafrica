"use client";

import React, { useState, useEffect, useRef } from 'react';
import { useParams } from 'next/navigation';
import { useAuth } from '@/store/auth-store';
import { SupportTicket } from '@/types/support-types';
import { Card, CardHeader, CardBody, CardFooter, Badge, Button, Input, ScrollArea, Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui';
import toast from 'react-hot-toast';
import { Send } from 'lucide-react';
import { formatDistanceToNow } from 'date-fns';

interface SupportMessage {
  id: string;
  text: string;
  sender_id: number;
  sender_name: string;
  created_at: string;
  isOwn: boolean;
}

export default function TicketDetailView() {
  const { id } = useParams();
  const { user, isAuthenticated } = useAuth();
  
  const [ticket, setTicket] = useState<SupportTicket | null>(null);
  const [messages, setMessages] = useState<SupportMessage[]>([]);
  const [chatInput, setChatInput] = useState('');
  const [ws, setWs] = useState<WebSocket | null>(null);
  
  const messagesEndRef = useRef<HTMLDivElement>(null);

  // Auto-scroll chat
  useEffect(() => {
    messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' });
  }, [messages]);

  const fetchTicketDetails = async () => {
    if (typeof window === 'undefined') return;
    const accessToken = localStorage.getItem('access_token');
    if (!accessToken) return;
    
    try {
      const apiBase = process.env.NEXT_PUBLIC_API_BASE_URL || 
        (typeof window !== 'undefined' ? `${window.location.protocol}//${window.location.host}` : '');
      const baseUrl = apiBase.replace(/\/api\/v1\/?$/, '') + '/api/v1';

      const res = await fetch(`${baseUrl}/support/tickets/${id}/`, {
        headers: { 'Authorization': `Bearer ${accessToken}` }
      });
      if (!res.ok) throw new Error('Failed to fetch ticket');
      const data = await res.json();
      setTicket(data);
      
      // Fetch existing messages if there's a conversation
      if (data.conversation) {
        const msgRes = await fetch(`${baseUrl}/messaging/conversations/${data.conversation}/`, {
          headers: { 'Authorization': `Bearer ${accessToken}` }
        });
        if (msgRes.ok) {
          const msgData = await msgRes.json();
          const history = (msgData.messages || []).map((m: any) => ({
            id: m.id,
            text: m.text,
            sender_id: m.sender.id,
            sender_name: m.sender.name,
            created_at: m.created_at,
            isOwn: m.is_own_message
          }));
          setMessages(history.reverse()); // Reverse to show oldest first if API returns newest first
        }
      }
    } catch (error) {
      toast.error('Could not load ticket details or messages');
    }
  };

  useEffect(() => {
    fetchTicketDetails();
  }, [isAuthenticated, id]);

  // WebSocket connection
  useEffect(() => {
    if (ticket?.conversation && isAuthenticated && typeof window !== 'undefined') {
      const accessToken = localStorage.getItem('access_token');
      if (!accessToken) return;

      const apiBase = process.env.NEXT_PUBLIC_API_BASE_URL ||
        `${window.location.protocol}//${window.location.host}`;
      const wsBase = apiBase.replace(/^http/, 'ws');
      const socket = new WebSocket(`${wsBase}/ws/support/?token=${accessToken}`);


      socket.onopen = () => {
        // Technically this is connected to the SupportConsumer which auto-listens
        // to group support_agents, but let's join the specific room too.
        socket.send(JSON.stringify({
          type: 'join_conversation',
          conversation_id: ticket.conversation
        }));
      };

      socket.onmessage = (event) => {
        const data = JSON.parse(event.data);
        if (data.type === 'new_message' && data.conversation_id === ticket.conversation && data.sender_id !== user?.id) {
          setMessages(prev => [...prev, {
            id: data.message_id,
            text: data.text,
            sender_id: data.sender_id,
            sender_name: data.sender_name,
            created_at: data.created_at,
            isOwn: false
          }]);
        }
      };

      setWs(socket);
      return () => socket.close();
    }
  }, [ticket, isAuthenticated]);

  const handleSendMessage = (e: React.FormEvent) => {
    e.preventDefault();
    if (!chatInput.trim() || !ws || ws.readyState !== WebSocket.OPEN) return;

    ws.send(JSON.stringify({
      type: 'chat_message',
      conversation_id: ticket?.conversation,
      text: chatInput
    }));

    // Optimistically add the agent's own message to the UI
    setMessages(prev => [...prev, {
      id: 'local-' + Date.now(),
      text: chatInput,
      sender_id: user?.id as unknown as number,
      sender_name: user?.first_name ? `${user.first_name} ${user.last_name}` : 'Agent',
      created_at: new Date().toISOString(),
      isOwn: true
    }]);

    setChatInput('');
  };

  const handleChangeStatus = async (newStatus: string) => {
    try {
      const accessToken = typeof window !== 'undefined' ? localStorage.getItem('access_token') : '';
      const res = await fetch(`/api/support/tickets/${ticket?.id}/change_status/`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${accessToken}`
        },
        body: JSON.stringify({ status: newStatus })
      });
      if (!res.ok) throw new Error('Failed to change status');
      toast.success('Ticket status updated.');
      fetchTicketDetails();
    } catch (error) {
      toast.error('Status update failed.');
    }
  };

  if (!ticket) return <div className="p-8 text-center">Loading ticket {id}...</div>;

  return (
    <div className="p-6 max-w-6xl mx-auto grid grid-cols-1 md:grid-cols-3 gap-6">
      {/* Sidebar Details */}
      <div className="space-y-6 md:col-span-1">
        <Card>
          <CardHeader>
            <div className="text-lg flex items-center justify-between font-semibold">
              Ticket #{ticket.id}
              <Badge variant="secondary">{ticket.status.toUpperCase()}</Badge>
            </div>
          </CardHeader>
          <CardBody className="space-y-4 text-sm">
            <div>
              <p className="text-muted-foreground">Requester</p>
              <p className="font-medium">{ticket.requester_name}</p>
            </div>
            <div>
              <p className="text-muted-foreground">Category</p>
              <p className="font-medium capitalize">{ticket.category}</p>
            </div>
            <div>
              <p className="text-muted-foreground">Priority</p>
              <p className="font-medium capitalize">{ticket.priority}</p>
            </div>
            <div>
              <p className="text-muted-foreground">Assigned Agent</p>
              <p className="font-medium">{ticket.assigned_agent_name || 'Unassigned'}</p>
            </div>
            <div>
              <p className="text-muted-foreground">Created</p>
              <p className="font-medium">{formatDistanceToNow(new Date(ticket.created_at), { addSuffix: true })}</p>
            </div>
            
            <div className="pt-4 border-t">
              <label className="text-xs font-semibold uppercase text-muted-foreground mb-2 block">Change Status</label>
              <Select value={ticket.status} onValueChange={handleChangeStatus}>
                <SelectTrigger>
                  <SelectValue placeholder="Status" />
                </SelectTrigger>
                <SelectContent>
                  <SelectItem value="open">Open</SelectItem>
                  <SelectItem value="assigned">Assigned</SelectItem>
                  <SelectItem value="in_progress">In Progress</SelectItem>
                  <SelectItem value="waiting_customer">Waiting on Customer</SelectItem>
                  <SelectItem value="resolved">Resolved</SelectItem>
                  <SelectItem value="closed">Closed</SelectItem>
                </SelectContent>
              </Select>
            </div>
          </CardBody>
        </Card>
      </div>

      {/* Main Chat Area */}
      <Card className="md:col-span-2 flex flex-col h-[700px]">
        <CardHeader className="border-b bg-muted/20">
          <div className="text-xl font-semibold">{ticket.subject}</div>
          <p className="text-sm text-muted-foreground mt-0.5">
            Conversation with <span className="font-medium text-foreground">{ticket.requester_name}</span>
          </p>
        </CardHeader>
        
        <ScrollArea className="flex-1">
          <div className="p-4 space-y-3">
            {messages.length === 0 ? (
              <div className="text-center text-muted-foreground py-12">
                No messages yet. Start the conversation with the customer.
              </div>
            ) : (
              messages.map((msg, idx) => {
                const isCustomer = !msg.isOwn && Number(msg.sender_id) === Number(ticket.requester);
                const roleLabel = msg.isOwn ? 'You (Agent)' : isCustomer ? 'Customer' : 'Support Agent';
                const initial = (msg.sender_name || '?').charAt(0).toUpperCase();

                return (
                  <div
                    key={msg.id || idx}
                    className={`rounded-lg border-l-4 p-4 ${
                      msg.isOwn
                        ? 'bg-blue-50 border-l-blue-500'
                        : isCustomer
                          ? 'bg-amber-50 border-l-amber-500'
                          : 'bg-emerald-50 border-l-emerald-500'
                    }`}
                  >
                    {/* Header: avatar + name + role + time */}
                    <div className="flex items-center gap-2 mb-2">
                      <div
                        className={`h-7 w-7 rounded-full flex items-center justify-center text-xs font-bold shrink-0 ${
                          msg.isOwn
                            ? 'bg-blue-600 text-white'
                            : isCustomer
                              ? 'bg-amber-500 text-white'
                              : 'bg-emerald-600 text-white'
                        }`}
                      >
                        {initial}
                      </div>
                      <span className={`text-sm font-semibold ${
                        msg.isOwn
                          ? 'text-blue-900'
                          : isCustomer
                            ? 'text-amber-900'
                            : 'text-emerald-900'
                      }`}>
                        {msg.sender_name || 'Unknown'}
                      </span>
                      <span
                        className={`text-[11px] px-2 py-0.5 rounded-full font-semibold ${
                          msg.isOwn
                            ? 'bg-blue-200 text-blue-800'
                            : isCustomer
                              ? 'bg-amber-200 text-amber-800'
                              : 'bg-emerald-200 text-emerald-800'
                        }`}
                      >
                        {roleLabel}
                      </span>
                      <span className="text-xs text-muted-foreground ml-auto shrink-0">
                        {new Date(msg.created_at).toLocaleString([], {
                          month: 'short', day: 'numeric',
                          hour: '2-digit', minute: '2-digit'
                        })}
                      </span>
                    </div>
                    {/* Message body */}
                    <p className={`text-sm whitespace-pre-wrap leading-relaxed pl-9 ${
                      msg.isOwn
                        ? 'text-blue-900'
                        : isCustomer
                          ? 'text-amber-900'
                          : 'text-emerald-900'
                    }`}>
                      {msg.text}
                    </p>
                  </div>
                );
              })
            )}
            <div ref={messagesEndRef} />
          </div>
        </ScrollArea>
        
        <CardFooter className="p-4 border-t bg-background">
          <form onSubmit={handleSendMessage} className="flex w-full items-center gap-2">
            <Input 
              placeholder="Type your response to the user..." 
              className="flex-1"
              value={chatInput}
              onChange={(e) => setChatInput(e.target.value)}
              disabled={ticket.status === 'closed'}
            />
            <Button type="submit" disabled={!chatInput.trim() || ticket.status === 'closed'}>
              <Send className="h-4 w-4 mr-2" />
              Reply
            </Button>
          </form>
        </CardFooter>
      </Card>
    </div>
  );
}
