"use client";

import React, { useState, useEffect, useRef } from 'react';
import { useParams } from 'next/navigation';
import { useAuth } from '@/context/auth-context';
import { SupportTicket } from '@/types/support-types';
import { Card, CardHeader, CardBody, CardFooter, Badge, Button, Input, ScrollArea, Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui';
import { useToast } from '@/hooks/use-toast';
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
  const { toast } = useToast();
  
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
      const res = await fetch(`/api/support/tickets/${id}/`, {
        headers: { 'Authorization': `Bearer ${accessToken}` }
      });
      if (!res.ok) throw new Error('Failed to fetch ticket');
      const data = await res.json();
      setTicket(data);
    } catch (error) {
      toast({ title: 'Error', description: 'Could not load ticket details', variant: 'destructive' });
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

      const socket = new WebSocket(
        `${process.env.NEXT_PUBLIC_WS_URL}/ws/support/?token=${accessToken}`
      );

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
        if (data.type === 'new_message' && data.conversation_id === ticket.conversation) {
          setMessages(prev => [...prev, {
            id: data.message_id,
            text: data.text,
            sender_id: data.sender_id,
            sender_name: data.sender_name,
            created_at: data.created_at,
            isOwn: data.sender_id === user?.id
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
      receiver_id: ticket?.requester, // Send back to requester
      text: chatInput
    }));

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
      toast({ title: 'Success', description: 'Ticket status updated.' });
      fetchTicketDetails();
    } catch (error) {
      toast({ title: 'Error', description: 'Status update failed.', variant: 'destructive' });
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
        </CardHeader>
        
        <ScrollArea className="flex-1 p-4">
          <div className="space-y-4">
            {messages.length === 0 ? (
              <div className="text-center text-muted-foreground py-8">
                No messages loaded. Wait for WebSocket connection.
              </div>
            ) : (
              messages.map((msg, idx) => (
                <div 
                  key={msg.id || idx} 
                  className={`flex flex-col max-w-[80%] ${msg.isOwn ? 'ml-auto' : 'mr-auto'}`}
                >
                  {!msg.isOwn && (
                    <span className="text-xs text-muted-foreground ml-1 mb-1">
                      {msg.sender_name}
                    </span>
                  )}
                  <div 
                    className={`p-3 rounded-lg text-sm ${
                      msg.isOwn 
                        ? 'bg-primary text-primary-foreground' 
                        : 'bg-muted'
                    }`}
                  >
                    {msg.text}
                  </div>
                </div>
              ))
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
