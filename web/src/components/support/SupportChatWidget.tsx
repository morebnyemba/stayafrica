"use client";

import React, { useState, useEffect, useRef } from 'react';
import { useAuth } from '@/context/auth-context';
import { Button, Card, CardHeader, Input, Select, SelectContent, SelectItem, SelectTrigger, SelectValue, ScrollArea, CardBody, CardFooter } from '@/components/ui';
import { MessageCircle, X, AlertCircle, Send } from 'lucide-react';
import toast from 'react-hot-toast';

interface SupportMessage {
  id: string;
  text: string;
  sender_id: number;
  sender_name: string;
  created_at: string;
  isOwn: boolean;
}

export const SupportChatWidget = () => {
  const { user, isAuthenticated, isLoading } = useAuth();

  const [isOpen, setIsOpen] = useState(false);
  const [step, setStep] = useState<'form' | 'chat'>('form');

  // Form State
  const [category, setCategory] = useState('');
  const [subject, setSubject] = useState('');
  const [initialMessage, setInitialMessage] = useState('');
  const [isSubmitting, setIsSubmitting] = useState(false);

  // Chat State
  const [ticketId, setTicketId] = useState<number | null>(null);
  const [conversationId, setConversationId] = useState<number | null>(null);
  const [messages, setMessages] = useState<SupportMessage[]>([]);
  const [chatInput, setChatInput] = useState('');
  const [ws, setWs] = useState<WebSocket | null>(null);

  const messagesEndRef = useRef<HTMLDivElement>(null);

  // Auto-scroll chat
  useEffect(() => {
    messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' });
  }, [messages]);

  // WebSocket connection when ticket is active
  useEffect(() => {
    if (step === 'chat' && conversationId && isAuthenticated && typeof window !== 'undefined') {
      const accessToken = localStorage.getItem('access_token');
      if (!accessToken) return;

      const socket = new WebSocket(
        `${process.env.NEXT_PUBLIC_WS_URL}/ws/chat/?token=${accessToken}`
      );

      socket.onopen = () => {
        socket.send(JSON.stringify({
          type: 'join_conversation',
          conversation_id: conversationId
        }));
      };

      socket.onmessage = (event) => {
        const data = JSON.parse(event.data);
        if (data.type === 'new_message' && data.conversation_id === conversationId) {
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

      return () => {
        socket.close();
      };
    }
  }, [step, conversationId, isAuthenticated]);

  const handleSubmitTicket = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!category || !subject || !initialMessage) return;

    setIsSubmitting(true);
    try {
      const accessToken = typeof window !== 'undefined' ? localStorage.getItem('access_token') : '';
      const response = await fetch('/api/support/tickets/', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${accessToken}`
        },
        body: JSON.stringify({
          category,
          subject,
          initial_message: initialMessage,
          priority: 'medium'
        })
      });

      if (!response.ok) throw new Error('Failed to create ticket');

      const data = await response.json();
      setTicketId(data.id);
      setConversationId(data.conversation);

      // Add local initial message
      setMessages([{
        id: 'local-' + Date.now(),
        text: initialMessage,
        sender_id: user?.id as unknown as number,
        sender_name: user?.first_name ? `${user.first_name} ${user.last_name}` : 'You',
        created_at: new Date().toISOString(),
        isOwn: true
      }]);

      setStep('chat');
    } catch (error) {
      toast.error('Failed to create support ticket. Please try again.');
    } finally {
      setIsSubmitting(false);
    }
  };

  const handleSendMessage = (e: React.FormEvent) => {
    e.preventDefault();
    if (!chatInput.trim() || !ws || ws.readyState !== WebSocket.OPEN) return;

    ws.send(JSON.stringify({
      type: 'chat_message',
      conversation_id: conversationId,
      receiver_id: null,
      text: chatInput
    }));

    setChatInput('');
  };

  // Don't render anything while auth is loading or if not logged in
  if (isLoading || !isAuthenticated) return null;

  return (
    <div className="fixed bottom-6 right-6 z-50" id="support-widget-container">
      {/* Floating Button */}
      {!isOpen && (
        <Button
          onClick={() => setIsOpen(true)}
          className="h-14 w-14 rounded-full shadow-xl bg-primary hover:bg-primary/90 flex items-center justify-center transition-transform hover:scale-105"
        >
          <MessageCircle className="h-6 w-6 text-white" />
        </Button>
      )}

      {/* Chat Widget Panel */}
      {isOpen && (
        <Card className="w-[350px] shadow-2xl border flex flex-col transition-all duration-300 transform scale-100 origin-bottom-right h-[500px]">
          <CardHeader className="bg-primary text-primary-foreground p-4 flex flex-row items-center justify-between space-y-0 rounded-t-xl">
            <h3 className="text-md font-semibold flex items-center gap-2">
              <MessageCircle className="h-5 w-5" />
              StayAfrica Support
            </h3>
            <Button
              variant="ghost"
              size="icon"
              className="h-8 w-8 text-white hover:bg-primary/80 rounded-full"
              onClick={() => setIsOpen(false)}
            >
              <X className="h-5 w-5" />
            </Button>
          </CardHeader>

          {step === 'form' ? (
            <CardBody className="p-4 flex-1 overflow-y-auto">
              <div className="mb-4 text-sm text-muted-foreground bg-muted/50 p-3 rounded-lg flex items-start gap-2">
                <AlertCircle className="h-4 w-4 text-primary mt-0.5 shrink-0" />
                <p>How can we help you today? Please provide some details so we can route you to the right agent.</p>
              </div>

              <form onSubmit={handleSubmitTicket} className="space-y-4">
                <div className="space-y-1">
                  <label className="text-xs font-medium">Issue Category</label>
                  <Select onValueChange={setCategory} required>
                    <SelectTrigger>
                      <SelectValue placeholder="Select a category" />
                    </SelectTrigger>
                    <SelectContent>
                      <SelectItem value="booking">Booking &amp; Reservations</SelectItem>
                      <SelectItem value="payment">Payments &amp; Refunds</SelectItem>
                      <SelectItem value="property">Property Issue</SelectItem>
                      <SelectItem value="account">Account &amp; Profile</SelectItem>
                      <SelectItem value="technical">Technical Bug</SelectItem>
                      <SelectItem value="other">Other</SelectItem>
                    </SelectContent>
                  </Select>
                </div>

                <div className="space-y-1">
                  <label className="text-xs font-medium">Subject</label>
                  <Input
                    placeholder="Brief description"
                    value={subject}
                    onChange={(e) => setSubject(e.target.value)}
                    required
                  />
                </div>

                <div className="space-y-1">
                  <label className="text-xs font-medium">Message</label>
                  <textarea
                    className="flex min-h-[100px] w-full rounded-md border border-input bg-transparent px-3 py-2 text-sm shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50"
                    placeholder="Describe your issue in detail..."
                    value={initialMessage}
                    onChange={(e) => setInitialMessage(e.target.value)}
                    required
                  />
                </div>

                <Button type="submit" className="w-full" disabled={isSubmitting || !category || !subject || !initialMessage}>
                  {isSubmitting ? 'Submitting...' : 'Start Chat'}
                </Button>
              </form>
            </CardBody>
          ) : (
            <>
              {/* Active Chat View */}
              <ScrollArea className="flex-1 p-4 bg-muted/10">
                <div className="space-y-4">
                  <div className="text-center text-xs text-muted-foreground">
                    Support Ticket #{ticketId} Created
                  </div>

                  {messages.map((msg, idx) => (
                    <div
                      key={msg.id || idx}
                      className={`flex flex-col max-w-[85%] ${msg.isOwn ? 'ml-auto' : 'mr-auto'}`}
                    >
                      {!msg.isOwn && (
                        <span className="text-[10px] text-muted-foreground ml-1 mb-1">
                          {msg.sender_name} (Support)
                        </span>
                      )}
                      <div
                        className={`p-3 rounded-2xl text-sm ${
                          msg.isOwn
                            ? 'bg-primary text-primary-foreground rounded-br-sm'
                            : 'bg-muted rounded-bl-sm'
                        }`}
                      >
                        {msg.text}
                      </div>
                      <span className={`text-[10px] text-muted-foreground mt-1 ${msg.isOwn ? 'text-right mr-1' : 'ml-1'}`}>
                        {new Date(msg.created_at).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })}
                      </span>
                    </div>
                  ))}
                  <div ref={messagesEndRef} />
                </div>
              </ScrollArea>

              <CardFooter className="p-3 border-t bg-background">
                <form onSubmit={handleSendMessage} className="flex w-full items-center gap-2">
                  <Input
                    placeholder="Type a message..."
                    className="flex-1 rounded-full bg-muted/50 border-transparent focus-visible:ring-primary/20"
                    value={chatInput}
                    onChange={(e) => setChatInput(e.target.value)}
                  />
                  <Button type="submit" size="icon" className="rounded-full shrink-0" disabled={!chatInput.trim()}>
                    <Send className="h-4 w-4" />
                    <span className="sr-only">Send</span>
                  </Button>
                </form>
              </CardFooter>
            </>
          )}
        </Card>
      )}
    </div>
  );
};
