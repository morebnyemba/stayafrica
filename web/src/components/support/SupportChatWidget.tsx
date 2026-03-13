"use client";

import { useState, useEffect, useRef } from 'react';
import { useAuth } from '@/context/auth-context';
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

  const [category, setCategory] = useState('');
  const [subject, setSubject] = useState('');
  const [initialMessage, setInitialMessage] = useState('');
  const [isSubmitting, setIsSubmitting] = useState(false);

  const [ticketId, setTicketId] = useState<number | null>(null);
  const [conversationId, setConversationId] = useState<number | null>(null);
  const [messages, setMessages] = useState<SupportMessage[]>([]);
  const [chatInput, setChatInput] = useState('');
  const [ws, setWs] = useState<WebSocket | null>(null);

  const messagesEndRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' });
  }, [messages]);

  useEffect(() => {
    if (step === 'chat' && conversationId && isAuthenticated && typeof window !== 'undefined') {
      const accessToken = localStorage.getItem('access_token');
      if (!accessToken) return;

      const socket = new WebSocket(
        `${process.env.NEXT_PUBLIC_WS_URL}/ws/chat/?token=${accessToken}`
      );

      socket.onopen = () => {
        socket.send(JSON.stringify({ type: 'join_conversation', conversation_id: conversationId }));
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
      return () => socket.close();
    }
  }, [step, conversationId, isAuthenticated]);

  const handleSubmitTicket = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!category || !subject || !initialMessage) return;
    setIsSubmitting(true);
    try {
      const accessToken = localStorage.getItem('access_token') || '';
      const res = await fetch('/api/support/tickets/', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json', 'Authorization': `Bearer ${accessToken}` },
        body: JSON.stringify({ category, subject, initial_message: initialMessage, priority: 'medium' })
      });
      if (!res.ok) throw new Error();
      const data = await res.json();
      setTicketId(data.id);
      setConversationId(data.conversation);
      setMessages([{
        id: 'local-' + Date.now(),
        text: initialMessage,
        sender_id: user?.id as unknown as number,
        sender_name: user?.first_name ? `${user.first_name} ${user.last_name}` : 'You',
        created_at: new Date().toISOString(),
        isOwn: true
      }]);
      setStep('chat');
    } catch {
      toast.error('Failed to create support ticket. Please try again.');
    } finally {
      setIsSubmitting(false);
    }
  };

  const handleSendMessage = (e: React.FormEvent) => {
    e.preventDefault();
    if (!chatInput.trim() || !ws || ws.readyState !== WebSocket.OPEN) return;
    ws.send(JSON.stringify({ type: 'chat_message', conversation_id: conversationId, receiver_id: null, text: chatInput }));
    setChatInput('');
  };

  if (isLoading || !isAuthenticated) return null;

  return (
    <div className="fixed bottom-6 right-6 z-50" id="support-widget-container">
      {/* Floating Toggle Button */}
      {!isOpen && (
        <button
          onClick={() => setIsOpen(true)}
          className="h-14 w-14 rounded-full shadow-xl bg-primary hover:bg-primary/90 flex items-center justify-center transition-transform hover:scale-105 text-white"
          aria-label="Open support chat"
        >
          <MessageCircle className="h-6 w-6" />
        </button>
      )}

      {/* Chat Panel */}
      {isOpen && (
        <div className="flex flex-col w-[360px] h-[520px] rounded-xl shadow-2xl border border-border bg-card overflow-hidden">
          {/* Header */}
          <div className="flex items-center justify-between px-4 py-3 bg-primary text-white rounded-t-xl shrink-0">
            <span className="font-semibold flex items-center gap-2 text-sm">
              <MessageCircle className="h-4 w-4" />
              StayAfrica Support
            </span>
            <button
              type="button"
              onClick={() => setIsOpen(false)}
              className="p-1 rounded-full hover:bg-white/20 transition-colors"
              aria-label="Close"
            >
              <X className="h-5 w-5" />
            </button>
          </div>

          {step === 'form' ? (
            /* Ticket Form */
            <div className="flex-1 overflow-y-auto p-4">
              <div className="mb-4 text-xs text-muted-foreground bg-muted/50 p-3 rounded-lg flex items-start gap-2">
                <AlertCircle className="h-4 w-4 text-primary mt-0.5 shrink-0" />
                <p>How can we help? Fill in the details below and we'll connect you with an agent.</p>
              </div>

              <form onSubmit={handleSubmitTicket} className="space-y-3">
                <div>
                  <label className="text-xs font-medium block mb-1">Issue Category</label>
                  <select
                    value={category}
                    onChange={(e) => setCategory(e.target.value)}
                    required
                    className="w-full h-10 rounded-md border border-input bg-background px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-ring"
                  >
                    <option value="" disabled>Select a category</option>
                    <option value="booking">Booking &amp; Reservations</option>
                    <option value="payment">Payments &amp; Refunds</option>
                    <option value="property">Property Issue</option>
                    <option value="account">Account &amp; Profile</option>
                    <option value="technical">Technical Bug</option>
                    <option value="other">Other</option>
                  </select>
                </div>

                <div>
                  <label className="text-xs font-medium block mb-1">Subject</label>
                  <input
                    type="text"
                    placeholder="Brief description"
                    value={subject}
                    onChange={(e) => setSubject(e.target.value)}
                    required
                    className="w-full h-10 rounded-md border border-input bg-background px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-ring"
                  />
                </div>

                <div>
                  <label className="text-xs font-medium block mb-1">Message</label>
                  <textarea
                    placeholder="Describe your issue in detail..."
                    value={initialMessage}
                    onChange={(e) => setInitialMessage(e.target.value)}
                    required
                    rows={4}
                    className="w-full rounded-md border border-input bg-background px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-ring resize-none"
                  />
                </div>

                <button
                  type="submit"
                  disabled={isSubmitting || !category || !subject || !initialMessage}
                  className="w-full h-10 rounded-md bg-primary text-white text-sm font-medium hover:bg-primary/90 disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
                >
                  {isSubmitting ? 'Submitting...' : 'Start Chat'}
                </button>
              </form>
            </div>
          ) : (
            /* Active Chat */
            <>
              <div className="flex-1 overflow-y-auto p-4 bg-muted/10 space-y-3">
                <p className="text-center text-xs text-muted-foreground">
                  Support Ticket #{ticketId} — An agent will reply shortly.
                </p>
                {messages.map((msg, idx) => (
                  <div key={msg.id || idx} className={`flex flex-col max-w-[85%] ${msg.isOwn ? 'ml-auto items-end' : 'mr-auto items-start'}`}>
                    {!msg.isOwn && (
                      <span className="text-[10px] text-muted-foreground mb-1">{msg.sender_name} (Support)</span>
                    )}
                    <div className={`px-3 py-2 rounded-2xl text-sm ${msg.isOwn ? 'bg-primary text-white rounded-br-sm' : 'bg-muted rounded-bl-sm'}`}>
                      {msg.text}
                    </div>
                    <span className="text-[10px] text-muted-foreground mt-1">
                      {new Date(msg.created_at).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })}
                    </span>
                  </div>
                ))}
                <div ref={messagesEndRef} />
              </div>

              <form onSubmit={handleSendMessage} className="shrink-0 flex items-center gap-2 p-3 border-t bg-background">
                <input
                  type="text"
                  placeholder="Type a message..."
                  value={chatInput}
                  onChange={(e) => setChatInput(e.target.value)}
                  className="flex-1 h-9 rounded-full border border-input bg-muted/40 px-4 text-sm focus:outline-none focus:ring-2 focus:ring-ring"
                />
                <button
                  type="submit"
                  disabled={!chatInput.trim()}
                  className="h-9 w-9 rounded-full bg-primary text-white flex items-center justify-center disabled:opacity-40 shrink-0 hover:bg-primary/90 transition-colors"
                >
                  <Send className="h-4 w-4" />
                </button>
              </form>
            </>
          )}
        </div>
      )}
    </div>
  );
};
