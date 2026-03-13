"use client";

import { useState, useEffect, useRef } from 'react';
import { useAuth } from '@/store/auth-store';
import { MessageCircle, X, AlertCircle, Send, ChevronDown } from 'lucide-react';
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
      const apiBase = process.env.NEXT_PUBLIC_API_BASE_URL ||
        `${window.location.protocol}//${window.location.host}`;
      const wsBase = apiBase.replace(/^http/, 'ws');
      const socket = new WebSocket(`${wsBase}/ws/chat/?token=${accessToken}`);

      socket.onopen = () => socket.send(JSON.stringify({ type: 'join_conversation', conversation_id: conversationId }));
      socket.onmessage = (event) => {
        const data = JSON.parse(event.data);
        if (data.type === 'new_message' && data.conversation_id === conversationId) {
          setMessages(prev => [...prev, {
            id: data.message_id, text: data.text, sender_id: data.sender_id,
            sender_name: data.sender_name, created_at: data.created_at,
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
        id: 'local-' + Date.now(), text: initialMessage,
        sender_id: user?.id as unknown as number,
        sender_name: user?.first_name ? `${user.first_name} ${user.last_name}` : 'You',
        created_at: new Date().toISOString(), isOwn: true
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
          aria-label="Open support chat"
          style={{
            background: 'linear-gradient(135deg, #2D5016 0%, #4a7c2e 100%)',
            boxShadow: '0 8px 32px rgba(45, 80, 22, 0.45), 0 2px 8px rgba(0,0,0,0.2)',
          }}
          className="h-14 w-14 rounded-full flex items-center justify-center text-white transition-all duration-300 hover:scale-110 hover:-translate-y-0.5"
        >
          <MessageCircle className="h-6 w-6" />
        </button>
      )}

      {/* Glassmorphism Chat Panel */}
      {isOpen && (
        <div
          style={{
            background: 'rgba(15, 25, 10, 0.75)',
            backdropFilter: 'blur(20px)',
            WebkitBackdropFilter: 'blur(20px)',
            border: '1px solid rgba(212, 177, 88, 0.25)',
            boxShadow: '0 24px 64px rgba(0,0,0,0.5), inset 0 1px 0 rgba(212,177,88,0.15)',
          }}
          className="flex flex-col w-[360px] h-[530px] rounded-2xl overflow-hidden"
        >
          {/* Header with gold accent */}
          <div
            style={{
              background: 'linear-gradient(135deg, rgba(45,80,22,0.9) 0%, rgba(30,55,12,0.95) 100%)',
              borderBottom: '1px solid rgba(212, 177, 88, 0.3)',
            }}
            className="flex items-center justify-between px-4 py-3 shrink-0"
          >
            <span className="font-semibold flex items-center gap-2 text-sm" style={{ color: '#d4b158' }}>
              <MessageCircle className="h-4 w-4" style={{ color: '#d4b158' }} />
              <span className="text-white">StayAfrica</span>
              <span style={{ color: '#d4b158' }}>Support</span>
            </span>
            <button
              type="button"
              onClick={() => setIsOpen(false)}
              className="p-1.5 rounded-full transition-colors"
              style={{ color: 'rgba(255,255,255,0.7)' }}
              onMouseEnter={e => (e.currentTarget.style.background = 'rgba(212,177,88,0.15)')}
              onMouseLeave={e => (e.currentTarget.style.background = 'transparent')}
              aria-label="Close"
            >
              <X className="h-4 w-4" />
            </button>
          </div>

          {step === 'form' ? (
            <div className="flex-1 overflow-y-auto p-4" style={{ scrollbarWidth: 'thin', scrollbarColor: 'rgba(212,177,88,0.3) transparent' }}>
              <div
                className="mb-4 text-xs p-3 rounded-xl flex items-start gap-2"
                style={{ background: 'rgba(212,177,88,0.08)', border: '1px solid rgba(212,177,88,0.15)', color: 'rgba(255,255,255,0.7)' }}
              >
                <AlertCircle className="h-4 w-4 mt-0.5 shrink-0" style={{ color: '#d4b158' }} />
                <p>How can we help? Fill in the details and we'll connect you with an agent.</p>
              </div>

              <form onSubmit={handleSubmitTicket} className="space-y-3">
                <div>
                  <label className="text-xs font-medium block mb-1" style={{ color: 'rgba(255,255,255,0.6)' }}>Issue Category</label>
                  <div className="relative">
                    <select
                      value={category}
                      onChange={(e) => setCategory(e.target.value)}
                      required
                      className="w-full h-10 rounded-xl px-3 py-2 text-sm appearance-none pr-8"
                      style={{
                        background: 'rgba(255,255,255,0.07)',
                        border: '1px solid rgba(212,177,88,0.2)',
                        color: category ? 'white' : 'rgba(255,255,255,0.4)',
                        outline: 'none',
                      }}
                    >
                      <option value="" disabled style={{ background: '#1a2e0a' }}>Select a category</option>
                      <option value="booking" style={{ background: '#1a2e0a' }}>Booking &amp; Reservations</option>
                      <option value="payment" style={{ background: '#1a2e0a' }}>Payments &amp; Refunds</option>
                      <option value="property" style={{ background: '#1a2e0a' }}>Property Issue</option>
                      <option value="account" style={{ background: '#1a2e0a' }}>Account &amp; Profile</option>
                      <option value="technical" style={{ background: '#1a2e0a' }}>Technical Bug</option>
                      <option value="other" style={{ background: '#1a2e0a' }}>Other</option>
                    </select>
                    <ChevronDown className="h-4 w-4 absolute right-2.5 top-3 pointer-events-none" style={{ color: 'rgba(212,177,88,0.6)' }} />
                  </div>
                </div>

                <div>
                  <label className="text-xs font-medium block mb-1" style={{ color: 'rgba(255,255,255,0.6)' }}>Subject</label>
                  <input
                    type="text"
                    placeholder="Brief description"
                    value={subject}
                    onChange={(e) => setSubject(e.target.value)}
                    required
                    className="w-full h-10 rounded-xl px-3 py-2 text-sm text-white placeholder-white/30 outline-none focus:ring-1"
                    style={{
                      background: 'rgba(255,255,255,0.07)',
                      border: '1px solid rgba(212,177,88,0.2)',
                    }}
                    onFocus={e => e.target.style.borderColor = 'rgba(212,177,88,0.5)'}
                    onBlur={e => e.target.style.borderColor = 'rgba(212,177,88,0.2)'}
                  />
                </div>

                <div>
                  <label className="text-xs font-medium block mb-1" style={{ color: 'rgba(255,255,255,0.6)' }}>Message</label>
                  <textarea
                    placeholder="Describe your issue in detail..."
                    value={initialMessage}
                    onChange={(e) => setInitialMessage(e.target.value)}
                    required
                    rows={4}
                    className="w-full rounded-xl px-3 py-2 text-sm text-white placeholder-white/30 outline-none resize-none"
                    style={{
                      background: 'rgba(255,255,255,0.07)',
                      border: '1px solid rgba(212,177,88,0.2)',
                    }}
                    onFocus={e => e.target.style.borderColor = 'rgba(212,177,88,0.5)'}
                    onBlur={e => e.target.style.borderColor = 'rgba(212,177,88,0.2)'}
                  />
                </div>

                <button
                  type="submit"
                  disabled={isSubmitting || !category || !subject || !initialMessage}
                  className="w-full h-10 rounded-xl text-sm font-semibold transition-all duration-200 disabled:opacity-40 disabled:cursor-not-allowed"
                  style={{
                    background: 'linear-gradient(135deg, #d4b158 0%, #b8952e 100%)',
                    color: '#0f1a07',
                    boxShadow: '0 4px 16px rgba(212,177,88,0.3)',
                  }}
                >
                  {isSubmitting ? 'Submitting...' : 'Start Chat'}
                </button>
              </form>
            </div>
          ) : (
            <>
              <div className="flex-1 overflow-y-auto p-4 space-y-3" style={{ scrollbarWidth: 'thin', scrollbarColor: 'rgba(212,177,88,0.2) transparent' }}>
                <p className="text-center text-xs" style={{ color: 'rgba(212,177,88,0.6)' }}>
                  Ticket #{ticketId} — An agent will reply shortly
                </p>
                {messages.map((msg, idx) => (
                  <div key={msg.id || idx} className={`flex flex-col max-w-[82%] ${msg.isOwn ? 'ml-auto items-end' : 'mr-auto items-start'}`}>
                    {!msg.isOwn && (
                      <span className="text-[10px] mb-1" style={{ color: 'rgba(212,177,88,0.7)' }}>{msg.sender_name} · Support</span>
                    )}
                    <div
                      className="px-3 py-2 rounded-2xl text-sm"
                      style={msg.isOwn
                        ? { background: 'linear-gradient(135deg, #2D5016, #4a7c2e)', color: 'white', borderBottomRightRadius: 4 }
                        : { background: 'rgba(255,255,255,0.08)', color: 'rgba(255,255,255,0.9)', borderBottomLeftRadius: 4, border: '1px solid rgba(255,255,255,0.06)' }
                      }
                    >
                      {msg.text}
                    </div>
                    <span className="text-[10px] mt-1" style={{ color: 'rgba(255,255,255,0.3)' }}>
                      {new Date(msg.created_at).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })}
                    </span>
                  </div>
                ))}
                <div ref={messagesEndRef} />
              </div>

              <form
                onSubmit={handleSendMessage}
                className="shrink-0 flex items-center gap-2 p-3"
                style={{ borderTop: '1px solid rgba(212,177,88,0.15)', background: 'rgba(0,0,0,0.2)' }}
              >
                <input
                  type="text"
                  placeholder="Type a message..."
                  value={chatInput}
                  onChange={(e) => setChatInput(e.target.value)}
                  className="flex-1 h-9 rounded-full px-4 text-sm text-white placeholder-white/30 outline-none"
                  style={{ background: 'rgba(255,255,255,0.08)', border: '1px solid rgba(212,177,88,0.15)' }}
                />
                <button
                  type="submit"
                  disabled={!chatInput.trim()}
                  className="h-9 w-9 rounded-full flex items-center justify-center disabled:opacity-40 shrink-0 transition-all"
                  style={{ background: 'linear-gradient(135deg, #d4b158, #b8952e)', color: '#0f1a07' }}
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
