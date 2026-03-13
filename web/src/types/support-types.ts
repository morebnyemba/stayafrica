export interface SupportTicketEvent {
  id: number;
  ticket: number;
  actor: number;
  actor_name: string;
  event_type: string;
  old_value: string;
  new_value: string;
  note: string;
  created_at: string;
}

export interface SupportTicket {
  id: number;
  conversation: number;
  conversation_summary?: any; // Reuse existing Conversation type from messaging
  requester: number;
  requester_name: string;
  assigned_agent: number | null;
  assigned_agent_name: string | null;
  category: 'booking' | 'payment' | 'property' | 'account' | 'technical' | 'other';
  priority: 'low' | 'medium' | 'high' | 'urgent';
  status: 'open' | 'assigned' | 'in_progress' | 'waiting_customer' | 'resolved' | 'closed';
  subject: string;
  related_booking: number | null;
  related_property: number | null;
  resolution_notes: string;
  created_at: string;
  updated_at: string;
  resolved_at: string | null;
  satisfaction_rating: number | null;
  events?: SupportTicketEvent[];
}

export interface CannedResponse {
  id: number;
  title: string;
  body: string;
  category: 'booking' | 'payment' | 'property' | 'account' | 'technical' | 'other';
  is_active: boolean;
}

export interface BugReport {
  id: number;
  reporter: number;
  reporter_name: string;
  title: string;
  description: string;
  steps_to_reproduce: string;
  expected_behavior: string;
  actual_behavior: string;
  severity: 'critical' | 'major' | 'minor' | 'cosmetic';
  status: 'new' | 'triaged' | 'in_progress' | 'fixed' | 'wont_fix' | 'duplicate';
  browser_info: Record<string, any>;
  page_url: string;
  console_logs: string;
  network_errors: string;
  screenshot: string | null;
  support_ticket: number | null;
  assigned_to: number | null;
  assigned_name: string | null;
  created_at: string;
  updated_at: string;
}
