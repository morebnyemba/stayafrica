export interface AutoMessageSettings {
  id: string;
  user: string;
  enabled: boolean;
  triggers: {
    booking_confirmed: boolean;
    check_in_reminder: boolean;
    check_out_reminder: boolean;
    booking_inquiry: boolean;
    booking_cancelled: boolean;
    review_request: boolean;
    payment_received: boolean;
    custom_trigger: boolean;
  };
  templates: Record<string, string>;
  created_at: string;
  updated_at: string;
}

export interface QuickReply {
  id: string;
  user: string;
  label: string;
  message: string;
  shortcut?: string;
  created_at: string;
  updated_at: string;
}

export interface ScheduledMessage {
  id: string;
  user: string;
  recipient: string;
  recipient_name?: string;
  message: string;
  scheduled_time: string;
  timezone: string;
  status: 'PENDING' | 'SENT' | 'FAILED' | 'CANCELLED';
  sent_at?: string;
  created_at: string;
}

export interface MessageTemplate {
  id: string;
  user: string;
  name: string;
  subject?: string;
  content: string;
  variables: string[];
  category?: string;
  created_at: string;
  updated_at: string;
}
