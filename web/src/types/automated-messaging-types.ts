export interface AutoMessageSettings {
  id: number;
  enable_auto_responses: boolean;
  enable_quick_replies: boolean;
  enable_scheduled_messages: boolean;
  away_mode_enabled: boolean;
  away_message: string;
  target_response_time_hours: number;
  created_at: string;
  updated_at: string;
}

export interface AutomatedMessage {
  id: number;
  name: string;
  trigger_type: string;
  trigger_type_display: string;
  template: number | null;
  template_name: string | null;
  delay_hours: number;
  custom_message: string;
  is_active: boolean;
  created_at: string;
  updated_at: string;
}

export interface QuickReply {
  id: number;
  shortcut: string;
  message_text: string;
  category: string;
  use_count: number;
  is_active: boolean;
  created_at: string;
  updated_at: string;
}

export interface ScheduledMessage {
  id: number;
  conversation: number;
  conversation_id: number;
  message_text: string;
  scheduled_time: string;
  status: 'pending' | 'sent' | 'failed' | 'cancelled';
  status_display: string;
  sent_at: string | null;
  booking: number | null;
  created_at: string;
}

export interface MessageTemplate {
  id: number;
  name: string;
  template_type: string;
  template_type_display: string;
  subject: string;
  body: string;
  is_active: boolean;
  created_at: string;
  updated_at: string;
}
