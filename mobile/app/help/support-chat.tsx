import React, { useState, useEffect, useRef } from 'react';
import {
  View, Text, TextInput, TouchableOpacity, FlatList,
  StyleSheet, KeyboardAvoidingView, Platform, ActivityIndicator, Alert
} from 'react-native';
import { SafeAreaView } from 'react-native-safe-area-context';
import { router } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import AsyncStorage from '@react-native-async-storage/async-storage';

const COLORS = {
  green: '#2D5016',
  greenLight: '#4a7c2e',
  gold: '#d4b158',
  dark: '#0f1a07',
  surface: '#1a2e0a',
  text: '#ffffff',
  textMuted: 'rgba(255,255,255,0.6)',
  border: 'rgba(212,177,88,0.25)',
};

const CATEGORIES = [
  { value: 'booking', label: 'Booking & Reservations' },
  { value: 'payment', label: 'Payments & Refunds' },
  { value: 'property', label: 'Property Issue' },
  { value: 'account', label: 'Account & Profile' },
  { value: 'technical', label: 'Technical Bug' },
  { value: 'other', label: 'Other' },
];

interface Message {
  id: string;
  text: string;
  sender_id: number | string;
  sender_name: string;
  created_at: string;
  isOwn: boolean;
}

const API_BASE = process.env.EXPO_PUBLIC_API_URL || 'http://localhost:8000';
const WS_URL = process.env.EXPO_PUBLIC_WS_URL || 'ws://localhost:8000';

export default function SupportChatScreen() {
  const [step, setStep] = useState<'category' | 'form' | 'chat'>('category');
  const [category, setCategory] = useState('');
  const [subject, setSubject] = useState('');
  const [initialMessage, setInitialMessage] = useState('');
  const [isSubmitting, setIsSubmitting] = useState(false);

  const [ticketId, setTicketId] = useState<number | null>(null);
  const [conversationId, setConversationId] = useState<number | null>(null);
  const [messages, setMessages] = useState<Message[]>([]);
  const [chatInput, setChatInput] = useState('');
  const [ws, setWs] = useState<WebSocket | null>(null);

  const listRef = useRef<FlatList>(null);

  useEffect(() => {
    return () => { ws?.close(); };
  }, [ws]);

  const connectWebSocket = async (convId: number) => {
    const token = await AsyncStorage.getItem('access_token');
    if (!token) return;
    const socket = new WebSocket(`${WS_URL}/ws/chat/?token=${token}`);
    socket.onopen = () => {
      socket.send(JSON.stringify({ type: 'join_conversation', conversation_id: convId }));
    };
    socket.onmessage = (event) => {
      const data = JSON.parse(event.data);
      if (data.type === 'new_message' && data.conversation_id === convId) {
        setMessages(prev => [...prev, {
          id: String(data.message_id || Date.now()),
          text: data.text,
          sender_id: data.sender_id,
          sender_name: data.sender_name,
          created_at: data.created_at,
          isOwn: false
        }]);
      }
    };
    setWs(socket);
  };

  const handleSubmitTicket = async () => {
    if (!category || !subject || !initialMessage) {
      Alert.alert('Missing Fields', 'Please fill in all fields.');
      return;
    }
    setIsSubmitting(true);
    try {
      const token = await AsyncStorage.getItem('access_token');
      const res = await fetch(`${API_BASE}/api/v1/support/tickets/`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json', Authorization: `Bearer ${token}` },
        body: JSON.stringify({ category, subject, initial_message: initialMessage, priority: 'medium' })
      });
      if (!res.ok) throw new Error();
      const data = await res.json();
      setTicketId(data.id);
      setConversationId(data.conversation);
      setMessages([{
        id: 'local-' + Date.now(), text: initialMessage,
        sender_id: 'me', sender_name: 'You',
        created_at: new Date().toISOString(), isOwn: true
      }]);
      await connectWebSocket(data.conversation);
      setStep('chat');
    } catch {
      Alert.alert('Error', 'Failed to create support ticket. Please try again.');
    } finally {
      setIsSubmitting(false);
    }
  };

  const handleSend = () => {
    if (!chatInput.trim() || !ws || ws.readyState !== WebSocket.OPEN) return;
    const text = chatInput.trim();
    ws.send(JSON.stringify({ type: 'chat_message', conversation_id: conversationId, receiver_id: null, text }));
    setMessages(prev => [...prev, {
      id: 'local-' + Date.now(), text, sender_id: 'me', sender_name: 'You',
      created_at: new Date().toISOString(), isOwn: true
    }]);
    setChatInput('');
  };

  return (
    <SafeAreaView style={styles.safe}>
      {/* Header */}
      <View style={styles.header}>
        <TouchableOpacity onPress={() => step === 'chat' ? setStep('form') : router.back()} style={styles.backBtn}>
          <Ionicons name="chevron-back" size={22} color={COLORS.gold} />
        </TouchableOpacity>
        <Text style={styles.headerTitle}>
          {step === 'chat' ? `Ticket #${ticketId}` : 'Support Chat'}
        </Text>
        <View style={{ width: 36 }} />
      </View>

      {step === 'category' && (
        <View style={styles.content}>
          <Text style={styles.sectionLabel}>What do you need help with?</Text>
          {CATEGORIES.map(cat => (
            <TouchableOpacity
              key={cat.value}
              style={[styles.categoryRow, category === cat.value && styles.categoryRowSelected]}
              onPress={() => { setCategory(cat.value); setStep('form'); }}
            >
              <Text style={[styles.categoryText, category === cat.value && { color: COLORS.gold }]}>
                {cat.label}
              </Text>
              <Ionicons name="chevron-forward" size={18} color={category === cat.value ? COLORS.gold : COLORS.textMuted} />
            </TouchableOpacity>
          ))}
        </View>
      )}

      {step === 'form' && (
        <KeyboardAvoidingView style={{ flex: 1 }} behavior={Platform.OS === 'ios' ? 'padding' : undefined}>
          <View style={styles.content}>
            <Text style={styles.sectionLabel}>Category: <Text style={{ color: COLORS.gold }}>{category}</Text></Text>
            <Text style={styles.fieldLabel}>Subject</Text>
            <TextInput
              style={styles.input}
              placeholder="Brief description of your issue"
              placeholderTextColor={COLORS.textMuted}
              value={subject}
              onChangeText={setSubject}
            />
            <Text style={styles.fieldLabel}>Message</Text>
            <TextInput
              style={[styles.input, { height: 120, textAlignVertical: 'top', paddingTop: 12 }]}
              placeholder="Describe your issue in detail..."
              placeholderTextColor={COLORS.textMuted}
              value={initialMessage}
              onChangeText={setInitialMessage}
              multiline
            />
            <TouchableOpacity
              style={[styles.btn, (!subject || !initialMessage || isSubmitting) && styles.btnDisabled]}
              onPress={handleSubmitTicket}
              disabled={!subject || !initialMessage || isSubmitting}
            >
              {isSubmitting
                ? <ActivityIndicator color={COLORS.dark} />
                : <Text style={styles.btnText}>Start Chat</Text>
              }
            </TouchableOpacity>
          </View>
        </KeyboardAvoidingView>
      )}

      {step === 'chat' && (
        <KeyboardAvoidingView style={{ flex: 1 }} behavior={Platform.OS === 'ios' ? 'padding' : undefined}>
          <FlatList
            ref={listRef}
            data={messages}
            keyExtractor={m => m.id}
            contentContainerStyle={{ padding: 16, flexGrow: 1 }}
            onContentSizeChange={() => listRef.current?.scrollToEnd({ animated: true })}
            ListHeaderComponent={
              <Text style={styles.systemMsg}>An agent will reply shortly.</Text>
            }
            renderItem={({ item }) => (
              <View style={[styles.bubble, item.isOwn ? styles.bubbleOwn : styles.bubbleOther]}>
                {!item.isOwn && (
                  <Text style={styles.bubbleSender}>{item.sender_name}</Text>
                )}
                <Text style={[styles.bubbleText, item.isOwn && { color: '#fff' }]}>{item.text}</Text>
                <Text style={styles.bubbleTime}>
                  {new Date(item.created_at).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })}
                </Text>
              </View>
            )}
          />
          <View style={styles.inputBar}>
            <TextInput
              style={styles.chatInput}
              placeholder="Type a message..."
              placeholderTextColor={COLORS.textMuted}
              value={chatInput}
              onChangeText={setChatInput}
              returnKeyType="send"
              onSubmitEditing={handleSend}
            />
            <TouchableOpacity style={styles.sendBtn} onPress={handleSend} disabled={!chatInput.trim()}>
              <Ionicons name="send" size={18} color={COLORS.dark} />
            </TouchableOpacity>
          </View>
        </KeyboardAvoidingView>
      )}
    </SafeAreaView>
  );
}

const styles = StyleSheet.create({
  safe: { flex: 1, backgroundColor: '#0f1a07' },
  header: { flexDirection: 'row', alignItems: 'center', justifyContent: 'space-between', paddingHorizontal: 16, paddingVertical: 12, borderBottomWidth: 1, borderColor: 'rgba(212,177,88,0.2)', backgroundColor: '#1a2e0a' },
  backBtn: { width: 36, height: 36, alignItems: 'center', justifyContent: 'center', borderRadius: 18, backgroundColor: 'rgba(212,177,88,0.1)' },
  headerTitle: { fontSize: 16, fontWeight: '600', color: '#fff' },
  content: { flex: 1, paddingHorizontal: 16, paddingTop: 20 },
  sectionLabel: { fontSize: 13, color: COLORS.textMuted, marginBottom: 16, fontWeight: '500' },
  categoryRow: { flexDirection: 'row', justifyContent: 'space-between', alignItems: 'center', paddingHorizontal: 16, paddingVertical: 14, borderRadius: 12, marginBottom: 8, backgroundColor: 'rgba(255,255,255,0.05)', borderWidth: 1, borderColor: 'rgba(212,177,88,0.15)' },
  categoryRowSelected: { borderColor: COLORS.gold, backgroundColor: 'rgba(212,177,88,0.08)' },
  categoryText: { fontSize: 14, color: '#fff', fontWeight: '500' },
  fieldLabel: { fontSize: 12, color: COLORS.textMuted, marginBottom: 6, marginTop: 12, fontWeight: '500' },
  input: { borderWidth: 1, borderColor: 'rgba(212,177,88,0.2)', borderRadius: 12, paddingHorizontal: 14, paddingVertical: 10, fontSize: 14, color: '#fff', backgroundColor: 'rgba(255,255,255,0.05)' },
  btn: { marginTop: 20, height: 48, borderRadius: 12, alignItems: 'center', justifyContent: 'center', backgroundColor: COLORS.gold },
  btnDisabled: { opacity: 0.4 },
  btnText: { fontWeight: '700', fontSize: 15, color: COLORS.dark },
  systemMsg: { textAlign: 'center', color: 'rgba(212,177,88,0.6)', fontSize: 12, marginBottom: 16 },
  bubble: { maxWidth: '80%', marginBottom: 12, borderRadius: 16 },
  bubbleOwn: { alignSelf: 'flex-end', backgroundColor: COLORS.green, padding: 12, borderBottomRightRadius: 4 },
  bubbleOther: { alignSelf: 'flex-start', backgroundColor: 'rgba(255,255,255,0.08)', padding: 12, borderWidth: 1, borderColor: 'rgba(255,255,255,0.06)', borderBottomLeftRadius: 4 },
  bubbleSender: { fontSize: 10, color: COLORS.gold, marginBottom: 4 },
  bubbleText: { fontSize: 14, color: 'rgba(255,255,255,0.9)', lineHeight: 20 },
  bubbleTime: { fontSize: 10, color: 'rgba(255,255,255,0.3)', marginTop: 4, alignSelf: 'flex-end' },
  inputBar: { flexDirection: 'row', alignItems: 'center', gap: 8, padding: 12, borderTopWidth: 1, borderColor: 'rgba(212,177,88,0.15)', backgroundColor: '#0f1a07' },
  chatInput: { flex: 1, height: 40, borderRadius: 20, paddingHorizontal: 16, fontSize: 14, color: '#fff', backgroundColor: 'rgba(255,255,255,0.08)', borderWidth: 1, borderColor: 'rgba(212,177,88,0.15)' },
  sendBtn: { width: 40, height: 40, borderRadius: 20, backgroundColor: COLORS.gold, alignItems: 'center', justifyContent: 'center' },
});
