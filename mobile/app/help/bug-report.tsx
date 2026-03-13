import React, { useState } from 'react';
import {
  View, Text, TextInput, TouchableOpacity, ScrollView,
  StyleSheet, Alert, ActivityIndicator, Platform
} from 'react-native';
import { SafeAreaView } from 'react-native-safe-area-context';
import { router } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import AsyncStorage from '@react-native-async-storage/async-storage';
import * as Device from 'expo-device';
import Constants from 'expo-constants';

const COLORS = {
  green: '#2D5016',
  gold: '#d4b158',
  dark: '#0f1a07',
  textMuted: 'rgba(255,255,255,0.6)',
  border: 'rgba(212,177,88,0.25)',
};

const API_BASE = process.env.EXPO_PUBLIC_API_URL || 'http://localhost:8000';

const SEVERITIES = [
  { value: 'critical', label: 'Critical (Crash / Data Loss)', color: '#ef4444' },
  { value: 'major', label: 'Major (Feature Broken)', color: '#f97316' },
  { value: 'minor', label: 'Minor (Workaround Exists)', color: '#eab308' },
  { value: 'cosmetic', label: 'Cosmetic (UI Glitch)', color: '#6b7280' },
];

export default function BugReportScreen() {
  const [title, setTitle] = useState('');
  const [description, setDescription] = useState('');
  const [steps, setSteps] = useState('');
  const [severity, setSeverity] = useState('minor');
  const [isSubmitting, setIsSubmitting] = useState(false);

  const getDeviceInfo = () => ({
    model: Device.modelName,
    os: `${Platform.OS} ${Device.osVersion}`,
    appVersion: Constants.expoConfig?.version || 'unknown',
    screenResolution: 'native',
  });

  const handleSubmit = async () => {
    if (!title || !description) {
      Alert.alert('Missing Fields', 'Please provide a title and description.');
      return;
    }
    setIsSubmitting(true);
    try {
      const token = await AsyncStorage.getItem('access_token');
      const res = await fetch(`${API_BASE}/api/v1/support/bugs/`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json', Authorization: `Bearer ${token}` },
        body: JSON.stringify({
          title,
          description,
          steps_to_reproduce: steps,
          severity,
          status: 'new',
          browser_info: getDeviceInfo(),
          page_url: 'mobile-app',
        })
      });
      if (!res.ok) throw new Error();
      Alert.alert('Thank You!', 'Your bug report has been submitted. Our team will review it shortly.', [
        { text: 'OK', onPress: () => router.back() }
      ]);
    } catch {
      Alert.alert('Error', 'Failed to submit the report. Please try again.');
    } finally {
      setIsSubmitting(false);
    }
  };

  return (
    <SafeAreaView style={styles.safe}>
      {/* Header */}
      <View style={styles.header}>
        <TouchableOpacity onPress={() => router.back()} style={styles.backBtn}>
          <Ionicons name="chevron-back" size={22} color={COLORS.gold} />
        </TouchableOpacity>
        <Text style={styles.headerTitle}>Report a Bug</Text>
        <View style={{ width: 36 }} />
      </View>

      <ScrollView style={{ flex: 1 }} contentContainerStyle={styles.content} keyboardShouldPersistTaps="handled">
        {/* Info Banner */}
        <View style={styles.banner}>
          <Ionicons name="information-circle-outline" size={16} color={COLORS.gold} />
          <Text style={styles.bannerText}>
            Device info, OS version, and app version are captured automatically.
          </Text>
        </View>

        {/* Title */}
        <Text style={styles.label}>Issue Title</Text>
        <TextInput
          style={styles.input}
          placeholder="e.g. Booking button not responding"
          placeholderTextColor={COLORS.textMuted}
          value={title}
          onChangeText={setTitle}
        />

        {/* Severity */}
        <Text style={styles.label}>Severity</Text>
        <View style={styles.severityGrid}>
          {SEVERITIES.map(s => (
            <TouchableOpacity
              key={s.value}
              style={[styles.severityChip, severity === s.value && { borderColor: s.color, backgroundColor: `${s.color}18` }]}
              onPress={() => setSeverity(s.value)}
            >
              <View style={[styles.severityDot, { backgroundColor: s.color }]} />
              <Text style={[styles.severityText, severity === s.value && { color: s.color }]}>
                {s.label.split(' (')[0]}
              </Text>
            </TouchableOpacity>
          ))}
        </View>

        {/* Description */}
        <Text style={styles.label}>Description</Text>
        <TextInput
          style={[styles.input, { height: 100, textAlignVertical: 'top', paddingTop: 12 }]}
          placeholder="What went wrong? What did you expect to happen?"
          placeholderTextColor={COLORS.textMuted}
          value={description}
          onChangeText={setDescription}
          multiline
        />

        {/* Steps */}
        <Text style={styles.label}>Steps to Reproduce <Text style={{ color: COLORS.textMuted }}>(Optional)</Text></Text>
        <TextInput
          style={[styles.input, { height: 80, textAlignVertical: 'top', paddingTop: 12 }]}
          placeholder="1. Open the app...   2. Tap on..."
          placeholderTextColor={COLORS.textMuted}
          value={steps}
          onChangeText={setSteps}
          multiline
        />

        {/* Submit */}
        <TouchableOpacity
          style={[styles.submitBtn, (!title || !description || isSubmitting) && styles.submitBtnDisabled]}
          onPress={handleSubmit}
          disabled={!title || !description || isSubmitting}
        >
          {isSubmitting
            ? <ActivityIndicator color={COLORS.dark} />
            : <Text style={styles.submitText}>Submit Bug Report</Text>
          }
        </TouchableOpacity>
      </ScrollView>
    </SafeAreaView>
  );
}

const styles = StyleSheet.create({
  safe: { flex: 1, backgroundColor: '#0f1a07' },
  header: { flexDirection: 'row', alignItems: 'center', justifyContent: 'space-between', paddingHorizontal: 16, paddingVertical: 12, borderBottomWidth: 1, borderColor: 'rgba(212,177,88,0.2)', backgroundColor: '#1a2e0a' },
  backBtn: { width: 36, height: 36, alignItems: 'center', justifyContent: 'center', borderRadius: 18, backgroundColor: 'rgba(212,177,88,0.1)' },
  headerTitle: { fontSize: 16, fontWeight: '600', color: '#fff' },
  content: { padding: 16, paddingBottom: 40 },
  banner: { flexDirection: 'row', gap: 8, alignItems: 'flex-start', padding: 12, borderRadius: 12, backgroundColor: 'rgba(212,177,88,0.08)', borderWidth: 1, borderColor: 'rgba(212,177,88,0.15)', marginBottom: 20 },
  bannerText: { flex: 1, fontSize: 12, color: 'rgba(255,255,255,0.6)', lineHeight: 18 },
  label: { fontSize: 12, fontWeight: '600', color: COLORS.textMuted, marginBottom: 8, marginTop: 16 },
  input: { borderWidth: 1, borderColor: COLORS.border, borderRadius: 12, paddingHorizontal: 14, paddingVertical: 10, fontSize: 14, color: '#fff', backgroundColor: 'rgba(255,255,255,0.05)' },
  severityGrid: { flexDirection: 'row', flexWrap: 'wrap', gap: 8 },
  severityChip: { flexDirection: 'row', alignItems: 'center', gap: 6, paddingHorizontal: 12, paddingVertical: 8, borderRadius: 20, borderWidth: 1, borderColor: COLORS.border, backgroundColor: 'rgba(255,255,255,0.04)' },
  severityDot: { width: 6, height: 6, borderRadius: 3 },
  severityText: { fontSize: 12, color: 'rgba(255,255,255,0.7)', fontWeight: '500' },
  submitBtn: { marginTop: 28, height: 52, borderRadius: 14, alignItems: 'center', justifyContent: 'center', backgroundColor: COLORS.gold },
  submitBtnDisabled: { opacity: 0.4 },
  submitText: { fontWeight: '700', fontSize: 15, color: COLORS.dark },
});
