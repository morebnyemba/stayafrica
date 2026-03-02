import { View, Text, ScrollView, TouchableOpacity, TextInput, Switch, ActivityIndicator, Alert } from 'react-native';
import { useState } from 'react';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { Skeleton } from '@/components/common/Skeletons';
import {
  useMessageTemplates,
  useQuickReplies,
  useAutomatedMessages,
  useHostMessageSettings,
  useUpdateHostMessageSettings,
} from '@/hooks/api-hooks';
import { useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';

type Tab = 'settings' | 'templates' | 'quick-replies';

export default function HostMessagingScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const { isAuthenticated } = useAuth();
  const [activeTab, setActiveTab] = useState<Tab>('settings');
  const queryClient = useQueryClient();

  // Data hooks
  const { data: settingsData, isLoading: settingsLoading } = useHostMessageSettings();
  const { data: templatesData, isLoading: templatesLoading } = useMessageTemplates();
  const { data: quickRepliesData, isLoading: quickRepliesLoading } = useQuickReplies();
  const { data: automatedData, isLoading: automatedLoading } = useAutomatedMessages();
  const { mutate: updateSettings, isPending: isUpdating } = useUpdateHostMessageSettings();

  // Quick reply creation
  const [newReplyLabel, setNewReplyLabel] = useState('');
  const [newReplyMessage, setNewReplyMessage] = useState('');
  const createQuickReply = useMutation({
    mutationFn: (data: { label: string; message: string }) => apiClient.createQuickReply(data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['messaging', 'quick-replies'] });
      setNewReplyLabel('');
      setNewReplyMessage('');
    },
  });
  const deleteQuickReply = useMutation({
    mutationFn: (id: string) => apiClient.deleteQuickReply(id),
    onSuccess: () => queryClient.invalidateQueries({ queryKey: ['messaging', 'quick-replies'] }),
  });

  if (!isAuthenticated) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100">
        <View className="flex-1 items-center justify-center px-6">
          <Text className="text-forest text-lg">Please sign in</Text>
        </View>
      </SafeAreaView>
    );
  }

  const templates = templatesData?.results || [];
  const quickReplies = quickRepliesData?.results || [];
  const automatedMessages = automatedData?.results || [];

  const handleToggleSetting = (key: string, value: boolean) => {
    updateSettings({ [key]: value });
  };

  const renderSettings = () => (
    <View className="px-4 py-4">
      {settingsLoading ? (
        [1, 2, 3].map((i) => (
          <View key={i} className="bg-white rounded-2xl p-4 mb-3">
            <Skeleton height={20} width="60%" className="mb-3" />
            <Skeleton height={14} width="80%" />
          </View>
        ))
      ) : (
        <>
          {/* Auto-reply toggle */}
          <View className="bg-white rounded-2xl p-4 mb-3" style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 4, elevation: 2 }}>
            <View className="flex-row items-center justify-between">
              <View className="flex-1 mr-4">
                <Text className="text-forest font-semibold text-base">Auto-Reply Enabled</Text>
                <Text className="text-moss text-sm mt-1">Automatically reply to new guest messages</Text>
              </View>
              <Switch
                value={settingsData?.enabled ?? false}
                onValueChange={(v) => handleToggleSetting('enabled', v)}
                trackColor={{ false: '#E5E7EB', true: '#D9B168' }}
                thumbColor={settingsData?.enabled ? '#122F26' : '#9CA3AF'}
              />
            </View>
          </View>

          {/* Automated Messages */}
          <Text className="text-lg font-bold text-forest mb-3 mt-4">Automated Triggers</Text>
          {automatedLoading ? (
            <Skeleton height={60} width="100%" className="mb-3" />
          ) : automatedMessages.length === 0 ? (
            <View className="bg-white rounded-2xl p-6 items-center mb-3">
              <Ionicons name="flash-outline" size={32} color="#7A8F85" />
              <Text className="text-moss mt-2 text-center">No automated messages configured yet</Text>
            </View>
          ) : (
            automatedMessages.map((msg: any) => (
              <View key={msg.id} className="bg-white rounded-2xl p-4 mb-3" style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 4, elevation: 2 }}>
                <View className="flex-row items-center justify-between">
                  <View className="flex-1 mr-4">
                    <Text className="text-forest font-semibold">{msg.name || msg.trigger_type}</Text>
                    <Text className="text-moss text-xs mt-1" numberOfLines={2}>{msg.message || msg.body}</Text>
                  </View>
                  <View className={`px-2 py-1 rounded-full ${msg.is_active ? 'bg-green-100' : 'bg-gray-100'}`}>
                    <Text className={`text-xs font-semibold ${msg.is_active ? 'text-green-700' : 'text-gray-500'}`}>
                      {msg.is_active ? 'Active' : 'Inactive'}
                    </Text>
                  </View>
                </View>
              </View>
            ))
          )}
        </>
      )}
    </View>
  );

  const renderTemplates = () => (
    <View className="px-4 py-4">
      <Text className="text-lg font-bold text-forest mb-3">Message Templates</Text>
      {templatesLoading ? (
        [1, 2].map((i) => (
          <View key={i} className="bg-white rounded-2xl p-4 mb-3">
            <Skeleton height={18} width="50%" className="mb-2" />
            <Skeleton height={40} width="100%" />
          </View>
        ))
      ) : templates.length === 0 ? (
        <View className="bg-white rounded-2xl p-8 items-center" style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 4 }, shadowOpacity: 0.08, shadowRadius: 8, elevation: 4 }}>
          <Ionicons name="document-text-outline" size={40} color="#7A8F85" />
          <Text className="text-forest font-semibold text-lg mt-3">No Templates</Text>
          <Text className="text-moss text-sm mt-1 text-center">Create templates on the web dashboard for quicker messaging</Text>
        </View>
      ) : (
        templates.map((template: any) => (
          <View key={template.id} className="bg-white rounded-2xl p-4 mb-3" style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 4, elevation: 2 }}>
            <View className="flex-row items-center justify-between mb-2">
              <Text className="text-forest font-semibold flex-1" numberOfLines={1}>{template.name}</Text>
              <View className={`px-2 py-1 rounded-full ${template.is_active ? 'bg-green-100' : 'bg-gray-100'}`}>
                <Text className={`text-xs font-semibold ${template.is_active ? 'text-green-700' : 'text-gray-500'}`}>
                  {template.template_type || 'Custom'}
                </Text>
              </View>
            </View>
            <Text className="text-moss text-sm" numberOfLines={3}>{template.body || template.content}</Text>
            {template.subject && (
              <Text className="text-xs text-moss/70 mt-2">Subject: {template.subject}</Text>
            )}
          </View>
        ))
      )}
    </View>
  );

  const renderQuickReplies = () => (
    <View className="px-4 py-4">
      <Text className="text-lg font-bold text-forest mb-3">Quick Replies</Text>

      {/* Add new quick reply */}
      <View className="bg-white rounded-2xl p-4 mb-4" style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 4, elevation: 2 }}>
        <Text className="text-forest font-semibold mb-3">Add New Quick Reply</Text>
        <TextInput
          className="bg-sand-100 rounded-xl p-3 text-forest mb-2"
          placeholder="Label (e.g. 'Welcome')"
          placeholderTextColor="#7A8F85"
          value={newReplyLabel}
          onChangeText={setNewReplyLabel}
        />
        <TextInput
          className="bg-sand-100 rounded-xl p-3 text-forest mb-3 min-h-[60px]"
          placeholder="Message content..."
          placeholderTextColor="#7A8F85"
          value={newReplyMessage}
          onChangeText={setNewReplyMessage}
          multiline
          textAlignVertical="top"
        />
        <TouchableOpacity
          onPress={() => {
            if (!newReplyLabel.trim() || !newReplyMessage.trim()) return;
            createQuickReply.mutate({ label: newReplyLabel.trim(), message: newReplyMessage.trim() });
          }}
          disabled={createQuickReply.isPending || !newReplyLabel.trim() || !newReplyMessage.trim()}
          className="bg-forest py-3 rounded-xl"
        >
          {createQuickReply.isPending ? (
            <ActivityIndicator color="#D9B168" />
          ) : (
            <Text className="text-gold text-center font-semibold">Add Quick Reply</Text>
          )}
        </TouchableOpacity>
      </View>

      {/* Existing quick replies */}
      {quickRepliesLoading ? (
        [1, 2].map((i) => (
          <View key={i} className="bg-white rounded-2xl p-4 mb-3">
            <Skeleton height={16} width="40%" className="mb-2" />
            <Skeleton height={30} width="100%" />
          </View>
        ))
      ) : quickReplies.length === 0 ? (
        <View className="bg-white rounded-2xl p-6 items-center">
          <Ionicons name="flash-outline" size={32} color="#7A8F85" />
          <Text className="text-moss mt-2">No quick replies yet</Text>
        </View>
      ) : (
        quickReplies.map((reply: any) => (
          <View key={reply.id} className="bg-white rounded-2xl p-4 mb-3" style={{ shadowColor: '#122F26', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.05, shadowRadius: 4, elevation: 2 }}>
            <View className="flex-row items-center justify-between mb-1">
              <View className="bg-gold/20 px-3 py-1 rounded-full">
                <Text className="text-forest font-semibold text-sm">{reply.label || reply.shortcut}</Text>
              </View>
              <TouchableOpacity
                onPress={() => {
                  Alert.alert('Delete', 'Remove this quick reply?', [
                    { text: 'Cancel', style: 'cancel' },
                    { text: 'Delete', style: 'destructive', onPress: () => deleteQuickReply.mutate(reply.id) },
                  ]);
                }}
              >
                <Ionicons name="trash-outline" size={18} color="#EF4444" />
              </TouchableOpacity>
            </View>
            <Text className="text-moss text-sm mt-2">{reply.message || reply.content}</Text>
            {reply.usage_count > 0 && (
              <Text className="text-xs text-moss/60 mt-2">Used {reply.usage_count} times</Text>
            )}
          </View>
        ))
      )}
    </View>
  );

  const tabs = [
    { id: 'settings' as Tab, label: 'Settings', icon: 'settings-outline' },
    { id: 'templates' as Tab, label: 'Templates', icon: 'document-text-outline' },
    { id: 'quick-replies' as Tab, label: 'Quick Replies', icon: 'flash-outline' },
  ];

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="px-4 pb-4"
        style={{ paddingTop: insets.top + 12 }}
      >
        <TouchableOpacity onPress={() => router.back()} className="mb-4">
          <View className="w-10 h-10 rounded-xl items-center justify-center" style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}>
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </View>
        </TouchableOpacity>
        <Text className="text-2xl font-black text-white tracking-tight mb-1">
          Messaging Automation
        </Text>
        <Text className="text-sand-200 text-sm">
          Templates, auto-replies, and quick responses
        </Text>

        {/* Tabs */}
        <View className="flex-row mt-4 bg-white/10 rounded-xl p-1">
          {tabs.map((tab) => (
            <TouchableOpacity
              key={tab.id}
              className={`flex-1 flex-row items-center justify-center py-2.5 rounded-lg ${activeTab === tab.id ? 'bg-gold' : ''}`}
              onPress={() => setActiveTab(tab.id)}
            >
              <Ionicons name={tab.icon as any} size={16} color={activeTab === tab.id ? '#122F26' : '#fff'} />
              <Text className={`ml-1.5 text-xs font-semibold ${activeTab === tab.id ? 'text-forest' : 'text-white'}`}>
                {tab.label}
              </Text>
            </TouchableOpacity>
          ))}
        </View>
      </LinearGradient>

      <ScrollView showsVerticalScrollIndicator={false} contentContainerStyle={{ paddingBottom: insets.bottom + 24 }}>
        {activeTab === 'settings' && renderSettings()}
        {activeTab === 'templates' && renderTemplates()}
        {activeTab === 'quick-replies' && renderQuickReplies()}
      </ScrollView>
    </SafeAreaView>
  );
}
