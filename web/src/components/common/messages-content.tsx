'use client';

import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { Send, MessageSquare, Search, Archive, Trash2, Edit2, Check, X, ArrowLeft } from 'lucide-react';
import dynamic from 'next/dynamic';
import { useState, useEffect, useRef } from 'react';
const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });

export function MessagesContent() {
  const [selectedConversation, setSelectedConversation] = useState<any>(null);
  const [messageText, setMessageText] = useState('');
  const [searchQuery, setSearchQuery] = useState('');
  const [editingMessageId, setEditingMessageId] = useState<string | null>(null);
  const [editText, setEditText] = useState('');
  const messagesEndRef = useRef<HTMLDivElement>(null);
  const queryClient = useQueryClient();

  // Fetch conversations
  const { data: conversationsData, isLoading: conversationsLoading } = useQuery({
    queryKey: ['conversations'],
    queryFn: async () => {
      const response = await apiClient.getConversations();
      return response.data;
    },
    refetchInterval: 10000, // Refresh every 10 seconds
  });

  // Fetch messages for selected conversation
  const { data: messagesData, isLoading: messagesLoading } = useQuery({
    queryKey: ['messages', selectedConversation?.id],
    queryFn: async () => {
      if (!selectedConversation?.id) return { results: [] };
      const response = await apiClient.getConversation(selectedConversation.id);
      return response.data;
    },
    enabled: !!selectedConversation?.id,
    refetchInterval: 5000, // Refresh messages every 5 seconds
  });

  // Fetch unread count
  const { data: unreadData } = useQuery({
    queryKey: ['unread-count'],
    queryFn: async () => {
      const response = await apiClient.getTotalUnreadCount();
      return response.data;
    },
    refetchInterval: 10000,
  });

  // Send message mutation
  const sendMessageMutation = useMutation({
    mutationFn: (data: any) => apiClient.sendMessage(data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['messages', selectedConversation?.id] });
      queryClient.invalidateQueries({ queryKey: ['conversations'] });
      setMessageText('');
    },
  });

  // Mark as read mutation
  const markAsReadMutation = useMutation({
    mutationFn: (conversationId: string) => apiClient.markConversationAsRead(conversationId),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['conversations'] });
      queryClient.invalidateQueries({ queryKey: ['unread-count'] });
    },
  });

  // Edit message mutation
  const editMessageMutation = useMutation({
    mutationFn: ({ messageId, text }: { messageId: string; text: string }) => 
      apiClient.editMessage(messageId, text),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['messages', selectedConversation?.id] });
      setEditingMessageId(null);
      setEditText('');
    },
  });

  // Delete message mutation
  const deleteMessageMutation = useMutation({
    mutationFn: (messageId: string) => apiClient.deleteMessage(messageId),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['messages', selectedConversation?.id] });
    },
  });

  // Archive conversation mutation
  const archiveMutation = useMutation({
    mutationFn: (conversationId: string) => apiClient.archiveConversation(conversationId),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['conversations'] });
      setSelectedConversation(null);
    },
  });

  const conversations = conversationsData?.results || [];
  const messages = messagesData?.messages || [];

  // Scroll to bottom when new messages arrive
  useEffect(() => {
    messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' });
  }, [messages]);

  // Mark conversation as read when selected
  useEffect(() => {
    if (selectedConversation?.id) {
      markAsReadMutation.mutate(selectedConversation.id);
    }
  }, [selectedConversation?.id]);

  // Filter conversations by search
  const filteredConversations = conversations.filter((conv: any) => {
    if (!searchQuery) return true;
    const otherParticipant = conv.other_participant?.email || '';
    const subject = conv.subject || '';
    return otherParticipant.toLowerCase().includes(searchQuery.toLowerCase()) ||
           subject.toLowerCase().includes(searchQuery.toLowerCase());
  });

  const handleSendMessage = (e: React.FormEvent) => {
    e.preventDefault();
    if (!messageText.trim() || !selectedConversation) return;

    sendMessageMutation.mutate({
      conversation: selectedConversation.id,
      receiver: selectedConversation.other_participant?.id,
      text: messageText,
      message_type: 'text',
    });
  };

  const handleEditMessage = (messageId: string, currentText: string) => {
    setEditingMessageId(messageId);
    setEditText(currentText);
  };

  const handleSaveEdit = (messageId: string) => {
    if (!editText.trim()) return;
    editMessageMutation.mutate({ messageId, text: editText });
  };

  const handleCancelEdit = () => {
    setEditingMessageId(null);
    setEditText('');
  };

  return (
    <ProtectedRoute>
      <div className="h-screen bg-sand-100 dark:bg-primary-900 flex flex-col">
        <div className="flex-1 flex overflow-hidden">
          {/* Conversations List */}
          <div className={`${selectedConversation ? 'hidden md:flex' : 'flex'} w-full md:w-96 bg-white dark:bg-primary-800 border-r border-primary-200 dark:border-primary-700 flex-col`}>
            {/* Header */}
            <div className="p-4 border-b border-primary-200 dark:border-primary-700">
              <h1 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-3">Messages</h1>
              {unreadData?.unread_count > 0 && (
                <div className="mb-3 px-3 py-2 bg-primary-100 dark:bg-primary-700 rounded-lg text-sm">
                  <span className="font-semibold text-primary-900 dark:text-sand-50">
                    {unreadData.unread_count} unread {unreadData.unread_count === 1 ? 'message' : 'messages'}
                  </span>
                </div>
              )}
              <div className="relative">
                <Search className="absolute left-3 top-1/2 -translate-y-1/2 w-5 h-5 text-primary-400 dark:text-sand-500" />
                <input
                  type="text"
                  placeholder="Search conversations..."
                  value={searchQuery}
                  onChange={(e) => setSearchQuery(e.target.value)}
                  className="w-full pl-10 pr-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg bg-white dark:bg-primary-900 text-primary-900 dark:text-sand-50 focus:ring-2 focus:ring-primary-500 focus:border-primary-500"
                />
              </div>
            </div>

            {/* Conversations List */}
            <div className="flex-1 overflow-y-auto">
              {conversationsLoading ? (
                <div className="p-4 space-y-3">
                  {[1, 2, 3].map(i => (
                    <div key={i} className="animate-pulse h-20 bg-primary-100 dark:bg-primary-700 rounded-lg"></div>
                  ))}
                </div>
              ) : filteredConversations.length === 0 ? (
                <div className="p-8 text-center">
                  <MessageSquare className="w-12 h-12 text-primary-400 dark:text-sand-500 mx-auto mb-3" />
                  <p className="text-primary-600 dark:text-sand-400">
                    {searchQuery ? 'No conversations found' : 'No messages yet'}
                  </p>
                </div>
              ) : (
                <div className="p-2">
                  {filteredConversations.map((conv: any) => (
                    <button
                      key={conv.id}
                      onClick={() => setSelectedConversation(conv)}
                      className={`w-full p-3 rounded-lg mb-2 text-left transition ${
                        selectedConversation?.id === conv.id
                          ? 'bg-primary-100 dark:bg-primary-700'
                          : 'hover:bg-primary-50 dark:hover:bg-primary-800'
                      }`}
                    >
                      <div className="flex items-start gap-3">
                        <div className="w-10 h-10 rounded-full bg-primary-600 text-white flex items-center justify-center font-semibold flex-shrink-0">
                          {conv.other_participant?.email?.[0]?.toUpperCase() || '?'}
                        </div>
                        <div className="flex-1 min-w-0">
                          <div className="flex items-center justify-between mb-1">
                            <span className="font-semibold text-primary-900 dark:text-sand-50 truncate">
                              {conv.other_participant?.email || 'Unknown'}
                            </span>
                            {conv.unread_count > 0 && (
                              <span className="ml-2 px-2 py-0.5 bg-primary-600 text-white text-xs rounded-full flex-shrink-0">
                                {conv.unread_count}
                              </span>
                            )}
                          </div>
                          {conv.subject && (
                            <div className="text-sm text-primary-600 dark:text-sand-400 mb-1 truncate">
                              {conv.subject}
                            </div>
                          )}
                          <div className="text-sm text-primary-500 dark:text-sand-500 truncate">
                            {conv.last_message?.text || 'No messages yet'}
                          </div>
                          <div className="text-xs text-primary-400 dark:text-sand-600 mt-1">
                            {conv.updated_at && new Date(conv.updated_at).toLocaleDateString()}
                          </div>
                        </div>
                      </div>
                    </button>
                  ))}
                </div>
              )}
            </div>
          </div>

          {/* Messages Panel */}
          <div className={`${selectedConversation ? 'flex' : 'hidden md:flex'} flex-1 flex-col bg-white dark:bg-primary-800`}>
            {selectedConversation ? (
              <>
                {/* Conversation Header */}
                <div className="p-4 border-b border-primary-200 dark:border-primary-700 flex items-center justify-between">
                  <div className="flex items-center gap-3">
                    {/* Back button for mobile */}
                    <button
                      onClick={() => setSelectedConversation(null)}
                      className="md:hidden p-2 hover:bg-primary-100 dark:hover:bg-primary-700 rounded-lg transition"
                      title="Back to conversations"
                    >
                      <ArrowLeft className="w-5 h-5 text-primary-600 dark:text-sand-400" />
                    </button>
                    <div className="w-10 h-10 rounded-full bg-primary-600 text-white flex items-center justify-center font-semibold">
                      {selectedConversation.other_participant?.email?.[0]?.toUpperCase() || '?'}
                    </div>
                    <div>
                      <h2 className="font-semibold text-primary-900 dark:text-sand-50">
                        {selectedConversation.other_participant?.email || 'Unknown'}
                      </h2>
                      {selectedConversation.subject && (
                        <p className="text-sm text-primary-600 dark:text-sand-400">
                          {selectedConversation.subject}
                        </p>
                      )}
                    </div>
                  </div>
                  <button
                    onClick={() => archiveMutation.mutate(selectedConversation.id)}
                    className="p-2 hover:bg-primary-100 dark:hover:bg-primary-700 rounded-lg transition"
                    title="Archive conversation"
                  >
                    <Archive className="w-5 h-5 text-primary-600 dark:text-sand-400" />
                  </button>
                </div>

                {/* Messages */}
                <div className="flex-1 overflow-y-auto p-4 space-y-3 bg-gradient-to-b from-primary-50 to-white dark:from-primary-900 dark:to-primary-800">
                  {messagesLoading ? (
                    <div className="space-y-3">
                      {[1, 2, 3].map(i => (
                        <div key={i} className="animate-pulse h-16 bg-primary-100 dark:bg-primary-700 rounded-lg max-w-md"></div>
                      ))}
                    </div>
                  ) : messages.length === 0 ? (
                    <div className="text-center py-12">
                      <MessageSquare className="w-12 h-12 text-primary-400 dark:text-sand-500 mx-auto mb-3" />
                      <p className="text-primary-600 dark:text-sand-400">No messages yet. Start the conversation!</p>
                    </div>
                  ) : (
                    <>
                      {messages.map((message: any, idx: number) => {
                        const isOwnMessage = message.is_own_message;
                        const isEditing = editingMessageId === message.id;
                        const prevMessage = idx > 0 ? messages[idx - 1] : null;
                        const sameUserAsPrev = prevMessage && prevMessage.is_own_message === isOwnMessage;
                        const senderName = isOwnMessage ? 'You' : (selectedConversation.other_participant?.email || 'Unknown');
                        const senderInitial = isOwnMessage 
                          ? '?' 
                          : (selectedConversation.other_participant?.email?.[0]?.toUpperCase() || '?');
                        
                        // Show sender info when changing from one person to another
                        const showSenderInfo = !sameUserAsPrev;
                        
                        // Format time with date if it's a new day
                        const messageTime = new Date(message.created_at);
                        const prevTime = prevMessage ? new Date(prevMessage.created_at) : null;
                        const isNewDay = !prevTime || messageTime.toDateString() !== prevTime.toDateString();

                        return (
                          <div key={message.id}>
                            {isNewDay && (
                              <div className="flex justify-center my-4">
                                <span className="text-xs text-primary-500 dark:text-sand-600 bg-white dark:bg-primary-700 px-3 py-1 rounded-full">
                                  {messageTime.toLocaleDateString([], { weekday: 'short', month: 'short', day: 'numeric' })}
                                </span>
                              </div>
                            )}
                            
                            <div className={`flex gap-3 ${isOwnMessage ? 'justify-end' : 'justify-start'}`}>
                              {/* Avatar - only show for other user and when sender info should display */}
                              {!isOwnMessage && showSenderInfo && (
                                <div className="w-8 h-8 rounded-full bg-primary-600 text-white text-xs flex items-center justify-center font-bold flex-shrink-0">
                                  {senderInitial}
                                </div>
                              )}
                              {!isOwnMessage && !showSenderInfo && (
                                <div className="w-8 h-8" />
                              )}

                              {/* Message Content */}
                              <div className={`flex flex-col max-w-md ${isOwnMessage ? 'items-end' : 'items-start'}`}>
                                {/* Sender name - shown when conversation starts or user changes */}
                                {showSenderInfo && (
                                  <span className={`text-xs font-semibold mb-1 ${
                                    isOwnMessage 
                                      ? 'text-primary-700 dark:text-sand-300' 
                                      : 'text-primary-600 dark:text-sand-400'
                                  }`}>
                                    {senderName}
                                  </span>
                                )}

                                {/* Message Bubble */}
                                {isEditing ? (
                                  <div className="bg-white dark:bg-primary-900 border-2 border-primary-500 rounded-2xl p-3 w-full">
                                    <textarea
                                      value={editText}
                                      onChange={(e) => setEditText(e.target.value)}
                                      className="w-full px-3 py-2 border border-primary-300 dark:border-primary-600 rounded bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50 focus:ring-2 focus:ring-primary-500 focus:border-primary-500 resize-none"
                                      rows={2}
                                      autoFocus
                                    />
                                    <div className="flex gap-2 mt-2 justify-end">
                                      <button
                                        onClick={handleCancelEdit}
                                        className="p-2 hover:bg-primary-100 dark:hover:bg-primary-700 rounded transition"
                                      >
                                        <X className="w-4 h-4" />
                                      </button>
                                      <button
                                        onClick={() => handleSaveEdit(message.id)}
                                        disabled={editMessageMutation.isPending}
                                        className="p-2 bg-primary-600 text-white rounded hover:bg-primary-700 transition disabled:opacity-50"
                                      >
                                        <Check className="w-4 h-4" />
                                      </button>
                                    </div>
                                  </div>
                                ) : (
                                  <div className="group flex flex-col">
                                    <div
                                      className={`px-4 py-2 rounded-2xl shadow-sm transition-shadow ${
                                        isOwnMessage
                                          ? 'bg-primary-600 text-white rounded-br-sm'
                                          : 'bg-primary-100 dark:bg-primary-700 text-primary-900 dark:text-sand-50 rounded-bl-sm'
                                      }`}
                                    >
                                      <p className="whitespace-pre-wrap break-words">{message.text}</p>
                                      {message.edited_at && (
                                        <p className="text-xs mt-1 opacity-70">(edited)</p>
                                      )}
                                    </div>

                                    {/* Time and Actions */}
                                    <div className={`flex items-center gap-2 mt-1.5 text-xs ${
                                      isOwnMessage 
                                        ? 'text-primary-600 dark:text-sand-500 flex-row-reverse' 
                                        : 'text-primary-500 dark:text-sand-600'
                                    }`}>
                                      <span>
                                        {messageTime.toLocaleTimeString([], { 
                                          hour: '2-digit', 
                                          minute: '2-digit' 
                                        })}
                                      </span>
                                      {isOwnMessage && !isEditing && (
                                        <div className="opacity-0 group-hover:opacity-100 transition flex gap-1">
                                          <button
                                            onClick={() => handleEditMessage(message.id, message.text)}
                                            className="p-1 hover:bg-primary-500 hover:text-white rounded transition"
                                            title="Edit message"
                                          >
                                            <Edit2 className="w-3 h-3" />
                                          </button>
                                          <button
                                            onClick={() => {
                                              if (confirm('Delete this message?')) {
                                                deleteMessageMutation.mutate(message.id);
                                              }
                                            }}
                                            className="p-1 hover:bg-red-500 hover:text-white rounded transition"
                                            title="Delete message"
                                          >
                                            <Trash2 className="w-3 h-3" />
                                          </button>
                                        </div>
                                      )}
                                    </div>
                                  </div>
                                )}
                              </div>
                            </div>
                          </div>
                        );
                      })}
                    </>
                  )}
                  <div ref={messagesEndRef} />
                </div>

                {/* Message Input */}
                <form onSubmit={handleSendMessage} className="p-3 sm:p-4 border-t border-primary-200 dark:border-primary-700">
                  <div className="flex gap-2">
                    <input
                      type="text"
                      value={messageText}
                      onChange={(e) => setMessageText(e.target.value)}
                      placeholder="Type a message..."
                      className="flex-1 px-3 sm:px-4 py-2 border border-primary-300 dark:border-primary-600 rounded-lg bg-white dark:bg-primary-900 text-primary-900 dark:text-sand-50 focus:ring-2 focus:ring-primary-500 focus:border-primary-500 text-sm sm:text-base"
                    />
                    <button
                      type="submit"
                      disabled={!messageText.trim() || sendMessageMutation.isPending}
                      className="px-3 sm:px-4 py-2 bg-primary-600 text-white rounded-lg hover:bg-primary-700 transition disabled:opacity-50 disabled:cursor-not-allowed flex items-center gap-2"
                    >
                      <Send className="w-4 h-4 sm:w-5 sm:h-5" />
                      <span className="hidden sm:inline">Send</span>
                    </button>
                  </div>
                </form>
              </>
            ) : (
              <div className="flex-1 flex items-center justify-center">
                <div className="text-center">
                  <MessageSquare className="w-16 h-16 text-primary-400 dark:text-sand-500 mx-auto mb-4" />
                  <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-2">
                    Select a conversation
                  </h3>
                  <p className="text-primary-600 dark:text-sand-400">
                    Choose a conversation from the list to start messaging
                  </p>
                </div>
              </div>
            )}
          </div>
        </div>
      </div>
    </ProtectedRoute>
  );
}
