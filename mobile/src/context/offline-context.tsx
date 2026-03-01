/**
 * Network/Offline context for StayAfrica mobile app.
 * Tracks connectivity state and provides offline-aware hooks.
 */
import React, { createContext, useContext, useEffect, useState, useCallback, useRef } from 'react';
import NetInfo, { NetInfoState } from '@react-native-community/netinfo';
import AsyncStorage from '@react-native-async-storage/async-storage';

interface OfflineContextType {
  isConnected: boolean;
  isInternetReachable: boolean | null;
  connectionType: string | null;
  pendingActions: PendingAction[];
  addPendingAction: (action: PendingAction) => void;
  clearPendingActions: () => void;
  syncPendingActions: () => Promise<void>;
}

export interface PendingAction {
  id: string;
  type: 'booking' | 'review' | 'message' | 'profile';
  action: string;
  payload: Record<string, unknown>;
  createdAt: string;
}

const PENDING_ACTIONS_KEY = '@stayafrica_pending_actions';

const OfflineContext = createContext<OfflineContextType>({
  isConnected: true,
  isInternetReachable: true,
  connectionType: null,
  pendingActions: [],
  addPendingAction: () => {},
  clearPendingActions: () => {},
  syncPendingActions: async () => {},
});

export function OfflineProvider({ children }: { children: React.ReactNode }) {
  const [isConnected, setIsConnected] = useState(true);
  const [isInternetReachable, setIsInternetReachable] = useState<boolean | null>(true);
  const [connectionType, setConnectionType] = useState<string | null>(null);
  const [pendingActions, setPendingActions] = useState<PendingAction[]>([]);
  const wasDisconnected = useRef(false);

  // Load pending actions from storage on mount
  useEffect(() => {
    AsyncStorage.getItem(PENDING_ACTIONS_KEY).then((data) => {
      if (data) {
        try {
          setPendingActions(JSON.parse(data));
        } catch {
          // Corrupted data, clear it
          AsyncStorage.removeItem(PENDING_ACTIONS_KEY);
        }
      }
    });
  }, []);

  // Subscribe to network state changes
  useEffect(() => {
    const unsubscribe = NetInfo.addEventListener((state: NetInfoState) => {
      const connected = state.isConnected ?? false;
      setIsConnected(connected);
      setIsInternetReachable(state.isInternetReachable);
      setConnectionType(state.type);

      // If we just reconnected, attempt to sync
      if (connected && wasDisconnected.current) {
        wasDisconnected.current = false;
        // Auto-sync will be triggered by the effect below
      }

      if (!connected) {
        wasDisconnected.current = true;
      }
    });

    return () => unsubscribe();
  }, []);

  // Save pending actions to storage whenever they change
  useEffect(() => {
    AsyncStorage.setItem(PENDING_ACTIONS_KEY, JSON.stringify(pendingActions));
  }, [pendingActions]);

  const addPendingAction = useCallback((action: PendingAction) => {
    setPendingActions((prev) => [...prev, action]);
  }, []);

  const clearPendingActions = useCallback(() => {
    setPendingActions([]);
    AsyncStorage.removeItem(PENDING_ACTIONS_KEY);
  }, []);

  const syncPendingActions = useCallback(async () => {
    if (!isConnected || pendingActions.length === 0) return;

    const remaining: PendingAction[] = [];

    for (const action of pendingActions) {
      try {
        // Import api client dynamically to avoid circular deps
        const { apiClient } = require('../services/api-client');
        await apiClient.post(`/api/v1/${action.type}s/${action.action}/`, action.payload);
      } catch {
        // Keep failed actions for retry
        remaining.push(action);
      }
    }

    setPendingActions(remaining);
  }, [isConnected, pendingActions]);

  // Auto-sync when connection is restored
  useEffect(() => {
    if (isConnected && pendingActions.length > 0) {
      syncPendingActions();
    }
  }, [isConnected]); // eslint-disable-line react-hooks/exhaustive-deps

  return (
    <OfflineContext.Provider
      value={{
        isConnected,
        isInternetReachable,
        connectionType,
        pendingActions,
        addPendingAction,
        clearPendingActions,
        syncPendingActions,
      }}
    >
      {children}
    </OfflineContext.Provider>
  );
}

export function useOffline() {
  return useContext(OfflineContext);
}
