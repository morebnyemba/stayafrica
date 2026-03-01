/**
 * Offline banner component.
 * Shows a non-intrusive banner when the device is offline.
 */
import React from 'react';
import { View, Text, StyleSheet, Animated } from 'react-native';
import { useOffline } from '../context/offline-context';
import { useEffect, useRef } from 'react';

export function OfflineBanner() {
  const { isConnected, pendingActions } = useOffline();
  const slideAnim = useRef(new Animated.Value(-50)).current;

  useEffect(() => {
    Animated.timing(slideAnim, {
      toValue: isConnected ? -50 : 0,
      duration: 300,
      useNativeDriver: true,
    }).start();
  }, [isConnected, slideAnim]);

  if (isConnected && pendingActions.length === 0) return null;

  return (
    <Animated.View
      style={[
        styles.container,
        { transform: [{ translateY: slideAnim }] },
        !isConnected ? styles.offline : styles.syncing,
      ]}
    >
      <Text style={styles.text}>
        {!isConnected
          ? '📡 You are offline. Some features may be limited.'
          : `🔄 Syncing ${pendingActions.length} pending action${pendingActions.length > 1 ? 's' : ''}...`}
      </Text>
    </Animated.View>
  );
}

const styles = StyleSheet.create({
  container: {
    position: 'absolute',
    top: 0,
    left: 0,
    right: 0,
    paddingVertical: 8,
    paddingHorizontal: 16,
    zIndex: 1000,
    alignItems: 'center',
  },
  offline: {
    backgroundColor: '#ef4444',
  },
  syncing: {
    backgroundColor: '#f59e0b',
  },
  text: {
    color: '#fff',
    fontSize: 13,
    fontWeight: '600',
    textAlign: 'center',
  },
});
