import React from 'react';
import { Modal, View, Text, TouchableOpacity } from 'react-native';

export interface AppDialogAction {
  label: string;
  onPress?: () => void;
  variant?: 'default' | 'destructive';
}

interface AppDialogProps {
  visible: boolean;
  title: string;
  message: string;
  primaryAction: AppDialogAction;
  secondaryAction?: AppDialogAction;
  onRequestClose: () => void;
}

export function AppDialog({
  visible,
  title,
  message,
  primaryAction,
  secondaryAction,
  onRequestClose,
}: AppDialogProps) {
  const handlePrimary = () => {
    onRequestClose();
    primaryAction.onPress?.();
  };

  const handleSecondary = () => {
    onRequestClose();
    secondaryAction?.onPress?.();
  };

  return (
    <Modal
      visible={visible}
      transparent
      animationType="fade"
      onRequestClose={onRequestClose}
    >
      <View className="flex-1 bg-black/50 items-center justify-center px-6">
        <View className="w-full max-w-sm bg-white rounded-2xl p-5">
          <Text className="text-lg font-bold text-gray-900 mb-2">{title}</Text>
          <Text className="text-sm text-gray-600 mb-5">{message}</Text>
          <View className="flex-row justify-end" style={{ gap: 10 }}>
            {secondaryAction ? (
              <TouchableOpacity
                onPress={handleSecondary}
                className="px-4 py-2 rounded-lg border border-gray-300"
              >
                <Text className="text-gray-700 font-semibold">{secondaryAction.label}</Text>
              </TouchableOpacity>
            ) : null}
            <TouchableOpacity
              onPress={handlePrimary}
              className={`px-4 py-2 rounded-lg ${primaryAction.variant === 'destructive' ? 'bg-red-600' : 'bg-emerald-600'}`}
            >
              <Text className="text-white font-semibold">{primaryAction.label}</Text>
            </TouchableOpacity>
          </View>
        </View>
      </View>
    </Modal>
  );
}
