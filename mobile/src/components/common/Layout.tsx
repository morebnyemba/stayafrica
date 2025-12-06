import { View, Text, TouchableOpacity } from 'react-native';
import { Ionicons } from '@expo/vector-icons';

interface HeaderProps {
  title: string;
  showBack?: boolean;
  onBackPress?: () => void;
  rightComponent?: React.ReactNode;
}

export function Header({ title, showBack, onBackPress, rightComponent }: HeaderProps) {
  return (
    <View className="flex-row items-center justify-between px-4 py-4 bg-white border-b border-gray-200">
      {showBack ? (
        <TouchableOpacity onPress={onBackPress} className="mr-4">
          <Ionicons name="chevron-back" size={24} color="#0ea5e9" />
        </TouchableOpacity>
      ) : (
        <View style={{ width: 24 }} />
      )}

      <Text className="text-xl font-bold flex-1 text-center">{title}</Text>

      {rightComponent || <View style={{ width: 24 }} />}
    </View>
  );
}

export function SafeAreaContainer({ children }: { children: React.ReactNode }) {
  return (
    <View className="flex-1 bg-white">
      <View className="pt-12" />
      {children}
    </View>
  );
}
