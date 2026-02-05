import { View, Text } from 'react-native';

interface DividerProps {
  label?: string;
  spacing?: 'sm' | 'md' | 'lg';
  thickness?: 'thin' | 'medium' | 'thick';
  color?: 'light' | 'medium' | 'dark';
  className?: string;
}

export function Divider({
  label,
  spacing = 'md',
  thickness = 'thin',
  color = 'light',
  className = '',
}: DividerProps) {
  const spacingClasses = {
    sm: 'my-2',
    md: 'my-4',
    lg: 'my-6',
  };

  const thicknessClasses = {
    thin: 'h-px',
    medium: 'h-0.5',
    thick: 'h-1',
  };

  const colorClasses = {
    light: 'bg-sand-200',
    medium: 'bg-sand-300',
    dark: 'bg-sand-400',
  };

  if (label) {
    return (
      <View className={`flex-row items-center ${spacingClasses[spacing]} ${className}`}>
        <View className={`flex-1 ${thicknessClasses[thickness]} ${colorClasses[color]}`} />
        <Text className="mx-4 text-sm font-medium text-moss">{label}</Text>
        <View className={`flex-1 ${thicknessClasses[thickness]} ${colorClasses[color]}`} />
      </View>
    );
  }

  return (
    <View
      className={`${thicknessClasses[thickness]} ${colorClasses[color]} ${spacingClasses[spacing]} ${className}`}
    />
  );
}
