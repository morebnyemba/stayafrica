import { View, Text } from 'react-native';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';

interface BadgeProps {
  label: string;
  variant?: 'success' | 'warning' | 'error' | 'info' | 'primary' | 'secondary';
  size?: 'sm' | 'md' | 'lg';
  icon?: keyof typeof Ionicons.glyphMap;
  withGradient?: boolean;
  className?: string;
}

export function Badge({
  label,
  variant = 'primary',
  size = 'md',
  icon,
  withGradient = false,
  className = '',
}: BadgeProps) {
  const sizeClasses = {
    sm: 'px-2 py-1',
    md: 'px-3 py-1.5',
    lg: 'px-4 py-2',
  };

  const textSizeClasses = {
    sm: 'text-xs',
    md: 'text-sm',
    lg: 'text-base',
  };

  const iconSizes = {
    sm: 12,
    md: 14,
    lg: 16,
  };

  const variantConfig = {
    success: {
      bg: 'bg-green-100',
      text: 'text-green-800',
      iconColor: '#059669',
      gradient: ['#10B981', '#059669'],
    },
    warning: {
      bg: 'bg-yellow-100',
      text: 'text-yellow-800',
      iconColor: '#D97706',
      gradient: ['#F59E0B', '#D97706'],
    },
    error: {
      bg: 'bg-red-100',
      text: 'text-red-800',
      iconColor: '#DC2626',
      gradient: ['#EF4444', '#DC2626'],
    },
    info: {
      bg: 'bg-blue-100',
      text: 'text-blue-800',
      iconColor: '#2563EB',
      gradient: ['#3B82F6', '#2563EB'],
    },
    primary: {
      bg: 'bg-primary-100',
      text: 'text-primary-900',
      iconColor: '#122F26',
      gradient: ['#3A5C50', '#2d4a40'],
    },
    secondary: {
      bg: 'bg-secondary-100',
      text: 'text-secondary-900',
      iconColor: '#4c3a18',
      gradient: ['#D9B168', '#bea04f'],
    },
  };

  const config = variantConfig[variant];

  if (withGradient) {
    return (
      <LinearGradient
        colors={config.gradient}
        className={`rounded-full ${sizeClasses[size]} flex-row items-center ${className}`}
        style={{
          shadowColor: config.iconColor,
          shadowOffset: { width: 0, height: 2 },
          shadowOpacity: 0.2,
          shadowRadius: 3,
          elevation: 2,
        }}
      >
        {icon && (
          <Ionicons
            name={icon}
            size={iconSizes[size]}
            color="#ffffff"
            style={{ marginRight: label ? 4 : 0 }}
          />
        )}
        {label && (
          <Text className={`font-bold ${textSizeClasses[size]} text-white`}>
            {label}
          </Text>
        )}
      </LinearGradient>
    );
  }

  return (
    <View
      className={`${config.bg} rounded-full ${sizeClasses[size]} flex-row items-center ${className}`}
    >
      {icon && (
        <Ionicons
          name={icon}
          size={iconSizes[size]}
          color={config.iconColor}
          style={{ marginRight: label ? 4 : 0 }}
        />
      )}
      {label && (
        <Text className={`font-bold ${textSizeClasses[size]} ${config.text}`}>
          {label}
        </Text>
      )}
    </View>
  );
}
