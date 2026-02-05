import { View, ViewProps } from 'react-native';
import Animated, { FadeInDown } from 'react-native-reanimated';

interface CardProps extends ViewProps {
  variant?: 'default' | 'elevated' | 'outlined' | 'gradient';
  padding?: 'none' | 'sm' | 'md' | 'lg';
  animated?: boolean;
  animationDelay?: number;
  children: React.ReactNode;
  className?: string;
}

export function Card({
  variant = 'default',
  padding = 'md',
  animated = false,
  animationDelay = 0,
  children,
  className = '',
  ...viewProps
}: CardProps) {
  const paddingClasses = {
    none: '',
    sm: 'p-3',
    md: 'p-4',
    lg: 'p-6',
  };

  const variantStyles = {
    default: {
      className: 'bg-white rounded-2xl',
      style: {
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 2 },
        shadowOpacity: 0.05,
        shadowRadius: 4,
        elevation: 2,
      },
    },
    elevated: {
      className: 'bg-white rounded-3xl',
      style: {
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 6 },
        shadowOpacity: 0.12,
        shadowRadius: 12,
        elevation: 6,
      },
    },
    outlined: {
      className: 'bg-white rounded-2xl border-2 border-sand-200',
      style: {},
    },
    gradient: {
      className: 'rounded-2xl overflow-hidden',
      style: {
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 4 },
        shadowOpacity: 0.1,
        shadowRadius: 8,
        elevation: 4,
      },
    },
  };

  const { className: variantClassName, style: variantStyle } = variantStyles[variant];
  const combinedClassName = `${variantClassName} ${paddingClasses[padding]} ${className}`;

  if (animated) {
    return (
      <Animated.View
        entering={FadeInDown.delay(animationDelay).duration(400).springify()}
        className={combinedClassName}
        style={variantStyle}
        {...viewProps}
      >
        {children}
      </Animated.View>
    );
  }

  return (
    <View className={combinedClassName} style={variantStyle} {...viewProps}>
      {children}
    </View>
  );
}
