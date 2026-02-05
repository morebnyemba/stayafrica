import { TouchableOpacity, Text, ActivityIndicator, View } from 'react-native';
import { LinearGradient } from 'expo-linear-gradient';
import { Ionicons } from '@expo/vector-icons';
import Animated, { useAnimatedStyle, useSharedValue, withSpring } from 'react-native-reanimated';

interface ButtonProps {
  title: string;
  onPress: () => void;
  variant?: 'primary' | 'secondary' | 'outline' | 'ghost' | 'danger';
  size?: 'sm' | 'md' | 'lg';
  loading?: boolean;
  disabled?: boolean;
  icon?: keyof typeof Ionicons.glyphMap;
  iconPosition?: 'left' | 'right';
  fullWidth?: boolean;
  className?: string;
}

const AnimatedTouchable = Animated.createAnimatedComponent(TouchableOpacity);

export function Button({
  title,
  onPress,
  variant = 'primary',
  size = 'md',
  loading = false,
  disabled = false,
  icon,
  iconPosition = 'left',
  fullWidth = false,
  className = '',
}: ButtonProps) {
  const scale = useSharedValue(1);

  const animatedStyle = useAnimatedStyle(() => ({
    transform: [{ scale: scale.value }],
  }));

  const handlePressIn = () => {
    scale.value = withSpring(0.96, { damping: 15, stiffness: 200 });
  };

  const handlePressOut = () => {
    scale.value = withSpring(1, { damping: 15, stiffness: 200 });
  };

  const sizeClasses = {
    sm: 'px-4 py-2',
    md: 'px-6 py-3.5',
    lg: 'px-8 py-4',
  };

  const textSizeClasses = {
    sm: 'text-sm',
    md: 'text-base',
    lg: 'text-lg',
  };

  const iconSizes = {
    sm: 18,
    md: 20,
    lg: 24,
  };

  const isDisabled = disabled || loading;

  // Gradient buttons
  if (variant === 'primary' || variant === 'secondary' || variant === 'danger') {
    const gradientColors = {
      primary: ['#D9B168', '#bea04f'],
      secondary: ['#3A5C50', '#2d4a40'],
      danger: ['#EF4444', '#DC2626'],
    };

    return (
      <AnimatedTouchable
        onPress={onPress}
        onPressIn={handlePressIn}
        onPressOut={handlePressOut}
        disabled={isDisabled}
        style={[
          animatedStyle,
          { opacity: isDisabled ? 0.6 : 1 },
        ]}
        className={`${fullWidth ? 'w-full' : ''} ${className}`}
      >
        <LinearGradient
          colors={gradientColors[variant]}
          className={`rounded-2xl ${sizeClasses[size]} flex-row items-center justify-center`}
          style={{
            shadowColor: variant === 'primary' ? '#D9B168' : '#122F26',
            shadowOffset: { width: 0, height: 4 },
            shadowOpacity: 0.3,
            shadowRadius: 8,
            elevation: 5,
          }}
        >
          {loading ? (
            <ActivityIndicator
              size="small"
              color={variant === 'primary' ? '#122F26' : '#ffffff'}
            />
          ) : (
            <>
              {icon && iconPosition === 'left' && (
                <Ionicons
                  name={icon}
                  size={iconSizes[size]}
                  color={variant === 'primary' ? '#122F26' : '#ffffff'}
                  style={{ marginRight: 8 }}
                />
              )}
              <Text
                className={`font-bold ${textSizeClasses[size]} ${
                  variant === 'primary' ? 'text-forest' : 'text-white'
                }`}
              >
                {title}
              </Text>
              {icon && iconPosition === 'right' && (
                <Ionicons
                  name={icon}
                  size={iconSizes[size]}
                  color={variant === 'primary' ? '#122F26' : '#ffffff'}
                  style={{ marginLeft: 8 }}
                />
              )}
            </>
          )}
        </LinearGradient>
      </AnimatedTouchable>
    );
  }

  // Outline and ghost variants
  const baseClasses = `rounded-2xl ${sizeClasses[size]} flex-row items-center justify-center ${
    variant === 'outline' ? 'border-2 border-primary-800 bg-transparent' : 'bg-transparent'
  }`;

  const textColor = variant === 'outline' ? 'text-primary-800' : 'text-moss';

  return (
    <AnimatedTouchable
      onPress={onPress}
      onPressIn={handlePressIn}
      onPressOut={handlePressOut}
      disabled={isDisabled}
      style={[
        animatedStyle,
        { opacity: isDisabled ? 0.6 : 1 },
      ]}
      className={`${baseClasses} ${fullWidth ? 'w-full' : ''} ${className}`}
    >
      {loading ? (
        <ActivityIndicator size="small" color="#3A5C50" />
      ) : (
        <>
          {icon && iconPosition === 'left' && (
            <Ionicons
              name={icon}
              size={iconSizes[size]}
              color="#3A5C50"
              style={{ marginRight: 8 }}
            />
          )}
          <Text className={`font-bold ${textSizeClasses[size]} ${textColor}`}>
            {title}
          </Text>
          {icon && iconPosition === 'right' && (
            <Ionicons
              name={icon}
              size={iconSizes[size]}
              color="#3A5C50"
              style={{ marginLeft: 8 }}
            />
          )}
        </>
      )}
    </AnimatedTouchable>
  );
}
