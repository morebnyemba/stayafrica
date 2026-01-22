import { View, Text, Image, TouchableOpacity } from 'react-native';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';

interface AvatarProps {
  uri?: string | null;
  firstName?: string;
  lastName?: string;
  size?: 'small' | 'medium' | 'large';
  onPress?: () => void;
  showBadge?: boolean;
}

const sizeMap = {
  small: { container: 36, fontSize: 14, iconSize: 18 },
  medium: { container: 48, fontSize: 18, iconSize: 24 },
  large: { container: 72, fontSize: 28, iconSize: 36 },
};

export function Avatar({ 
  uri, 
  firstName, 
  lastName, 
  size = 'medium', 
  onPress,
  showBadge = false,
}: AvatarProps) {
  const dimensions = sizeMap[size];
  
  const getInitials = () => {
    const first = firstName?.charAt(0)?.toUpperCase() || '';
    const last = lastName?.charAt(0)?.toUpperCase() || '';
    return first + last || '?';
  };

  const content = uri ? (
    <Image
      source={{ uri }}
      style={{
        width: dimensions.container,
        height: dimensions.container,
        borderRadius: dimensions.container / 2,
      }}
    />
  ) : firstName || lastName ? (
    <LinearGradient
      colors={['#D9B168', '#bea04f']}
      style={{
        width: dimensions.container,
        height: dimensions.container,
        borderRadius: dimensions.container / 2,
        alignItems: 'center',
        justifyContent: 'center',
      }}
    >
      <Text 
        style={{ 
          color: '#122F26', 
          fontSize: dimensions.fontSize, 
          fontWeight: 'bold' 
        }}
      >
        {getInitials()}
      </Text>
    </LinearGradient>
  ) : (
    <LinearGradient
      colors={['#D9B168', '#bea04f']}
      style={{
        width: dimensions.container,
        height: dimensions.container,
        borderRadius: dimensions.container / 2,
        alignItems: 'center',
        justifyContent: 'center',
      }}
    >
      <Ionicons name="person" size={dimensions.iconSize} color="#122F26" />
    </LinearGradient>
  );

  const containerStyle = {
    width: dimensions.container,
    height: dimensions.container,
    borderRadius: dimensions.container / 2,
    overflow: 'hidden' as 'hidden',
    shadowColor: '#D9B168',
    shadowOffset: { width: 0, height: 2 },
    shadowOpacity: 0.3,
    shadowRadius: 4,
    elevation: 4,
  };

  if (onPress) {
    return (
      <TouchableOpacity onPress={onPress} style={containerStyle}>
        {content}
        {showBadge && (
          <View 
            style={{
              position: 'absolute',
              top: 0,
              right: 0,
              width: 12,
              height: 12,
              borderRadius: 6,
              backgroundColor: '#22c55e',
              borderWidth: 2,
              borderColor: '#fff',
            }}
          />
        )}
      </TouchableOpacity>
    );
  }

  return (
    <View style={containerStyle}>
      {content}
      {showBadge && (
        <View 
          style={{
            position: 'absolute',
            top: 0,
            right: 0,
            width: 12,
            height: 12,
            borderRadius: 6,
            backgroundColor: '#22c55e',
            borderWidth: 2,
            borderColor: '#fff',
          }}
        />
      )}
    </View>
  );
}
