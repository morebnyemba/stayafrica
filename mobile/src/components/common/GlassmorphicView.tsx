import React, { ReactNode } from 'react';
import { View, ViewStyle, StyleProp } from 'react-native';
import { BlurView } from 'expo-blur';

interface GlassmorphicViewProps {
  children: ReactNode;
  intensity?: number;
  tint?: 'light' | 'dark' | 'default';
  className?: string;
  style?: StyleProp<ViewStyle>;
  borderRadius?: number;
}

/**
 * A glassmorphic effect component using blur and transparency
 * Perfect for modern UI overlays, cards, and headers
 */
export function GlassmorphicView({
  children,
  intensity = 20,
  tint = 'light',
  className,
  style,
  borderRadius = 16,
}: GlassmorphicViewProps) {
  return (
    <BlurView
      intensity={intensity}
      tint={tint}
      className={className}
      style={[
        {
          borderRadius,
          overflow: 'hidden',
          backgroundColor: tint === 'dark' 
            ? 'rgba(0, 0, 0, 0.2)' 
            : 'rgba(255, 255, 255, 0.2)',
        },
        style,
      ]}
    >
      {children}
    </BlurView>
  );
}
