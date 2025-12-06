module.exports = {
  preset: 'jest-preset-expo',
  testPathIgnorePatterns: [
    '<rootDir>/node_modules',
    '<rootDir>/.expo',
  ],
  transformIgnorePatterns: [
    'node_modules/(?!(react-native|expo|expo-router|react-native-maps|react-native-gesture-handler|react-native-reanimated|react-native-screens|react-native-safe-area-context)/)',
  ],
  setupFilesAfterEnv: ['<rootDir>/jest.setup.js'],
  testEnvironment: 'node',
  collectCoverageFrom: [
    'src/**/*.{ts,tsx}',
    '!src/**/*.d.ts',
    '!src/**/*.stories.tsx',
  ],
};
