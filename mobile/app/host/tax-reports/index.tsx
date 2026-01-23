import { View, Text, ScrollView, TouchableOpacity, Platform } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';

export default function HostTaxReportsScreen() {
  const router = useRouter();
  const { isAuthenticated, user } = useAuth();

  if (!isAuthenticated) {
    return (
      <View className="flex-1 bg-sand-100">
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
        >
          <TouchableOpacity onPress={() => router.back()} className="mb-4">
            <View className="w-10 h-10 rounded-xl items-center justify-center" style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}>
              <Ionicons name="arrow-back" size={24} color="#fff" />
            </View>
          </TouchableOpacity>
          <Text className="text-3xl font-black text-white tracking-tight">
            Tax Reports
          </Text>
        </LinearGradient>
        <View className="flex-1 items-center justify-center px-6">
          <Text className="text-forest text-lg">Please sign in to view tax reports</Text>
        </View>
      </View>
    );
  }

  // Sample tax reports (empty state)
  const taxReports: any[] = [];
  const currentYear = new Date().getFullYear();

  const TaxReportCard = ({ year, status, totalEarnings, downloadUrl }: any) => (
    <TouchableOpacity
      className="bg-white rounded-2xl p-4 mb-3 flex-row items-center"
      onPress={() => downloadUrl && {}}
      style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 2 },
        shadowOpacity: 0.05,
        shadowRadius: 4,
        elevation: 2,
      }}
    >
      <LinearGradient
        colors={['#14B8A620', '#14B8A610']}
        className="w-14 h-14 rounded-2xl items-center justify-center"
      >
        <Ionicons name="document-text" size={28} color="#14B8A6" />
      </LinearGradient>
      <View className="flex-1 ml-4">
        <Text className="text-lg font-bold text-forest">{year} Tax Summary</Text>
        <Text className="text-sm text-moss mt-1">Total Earnings: ${totalEarnings || '0.00'}</Text>
        <View className={`px-2 py-1 rounded-full mt-2 self-start ${
          status === 'ready' ? 'bg-green-100' : 'bg-yellow-100'
        }`}>
          <Text className={`text-xs font-semibold ${
            status === 'ready' ? 'text-green-700' : 'text-yellow-700'
          }`}>
            {status === 'ready' ? 'Ready to Download' : 'Pending'}
          </Text>
        </View>
      </View>
      <Ionicons name="download-outline" size={24} color="#14B8A6" />
    </TouchableOpacity>
  );

  return (
    <ScrollView className="flex-1 bg-sand-100" showsVerticalScrollIndicator={false}>
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="px-4 pb-8"
        style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
      >
        <TouchableOpacity onPress={() => router.back()} className="mb-4">
          <View className="w-10 h-10 rounded-xl items-center justify-center" style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}>
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </View>
        </TouchableOpacity>
        <Text className="text-3xl font-black text-white tracking-tight mb-2">
          Tax Reports
        </Text>
        <View className="flex-row items-center">
          <Ionicons name="receipt" size={16} color="#D9B168" />
          <Text className="text-sand-100 ml-2">
            View and download your tax documents
          </Text>
        </View>
      </LinearGradient>

      <View className="px-4 -mt-4">
        {/* Current Year Summary */}
        <LinearGradient
          colors={['#14B8A6', '#0D9488']}
          start={{ x: 0, y: 0 }}
          end={{ x: 1, y: 1 }}
          className="rounded-2xl p-5 mb-4"
          style={{
            shadowColor: '#14B8A6',
            shadowOffset: { width: 0, height: 8 },
            shadowOpacity: 0.25,
            shadowRadius: 12,
            elevation: 8,
          }}
        >
          <View className="flex-row items-center mb-4">
            <View className="bg-white/20 rounded-full p-2">
              <Ionicons name="calendar" size={24} color="#fff" />
            </View>
            <Text className="text-white text-xl font-bold ml-3">{currentYear} Summary</Text>
          </View>
          <View className="flex-row justify-between">
            <View>
              <Text className="text-white/70 text-sm">Total Earnings</Text>
              <Text className="text-white text-2xl font-black">$0.00</Text>
            </View>
            <View className="items-end">
              <Text className="text-white/70 text-sm">Transactions</Text>
              <Text className="text-white text-2xl font-black">0</Text>
            </View>
          </View>
        </LinearGradient>

        {/* Tax Documents */}
        <Text className="text-lg font-bold text-forest mb-3 mt-4">Available Reports</Text>

        {taxReports.length === 0 ? (
          <View className="bg-white rounded-2xl p-8 items-center" style={{
            shadowColor: '#122F26',
            shadowOffset: { width: 0, height: 4 },
            shadowOpacity: 0.08,
            shadowRadius: 8,
            elevation: 4,
          }}>
            <View className="bg-sand-200 rounded-full p-6 mb-4">
              <Ionicons name="document-text-outline" size={40} color="#3A5C50" />
            </View>
            <Text className="text-forest font-semibold text-lg">No Tax Reports Available</Text>
            <Text className="text-moss text-sm mt-2 text-center">
              Tax reports will be generated at the end of each tax year
            </Text>
          </View>
        ) : (
          taxReports.map((report, index) => (
            <TaxReportCard key={index} {...report} />
          ))
        )}

        {/* Tax Info Section */}
        <Text className="text-lg font-bold text-forest mb-3 mt-6">Tax Information</Text>

        <View className="bg-white rounded-2xl p-4 mb-3" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 2 },
          shadowOpacity: 0.05,
          shadowRadius: 4,
          elevation: 2,
        }}>
          <TouchableOpacity className="flex-row items-center justify-between py-3 border-b border-sand-100">
            <View className="flex-row items-center">
              <Ionicons name="person-outline" size={20} color="#3A5C50" />
              <Text className="text-forest font-medium ml-3">Tax Identification</Text>
            </View>
            <View className="flex-row items-center">
              <Text className="text-moss mr-2">Not Set</Text>
              <Ionicons name="chevron-forward" size={20} color="#3A5C50" />
            </View>
          </TouchableOpacity>

          <TouchableOpacity className="flex-row items-center justify-between py-3 border-b border-sand-100">
            <View className="flex-row items-center">
              <Ionicons name="location-outline" size={20} color="#3A5C50" />
              <Text className="text-forest font-medium ml-3">Tax Residence</Text>
            </View>
            <View className="flex-row items-center">
              <Text className="text-moss mr-2">{user?.country_of_residence || 'Not Set'}</Text>
              <Ionicons name="chevron-forward" size={20} color="#3A5C50" />
            </View>
          </TouchableOpacity>

          <TouchableOpacity className="flex-row items-center justify-between py-3">
            <View className="flex-row items-center">
              <Ionicons name="business-outline" size={20} color="#3A5C50" />
              <Text className="text-forest font-medium ml-3">Business Information</Text>
            </View>
            <View className="flex-row items-center">
              <Text className="text-moss mr-2">Not Set</Text>
              <Ionicons name="chevron-forward" size={20} color="#3A5C50" />
            </View>
          </TouchableOpacity>
        </View>

        {/* Disclaimer */}
        <View className="bg-yellow-50 rounded-2xl p-4 mb-8 mt-4">
          <View className="flex-row items-start">
            <Ionicons name="information-circle" size={20} color="#D97706" />
            <View className="flex-1 ml-3">
              <Text className="text-yellow-800 font-semibold mb-1">Tax Disclaimer</Text>
              <Text className="text-yellow-700 text-sm">
                The information provided here is for informational purposes only and should not be considered tax advice. Please consult a qualified tax professional for guidance specific to your situation.
              </Text>
            </View>
          </View>
        </View>
      </View>
    </ScrollView>
  );
}
