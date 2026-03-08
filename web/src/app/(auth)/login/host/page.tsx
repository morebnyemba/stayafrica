import type { Metadata } from 'next';
import { LoginContent } from '@/components/common/login-content';

export const metadata: Metadata = {
    title: 'Host Login - StayAfrica',
    description: 'Login to your StayAfrica host account to manage your properties',
};

export default function HostLoginPage() {
    return <LoginContent loginMode="host" />;
}
