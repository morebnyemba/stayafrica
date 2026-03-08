import type { Metadata } from 'next';
import { LoginContent } from '@/components/common/login-content';

export const metadata: Metadata = {
    title: 'Traveler Login - StayAfrica',
    description: 'Login to your StayAfrica account and find your next stay',
};

export default function GuestLoginPage() {
    return <LoginContent loginMode="guest" />;
}
