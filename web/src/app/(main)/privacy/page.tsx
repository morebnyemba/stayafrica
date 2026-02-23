import type { Metadata } from 'next';

export const metadata: Metadata = {
    title: 'Privacy Policy',
    description: 'Learn how StayAfrica collects, uses, and protects your data.',
};

export default function PrivacyPage() {
    return (
        <div className="max-w-4xl mx-auto px-4 py-12 md:py-20">
            <h1 className="text-3xl md:text-5xl font-black text-primary-900 dark:text-sand-100 mb-6 tracking-tight">
                Privacy Policy
            </h1>
            <p className="text-sm text-primary-500 dark:text-sand-400 mb-10">Last updated: {new Date().toLocaleDateString('en-US', { month: 'long', year: 'numeric' })}</p>

            <div className="prose prose-sand dark:prose-invert max-w-none">
                <section className="mb-8">
                    <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-100 mb-4">1. Information We Collect</h2>
                    <p className="text-primary-700 dark:text-sand-300 leading-relaxed mb-4">
                        We collect information you provide directly to us when registering for an account, including your name, email address, phone number, and profile picture. We also collect payment information when you make a booking, though full card details are processed by our secure payment partners and never stored on our servers.
                    </p>
                </section>

                <section className="mb-8">
                    <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-100 mb-4">2. How We Use Your Information</h2>
                    <p className="text-primary-700 dark:text-sand-300 leading-relaxed mb-4">
                        We use your personal information to:
                    </p>
                    <ul className="list-disc pl-5 text-primary-700 dark:text-sand-300 space-y-2 mb-4">
                        <li>Provide, maintain, and improve the StayAfrica platform</li>
                        <li>Process transactions and send related information (e.g., confirmations, receipts)</li>
                        <li>Send technical notices, updates, security alerts, and support messages</li>
                        <li>Respond to your comments, questions, and requests</li>
                        <li>Communicate with you about products, services, offers, and events</li>
                    </ul>
                </section>

                <section className="mb-8">
                    <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-100 mb-4">3. Sharing of Information</h2>
                    <p className="text-primary-700 dark:text-sand-300 leading-relaxed mb-4">
                        We may share your information with third-party vendors, consultants, and other service providers who need access to such information to carry out work on our behalf. When you book a property, we share necessary information with your host to facilitate the stay.
                    </p>
                </section>

                <section className="mb-8">
                    <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-100 mb-4">4. Data Security</h2>
                    <p className="text-primary-700 dark:text-sand-300 leading-relaxed mb-4">
                        We implement appropriate technical and organizational security measures to protect your personal information against unauthorized access, loss, or misuse. However, no internet transmission is entirely secure, and we cannot guarantee absolute security.
                    </p>
                </section>

                <section className="mb-8">
                    <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-100 mb-4">5. Your Rights</h2>
                    <p className="text-primary-700 dark:text-sand-300 leading-relaxed mb-4">
                        Depending on your location, you may have the right to access, update, delete, or restrict the processing of your personal data. You can manage your account information from your Profile settings.
                    </p>
                </section>

                <section className="mb-8">
                    <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-100 mb-4">6. Contact Us</h2>
                    <p className="text-primary-700 dark:text-sand-300 leading-relaxed mb-4">
                        If you have any questions or concerns about this Privacy Policy or our data practices, please contact our Data Protection Officer at <a href="mailto:privacy@stayafrica.app" className="text-accent-600 dark:text-accent-400 hover:underline">privacy@stayafrica.app</a>.
                    </p>
                </section>
            </div>
        </div>
    );
}
